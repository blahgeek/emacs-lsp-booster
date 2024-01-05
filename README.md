# Emacs lsp-mode performance booster

Improve performance of [lsp-mode](https://github.com/emacs-lsp/lsp-mode) using a wrapper executable.

## Background & Prior work

(Huge thanks to @yyoncho for both maintaining lsp-mode and giving me the inspiration of this project).

According to [yyoncho](https://www.reddit.com/r/emacs/comments/ymrkyn/comment/iv90q4i/?utm_source=share&utm_medium=web2x&context=3),
the are several performance issues related to lsp-mode:

1. Json parsing in Emacs is slow
2. The server may block on sending data to emacs when the buffer is full, because Emacs may consume the data too slo
3. Similarly, Emacs may block on sending data to the server (hence block Emacs UI) when the buffer is full, because the server may be busy

@yyoncho tried to solve these issues by implementing [native async non-blocking jsonrpc](https://github.com/emacs-lsp/emacs).
The result is very good regarding performance. However it requires modifications in Emacs source code and it seems unlikely that those changes would be merged into upstream.
Also, frankly I doubt that this would be well-maintained in the future since it also requires seperated code path in lsp-mode (I myself encountered some [issues](https://github.com/emacs-lsp/emacs/issues/12)).

## How this project work

This project provides an wrapper executable around lsp servers to work around above-mentioned issues:

- It converts json responses from server into **elisp bytecode** (in text representation) for Emacs to read.
    * e.g. `{"objs":[{"a":1},{"a":2}]}` would be converted to `#[0 "\301\302\300\303D\300\304D\"D\207" [:a :objs vector 1 2] 13]`
    * This would improve the message parsing performance in Emacs by ~4x for large json objects, see benchmark result [here](https://github.com/blahgeek/emacs-lsp-booster/actions/runs/7416840025/job/20182439682#step:5:142)
    * Although Emacs still needs to parse the text representation and interpret it into elisp object, the performance gain mainly comes from:
        * Parsing (`read`ing) elisp object is apparently better optimized and simpler in Emacs
        * Using bytecode to construct objects, we can eliminate duplicated objects (e.g. the "a" json key in above example)
- It separates reading and writing into different threads and keeps pending messages in internal buffers, to avoid blocking on IO, which solves above-mentioned issue (2) and (3).

Overall, this achieves similar result as the native async non-blocking jsonrpc approach without requiring modifications in Emacs source code.


## How to use

Prerequisite:

1. Emacs 28 or newer (NOT the [native-jsonrpc custom version](https://github.com/emacs-lsp/emacs)), recent version of lsp-mode
2. **Use [plist for deserialization](https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization) for lsp-mode**

Steps:

1. Build the target or download it from [release](https://github.com/blahgeek/emacs-lsp-booster/releases), put the executable `emacs-lsp-booster` in $PATH (e.g. `~/.local/bin`)
2. Add the following code to your `init.el`:

> NOTE: if your emacs does not have json support (aka, `(fboundp 'json-parse-buffer)` is nil),
> replace `json-parse-buffer` with `json-read` in the first line!

> **For eglot users**: see https://github.com/blahgeek/emacs-lsp-booster/issues/1

```elisp
(define-advice json-parse-buffer (:around (old-fn &rest args) lsp-booster-parse-bytecode)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(define-advice lsp-resolve-final-command (:around (old-fn cmd &optional test?) add-lsp-server-booster)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
```

Done! Now try to use lsp-mode as usual.

You can verify that it works by checking that `emacs-lsp-booster` is running along with lsp servers and lsp-mode related functions works as expected.
