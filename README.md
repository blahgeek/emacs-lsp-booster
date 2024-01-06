# Emacs LSP performance booster

![Build and test status](https://github.com/blahgeek/emacs-lsp-booster/actions/workflows/build-test.yml/badge.svg)
![Release status](https://github.com/blahgeek/emacs-lsp-booster/actions/workflows/release.yml/badge.svg)

Improve performance of [lsp-mode](https://github.com/emacs-lsp/lsp-mode) or [eglot](https://github.com/joaotavora/eglot) using a wrapper executable.

## Background & Prior work

(Huge thanks to @yyoncho for both maintaining lsp-mode and giving me the inspiration of this project).

According to [yyoncho](https://www.reddit.com/r/emacs/comments/ymrkyn/comment/iv90q4i/?utm_source=share&utm_medium=web2x&context=3),
the are several performance issues related to lsp-mode (mostly the same for elot):

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

Generally, what you need to do is:

1. Wrap your lsp server command with this `emacs-lsp-booster` executable.
   For example, if the original lsp server command is `pyright-langserver --stdio`, configure lsp-mode or eglot to run `emacs-lsp-booster pyright-langserver --stdio` instead.
2. Advise the json parsing function in lsp-mode or eglot to try to parse the input as bytecode instead of json.

See more detailed steps below.

### Obtain or build `emacs-lsp-booster`

For linux users, you may download the prebuilt binary from [release](https://github.com/blahgeek/emacs-lsp-booster/releases).
*(The macOS binary in the release page lacks proper code signing for now.)*

Or alternatively, you may build the target locally:

1. Setup [Rust toolchain](https://www.rust-lang.org/tools/install)
2. Run `cargo build --release`
3. Find the built binary in `target/release/emacs-lsp-booster`

Then, put the `emacs-lsp-booster` binary in your $PATH (e.g. `~/.local/bin`).

### Configure `lsp-mode`

(Make sure NOT to use the [native-jsonrpc custom version](https://github.com/emacs-lsp/emacs))

1. **Use [plist for deserialization](https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization) for lsp-mode**
3. Add the following code to your `init.el`:

```elisp
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
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
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
```

Done! Now try to use lsp-mode as usual.

### Configure `eglot`

Please see https://github.com/blahgeek/emacs-lsp-booster/issues/1 . Huge thanks to @jdtsmith

### How to verify it's working

1. Check that `emacs-lsp-booster` process is running
2. Check the stderr buffer (e.g. for lsp-mode, `*pyright::stderr*` buffer), it should contain `emacs_lsp_booster` related log.
