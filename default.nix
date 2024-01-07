{ pkgs, fetchFromGitHub, rustPlatform, lib, ... }:

rustPlatform.buildRustPackage rec {
  pname = "emacs-lsp-booster";
  version = "0.1.1";

  cargoSha256 = "sha256-quqhAMKsZorYKFByy2bojGgYR2Ps959Rg/TP8SnwbqM=";

  src = fetchFromGitHub {
    owner = "blahgeek";
    repo = pname;
    rev = "72d718ea35dfd6cdde582ea1824e9a93e084e64e";
    hash = "sha256-0roQxzQrxcmS2RHQPguBRL76xSErf2hVjuJEyFr5MeM=";
  };

  nativeBuildInputs = with pkgs; [
    emacs  # For tests
  ];

  meta = with lib; {
    description = "Improve performance of Emacs LSP servers by converting JSON to bytecode";
    homepage = "https://github.com/${src.owner}/${pname}";
    changelog = "https://github.com/${src.owner}/${pname}/releases/tag/${version}";
    license = [ licenses.mit ];
    maintainers = [];
    mainProgram = "emacs-lsp-booster";
  };

}
