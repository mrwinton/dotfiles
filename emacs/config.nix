{ pkgs, ... }:

let
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/develop.tar.gz;
  }) {
    doomPrivateDir = ./doom.d;
  };
in {
  home.packages = [ doom-emacs ];
  home.file.".emacs.d/init.el".text = ''
      (load "default.el")
  '';

  programs.zsh.sessionVariables = {
    EDITOR = "${doom-emacs}/bin/emacsclient";
    VISUAL = "${doom-emacs}/bin/emacs";
  };
}
