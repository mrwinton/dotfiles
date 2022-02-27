{ config, lib, pkgs, ... }:

with pkgs;

let
  mrwintonPath = "~/src/github.com/mrwinton/dotfiles/emacs/emacs.d";
  purcellPath = ".config/emacs/purcell.emacs.d";

  chemacsRepo = pkgs.chemacs-repo;
  purcellRepo = pkgs.purcell-repo;
in
{
  home.file = {
    chemacs = {
      source = chemacsRepo;
      target = ".emacs.d";
      recursive = true;
    };

    purcell = {
      source = purcellRepo;
      target = purcellPath;
      recursive = true;
    };

    ".emacs-profiles.el".text = ''
      (("default" . ((user-emacs-directory . "${mrwintonPath}")))
       ("purcell" . ((user-emacs-directory . "${purcellPath}"))))
    '';

    proselint = {
      source = ./proselint/config.json;
      target = ".config/proselint/config.json";
    };
  };
}
