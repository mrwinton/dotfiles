{ config, lib, pkgs, ... }:

with pkgs;

let
  mrwintonPath = "~/src/github.com/mrwinton/dotfiles/emacs/emacs.d";
  purcellPath = ".config/emacs/purcell.emacs.d";
  doomPath = ".config/emacs/doom.emacs.d";
  doomConfigPath = ".config/emacs/doom-config";

  chemacsRepo = pkgs.chemacs-repo;
  purcellRepo = pkgs.purcell-repo;
  doomRepo = pkgs.doom-repo;
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

    doom = {
      source = doomRepo;
      target = doomPath;
      recursive = true;
    };

    ".emacs-profiles.el".text = ''
      (("default" . ((user-emacs-directory . "${mrwintonPath}")))
       ("purcell" . ((user-emacs-directory . "~/${purcellPath}")))
       ("doom" . ((user-emacs-directory . "~/${doomPath}")
                  (env . (("DOOMDIR" . "~/${doomConfigPath}"))))))
    '';

    proselint = {
      source = ./proselint/config.json;
      target = ".config/proselint/config.json";
    };
  };
}
