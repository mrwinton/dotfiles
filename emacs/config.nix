{ config, lib, pkgs, ... }:

with pkgs;

let
  mrwintonPath = "~/src/github.com/mrwinton/dotfiles/emacs/emacs.d";
  purcellPath = ".config/emacs/purcell.emacs.d";
  doomPath = ".config/emacs/doom.emacs.d";
  nanoPath = ".config/emacs/nano.emacs.d";
  doomConfigPath = "~/src/github.com/mrwinton/dotfiles/emacs/doom.d";
  doomLocalPath = "~/src/github.com/mrwinton/dotfiles/emacs/doom.d/local";

  chemacsRepo = pkgs.chemacs-repo;
  purcellRepo = pkgs.purcell-repo;
  doomRepo = pkgs.doom-repo;
  nanoRepo = pkgs.nano-repo;
in {
  home = {
    sessionVariables = {
      DOOMDIR = "${doomConfigPath}";
      DOOMLOCALDIR = "${doomLocalPath}";
    };
    sessionPath = [ "${config.home.homeDirectory}/${doomPath}/bin" ];
  };
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

    nano = {
      source = nanoRepo;
      target = nanoPath;
      recursive = true;
    };

    ".emacs-profiles.el".text = ''
      (("default" . ((user-emacs-directory . "~/${doomPath}")))
       ("purcell" . ((user-emacs-directory . "~/${purcellPath}")))
       ("nano" . ((user-emacs-directory . "~/${nanoPath}")))
       ("doom" . ((user-emacs-directory . "~/${doomPath}"))))
    '';

    proselint = {
      source = ./proselint/config.json;
      target = ".config/proselint/config.json";
    };
  };
}
