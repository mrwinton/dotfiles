{ config, lib, pkgs, inputs, ... }:

with pkgs;

let
  dotfilesPath = "~/src/github.com/mrwinton/dotfiles";
  mrwintonPath = "${dotfilesPath}/home-manager/emacs/emacs.d";
  purcellPath = ".config/emacs/purcell.emacs.d";
  doomPath = ".config/emacs/doom.emacs.d";
  nanoPath = ".config/emacs/nano.emacs.d";
  doomConfigPath = "${dotfilesPath}/home-manager/emacs/doom.d";
  doomLocalPath = "${dotfilesPath}/home-manager/emacs/doom.d/local";

  chemacsRepo = inputs.chemacs-repo;
  purcellRepo = inputs.purcell-repo;
  doomRepo = inputs.doom-repo;
  nanoRepo = inputs.nano-repo;
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
      (("default" . ((user-emacs-directory . "${mrwintonPath}")))
       ("purcell" . ((user-emacs-directory . "~/${purcellPath}")))
       ("nano" . ((user-emacs-directory . "~/${nanoPath}")))
       ("doom" . ((user-emacs-directory . "~/${doomPath}")
                  (env . (("DOOMDIR" . "${doomConfigPath}")
                          ("DOOMLOCALDIR" . "${doomLocalPath}"))))))
    '';

    proselint = {
      source = ./proselint/config.json;
      target = ".config/proselint/config.json";
    };
  };
}
