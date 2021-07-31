{ config, lib, pkgs, ... }:

{
  home.file = {
    emacs = {
      source = ./emacs.d;
      target = ".emacs.d";
      recursive = true;
    };
  };
}
