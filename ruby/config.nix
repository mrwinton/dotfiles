{ config, lib, pkgs, ... }:

{
  home.file = {
    pryrc = {
      source = ./pryrc;
      target = ".pryrc";
    };
  };
}
