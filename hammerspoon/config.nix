{ config, lib, pkgs, ... }:

{
  home.file = {
    init = {
      source = ./init.lua;
      target = ".hammerspoon/init.lua";
    };
  };

  home.file = {
    utils = {
      source = ./utils.lua;
      target = ".hammerspoon/utils.lua";
    };
  };
}
