{ config, lib, pkgs, ... }:

{
  home.file = {
    init = {
      source = ./init.lua;
      target = ".hammerspoon/init.lua";
    };
  };

  home.file = {
    functions = {
      source = ./functions.lua;
      target = ".hammerspoon/functions.lua";
    };
  };
}
