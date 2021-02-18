{ config, lib, pkgs, ... }:

{
  home.file = {
    ".asdfrc".text = "legacy_version_file = yes";
  };
}
