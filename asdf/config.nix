{ config, lib, pkgs, ... }:

{
  home.file = {
    ".asdfrc".text = "legacy_version_file = yes";
  };

  home.file = {
    ".default-gems".text = ''
      awesome_print
      pry
      rubocop
      rubocop-performance
      rubocop-rails
      solargraph
    '';
  };

  home.file = {
    ".default-npm-packages".text = ''
      yarn
      typescript
      typescript-language-server
    '';
  };
}
