{ config, lib, pkgs, ... }:

with pkgs;

let
  emacsDir = ".config/emacs";
  mrwintonPath = "${emacsDir}/mrwinton.emacs.d";
  purcellPath = "${emacsDir}/purcell.emacs.d";

  chemacsRepo = fetchFromGitHub {
    owner = "plexus";
    repo = "chemacs2";
    rev = "ef82118824fac2b2363d3171d26acbabe1738326";
    sha256 = "1gg4aa6dxc4k9d78j8mrrhy0mvhqmly7jxby69518xs9njxh00dq";
  };

  purcellRepo = fetchFromGitHub {
    owner = "purcell";
    repo = "emacs.d";
    rev = "a42311c994d9966e250cefd26d33ed1de59a3f7f";
    sha256 = "1487lx4xrcl8jss913h39ny0dwj5j3bn38k53sjfqmzh9g6aikng";
  };
in
{
  home.file = {
    chemacs = {
      source = chemacsRepo;
      target = ".emacs.d";
      recursive = true;
    };

    mrwinton = {
      source = ./emacs.d;
      target = mrwintonPath;
      recursive = true;
    };

    purcell = {
      source = purcellRepo;
      target = purcellPath;
      recursive = true;
    };

    ".emacs-profiles.el".text = ''
      (("default" . ((user-emacs-directory . "~/${mrwintonPath}")))
       ("purcell" . ((user-emacs-directory . "~/${purcellPath}"))))
    '';
  };
}
