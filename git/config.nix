{ ... }:

{
  programs.git = {
    enable = true;

    userName = "Michael Winton";
    userEmail = "wintonmr@gmail.com";

    ignores = [
      ".DS_Store"
      ".env"
      "*.orig"
    ];

    aliases = {
      co = "checkout";
      cp = "cherry-pick";
      hreset = "!git reset --hard $(git rev-parse --abbrev-ref --symbolic-full-name @{u})";
      pushf = "push --force-with-lease";
      uncommit = "reset --soft HEAD^";
      safe = "!mkdir $(git rev-parse --git-dir)/safe";
    };

    extraConfig = {
      branch.autosetuprebase = "always";
      color.ui = true;
      fetch.prune = true;
      ghq.root = "~/src";
      github.user = "mrwinton";
      hub.protocol = "https";
      merge.summary = true;
      pull.rebase = true;
      push.default = "current";
      rebase.autosquash = true;
    };

    includes =
      [
        {
          condition = "gitdir:~/src/github.com/UniversalAvenue/";
          contents = {
            user = {
              email = "michael.winton@velory.com";
            };
            branch = {
              master = { pushRemote = "no_push"; };
            };
          };
        }
      ];
  };
}
