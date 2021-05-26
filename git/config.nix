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
       color.ui = true;
       github.user = "mrwinton";
       hub.protocol = "https";
       merge.summary = true;
       pull.rebase = true;
       push.default = "current";
       fetch.prune = true;
    };
  };
}
