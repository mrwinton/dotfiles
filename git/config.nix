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
       glog = "log --oneline --decorate --all --graph";
       hreset = "!git reset --hard $(git rev-parse --abbrev-ref --symbolic-full-name @{u})";
       last = "log -1";
       lol = "log --oneline";
       pushf = "push --force-with-lease";
       recommit = "commit -a --amend --no-edit";
       s = "status -s";
       uncommit = "reset --soft HEAD^";
     };

     extraConfig = {
       color.ui = true;
       github.user = "mrwinton";
       hub.protocol = "https";
       merge.ff = "only";
       pull.rebase = true;
       fetch.prune = true;
    };
  };
}
