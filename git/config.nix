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
       s = "status -s";
       last = "log -1";
       lol = "log --oneline";
       pushf = "push --force-with-lease";
       glog = "log --oneline --decorate --all --graph";
       recommit = "commit -a --amend --no-edit";
       uncommit = "reset --soft HEAD^";
       hreset = "!git reset --hard $(git rev-parse --abbrev-ref --symbolic-full-name @{u})";
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
