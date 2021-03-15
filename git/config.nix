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
       hreset = "!git reset --hard $(git rev-parse --abbrev-ref --symbolic-full-name @{u})";
       pushf = "push --force-with-lease";
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
