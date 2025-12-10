{
  description = "Michael's system";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    chemacs-repo = {
      url = "github:plexus/chemacs2";
      flake = false;
    };
    purcell-repo = {
      url = "github:purcell/emacs.d";
      flake = false;
    };
    doom-repo = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    nano-repo = {
      url = "github:rougier/nano-emacs";
      flake = false;
    };
  };

  outputs = {
    self,
    darwin,
    nixpkgs,
    home-manager,
    ...
    } @ inputs:
    {
      darwinConfigurations."Michaels-MacBook-Pro" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./darwin/darwin.nix
          home-manager.darwinModules.home-manager
          {
            _module.args = { inherit inputs; };
            nixpkgs.config.allowUnfree = true;
            home-manager = {
              users.michaelwinton = import ./home-manager;
              extraSpecialArgs = { inherit inputs; };
              useGlobalPkgs = true;
            };
            users.users.michaelwinton.home = "/Users/michaelwinton";
            nix.settings.trusted-users = [ "michaelwinton" ];
          }
        ];
      };
    };
}
