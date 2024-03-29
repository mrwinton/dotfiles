{
  description = "Michael's system";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
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

  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs:
    let
      nixpkgsConfig = {
        config = { allowUnfree = true; };
        overlays = [
          inputs.emacs-overlay.overlay
          (final: prev: { chemacs-repo = inputs.chemacs-repo; })
          (final: prev: { purcell-repo = inputs.purcell-repo; })
          (final: prev: { doom-repo = inputs.doom-repo; })
          (final: prev: { nano-repo = inputs.nano-repo; })
        ];
      };
    in
    {
      darwinConfigurations."m-one" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./nix/darwin.nix
          home-manager.darwinModules.home-manager
          {
            nixpkgs = nixpkgsConfig;
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.michaelwinton = import ./nix/home.nix;
          }
        ];
      };
    };
}
