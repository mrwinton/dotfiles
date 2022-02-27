{
  description = "Michael's system";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs:
  {
    darwinConfigurations."m-one" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [ 
          ./nix/darwin.nix 
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.michaelwinton = import ./nix/home.nix;            
          }
      ];
    };
  };
}