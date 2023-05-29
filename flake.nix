{
  description = "Home Manager configuration of robbert";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }:
    let username = "robbert";
        # This needs to be set for the `toAbsolutePath` function defined below
        # to work. It's set in the `update-dotfiles` script and it requires this
        # to be run with `--impure`.
        dotfilesPath =
          let path = builtins.getEnv "DOTFILES_PATH";
              assertion = pkgs.lib.asserts.assertMsg
                (pkgs.lib.sources.pathIsDirectory path)
                "'$DOTFILES_DIR' is not set, make sure to run this through the 'update-dotfiles' script";
           in assert assertion; path;
        system = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations."robbert" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ];

        extraSpecialArgs = {
          inherit username;
        };
      };
    };
}
