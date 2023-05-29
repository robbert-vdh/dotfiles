{
  description = "Home Manager configuration of robbert";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # Directly passing the inputs along makes it easier to use files from
  # non-flake repos
  outputs = inputs@{ nixpkgs, home-manager, ... }:
    let
      username = "robbert";
      # This needs to be set for the `toAbsolutePath` function defined below to
      # work. It's set in the `update-dotfiles` script and it requires this to
      # be run with `--impure`.
      dotfilesPath = let
        path = builtins.getEnv "DOTFILES_DIR";
        assertion = pkgs.lib.asserts.assertMsg
          (path != "" && pkgs.lib.sources.pathIsDirectory path)
          "'$DOTFILES_DIR' is not set, make sure to run this through the 'update-dotfiles' script";
      in assert assertion; path;
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations."robbert" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [ ./home.nix ];

        extraSpecialArgs = {
          inherit inputs username;

          # This is a super hacky way to get absolute paths from a Nix path.
          # Flakes intentionally don't allow you to get this information, but we
          # need this to be able to use `mkOutOfStoreSymlink` to create regular
          # symlinks for configurations that should be mutable, like for Emacs'
          # config and for fonts. This relies on `dotfilesPath` pointing to the
          # directory that contains this file.
          # FIXME: I couldn't figure out how to define this in a module so we
          #        don't need to pass config in here
          mkAbsoluteSymlink = config: repoRelativePath:
            let
              fullPath = "${dotfilesPath}/${repoRelativePath}";
              assertion =
                pkgs.lib.asserts.assertMsg (builtins.pathExists fullPath)
                "'${fullPath}' does not exist (make sure --impure is enabled)";
            in assert assertion; config.lib.file.mkOutOfStoreSymlink fullPath;
        };
      };
    };
}
