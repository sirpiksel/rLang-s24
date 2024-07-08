{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, devshell, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs =
          import nixpkgs {
            inherit system;
            overlays = [ devshell.overlays.default ];
          };
        rPackages = pkgs.rPackages;
        rWrapper = pkgs.rWrapper.override{ packages = with rPackages; [ devtools dplyr stringr tidyverse tibble magrittr ]; };

      in
      {
        devShells.default = (pkgs.devshell.mkShell {
          imports = [ "${devshell}/extra/git/hooks.nix" ];
          name = "rLang-shell";
          packages = with pkgs; [ rWrapper nixpkgs-fmt ];
          git.hooks = {
            enable = true;
            pre-commit.text = ''
              nix flake check
            '';
          };
          commands = [
            {
              name = "run";
              command = ''
                for file in $(ls *.R | sort); do printf "\n\nRunning $file\n"; Rscript $file; done
              '';
              help = "run script";
            }
            {
              name = "build";
              command = ''
                R CMD build
              '';
              help = "build the package tarball";
            }
            {
              name = "check";
              command = ''
                R CMD check
              '';
              help = "check the package tarball";
            }
          ];
        });
      }
    );
}
