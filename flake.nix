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
        pandoc = pkgs.pandoc;
        rWrapper = pkgs.rWrapper.override{ packages = with rPackages; [ devtools tidyverse ]; };

      in
      {
        devShells.default = (pkgs.devshell.mkShell {
          imports = [ "${devshell}/extra/git/hooks.nix" ];
          name = "rLang-shell";
          packages = with pkgs; [ rWrapper pandoc ];
          commands = [
            {
              name = "run";
              command = ''
                Rscript
              '';
              help = "run script";
            }
            {
              name = "build";
              command = ''
                R CMD build 'src'
              '';
              help = "build the package tarball";
            }
            {
              name = "check";
              command = ''
                R CMD check 'src'
              '';
              help = "check the package tarball";
            }
            {
              name = "document";
              command = ''
                Rscript scr-roxygenize.R
              '';
              help = "generate documentation using roxygen2";
            }
            {
              name = "format";
              command = ''
                Rscript scr-formatting.R
              '';
              help = "format the code using styler";
            }
          ];
        });
      }
    );
}
