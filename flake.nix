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
        rWrapper = pkgs.rWrapper.override{ packages = with rPackages; [ devtools tidyverse ]; };

      in
      {
        devShells.default = (pkgs.devshell.mkShell {
          imports = [ "${devshell}/extra/git/hooks.nix" ];
          name = "rLang-shell";
          packages = with pkgs; [ rWrapper ];
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
              name = "format";
              command = ''
                Rscript -e "install.packages('styler', repos = 'https://cloud.r-project.org', quiet = TRUE); styler::style_pkg('src')"
              '';
              help = "format the code using styler";
            }
          ];
        });
      }
    );
}
