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
        rWrapper = pkgs.rWrapper.override{ packages = with rPackages; [ styler roxygen2 formatR devtools tidyverse ]; };
        pandoc = pkgs.pandoc;
        qpdf = pkgs.qpdf;
        texLive = pkgs.texlive.combine {
          inherit (pkgs.texlive) scheme-basic
            framed xcolor fancyvrb etoolbox;
        };

      in
      {
        devShells.default = (pkgs.devshell.mkShell {
          imports = [ "${devshell}/extra/git/hooks.nix" ];
          name = "rLang-shell";
          packages = with pkgs; [ R rWrapper pandoc qpdf texLive ];
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
                Rscript -e 'devtools::document(pkg = "src/")'
                Rscript -e 'devtools::build_vignettes(pkg = "src/")'
              '';
              help = "build the package tarball";
            }
            {
              name = "check";
              command = ''
                Rscript -e 'devtools::check(pkg = "src/")'
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
            {
              name = "clean";
              command = ''
                rm -rf src.Rcheck *.tar.gz
              '';
              help = "remove Rcheck directory and compiled package";
            }
          ];
        });
      }
    );
}
