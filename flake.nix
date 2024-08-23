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
        rWrapper = pkgs.rWrapper.override{ packages = with rPackages; [ styler roxygen2 formatR devtools usethis ]; };
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
              name = "build";
              command = ''
                Rscript -e 'devtools::document(pkg = "src/")'
              '';
              help = "build the package";
            }
            {
              name = "release";
              command = ''
                R CMD build src/
              '';
              help = "build the package tarball";
            }
            {
              name = "check";
              command = ''
                Rscript -e 'devtools::check(pkg = "src/")'
              '';
              help = "check the package";
            }
            {
              name = "document";
              command = ''
                Rscript scr-document.R
              '';
              help = "generate documentation & vignettes";
            }
            {
              name = "format";
              command = ''
                Rscript scr-format.R
              '';
              help = "format the code using styler";
            }
          ];
        });
      }
    );
}
