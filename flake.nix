{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
      in rec {
        flakedPkgs = pkgs;

        devShell = pkgs.mkShell {
          packages = with pkgs; [
            swiProlog
            mermaid-cli

            go
            #gcc
            go-tools
            air

            pyright
            python311Packages.mypy
            nodePackages.nodemon

            kubectl
          ];

          LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";

          GIT_CONFIG_GLOBAL =
            pkgs.writeText
              "git.conf"
              ''
                [user]
                    email = "noah.godel@edu.hefr.ch"
                    name = "Noah Godel"
              ''
          ;
        }; 
      }
    );
}
