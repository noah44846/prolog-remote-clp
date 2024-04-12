{
  description = "Nix shell for java project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { system = system; config.allowUnfree = true; };
    in
    {
      devShells.${system}.default = pkgs.mkShellNoCC {
        packages = with pkgs; [
          swiProlog
          mermaid-cli

          go
          gcc
          go-tools

          pyright
          python311Packages.numpy
          python311Packages.pika
          python311Packages.mypy
        ];

        GIT_CONFIG_GLOBAL =
          pkgs.writeText
            "git.conf"
            ''
              [user]
                  email = "noah.godel@edu.hefr.ch"
                  name = "Noah Godel"
            ''
        ;

        shellHook = ''
          alias python='./venv/bin/python3.11'
          alias pip='./venv/bin/pip3.11'
          alias mypy='./venv/bin/mypy --python-executable=./venv/bin/python3.11'
        '';
      }; 
    };
}
