{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.bash
  ];

  shellhook = ''
    echo "installing magic"
    curl -ssL https://magic.modular.com/df3f9200-51e3-4624-8d08-d7d4461865a5 | bash

    # need to install like this since they are not yet on the nix registry
    echo "installing libraries used by quickfix"     
    sudo apt install build-essential python3-dev cmake

    echo "entering magic venv shell..."
    magic shell
  '';
}
