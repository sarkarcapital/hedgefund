{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.scala
    pkgs.mill
  ];

  shellHook = ''
    mill -w myModule.run
  '';
}
