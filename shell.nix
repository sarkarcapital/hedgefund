{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.erlang
    pkgs.elmPackages.elm
  ];

  shellhook = ''
    erlc erlang/app.erl
    elm reactor    
  '';
}
