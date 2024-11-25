{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.erlang
    pkgs.elmPackages.elm
    pkgs.rebar3
  ];

  shellhook = ''
    erlc erlang/app.erl
    elm reactor    
  '';
}