{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.erlang
    pkgs.elmPackages.elm
    pkgs.rebar3
  ];

  shellhook = ''
    cd backend/
    rebar3 compile
    cd ..
    cd frontend/
    elm reactor   
  '';
}
