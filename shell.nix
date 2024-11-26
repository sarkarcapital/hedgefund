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

    echo "run 'erl' then run this commands: "
    echo "application:start(sasl),"
    echo "fix_supervisor:start_link()."
    echo "fix_client:start("127.0.0.1", 12345, 1000000)."
  '';
}
