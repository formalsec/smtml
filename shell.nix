{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  }) {}
}:

pkgs.mkShell {
  dontDetectOcamlConflicts = true;
  nativeBuildInputs = with pkgs.ocamlPackages; [
    dune_3
    dune-glob
    findlib
    bisect_ppx
    mdx
    menhir
    merlin
    ocaml
    ocamlformat
    ocp-browser
    odoc
    ounit2
  ];
  buildInputs = with pkgs.ocamlPackages; [
    bos
    cmdliner
    cohttp-lwt-unix
    dune-build-info
    dolmen_model
    dolmen_type
    extunix
    fpath
    hc
    logs
    lwt
    menhirLib
    mtime
    ocaml_intrinsics
    prelude
    scfg
    sexplib
    tls-lwt
    yojson
    z3
    zarith
  ];
}
