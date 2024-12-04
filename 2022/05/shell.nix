{ pkgs ? import <nixpkgs> {} }:

let
  ocamlPackages = with pkgs.ocamlPackages; [ ocaml dune_3 core findlib menhir ];
  devPackages = [ pkgs.ocamlPackages.utop pkgs.ocamlPackages.ocaml-lsp pkgs.ocamlformat ];
in
pkgs.mkShell {
  packages = ocamlPackages ++ devPackages;
}
