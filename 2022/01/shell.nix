{ pkgs ? import <nixpkgs> {} }:

let
  ocamlPackages = with pkgs.ocamlPackages; [ ocaml dune_2 core findlib ];
  devPackages = [ pkgs.ocamlPackages.utop pkgs.ocamlPackages.ocaml-lsp pkgs.ocamlformat ];
in
pkgs.mkShell {
  packages = ocamlPackages ++ devPackages;
}
