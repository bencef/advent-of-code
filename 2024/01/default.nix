{ ocamlPackages
, nix-filter ? null
}:

ocamlPackages.buildDunePackage {
  pname = "aoc_2024_01";

  version = "0.0.1";

  src = if nix-filter == null
        then ./.
        else nix-filter {
          root = ./.;
          include = [
            "./bin/dune"
            "./bin/main.ml"
            "./bin/part1.ml"
            "./bin/part2.ml"
            "./bin/main.mli"
            "./dune-project"
            "./lib/dune"
            "./lib/parser.mly"
            "./lib/lexer.ml"
            "./lib/reading.ml"
            "./lib/reading.mli"
            "./aoc_2024_01.opam"
          ];
        };

  nativeBuildInputs = with ocamlPackages; [ menhir ];
  propagatedBuildInputs = with ocamlPackages; [ core sedlex menhirLib ppx_inline_test ];
}
