{ ocamlPackages
, nix-filter ? null
}:

ocamlPackages.buildDunePackage {
  pname = "aoc_2024_04";

  version = "0.0.1";

  src = if nix-filter == null
        then ./.
        else nix-filter {
          root = ./.;
          include = [
            "./.ocamlformat"
            "./bin/dune"
            "./bin/main.mli"
            "./bin/main.ml"
            "./bin/part2.ml"
            "./bin/part1.ml"
            "./dune-project"
            "./lib/dune"
            "./lib/lexer.ml"
            "./lib/parser.mly"
            "./lib/reading.ml"
            "./lib/reading.mli"
            "./lib/word_puzzle.ml"
            "./lib/word_puzzle.mli"
            "./lib/testing.ml"
            "./lib/testing.mli"
            "./aoc_2024_04.opam"
          ];
        };

  nativeBuildInputs = with ocamlPackages; [ menhir ];
  propagatedBuildInputs = with ocamlPackages; [ core sedlex menhirLib ppx_inline_test ];
}
