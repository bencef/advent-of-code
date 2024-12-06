{ ocamlPackages
, nix-filter ? null
}:

ocamlPackages.buildDunePackage {
  pname = "aoc_2024_05";

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
            "./lib/lexer.ml"
            "./lib/parser.mly"
            "./lib/reading.ml"
            "./lib/reading.mli"
            "./lib/printer_queue.ml"
            "./lib/printer_queue.mli"
            "./lib/testing.ml"
            "./lib/testing.mli"
            "./aoc_2024_05.opam"
          ];
        };

  nativeBuildInputs = with ocamlPackages; [ menhir ];
  propagatedBuildInputs = with ocamlPackages; [ core sedlex menhirLib ppx_inline_test ];
}
