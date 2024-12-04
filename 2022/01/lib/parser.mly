%token <Elf.calories> CALORIES
%token SEPARATOR
%token EOF
%start <Elf.t Core.Array.t> prog
%%

prog:
  | elfs = separated_list(SEPARATOR, elf); EOF { elfs |> Core.Array.of_list }
  ;

elf:
  | cs = nonempty_list(CALORIES) { cs |> Core.Array.of_list |> Elf.make }
  ;
