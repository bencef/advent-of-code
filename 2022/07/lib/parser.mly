%{
    open! Core
    open Domain.Fs
%}
%token EOF

%token NEW_LINE

%token <string> NAME
%token <int> SIZE

%token ROOT UP_DIR
%token CD LS
%token DIR
%token PROMPT

%start <t> prog
%%

prog:
  | commands = command_invokation+; EOF { from_commands commands }

command_invokation:
  | invokation = ls_invokation { invokation }
  | invokation = cd_invokation { invokation }

ls_invokation:
  | PROMPT; LS; NEW_LINE; output = ls_output* { Ls output }

cd_invokation:
  | PROMPT; CD; arg = cd_arg; NEW_LINE { Cd arg }

%inline cd_arg:
  | UP_DIR { Cd_Up }
  | ROOT { Cd_Root }
  | dir = NAME { Cd_Dir dir }

ls_output:
  | size = SIZE; name = NAME; NEW_LINE { File { size; name }}
  | DIR; name = NAME; NEW_LINE { Dir name }
