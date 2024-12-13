%{
open! Core
open Patrol
%}

%token EMPTY
%token OBSTACLE
%token <Patrol.facing> SENTRY
%token EOL
%token EOF
%start <floor> parse

%%

parse:
  | rows = list(row); EOF { make_floor rows }
  ;

row:
  | tiles = list(tile); EOL { tiles }
  ;

tile:
  | EMPTY { Tile.Empty }
  | OBSTACLE { Tile.Obstacle }
  | facing = SENTRY { Tile.Sentry facing }
  ;
