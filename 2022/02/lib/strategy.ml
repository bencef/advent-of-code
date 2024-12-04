type move =
  | Rock
  | Paper
  | Scissors

type strategy =
  | X
  | Y
  | Z

type step = move * strategy

type outcome =
  | Win
  | Lose
  | Draw

let get_outcome =
  function
  | (Rock, Paper)     -> Win
  | (Paper, Scissors) -> Win
  | (Scissors, Rock)  -> Win
  | (a, b) when a = b -> Draw
  | _                 -> Lose

let outcome_score outcome =
  match outcome with
  | Win  -> 6
  | Draw -> 3
  | Lose -> 0

let strategy_score move =
  match move with
  | Rock     -> 1
  | Paper    -> 2
  | Scissors -> 3

let score1 ((move, strategy): step) =
  let answer = match strategy with
    | X -> Rock
    | Y -> Paper
    | Z -> Scissors
  in
  let outcome = get_outcome (move, answer) in
  (outcome_score outcome) + (strategy_score answer)

let score2 ((move, strategy): step) =
  let expected_outcome =
    match strategy with
    | X -> Lose
    | Y -> Draw
    | Z -> Win
  in
  let my_move =
    match expected_outcome with
    | Draw -> move
    | Win  -> (match move with
              | Rock     -> Paper
              | Paper    -> Scissors
              | Scissors -> Rock)
    | Lose -> (match move with
               | Paper    -> Rock
               | Scissors -> Paper
               | Rock     -> Scissors)
  in
  (outcome_score expected_outcome) + (strategy_score my_move)
