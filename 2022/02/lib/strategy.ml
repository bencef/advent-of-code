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

let score1 ((move, strategy): step) =
  let answer = match strategy with
    | X -> Rock
    | Y -> Paper
    | Z -> Scissors
  in
  let outcome = get_outcome (move, answer) in
  let outcome_score =
    match outcome with
    | Win  -> 6
    | Draw -> 3
    | Lose -> 0
  in
  let strategy_score =
    match answer with
    | Rock     -> 1
    | Paper    -> 2
    | Scissors -> 3
  in
  outcome_score + strategy_score
