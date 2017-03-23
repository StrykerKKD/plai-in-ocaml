open Core.Std

type arithS =
    | NumS of int
    | PlusS of arithS * arithS
    | BminusS of arithS * arithS
    | MultS of arithS * arithS
    | UminusS of arithS 

type arithC =
    | NumC of int
    | PlusC of arithC * arithC
    | MultC of arithC * arithC

let rec desugar = function
    | NumS n -> NumC n
    | PlusS(l, r) -> PlusC(desugar l, desugar r)
    | MultS(l, r) -> MultC(desugar l, desugar r)
    | BminusS(l, r) -> PlusC(desugar l, MultC(NumC (-1), desugar r))
    | UminusS e -> BminusS(NumS 0, e) |> desugar

let rec arithC_to_string = function
    | NumC n -> "(NumC " ^ (Int.to_string n) ^ " )"
    | PlusC(l, r) -> "(PlusC " ^ (arithC_to_string l) ^ " " ^ (arithC_to_string r) ^ " )"
    | MultC(l, r) -> "(MultC " ^ (arithC_to_string l) ^ " " ^ (arithC_to_string r) ^ " )"

let _ = PlusS(MultS(NumS 1, NumS 2), NumS 3) |> desugar |> arithC_to_string |> print_endline
let _ = BminusS(NumS 1, NumS 2) |> desugar |> arithC_to_string |> print_endline
let _ = UminusS(NumS 1) |> desugar |> arithC_to_string |> print_endline