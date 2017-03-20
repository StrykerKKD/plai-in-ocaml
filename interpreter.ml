type arithC =
    | NumC of int
    | PlusC of arithC * arithC
    | MultC of arithC * arithC

let rec interp = function
    | NumC n -> n
    | PlusC(l, r) -> (interp l) + (interp r)
    | MultC(l, r) -> (interp l) * (interp r)

let _ = PlusC( NumC 1, NumC 2) |> interp |> print_int |> print_newline