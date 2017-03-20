open Core

type arithC =
    | NumC of int
    | PlusC of arithC * arithC
    | MultC of arithC * arithC

let rec parse = function
    | Sexp.Atom value -> NumC (Int.of_string value)
    | Sexp.List list ->
        match list with
        | Sexp.Atom "+" :: l :: r :: [] -> PlusC(parse l, parse r)
        | Sexp.Atom "*" :: l :: r :: [] -> MultC(parse l, parse r)
        | _ -> failwith "invalid list input"

let rec arithC_to_string = function
    | NumC n -> "(NumC " ^ (Int.to_string n) ^ " )"
    | PlusC(l, r) -> "(PlusC " ^ (arithC_to_string l) ^ " " ^ (arithC_to_string r) ^ " )"
    | MultC(l, r) -> "(MultC " ^ (arithC_to_string l) ^ " " ^ (arithC_to_string r) ^ " )"

let _ = "(* (+ 1 2) (+ 3 4))" |> Sexp.of_string |> parse |> arithC_to_string |> print_endline
