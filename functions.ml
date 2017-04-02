type symbol = string

type exprC =
    | NumC of int
    | IdC of symbol
    | AppC of symbol * exprC
    | PlusC of exprC * exprC
    | MultC of exprC * exprC

type funDefC = 
    | FdC of symbol * symbol * exprC

let fdc_name = function
    | FdC(name, arg, body) -> name

let fdc_arg = function
    | FdC(name, arg, body) -> arg

let fdc_body = function
    | FdC(name, arg, body) -> body

let rec subst what for_ in_ =
    match in_ with
    | NumC n -> in_
    | IdC s -> if s = for_ then what else in_
    | AppC(f, a) -> AppC(f, (subst what for_ a))
    | PlusC(l, r) -> PlusC( (subst what for_ l), (subst what for_ r) )
    | MultC(l, r) -> MultC( (subst what for_ l), (subst what for_ r) )

let rec get_fundef n fds =
    match fds with
    | [] -> failwith "reference to undefined function"
    | head :: tail ->
        if n = (fdc_name head) then
            head
        else
            get_fundef n tail

let rec interp_lazy e fds =
    match e with
    | NumC n -> n
    | IdC _ -> failwith "shouldn't get here"
    | AppC (f, a) -> 
        let fd = get_fundef f fds in
        interp_lazy (subst a (fdc_arg fd) (fdc_body fd)) fds
    | PlusC(l, r) -> (interp_lazy l fds) + (interp_lazy r fds)
    | MultC(l, r) -> (interp_lazy l fds) * (interp_lazy r fds)

let rec interp_eager e fds =
    match e with
    | NumC n -> n
    | IdC _ -> failwith "shouldn't get here"
    | AppC (f, a) -> 
        let fd = get_fundef f fds in
        let a = NumC(interp_eager a fds) in
        interp_eager (subst a (fdc_arg fd) (fdc_body fd)) fds
    | PlusC(l, r) -> (interp_eager l fds) + (interp_eager r fds)
    | MultC(l, r) -> (interp_eager l fds) * (interp_eager r fds)

let _ = 
    let e = AppC("double", NumC 5) in
    let fds = [FdC("double", "x", PlusC( IdC "x", IdC "x"))] in
    interp_lazy e fds |> print_int |> print_newline

let _ = 
    let e = AppC("quadruple", NumC 5) in
    let fds = [ FdC("double", "x", PlusC( IdC "x", IdC "x"));
                FdC("quadruple", "x", AppC("double", AppC("double", IdC "x"))) ] in
    interp_lazy e fds |> print_int |> print_newline

let _ = 
    let e = AppC("quadruple", AppC("const5", NumC 100)) in
    let fds = [ FdC("double", "x", PlusC( IdC "x", IdC "x"));
                FdC("quadruple", "x", AppC("double", AppC("double", IdC "x")));
                FdC("const5", "_", NumC 5) ] in
    interp_lazy e fds |> print_int |> print_newline

let _ = 
    let e = AppC("quadruple", AppC("const5", NumC 100)) in
    let fds = [ FdC("double", "x", PlusC( IdC "x", IdC "x"));
                FdC("quadruple", "x", AppC("double", AppC("double", IdC "x")));
                FdC("const5", "_", NumC 5) ] in
    interp_eager e fds |> print_int |> print_newline