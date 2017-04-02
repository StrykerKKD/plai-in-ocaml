open Core.Std

type symbol = string

type exprC =
    | NumC of int
    | IdC of symbol
    | AppC of symbol * exprC
    | PlusC of exprC * exprC
    | MultC of exprC * exprC

type funDefC = 
    | FdC of symbol * symbol * exprC

type binding =
    |Bind of symbol * int

type env = binding list
let mt_env = []
let extend_env = List.cons

let fdc_name = function
    | FdC(name, arg, body) -> name

let fdc_arg = function
    | FdC(name, arg, body) -> arg

let fdc_body = function
    | FdC(name, arg, body) -> body

let bind_name = function
    | Bind(name, value) -> name

let bind_value = function
    | Bind(name, value) -> value

let rec get_fundef n fds =
    match fds with
    | [] -> failwith "reference to undefined function"
    | head :: tail ->
        if n = (fdc_name head) then
            head
        else
            get_fundef n tail

let rec lookup for_ env =
    match env with
    | [] -> failwith "name not found"
    | head :: tail ->
        if for_ = (bind_name head) then
            bind_value head
        else
            lookup for_ tail

let rec interp_faulty expr env fds =
    match expr with
    | NumC n -> n
    | IdC n -> lookup n env
    | AppC(f,a) -> 
        let fd = get_fundef f fds in
        let expr = (fdc_body fd) in
        let bind =  Bind( fdc_arg fd, interp_faulty a env fds ) in 
        let env = extend_env bind env in
        interp_faulty expr env fds
    | PlusC(l, r) -> (interp_faulty l env fds) + (interp_faulty r env fds)
    | MultC(l, r) -> (interp_faulty l env fds) * (interp_faulty r env fds)

let rec interp expr env fds =
    match expr with
    | NumC n -> n
    | IdC n -> lookup n env
    | AppC(f,a) -> 
        let fd = get_fundef f fds in
        let expr = (fdc_body fd) in
        let bind =  Bind( fdc_arg fd, interp a env fds ) in 
        let env = extend_env bind mt_env in
        interp expr env fds
    | PlusC(l, r) -> (interp l env fds) + (interp r env fds)
    | MultC(l, r) -> (interp l env fds) * (interp r env fds)

let _ = 
    let expr = PlusC(NumC 10, AppC("const5", NumC 10)) in
    let env = mt_env in
    let fds = [ FdC("const5", "_", NumC 5) ] in
    interp_faulty expr env fds |> print_int |> print_newline

let _ = 
    let expr = PlusC(NumC 10, AppC("double", PlusC(NumC 1, NumC 2))) in
    let env = mt_env in
    let fds = [ FdC("double", "x", PlusC( IdC "x", IdC "x")) ] in
    interp_faulty expr env fds |> print_int |> print_newline

let _ = 
    let expr = PlusC(NumC 10, AppC("quadruple", PlusC(NumC 1, NumC 2))) in
    let env = mt_env in
    let fds = [ FdC("double", "x", PlusC( IdC "x", IdC "x"));
                FdC("quadruple", "x", AppC("double", AppC("double", IdC "x"))) ] in
    interp_faulty expr env fds |> print_int |> print_newline

let _ = 
    let expr = AppC("f1", NumC 3) in
    let env = mt_env in
    let fds = [ FdC("f1", "x", AppC( "f2", NumC 4));
                FdC("f2", "y", PlusC(IdC "x", IdC "y")) ] in
    interp_faulty expr env fds |> print_int |> print_newline

let _ = 
    let expr = AppC("f1", NumC 3) in
    let env = mt_env in
    let fds = [ FdC("f1", "x", AppC( "f2", NumC 4));
                FdC("f2", "y", PlusC(IdC "x", IdC "y")) ] in
    interp expr env fds |> print_int |> print_newline