open Core.Std

type symbol = string

type exprC =
    | NumC of int
    | IdC of symbol
    | AppC of exprC * exprC
    | PlusC of exprC * exprC
    | MultC of exprC * exprC
    | FdC of symbol * symbol * exprC

type value =
    | NumV of int
    | FunV of symbol * symbol * exprC

type binding =
    |Bind of symbol * value

type env = binding list
let mt_env = []
let extend_env = List.cons

let print_value = function
    | NumV v -> print_int v
    | FunV(name, arg, body) -> print_string (name ^ "(" ^ arg ^ ")")

let is_fdc = function
    | FdC _ -> true
    | _ -> false

let numv_n = function
    | FunV _ -> failwith "got FunV instead of NumV"
    | NumV n -> n

let is_numv = function
    | NumV _ -> true
    | FunV _ -> false

let funv_body = function
    | FunV(name, arg, body) -> body
    | NumV _ -> failwith "got NumV instead of FunV"

let funv_arg = function
    | FunV(name, arg, body) -> arg
    | NumV _ -> failwith "got NumV instead of FunV"

let bind_name = function
    | Bind(name, value) -> name

let bind_value = function
    | Bind(name, value) -> value

let rec lookup for_ env =
    match env with
    | [] -> failwith "name not found"
    | head :: tail ->
        if for_ = (bind_name head) then
            bind_value head
        else
            lookup for_ tail

let num_plus l r =
    if (is_numv) l && (is_numv r) then
        NumV( (numv_n l) + (numv_n r) )
    else
        failwith "one argument was not a number"

let num_mult l r =
    if (is_numv l) && (is_numv r) then
        NumV( (numv_n l) * (numv_n r) )
    else
        failwith "one argument was not a number"

let rec interp expr env =
    match expr with
    | NumC n -> NumV n
    | IdC n -> lookup n env
    | AppC(f, a) ->
        let fd = interp f env in
        let expr = funv_body fd in
        let bind = Bind( funv_arg fd, interp a env) in
        let env = extend_env bind mt_env in
        interp expr env
    | PlusC(l, r) -> num_plus (interp l env) (interp r env)
    | MultC(l, r) -> num_mult (interp l env) (interp r env)
    | FdC(n, a, b) -> FunV(n, a, b)

let rec interp_check_fdc expr env =
    match expr with
    | NumC n -> NumV n
    | IdC n -> lookup n env
    | AppC(f, a) when is_fdc f ->
        let fd = interp_check_fdc f env in
        let expr = funv_body fd in
        let bind = Bind( funv_arg fd, interp_check_fdc a env) in
        let env = extend_env bind mt_env in
        interp_check_fdc expr env
    | AppC _ -> failwith "the input is not a function"
    | PlusC(l, r) -> num_plus (interp_check_fdc l env) (interp_check_fdc r env)
    | MultC(l, r) -> num_mult (interp_check_fdc l env) (interp_check_fdc r env)
    | FdC(n, a, b) -> FunV(n, a, b)


let _ = 
    let e = PlusC( NumC 10, AppC( FdC( "const5", "_", NumC 5), NumC 10 )) in
    let fds = mt_env in
    interp e fds |> print_value |> print_newline

let _ = 
    let e = AppC( FdC( "f1", "x", AppC( FdC("f2", "y", PlusC(IdC "x", IdC "y")), NumC 4)), NumC 3) in
    let fds = mt_env in
    interp e fds |> print_value |> print_newline