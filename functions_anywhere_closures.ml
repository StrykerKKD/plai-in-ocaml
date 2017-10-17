open Core

type symbol = string

and exprC =
  | NumC of int
  | IdC of symbol
  | AppC of exprC * exprC
  | PlusC of exprC * exprC
  | MultC of exprC * exprC
  | LamC of symbol * exprC [@@deriving show]

type value =
  | NumV of int
  | ClosV of symbol * exprC * env

and binding =
  |Bind of symbol * value

and env = binding list [@@deriving show]

let mt_env = []
let extend_env = List.cons

let print_value = function
  | NumV v -> printf "%d \n" v
  | ClosV(arg, body, env) -> printf "arg:(%s)\nbody:(%s)\nenv:(%s)\n" arg  (show_exprC body) (show_env env)

let is_lamc = function
  | LamC _ -> true
  | _ -> false

let numv_n = function
  | ClosV _ -> failwith "got ClosV instead of NumV"
  | NumV n -> n

let is_numv = function
  | NumV _ -> true
  | ClosV _ -> false

let closv_arg = function
  | ClosV(arg, body, env) -> arg
  | NumV _ -> failwith "got NumV instead of ClosV"

let closv_body = function
  | ClosV(arg, body, env) -> body
  | NumV _ -> failwith "got NumV instead of ClosV"

let closv_env = function
  | ClosV(arg, body, env) -> env
  | NumV _ -> failwith "got NumV instead of ClosV"

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
    let f_value = interp f env in
    let expr = closv_body f_value in
    let bind = Bind(closv_arg f_value, interp a env) in
    let env = extend_env bind (closv_env f_value) in
    interp expr env
  | PlusC(l, r) -> num_plus (interp l env) (interp r env)
  | MultC(l, r) -> num_mult (interp l env) (interp r env)
  | LamC(a, b) -> ClosV(a, b, env)


let rec interp_check_lamc expr env =
  match expr with
  | NumC n -> NumV n
  | IdC n -> lookup n env
  | AppC(f, a) when is_lamc f ->
    let f_value = interp_check_lamc f env in
    let expr = closv_body f_value in
    let bind = Bind(closv_arg f_value, interp_check_lamc a env) in
    let env = extend_env bind (closv_env f_value) in
    interp_check_lamc expr env
  | AppC _ -> failwith "the input is not a function"
  | PlusC(l, r) -> num_plus (interp_check_lamc l env) (interp_check_lamc r env)
  | MultC(l, r) -> num_mult (interp_check_lamc l env) (interp_check_lamc r env)
  | LamC(a, b) -> ClosV(a, b, env)

let rec interp_present_env expr env = 
  match expr with
  | NumC n -> NumV n
  | IdC n -> lookup n env
  | AppC(f, a) ->
    let f_value = interp_present_env f env in
    let expr = closv_body f_value in
    let bind = Bind(closv_arg f_value, interp_present_env a env) in
    let env = extend_env bind env in
    interp_present_env expr env
  | PlusC(l, r) -> num_plus (interp_present_env l env) (interp_present_env r env)
  | MultC(l, r) -> num_mult (interp_present_env l env) (interp_present_env r env)
  | LamC(a, b) -> ClosV(a, b, env)

let _ = 
  let e = PlusC( NumC 10, AppC( LamC( "_", NumC 5), NumC 10 )) in
  let fds = mt_env in
  interp e fds |> print_value

let _ = 
  let e = AppC( LamC( "x", AppC( LamC( "y", PlusC(IdC "x", IdC "y")), NumC 4)), NumC 3) in
  let fds = mt_env in
  interp e fds |> print_value

(*TODO: Figure out how to make this throw an unbound identifier error.*)
let _ =
  let e = AppC( LamC( "f", LamC("x", AppC( IdC "f", NumC 10))), LamC( "y", PlusC( IdC "x", IdC "y"))) in
  let fds = mt_env in
  interp e fds |> print_value