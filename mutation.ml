open Core

type location = int
type symbol = string

and exprC =
  | NumC of int
  | IdC of symbol
  | AppC of exprC * exprC
  | PlusC of exprC * exprC
  | MultC of exprC * exprC
  | LamC of symbol * exprC
  | BoxC of exprC
  | UnboxC of exprC
  | SetboxC of exprC * exprC
  | SeqC of exprC * exprC

type value =
  | NumV of int
  | ClosV of symbol * exprC * env
  | BoxV of location

and binding =
  | Bind of symbol * location

and env = binding list

type storage = 
  | Cell of location * value

type store = storage list

let mt_store = []
let override_store = List.cons

let mt_env = []
let extend_env = List.cons

let is_lamc = function
  | LamC _ -> true
  | _ -> false

let numv_n = function
  | NumV n -> n
  | ClosV _ -> failwith "got ClosV instead of NumV"
  | BoxV _ -> failwith "got BoxV instead of NumV"

let is_numv = function
  | NumV _ -> true
  | _ -> false

let closv_arg = function
  | ClosV(arg, body, env) -> arg
  | NumV _ -> failwith "got NumV instead of ClosV"
  | BoxV _ -> failwith "got BoxV instead of ClosV"

let closv_body = function
  | ClosV(arg, body, env) -> body
  | NumV _ -> failwith "got NumV instead of ClosV"
  | BoxV _ -> failwith "got BoxV instead of ClosV"

let closv_env = function
  | ClosV(arg, body, env) -> env
  | NumV _ -> failwith "got NumV instead of ClosV"
  | BoxV _ -> failwith "got BoxV instead of ClosV"

let bind_name = function
  | Bind(name, value) -> name

let bind_location = function
  | Bind(name, value) -> value

let store_location = function
  | Cell(location, value) -> location

let store_value = function
  | Cell(location, value) -> value

let boxv_v = function
  | NumV _ -> failwith "got NumV instead of BoxV"
  | ClosV _ -> failwith "got ClosV instead of BoxV"
  | BoxV v -> v

let rec lookup for_ env =
  match env with
  | [] -> failwith "location not found"
  | head :: tail ->
    if for_ = (bind_name head) then
      bind_location head
    else
      lookup for_ tail

let rec fetch loc sto =
  match sto with
  | [] -> failwith "value not found"
  | head :: tail ->
    if loc = (store_location head) then
      store_value head
    else
      fetch loc tail

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