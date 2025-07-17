open Printf

type expr =
  | Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec eval expr = 
  match expr with
  | Num n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> 
      let divisor = eval e2 in
      if divisor = 0 then failwith "Division by zero"
      else eval e1 / divisor

let parse_op op x y = 
  match op with 
  | "add" -> Add (Num(int_of_string x), Num(int_of_string y))
  | "sub" -> Sub (Num(int_of_string x), Num(int_of_string y))
  | "mul" -> Mul (Num(int_of_string x), Num(int_of_string y))
  | "div" -> Div (Num(int_of_string x), Num(int_of_string y))
  | _ -> failwith "Unknown operation"

let safe_eval expr = 
  try
    match expr with 
    | op::x::y::_->string_of_int(eval(parse_op op x y))
    | _ -> failwith "Invalid expression format"
  with
  | Failure msg -> "Error: " ^ msg
  | _ -> "Error: Unknown error occurred";;


printf"Result: %s\n" (safe_eval ["add"; "3"; "4"]); (* Example usage *)