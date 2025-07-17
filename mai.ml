type expr =
  | Num of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec eval expr =
  match expr with
  | Num n -> n
  | Add (e1, e2) -> eval e1 +. eval e2
  | Sub (e1, e2) -> eval e1 -. eval e2
  | Mul (e1, e2) -> eval e1 *. eval e2
  | Div (e1, e2) ->
      let v2 = eval e2 in
      if v2 = 0.0 then failwith "Division by zero"
      else eval e1 /. v2

let rec parse_expr tokens =
  let rec parse_factor tokens =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | "(" :: rest ->
        let (expr, rest') = parse_expr rest in
        (match rest' with
         | ")" :: rest'' -> (expr, rest'')
         | _ -> failwith "Missing closing parenthesis")
    | token :: rest ->
        try (Num (float_of_string token), rest)
        with Failure _ -> failwith ("Invalid token: " ^ token)
  in
  let rec parse_term tokens =
    let (e1, rest) = parse_factor tokens in
    match rest with
    | "*" :: rest' ->
        let (e2, rest'') = parse_term rest' in
        (Mul (e1, e2), rest'')  
    | "/" :: rest' ->
        let (e2, rest'') = parse_term rest' in
        (Div (e1, e2), rest'') 
    | _ -> (e1, rest) 
  in
  let (e1, rest) = parse_term tokens in
  match rest with
  | "+" :: rest' ->
      let (e2, rest'') = parse_expr rest' in
      (Add (e1, e2), rest'')
  | _ -> (e1, rest)
  
let parse input =
  let tokens = String.split_on_char ' ' (String.trim input) in
  let (expr, rest) = parse_expr tokens in
  if rest <> [] then failwith ("Unexpected tokens: " ^ String.concat " " rest)
  else expr

let test input =
  try
    let expr = parse input in
    let result = eval expr in
    print_endline (string_of_float result)
  with Failure msg ->
    print_endline ("Error: " ^ msg)

let () =
  test "2 + 3 * 4" ;; 
  test "( 1 + 2 ) * 3" ;;
  test "10 / 2" ;; 
  test "6 * 2 / 3" ;; 
  test "5 / 0" ;; 
  test "2 + a" ;; 