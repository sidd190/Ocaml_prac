type 'a bst = 
  | Empty
  | Node of 'a * 'a bst * 'a bst

let rec insert value tree =
  match tree with
  | Empty -> Node (value, Empty, Empty)
  | Node (v, left, right) ->
      if value < v then
        Node (v, insert value left, right)
      else if value > v then
        Node (v, left, insert value right)
      else
        tree  (* No duplicates allowed *)

let rec search value tree =
  match tree with
  | Empty -> false
  | Node(v,left,right)->
      if value = v then
        true
      else if value < v then
        search value left
      else
        search value right

let inorder tree = 
  let rec inorder_acc tree acc = 
    match tree with
    | Empty -> acc
    | Node (v, left, right) ->
        inorder_acc left (v :: inorder_acc right acc)
  in
  inorder_acc tree []    

let rec min_value tree =
  match tree with
  | Empty -> failwith "Tree is empty"
  | Node (v, Empty, _) -> v
  | Node (_, left, _) -> min_value left

let rec delete value tree = 
  match tree with
  | Empty -> Empty
  | Node(v,left, right)->
    if value < v then
      Node(v,delete value left, right)
    else if value > v then
      Node(v,left,delete value right)
    else
      match left, right with 
      | Empty, _ -> right
      | _, Empty -> left
      |_,_ ->
        let min_right = min_value right in
        Node(min_right, left, delete min_right right)


let rec height tree =
  match tree with
  | Empty -> -1
  | Node (_,left,right) ->
    1 + max(height left) (height right)


let tree = insert 5 Empty ;;
let tree = insert 3 tree ;;
let tree = insert 7 tree ;;
let tree = insert 1 tree ;;

print_endline (string_of_bool (search 3 tree)) ;; (* Prints: true *)
print_endline (string_of_int (height tree)) ;; (* Prints: 2 *)
(* let tree = delete 3 tree ;; *)
print_endline (string_of_bool (search 3 tree)) ;; (* Prints: false *)
let result = inorder tree ;;
List.iter (fun x -> print_int x; print_string " ") result ;; (* Prints: 1 5 7 *)