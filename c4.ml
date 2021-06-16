(* mystery operator 1 *)
let ( $ ) f x = f x (* applies f to x *)

(* mystery operator 2 *)
let ( @@ ) f g x = x |> g |> f (* applies g and then f to x *)

(* repeat *)
let rec repeat f n x = if n = 0 then x else repeat f (n - 1) (f x)

(* product *)
let product_left = List.fold_left (fun a b -> a * b) 1

let product_right lst = List.fold_right (fun a b -> a * b) lst 1

(* clip *)
let clip n = if n < 0 then 0 else if n > 10 then 10 else n

let cliplist_map = List.map clip

let rec cliplist_rec = function
  | [] -> []
  | hd :: tl -> clip hd :: cliplist_rec tl

(* sum_cube_odd *)

let sum_cube_odd n =
  let rec range i j =
    if i > j then [] else if i = j then [ j ] else i :: range (i + 2) j
  in
  range 0 n
  |> List.map (fun n -> n * n * n)
  |> List.fold_right (fun a b -> a + b)

(* exists *)
let rec exists_rec p = function
  | [] -> false
  | hd :: tl -> if p hd then true else exists_rec p tl

let exists_fold p lst = List.fold_left (fun a b -> a || p b) false lst

(* budget *)
let budget_process budget expenses =
  budget - List.fold_right (fun a b -> a + b) expenses 0

(* library uncurried *)
let uncurried_nth (lst, n) = List.nth lst n

let uncurried_append (l1, l2) = List.append l1 l2

let uncurried_compare (c1, c2) = Char.compare c1 c2

let uncurried_max (a, b) = Stdlib.max a b

(* uncurry *)
let uncurry f (x, y) = f x y

(* let uncurried_nth' = uncurry List.nth *)

(* let uncurried_append' = uncurry List.append *)

(* let uncurried_compare' = uncurry Char.compare *)

(* let uncurried_max' = uncurry Stdlib.max *)

(* curry *)
let curry f x y = f (x, y)

(* terse product *)
let terse_product = List.fold_left ( * ) 1

(* map composition *)
let map_compose f g = List.map (f g)

(* more list fun *)
let gt3 = List.filter (fun s -> String.length s > 3)

let add1 = List.rev_map (fun f -> f +. 1.)

let join strs sep =
  let rec join_h acc strs sep =
    match strs with [] -> acc | hd :: tl -> join_h (acc ^ sep ^ hd) tl sep
  in
  match strs with [] -> "" | hd :: tl -> join_h hd tl sep

(* tree map *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec tree_map f = function
  | Leaf -> Leaf
  | Node (d, l, r) -> Node (f d, tree_map f l, tree_map f r)

(* valid matrix *)
let is_valid_matrix m =
  if List.length m < 1 then false
  else
    let s = List.length (List.nth m 1) in
    if s < 1 then false
    else m |> List.map List.length |> List.for_all (fun n -> n = s)

(* row vector add *)
let add_row_vectors = List.map2 (fun a b -> a + b)

(* matrix add *)
let add_matricies = List.map2 add_row_vectors
