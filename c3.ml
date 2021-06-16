(* list expressions *)
let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1; 2; 3; 4; 5 ]

let _ = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* product *)
let rec product = function [] -> 1 | h :: t -> h * product t

(* concat *)
let rec concat = function [] -> "" | h :: t -> h ^ concat t

(* patterns *)
let first_bigred = function [] -> false | h :: _ -> h = "bigred"

let two_or_four lst =
  let rec h acc lst =
    match lst with [] -> acc = 2 || acc = 4 | _ :: t -> h (acc + 1) t
  in
  h 0 lst

let two_equal = function [] | [ _ ] -> false | h1 :: h2 :: _ -> h1 = h2

(* library *)
let fifth = function _ :: _ :: _ :: _ :: n :: _ -> n | _ -> 0

let sort lst = lst |> List.sort Stdlib.compare |> List.rev

(* library puzzle *)
let last_unchecked lst = List.nth lst (List.length lst - 1)

let any_zeroes lst = List.length (List.filter (fun n -> n = 0) lst) > 0

(* take drop *)
let take n lst =
  let rec helper acc n lst =
    if n = 0 then acc
    else match lst with [] -> acc | h :: t -> helper (acc @ [ h ]) (n - 1) t
  in
  helper [] n lst

let rec drop n lst =
  if n = 0 then lst else match lst with [] -> [] | _ :: t -> drop (n - 1) t

(* unimodal *)
let is_unimodal lst =
  let rec helper asc last lst =
    match lst with
    | [] -> true
    | h :: t ->
        if asc then if h >= last then helper true h t else helper false h t
        else if h <= last then helper false h t
        else false
  in
  helper true min_int lst

(* powerset *)

(* print int list rec *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      print_int_list t

(* print int list iter *)
let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst

(* student *)
type student = { first_name : string; last_name : string; gpa : float }

let _ = { first_name = "John"; last_name = "Smith"; gpa = 4.0 }

let student_name student = (student.first_name, student.last_name)

let student_new first_name last_name gpa = { first_name; last_name; gpa }

(* pokerecord *)
type poketype = Normal | Fire | Water

type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "Charizard"; hp = 78; ptype = Fire }

let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

(* safe hd and tl *)
let safe_hd = function [] -> None | h :: _ -> Some h

let rec safe_tl = function [] -> None | [ h ] -> Some h | _ :: t -> safe_tl t

(* pokefun *)
let max_hp lst =
  let rec helper max lst =
    match lst with
    | [] -> max
    | h :: t -> (
        match max with
        | Some max_p ->
            if h.hp > max_p.hp then helper (Some h) t else helper max t
        | None -> helper (Some h) t)
  in
  helper None lst

(* date before *)
type date = int * int * int

let is_before d1 d2 =
  let year = function y, _, _ -> y in
  let month = function _, m, _ -> m in
  let day = function _, _, d -> d in

  let y1 = year d1 in
  let y2 = year d2 in

  if y1 < y2 then true
  else if y1 > y2 then false
  else
    let m1 = month d1 in
    let m2 = month d2 in

    if m1 < m2 then true
    else if m1 > m2 then false
    else
      let a1 = day d1 in
      let a2 = day d2 in

      if a1 < a2 then true else false

(* earliest date *)
let earliest lst =
  let rec helper e lst =
    match lst with
    | [] -> e
    | h :: t -> (
        match e with
        | Some ed -> if is_before h ed then helper (Some h) t else helper e t
        | None -> helper (Some h) t)
  in
  helper None lst

(* assoc list *)
let insert k v d = (k, v) :: d

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let assoc_list = [] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three"

let _ = lookup 2 assoc_list

let _ = lookup 4 assoc_list

(* cards *)
type suit = Clubs | Diamonds | Hearts | Spades

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = { suit : suit; rank : rank }

let ace_clubs = { suit = Clubs; rank = Ace }

let queen_hearts = { suit = Hearts; rank = Queen }

let two_diamonds = { suit = Diamonds; rank = Two }

let seven_spades = { suit = Spades; rank = Seven }

(* matching *)
let _ = [ None ] (* (Some x)::tl *)

let _ = [ None ] (* [Some 3110; None] *)

let _ = [ None ] (* [Some x; _] *)

let _ = [ None ] (* h1::h2::tl *)

(* quadrant *)
type quad = I | II | III | IV

type sign = Neg | Zero | Pos

let sign x : sign = if x < 0 then Neg else if x > 0 then Pos else Zero

let quadrant = function
  | x, y -> (
      match (sign x, sign y) with
      | Pos, Pos -> Some I
      | Pos, Neg -> Some II
      | Neg, Neg -> Some III
      | Neg, Pos -> Some IV
      | _ -> None)

(* quadrant when *)
let quadrant_when = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x > 0 && y < 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x < 0 && y > 0 -> Some IV
  | _ -> None

(* depth *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let depth tree =
  let rec helper depth = function
    | Leaf -> depth
    | Node (_, left, right) ->
        Stdlib.max (helper (depth + 1) left) (helper (depth + 1) right)
  in
  helper 0 tree

(* shape *)
let rec shape t1 t2 =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Node _, Leaf | Leaf, Node _ -> false
  | Node (_, l1, r1), Node (_, l2, r2) -> shape l1 l2 && shape r1 r2

(* list max exn *)
let list_max = function
  | [] -> raise (Failure "list_max")
  | lst ->
      let rec h max = function
        | [] -> max
        | hd :: tl -> if hd > max then h hd tl else h max tl
      in
      h min_int lst

(* list max exn string *)
let list_max_string = function
  | [] -> "empty"
  | lst ->
      let rec h max = function
        | [] -> max
        | hd :: tl -> if hd > max then h hd tl else h max tl
      in
      string_of_int (h min_int lst)

(* is_bst *)

(* quadrant poly *)
let sign_poly x = if x < 0 then `Neg else if x > 0 then `Pos else `Zero

let quadrant = function
  | x, y -> (
      match (sign x, sign y) with
      | Pos, Pos -> Some `I
      | Pos, Neg -> Some `II
      | Neg, Neg -> Some `III
      | Neg, Pos -> Some `IV
      | _ -> None)
