(* complex synonym *)
module type ComplexSig = sig
  type t = float * float

  val zero : t

  val add : t -> t -> t
end

(* binary search tree dictionary *)
exception Not_found

module type Dictionary = sig
  type ('k, 'v) t

  (* The empty dictionary *)
  val empty : ('k, 'v) t

  (* [insert k v d] produces a new dictionary [d'] with the same mappings 
   * as [d] and also a mapping from [k] to [v], even if [k] was already 
   * mapped in [d]. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (* [lookup k d] returns the value associated with [k] in [d].  
   * raises:  [Not_found] if [k] is not mapped to any value in [d]. *)
  val lookup : 'k -> ('k, 'v) t -> 'v
end

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

module BstDict : Dictionary = struct
  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

  let insert k v d =
    let rec h k v n =
      match n with
      | Leaf -> Node ((k, v), Leaf, Leaf)
      | Node ((k', v'), l, r) ->
          if k < k' then Node ((k', v'), h k v l, r)
          else if k > k' then Node ((k', v'), l, h k v r)
          else Node ((k, v), l, r)
    in
    h k v d

  let lookup k d =
    let rec h k n =
      match n with
      | Leaf -> raise Not_found
      | Node ((k', v'), l, r) ->
          if k < k' then h k l else if k > k' then h k r else v'
    in
    h k d
end

(* fraction *)
module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int

  val denominator : t -> int

  val to_string : t -> string

  val to_float : t -> float

  val add : t -> t -> t

  val mul : t -> t -> t
end

module FractionImpl : Fraction with type t = int * int = struct
  type t = int * int

  let make n d = (n, d)

  let numerator = function n, _ -> n

  let denominator = function _, d -> d

  let to_string = function n, d -> string_of_int n ^ "/" ^ string_of_int d

  let to_float = function n, d -> float_of_int n /. float_of_int d

  let add x y =
    match (x, y) with (n1, d1), (n2, d2) -> ((n1 * d2) + (n2 * d1), d1 * d2)

  let mul x y = match (x, y) with (n1, d1), (n2, d2) -> (n1 * n2, d1 * d2)
end

(* fraction reduced *)
module FractionReduced : Fraction = struct
  include FractionImpl

  (* [gcd x y] is the greatest common divisor of [x] and [y].
   * requires: [x] and [y] are positive.
   *)
  let rec gcd x y =
    if x = 0 then y else if x < y then gcd (y - x) x else gcd y (x - y)

  let simplify (x : t) : t =
    match x with
    | n, d ->
        let fac = gcd n d in
        let n' = n / fac in
        let d' = d / fac in
        (n', d')

  let add x y : t = simplify (add x y)
end

(* make char map *)
module CharMap = Map.Make (Char)

(* use char map *)
let char_map =
  CharMap.(
    empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra"
    |> add 'V' "Victor")

let _ = CharMap.find 'E' char_map

let char_map = CharMap.remove 'A' char_map

let _ = CharMap.mem 'A' char_map

let _ = CharMap.bindings char_map

(* bindings *)
let _ =
  CharMap.(empty |> add 'x' 0 |> add 'y' 1 |> bindings)
  = CharMap.(
      empty |> add 'x' 2 |> add 'y' 1 |> remove 'x' |> add 'x' 0 |> bindings)

(* date order *)
type date = { month : int; day : int }

module Date = struct
  type t = date

  let compare x y =
    match (x, y) with
    | { month = m1; day = d1 }, { month = m2; day = d2 } ->
        if m1 < m2 then -1
        else if m1 > m2 then 1
        else if d1 < d2 then -1
        else if d1 > d2 then 1
        else 0
end

(* calendar *)
module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let _ =
  DateMap.(
    empty
    |> add { month = 1; day = 1 } "New Year's Day"
    |> add { month = 10; day = 31 } "Halloween")

(* print calendar *)
let print_calendar cal =
  DateMap.iter
    (fun { month; day } event ->
      print_endline (string_of_int month ^ string_of_int day ^ ": " ^ event))
    cal

(* is for *)
let is_for m = CharMap.mapi (fun k v -> Printf.sprintf "%c is for %s" k v) m

(* first after *)
let first_after cal d =
  match DateMap.find_first (fun d' -> d' > d) cal with _, event -> event

(* sets *)
module Cis = struct
  type t = string

  let compare a b =
    String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)
end

module CisSet = Set.Make (Cis)

let _ = CisSet.(equal (of_list [ "grr"; "argh" ]) (of_list [ "GRR"; "aRgh" ]))

(* ToString *)
module type ToString = sig
  type t

  val to_string : t -> string
end

(* Print *)
module Print (M : ToString) = struct
  let print v = print_endline (M.to_string v)
end

(* Print Int *)
module Int = struct
  type t = int

  let to_string = string_of_int
end

module PrintInt = Print (Int)

(* Print String *)
module MyString = struct
  type t = string

  let to_string s = s
end

module PrintString = Print (MyString)

(* Print String reuse revisited *)
module StringWithPrint = struct
  include String
  include Print (MyString)
end
