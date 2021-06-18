(* mutable fields *)
type student = { name : string; mutable gpa : float }

let alice = { name = "Alice"; gpa = 3.7 }

;;
alice.gpa <- 4.0

(* refs *)
let (_ : bool ref) = ref true

let (_ : int list ref) = ref [ 1; 2 ]

let (_ : int ref list) = [ ref 1; ref 2 ]
