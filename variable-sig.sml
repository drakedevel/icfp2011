(* A simple interface for named variables. Support is provided
 * for creating fresh variables as well as comparing variables. *)
signature VARIABLE =
sig
  eqtype var

  (* Create a variable with a given base name *)
  val named : string -> var

  (* Create a fresh variable with the same name as a given variable *)
  val clone : var -> var

  (* Create a fresh variable with no base name *)
  val new : unit -> var

  (* Get a string representation of a variable *)
  val name : var -> string

  (* Compare two variables for equality *)
  val equal : var * var -> bool

  (* Impose an ordering on two variables *)
  val compare : var * var -> order

end
