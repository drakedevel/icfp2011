structure Variable :> VARIABLE =
struct
  type var = int * string

  local
      val next_stamp = ref 0
  in
  fun new_stamp () = !next_stamp before next_stamp := !next_stamp + 1
  end

  (* Create a variable with a given base name *)
  fun named s = (new_stamp (), s)

  (* Create a fresh variable with the same name as a given variable *)
  fun clone (_, s) = named s

  (* Create a fresh variable with no base name *)
  fun new () = named "$"

  (* Get a string representation of a variable *)
  fun name (n, s) = s ^ "_" ^ (Int.toString n)

  (* Compare two variables for equality *)
  fun equal ((n1, _), (n2, _)) = n1 = n2

  (* Impose an ordering on two variables *)
  fun compare ((n1, _), (n2, _)) = Int.compare (n1, n2)

end
