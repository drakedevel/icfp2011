structure Log =
struct
  local open TextIO in

  (* whether we are currently logging *)
  val logging : bool ref = ref true

  fun logRaw s = if ! logging then output (stdErr, s) else ()
  fun log s = logRaw (s ^ "\n")

  end
end
