structure X = struct
  val _ = OS.Process.exit (Top.main (CommandLine.name (), CommandLine.arguments ()))
end
