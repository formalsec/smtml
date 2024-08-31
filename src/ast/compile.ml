let until_rewrite filename =
  let script = Parse.from_file filename in
  Rewrite.rewrite script
