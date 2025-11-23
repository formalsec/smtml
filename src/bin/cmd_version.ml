let version_string v =
  match v with None -> "n/a" | Some v -> Build_info.V1.Version.to_string v

let version = version_string @@ Build_info.V1.version ()

let run () =
  Fmt.pr "version: %s@." version;
  let libs = Build_info.V1.Statically_linked_libraries.to_list () in
  Fmt.pr "statically linked libraries:@.";
  List.iter
    (fun lib ->
      let name = Build_info.V1.Statically_linked_library.name lib in
      let version = Build_info.V1.Statically_linked_library.version lib in
      Fmt.pr "- %s (%s)@." name (version_string version) )
    libs
