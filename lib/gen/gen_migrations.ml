(* Embeds migrations/<version>_<name>.sql files as an OCaml list, so the binary needs no
   files beside it. Arguments that are not .sql files are ignored. *)

let () =
  let files =
    Array.to_list Sys.argv
    |> List.tl
    |> List.filter (fun f -> Filename.check_suffix f ".sql")
    |> List.sort (fun a b -> compare (Filename.basename a) (Filename.basename b))
  in
  print_string "(* Generated from migrations/*.sql by gen/gen_migrations.ml. *)\n\n";
  print_string "let all = [\n";
  List.iter
    (fun file ->
      let base = Filename.chop_suffix (Filename.basename file) ".sql" in
      let version, name =
        match String.index_opt base '_' with
        | Some i ->
          String.sub base 0 i, String.sub base (i + 1) (String.length base - i - 1)
        | None -> failwith ("migration file without a version: " ^ base)
      in
      let ic = open_in_bin file in
      let sql = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Printf.printf "  %d, %S, %S;\n" (int_of_string version) name sql)
    files;
  print_string "]\n"
;;
