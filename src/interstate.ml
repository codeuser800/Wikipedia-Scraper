open! Core

let load_file file =
  let adjacency_list = Set.empty in
  In_channel.read_lines (File_path.to_string file)
  |> List.fold ~init:adjacency_list ~f:(fun s ->
       (* let () = print_s [%message (s : string)] in [ s ]) *)
       let (interstate :: highway) = String.split s ~on:',' in let current_highway = 
       List.mapi highway ~f:(index current city ->
       if i < List.length highway 
      then (current_city, List.ne)
      else ) 
      in 
;;

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let _return_list = load_file input_file in
        ()]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
