open! Core

let solve file =
  let file_list = In_channel.read_lines (File_path.to_string file) in
  let array =
    List.fold file_list ~init:[ [] ] ~f:(fun acc curr_row ->
      acc @ [ String.to_list curr_row ])
  in
  let () = print_s [%message (array : char list list)] in 
  let graph = (Int * Int).Map.empty in 
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () -> solve input_file]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
