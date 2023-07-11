open! Core

let no_dots city = String.substr_replace_all city ~pattern:"." ~with_:""

let load_file file =
  let file_list = In_channel.read_lines (File_path.to_string file) in
  let final_cities =
    List.fold file_list ~init:[] ~f:(fun city_list s ->
      let potential_cities = String.split s ~on:',' in
      match potential_cities with
      | interstate :: highway ->
        city_list
        @ List.foldi highway ~init:[] ~f:(fun index acc current_city ->
            if index < List.length highway - 1
            then
              acc
              @ [ ( no_dots current_city
                  , interstate
                  , no_dots (List.nth_exn highway (index + 1)) )
                ]
            else acc)
      | _ -> city_list)
  in
  let () =
    print_s [%message (final_cities : (string * string * string) list)]
  in
  final_cities
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

module MyString = struct
  include String

  let default = "hello"
end

module G = Graph.Imperative.Graph.ConcreteLabeled (String) (MyString)

module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes e = [ `Dir `None; `Label (G.E.label e) ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

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
        let network = load_file input_file in
        let graph = G.create () in
        List.iter network ~f:(fun (city_1, highway, city_2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          let edge = G.E.create city_1 highway city_2 in
          let () =
            print_s
              [%message (city_1, highway, city_2 : string * string * string)]
          in
          G.add_edge_e graph edge);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
