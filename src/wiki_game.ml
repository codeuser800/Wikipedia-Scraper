open! Core
open! String
open! Str

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)

type article =
  { name : string
  ; url : string
  }
[@@deriving sexp]

let wiki_link to_check =
  let value = String.split_on_chars to_check ~on:[ '/' ] in
  match value with
  | _ :: compare :: _ -> if String.( = ) compare "wiki" then true else false
  | _ -> false
;;

(*Remove duplicates from the list*)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.fold ~init:[] ~f:(fun acc a ->
       let attr = R.attribute "href" a in
       let wiki_type = Wikipedia_namespace.namespace attr in
       if wiki_link attr
       then (
         match wiki_type with
         | None -> acc @ [ R.attribute "href" a ]
         | Some _name -> acc)
       else acc)
  |> List.dedup_and_sort ~compare:String.compare
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

let remove_url city =
  (* String.substr_replace_all city ~pattern:"/wiki/" ~with_:"" |> *)
  String.substr_replace_all city ~pattern:"(" ~with_:""
  |> String.substr_replace_all ~pattern:")" ~with_:""
  |> String.substr_replace_all ~pattern:"/" ~with_:""
;;

let next_articles ~origin ~how_to_fetch =
  let contents = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
  let articles = get_linked_articles contents in
  articles
;;

let check_if_link link =
  if String.is_substring link ~substring:"https://en.wikipedia.org"
  then link
  else "https://en.wikipedia.org" ^ link
;;

let rec bfs_wiki ~(depth : int) ~visited ~queue ~path ~end_link ~how_to_fetch
  =
  let curr_link = Queue.dequeue queue in
  match curr_link with
  | None -> None
  | Some curr_node ->
    let curr_node = check_if_link curr_node in
    let node_neighbors = next_articles ~origin:curr_node ~how_to_fetch in
    (match node_neighbors with
     | [] ->
       print_s [%message "here"];
       bfs_wiki
         ~depth:(depth - 1)
         ~visited
         ~queue
         ~path
         ~end_link
         ~how_to_fetch
     | _ ->
       List.fold_until
         node_neighbors
         ~init:path
         ~f:(fun acc curr_neighbor ->
           let curr_neighbor = check_if_link curr_neighbor in
           let visited = Core.Set.add visited curr_node in
           if String.equal curr_neighbor end_link || Int.( = ) depth 0
           then (
             print_endline "finished";
             Continue_or_stop.Stop (Some (path @ [ curr_neighbor ])))
           else if not (Core.Set.mem visited curr_neighbor)
           then (
             Queue.enqueue queue curr_neighbor;
             let current_prog =
               bfs_wiki
                 ~depth:(depth - 1)
                 ~visited
                 ~queue
                 ~path:(path @ [ curr_neighbor ])
                 ~end_link
                 ~how_to_fetch
             in
             match current_prog with
             | Some list -> Stop (Some list)
             | None -> Continue acc)
           else Continue acc)
         ~finish:(fun _acc -> None))
;;

let rec get_all_articles
  ~(article_list : (article * article) list)
  ~(curr_link : string)
  ~(curr_article : article)
  ~(visited_links : String.Hash_set.t)
  ~(depth : int)
  ~how_to_fetch
  : (article * article) list
  =
  if Int.( = ) depth 0
  then article_list
  else (
    Core.Hash_set.add visited_links curr_link;
    let next_articles = next_articles ~origin:curr_link ~how_to_fetch in
    List.fold next_articles ~init:article_list ~f:(fun acc next_article ->
      let next_article_rec =
        { name = remove_url next_article; url = next_article }
      in
      let edge = curr_article, next_article_rec in
      (* let () = print_s [%message (edge : article * article)] in *)
      let new_article_list = edge :: acc in
      (* let () = print_s [%message (new_article_list : (article * article)
         list)] in *)
      if not (Core.Hash_set.mem visited_links next_article)
      then
        acc
        @ get_all_articles
            ~article_list:new_article_list
            ~curr_link:next_article
            ~curr_article:next_article_rec
            ~visited_links
            ~depth:(depth - 1)
            ~how_to_fetch
      else new_article_list))
;;

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `Forward ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
let visualize ?(max_depth = 5) ~origin ~output_file ~how_to_fetch () : unit =
  (* ignore (output_file : File_path.t); *)
  (* ignore (how_to_fetch : File_fetcher.How_to_fetch.t); *)
  let curr_article = { name = remove_url origin; url = origin } in
  let set : String.Hash_set.t = Core.String.Hash_set.create () in
  let edges =
    get_all_articles
      ~article_list:[]
      ~curr_link:origin
      ~curr_article
      ~visited_links:set
      ~depth:max_depth
      ~how_to_fetch
  in
  let () = print_s [%message (edges : (article * article) list)] in
  let graph = G.create () in
  List.iter edges ~f:(fun (first_article, second_article) ->
    (* [G.add_edge] auomatically adds the endpoints as vertices in the graph
       if they don't already exist. *)
    let first_name = first_article.name in
    let second_name = second_article.name in
    G.add_edge graph first_name second_name);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)
let find_path ~max_depth ~origin ~destination ~how_to_fetch () =
  let queue = Queue.create () in
  let () = Queue.enqueue queue origin in
  bfs_wiki
    ~depth:max_depth
    ~visited:String.Set.empty
    ~queue
    ~path:[ origin ]
    ~end_link:destination
    ~how_to_fetch
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
