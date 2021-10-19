open Core
open Cmdliner
open Scil_tip
open Analysis

let show_cfg p = Cfg.show (Cfg.generate_from_program p)

(* let show_icfg = Inter_cfg.show % Inter_cfg.generate_from_program *)

let ae p =
  let open Printf in
  print_endline "Available Expressions";
  let graph = Cfg.generate_from_program p in
  let module AvailableExpressions = Available_expressions.Solve (struct
    let graph = graph
  end) in
  let open AvailableExpressions in
  let labels = Cfg.labels graph in
  print_endline "Entry";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string))
    labels;
  print_endline "Exit";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string))
    labels

let rd p =
  let open Printf in
  print_endline "Reaching Definitions";
  let graph = Cfg.generate_from_program p in
  let module ReachingDefinitions = Reaching_definitions.Solve (struct
    let graph = graph
  end) in
  let open ReachingDefinitions in
  let labels = Cfg.labels graph in
  print_endline "Entry";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string))
    labels;
  print_endline "Exit";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string))
    labels

let vb p =
  let open Printf in
  print_endline "Very Busy Expressions";
  let graph = Cfg.generate_from_program p in
  let module VeryBusyExpressions = Very_busy_expressions.Solve (struct
    let graph = graph
  end) in
  let open VeryBusyExpressions in
  let labels = Cfg.labels graph in
  print_endline "Entry";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string))
    labels;
  print_endline "Exit";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string))
    labels

let lv p =
  let open Printf in
  print_endline "Live Variables";
  let graph = Cfg.generate_from_program p in
  let module LiveVariables = Live_variables.Solve (struct
    let graph = graph
  end) in
  let open LiveVariables in
  let labels = Cfg.labels graph in
  print_endline "Entry";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string))
    labels;
  print_endline "Exit";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string))
    labels

let sa p =
  let open Printf in
  print_endline "Sign";
  let graph = Cfg.generate_from_program p in
  let module SignAnalysis = Sign.Solve (struct
    let graph = graph
  end) in
  let open SignAnalysis in
  let labels = Cfg.labels graph in
  print_endline "Entry";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_entry_result l |> result_to_string))
    labels;
  print_endline "Exit";
  Set.iter
    ~f:(fun l -> printf "%d: %s\n" l (get_exit_result l |> result_to_string))
    labels

let ta p =
  let open Printf in
  print_endline "Taint Analysis";
  let graph = Cfg.generate_from_program p in
  let module TaintAnalysis = Taint.Solve (struct
    let graph = graph
  end) in
  let open TaintAnalysis in
  let sinks =
    Hashtbl.fold
      ~f:(fun ~key:l ~data:n acc ->
        let open Cfg_node in
        match n.stmt_s with
        | Cfg_call (_, args) ->
            List.fold_left ~f:(fun acc n -> (l, n) :: acc) ~init:acc args
        | _ -> acc)
      (Cfg.get_blocks graph) ~init:[]
    |> List.rev
  in
  let aux (l, e) =
    let result = snd (get_entry_result l) in
    let open Ast in
    let rec aux_rec =
      let open Expr in
      function
      | Ecallf _ | Ecallfptr _ | Ecst _ | Einput | Emalloc | Enull -> ()
      | Ebinop (_, e1, e2) ->
          aux_rec e1;
          aux_rec e2
      | Eunop (_, e) -> aux_rec e
      | Eident i -> (
          match Map.find_exn result i with
          | Lattices.Flat.Element true ->
              printf "Tainted information reaches sink in %d\n" l
          | Lattices.Flat.Top ->
              printf "Tainted information may reach sink in %d\n" l
          | _ -> ())
    in
    aux_rec e
  in
  List.iter ~f:aux sinks

let run cfg _ livevars vbusy reaching available sign taint filename =
  let p = Parsing.Utils.compile filename in
  if cfg then show_cfg p;
  (* if icfg then show_icfg p; *)
  if livevars then lv p;
  if vbusy then vb p;
  if reaching then rd p;
  if available then ae p;
  if sign then sa p;
  if taint then ta p

let command =
  let doc = "run monotone static analyses" in
  ( Term.(
      const run
      $ Arg.(
          value & flag
          & info [ "cfg" ]
              ~doc:"construct the (intraprocedural) control-flow graph")
      $ Arg.(
          value & flag
          & info [ "icfg" ]
              ~doc:"construct the interprocedural control-flow graph")
      $ Arg.(
          value & flag
          & info [ "livevars" ] ~doc:"enable live variables analysis")
      $ Arg.(
          value & flag
          & info [ "vbusy" ] ~doc:"enable very busy expressions analysis")
      $ Arg.(
          value & flag
          & info [ "reaching" ] ~doc:"enable reaching definitions analysis")
      $ Arg.(
          value & flag
          & info [ "available" ] ~doc:"enable available expressions analysis")
      $ Arg.(value & flag & info [ "sign" ] ~doc:"enable sign analysis")
      $ Arg.(value & flag & info [ "taint" ] ~doc:"enable taint analysis")
      $ Arg.(
          required
          & pos ~rev:true 0 (some non_dir_file) None
          & info [] ~docv:"FILE")),
    Term.info "mframework" ~doc ~exits:Term.default_exits )

let () = Term.(exit @@ eval command)
