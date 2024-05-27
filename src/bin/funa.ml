open Framework

let usage_string =
  Printf.sprintf
    {|Usage: %s <command> [<args>]

The output is a graph in a dot language. <k_cfa> argument is a number indicating a context limit in K-CFA.

Commands:
  print <program>                           The program representation.
  cfg <program> <k_cfa>                     The program representation with control flow components.
  analyse <program> <analysis_name> <k_cfa> The program representation with specific data-flow components.
|}
    Sys.argv.(0)

let abort () =
  let _ = print_string usage_string in
  exit 1

let nth_cli_arg n = try Sys.argv.(n) with Invalid_argument _ -> abort ()

let read_file file =
  let ch =
    try open_in_bin file
    with Sys_error s ->
      Printf.eprintf "%s\n" s;
      exit 1
  in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let file = nth_cli_arg 2
let source = read_file file
let program = Lang.Label.parse_from_string source |> Lang.Label.attach_labels

module ControlFlowSolver (P : sig
  val k_cfa : int
end) =
struct
  open P

  module ControlFlowSolver =
    Solver.Solver
      (Cfg.ControlFlowMonotoneInstance)
      (struct
        let k_cfa = k_cfa
        let cfg = None
      end)

  let cfg = ControlFlowSolver.analyse program

  let cfg_string =
    Solver.convert_results_to_string cfg
      Cfg.ControlFlowMonotoneInstance.to_string

  let cfg_transparent =
    cfg |> Hashtbl.to_seq
    |> Seq.map (fun (d, y) -> (d, Cfg.ControlFlowMonotoneInstance.to_funcset y))
    |> Hashtbl.of_seq
end

module DataFlowSolver
    (P : sig
      val k_cfa : int
    end)
    (AnalysisInstance : Api.MonotoneInstance) =
struct
  open P

  module ControlFlowSolved = ControlFlowSolver (struct
    let k_cfa = k_cfa
  end)

  module DataFlowSolver =
    Solver.Solver
      (AnalysisInstance)
      (struct
        let k_cfa = k_cfa
        let cfg = Some ControlFlowSolved.cfg_transparent
      end)

  let analysis = DataFlowSolver.analyse program

  let analysis_string =
    Solver.convert_results_to_string analysis AnalysisInstance.to_string
end

let results =
  match nth_cli_arg 1 with
  | "print" -> None
  | "cfg" ->
      let k_cfa = int_of_string (nth_cli_arg 3) in
      let module ControlFlowResults = ControlFlowSolver (struct
        let k_cfa = k_cfa
      end) in
      Some ControlFlowResults.cfg_string
  | "analyse" ->
      let k_cfa = int_of_string (nth_cli_arg 4) in
      let module AnalysisInstance =
        (val Hashtbl.find Analyses.registery (nth_cli_arg 3)
            : Api.MonotoneInstance)
      in
      let module AnalysisResults =
        DataFlowSolver
          (struct
            let k_cfa = k_cfa
          end)
          (AnalysisInstance)
      in
      Some AnalysisResults.analysis_string
  | _ -> abort ()

module DotGraph = Printer.Dot.MakeDotProgram (struct
  let program = program
  let results = results
end)

let _ = print_endline DotGraph.dot_program
