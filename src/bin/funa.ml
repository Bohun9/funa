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

let nth_cmdline_arg n = try Sys.argv.(n) with Invalid_argument _ -> abort ()

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

let file = nth_cmdline_arg 2
let source = read_file file
let program = Lang.Label.parse_from_string source |> Lang.Label.attach_labels

let k_cfa =
  match nth_cmdline_arg 1 with
  | "cfg" -> int_of_string (nth_cmdline_arg 3)
  | "analyse" -> int_of_string (nth_cmdline_arg 4)
  | _ -> 0

module MonotoneInstanceToString (MI : Api.MonotoneInstance) = struct
  let convert (result : MI.t Api.state) : string Api.state =
    result |> Hashtbl.to_seq
    |> Seq.map (fun (d, y) -> (d, MI.to_string y))
    |> Hashtbl.of_seq
end

module CFGSolver = struct
  module CFGSolver =
    Solver.Solver
      (Cfg.CFG)
      (struct
        let k = k_cfa
        let cfg = None
      end)

  module CfgToString = MonotoneInstanceToString (Cfg.CFG)

  let cfg = CFGSolver.analyse program
  let cfg_string = CfgToString.convert cfg

  let cfg_transparent =
    cfg |> Hashtbl.to_seq
    |> Seq.map (fun (d, y) -> (d, Cfg.CFG.to_funcset y))
    |> Hashtbl.of_seq
end

module DataFlowAnalysisSolver (AnalysisInstance : Api.MonotoneInstance) = struct
  module AnalysisSolver =
    Solver.Solver
      (AnalysisInstance)
      (struct
        let k = k_cfa
        let cfg = Some CFGSolver.cfg_transparent
      end)

  module AnalysisConv = MonotoneInstanceToString (AnalysisInstance)

  let analysis = AnalysisSolver.analyse program
  let analysis_string = AnalysisConv.convert analysis
end

let _ =
  match nth_cmdline_arg 1 with
  | "print" ->
      let module DotRawProgram = Printer.Dot.MakeDotProgram (struct
        let e = program
        let results = None
      end) in
      print_endline DotRawProgram.dot_program
  | "cfg" ->
      let module DotCfg = Printer.Dot.MakeDotProgram (struct
        let e = program
        let results = Some CFGSolver.cfg_string
      end) in
      print_endline DotCfg.dot_program
  | "analyse" ->
      let module AnalysisInstance =
        (val Hashtbl.find Analyses.registery (nth_cmdline_arg 3)
            : Api.MonotoneInstance)
      in
      let module AnalysisResults = DataFlowAnalysisSolver (AnalysisInstance) in
      let module DotDataFlow = Printer.Dot.MakeDotProgram (struct
        let e = program
        let results = Some AnalysisResults.analysis_string
      end) in
      print_endline DotDataFlow.dot_program
  | _ -> abort ()
