open Lexing

exception CommandError of string

exception CompileError of string

module S = Set.Make (String)
module M = Map.Make (String)

(* input/output filename *)
let output_file = ref None

let input_file = ref None

(* Parallel *)
let thread = ref 1

(* Use GPU *)
let use_gpu = ref false

(* Analyze command line argument *)
let speclist =
  [ (* tuple of (key=string,spec,doc=string) *)
    ( "-o"
    , Arg.String (fun s -> output_file := Some s)
    , "[file] Write Output file" );
    ( "-thread", Arg.Int (fun v -> thread := v), "[thread] The parallel degree" );
    ( "-gpu" , Arg.Bool (fun b -> use_gpu := b), "[gpu] Use GPU or not (true/false)");
  ]

let compile in_c : string =
  let lexbuf = from_channel in_c in
  try
    let ast : Syntax.ast = Parser.top Lexer.read lexbuf in
    (* if not using GPU, gnode is translated to node array *)
    let translated_gpu = ref false in
    let message id = prerr_endline (id ^ " is declared as gnode, but translated to node array") in
    let gnode_to_nodearray = function | Syntax.GNode (id, n, init, d, e) -> translated_gpu := true; message (fst id); Syntax.NodeA (id, n, init, d, e) | n -> n in
    let ast : Syntax.ast = if !use_gpu then ast
                           else { ast with definitions = List.map gnode_to_nodearray ast.definitions} in
    let _ : unit = if !translated_gpu then prerr_endline "Warning : updating gnode on GPU requires gpu option (-gpu true)" in
    (* programはastからデータを構築.ここでデータは依存関係だったり... *)
    let program = Module.ast_to_program ast in
    (* debug *)
    (* Module.show_id_table program; *)

    (* C/C++のソースコード *)
    let code : string = Codegen.code_of_ast ast program !thread !use_gpu in

    (* ユーザー設定の部分を含むコードを出力する *)
    User_setting.generate_user_setting_file !thread ast program;

    (* C言語のコードを返す *)
    code
  with
  | Lexer.Error msg ->
      raise (CompileError ("Lexing error: " ^ msg))
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      raise
        (CompileError
           (Printf.sprintf "Syntax error at Line %d, Char %d." pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol + 1)))

let main () =
  Arg.parse speclist (fun s -> input_file := Some s) "Usage:" ;
  try
    let input : in_channel =
      open_in
        ( match !input_file with
        | Some s ->
            s
        | None ->
            raise (CommandError "Input file is not specified.") )
    in
    let c_code = compile input in
    (* print_endline "======================================" ; *)
    print_endline c_code
  with CommandError msg -> Printf.eprintf "Command Error: %s" msg

let () = main ()
