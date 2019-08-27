open Lexing

exception CommandError of string

exception CompileError of string

module S = Set.Make (String)
module M = Map.Make (String)

(* input/output filename *)
let output_file = ref None

let input_file = ref None

(* Analyze command line argument *)
let speclist =
  [ (* tuple of (key=string,spec,doc=string) *)
    ( "-o"
    , Arg.String (fun s -> output_file := Some s)
    , "[file] Write Output file" ) ]

let compile in_c : string =
  let lexbuf = from_channel in_c in
  try
    let ast : Syntax.ast = Parser.top Lexer.read lexbuf in
    (* "Module_ID = " ^ ast.module_id ^ "\n" ^ "Input Node = [" *)
    (* ^ String.concat "," (List.map (fun (i, t) -> i) ast.in_nodes) *)
    (* ^ "]\n" ^ "Output Node = [" *)
    (* ^ String.concat "," (List.map (fun (i, t) -> i) ast.out_nodes) *)
    (* ^ "]\n" ^ "USE = [" ^ String.concat "," ast.use ^ "]\n" *)
    (* ^ String.concat "," (List.map Syntax.string_of_definition ast.definitions) *)
    let code : string = Codegen.code_of_ast ast in
    let program = Module.ast_to_program ast in
    Module.print_program program;
    code
  with
  | Lexer.Error msg -> raise (CompileError ("Lexing error: " ^ msg))
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
        | Some s -> s
        | None -> raise (CommandError "Input file is not specified.") )
    in
    let c_code = compile input in
    print_endline c_code
  with CommandError msg -> Printf.eprintf "Command Error: %s" msg

let () = main ()