open Syntax
exception Unreachable of string
exception TypeError of string

type id = string

type const = string

type cudaAST =
  | Empty
  | Const of const (* 定数 *)
  | Var of id
  | VarA of id * cudaAST (* symbol * index *)
  | VarDec of Type.t * string (* 変数宣言 *)
  | Assignment of Type.t option * string * cudaAST
  | Binop of id * cudaAST * cudaAST
  | Uniop of id * cudaAST
  | FunCall of id * cudaAST list
  | CondExpr of cudaAST * cudaAST * cudaAST (* e1 ? e2 : e3 *)
  | If of
      (Type.t * string) * (* if式の結果を保存する変数 *)
      string * (* Temperature variable for saving the result of condition *)
      (cudaAST * cudaAST) * (* cond *)
      (cudaAST * cudaAST) * (* then *)
      (cudaAST * cudaAST) (* else *)
  | CodeSeq of cudaAST list


let rec get_gexpr_type (gexpr : Syntax.expr) (program : Module.program) : Type.t = 
  match gexpr with 
  | ESelf -> Type.TInt
  | EConst c -> Syntax.type_of_const c
  | Eid sym -> 
      let id = Hashtbl.find program.id_table sym in
      let info = Hashtbl.find program.info_table id in
      info.t
  | EAnnot (sym,_) -> 
      let id = Hashtbl.find program.id_table sym in
      let info = Hashtbl.find program.info_table id in
      info.t
  | EidA (sym, e) -> 
      let id = Hashtbl.find program.id_table sym in
      let info = Hashtbl.find program.info_table id in
      info.t
  | EAnnotA (sym, _ , _) -> 
      let id = Hashtbl.find program.id_table sym in
      let info = Hashtbl.find program.info_table id in
      info.t
  | Ebin (op, e1, e2) -> 
    (match op with 
      | BEq |BOr |BLte |BLt |BRte |BRt -> Type.TBool
      | _ -> Type.union_type (get_gexpr_type e1 program) (get_gexpr_type e2 program))
  | EUni (op, e) ->
    Type.union_type Type.TInt (get_gexpr_type e program) (* TODO : opが一意なのでbinより弱いが、binも全然やってないのが丸わかり *)
  | EApp (fsym, _) ->
      let t = Hashtbl.find program.func_table fsym in
      t
  | Eif (_, ge1, ge2) -> 
      let t1 = get_gexpr_type ge1 program in
      let t2 = get_gexpr_type ge2 program in
      if t1 = t2 then t1
                 else raise(TypeError ("In If-gexpr, the value of then and else are not matched"))


(* Convert from gexpr to cudaAST *)
(* This function is almost same as the function that convert from expr to cAST *)
let convert_from_gexpr_to_cudaAST (gexpr : Syntax.expr) (program : Module.program) : cudaAST * cudaAST = 
  let current_index = ref 0 in
  let get_unique_variable () : string = 
    let id = string_of_int !current_index in
    current_index := !current_index + 1; 
    "tmp_" ^ id
  in
  let rec converter gexpr = 
    match gexpr with 
    | ESelf -> (Empty, Var "self")
    | EConst c -> (Empty, Const (Syntax.string_of_const c))
    | Eid i -> (Empty, Var i)
    | EAnnot (sym,_) -> (Empty, Var (sym ^ "_ATLAST"))
    | EidA (sym, index_ge) -> 
        let ge_index_pre, ge_index_post = converter index_ge in
        let node_id = Hashtbl.find program.id_table sym in
        let node = Hashtbl.find program.info_table node_id in
        let default_value = Option.get node.default in
        let condition = Binop("&&", Binop("<=",Const "0",ge_index_post),
                                    Binop("<",ge_index_post, Var(string_of_int node.number))) in (* ge_index_postの計算が複数回行われている:まあCUDAコンパイラ任せでもいいか *)
        let default_code = Const(Syntax.string_of_const default_value) in
        (ge_index_pre, CondExpr(condition, VarA (sym , ge_index_post), default_code))
    | EAnnotA (sym, index_ge, _) ->
        let ge_index_pre, ge_index_post = converter index_ge in
        let access_sym = sym ^ "_ATLAST" in
        (ge_index_pre, VarA ( access_sym , ge_index_post))
    | Ebin(op, ge1, ge2) -> 
        let op_sym = Syntax.string_of_binop op in
        let pre1, post1 = converter ge1 in
        let pre2, post2 = converter ge2 in
        let pre = match (pre1, pre2) with
                  | (Empty, Empty) -> Empty
                  | (Empty, p2) -> p2
                  | (p1, Empty) -> p1
                  | (p1, p2) -> CodeSeq [p1; p2]
        in
        (pre, Binop(op_sym, post1, post2))
    | EUni(op, ge) ->
        let op_sym = Syntax.string_of_uniop op in
        let pre, post = converter ge in
        (pre, Uniop(op_sym, post))
    | EApp (fun_sym, args) -> 
        let args_cudaAST = List.map converter args in
        let args_pre = List.map fst args_cudaAST in
        let args_post = List.map snd args_cudaAST in
        (CodeSeq args_pre, FunCall(fun_sym, args_post))
    | Eif (ge_cond, ge_then, ge_else) ->
        let res_var = get_unique_variable () in
        let cond_var = get_unique_variable () in
        let then_type = get_gexpr_type ge_then program in
        let else_type = get_gexpr_type ge_else program in
        let res_type = Type.union_type then_type else_type in
        ( If ((res_type, res_var), cond_var,
              converter ge_cond,
              converter ge_then,
              converter ge_else) ,
          Var res_var)
  in
  let cuda_ast = converter gexpr in
  cuda_ast


(* Convert from the cudaAST to CUDA Code. *)
let rec convert_cudaAST_to_code (ast : cudaAST) (indent : int) : string = 
  let tab = String.make indent '\t' in
  match ast with
  | Empty -> ""
  | Const c ->  c
  | Var v -> v
  | VarA (sym, index_ast) -> 
      Printf.sprintf "%s%s[%s]" tab sym (convert_cudaAST_to_code index_ast 0)
  | VarDec (t, sym) ->
      Printf.sprintf "%s%s %s;" tab (Type.of_string t) sym
  | Assignment (_, var, a) ->
      Printf.sprintf "%s%s = %s;" tab var (convert_cudaAST_to_code a 0)
  | Binop(op, ast1, ast2) -> 
      Printf.sprintf "(%s %s %s)" (convert_cudaAST_to_code ast1 0) op (convert_cudaAST_to_code ast2 0)
  | Uniop(op, ast) -> 
      Printf.sprintf "(%s %s)" op (convert_cudaAST_to_code ast 0)
  | CondExpr(ast1, ast2, ast3) ->
      Printf.sprintf "(%s ? %s : %s)" (convert_cudaAST_to_code ast1 0) (convert_cudaAST_to_code ast2 0) (convert_cudaAST_to_code ast3 0)
  | FunCall(sym, asts) -> 
      Printf.sprintf "%s(%s)" sym (List.map (fun ast -> convert_cudaAST_to_code ast 0) asts |> String.concat ",")
  | If ((t,res_var), cond_var, (cond_pre, cond_post), (then_pre, then_post), (else_pre, else_post)) ->
      let if_result_dec = tab ^ (Printf.sprintf "%s %s;" (Type.of_string t) res_var) in (* declare the tmp variable that save the result of if statement *)
      let cond_pre_code = convert_cudaAST_to_code cond_pre indent in
      let cond_var_dec = tab ^ (Printf.sprintf "bool %s = %s;" cond_var (convert_cudaAST_to_code cond_post 0)) in
      let if_statement = tab ^ (Printf.sprintf "if(!!%s){" cond_var) in
      let then_st_pre = convert_cudaAST_to_code then_pre (indent+1) in
      let then_st_post = tab ^ Printf.sprintf "\t%s = %s;" res_var (convert_cudaAST_to_code then_post 0) in
      let else_st_pre = convert_cudaAST_to_code else_pre (indent+1) in
      let else_st_post = tab ^ Printf.sprintf "\t%s = %s;" res_var (convert_cudaAST_to_code else_post 0) in
      Utils.concat_without_empty "\n"
        [ if_result_dec;
          cond_pre_code;
          cond_var_dec;
          if_statement;
          then_st_pre;
          then_st_post;
          else_st_pre;
          else_st_post;
          "}";
        ]
  | CodeSeq codes -> 
      List.filter_map
        (function Empty -> None | ast -> Some (convert_cudaAST_to_code ast indent))
        codes
      |> Utils.concat_without_empty "\n"
