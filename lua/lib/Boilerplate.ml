(**
   Boilerplate to be used as a template when mapping the lua CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_field_sep (env : env) (x : CST.field_sep) =
  (match x with
  | `COMMA tok -> token env tok (* "," *)
  | `SEMI tok -> token env tok (* ";" *)
  )

let map_number (env : env) (tok : CST.number) =
  token env tok (* number *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)

let map_global_variable (env : env) (x : CST.global_variable) =
  (match x with
  | `X__G tok -> token env tok (* "_G" *)
  | `X__VERSION tok -> token env tok (* "_VERSION" *)
  )

let map_string_ (env : env) (tok : CST.string_) =
  token env tok (* string *)

let map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Self tok -> token env tok (* "self" *)
          | `Spread tok -> token env tok (* "..." *)
          | `Id tok ->
              token env tok (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              token env v2 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = token env v2 (* "..." *) in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

let map_local_variable_declarator (env : env) ((v1, v2) : CST.local_variable_declarator) =
  let v1 =
    token env v1 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 =
        token env v2 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let map_function_name_field (env : env) ((v1, v2) : CST.function_name_field) =
  let v1 =
    token env v1 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "." *) in
      let v2 =
        token env v2 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let map_function_name (env : env) ((v1, v2) : CST.function_name) =
  let v1 =
    (match v1 with
    | `Id tok ->
        token env tok (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
    | `Func_name_field x -> map_function_name_field env x
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* ":" *) in
        let v2 =
          token env v2 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

let rec map_anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_arguments (env : env) (x : CST.arguments) =
  (match x with
  | `LPAR_opt_exp_rep_COMMA_exp_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 =
        (match v2 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Table x -> map_table env x
  | `Str tok -> token env tok (* string *)
  )

and map_binary_operation (env : env) (x : CST.binary_operation) =
  (match x with
  | `Exp_or_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "or" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_and_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "and" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_TILDEEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "~=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_TILDE_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "~" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASHSLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "//" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOTDOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ".." *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_else_ (env : env) ((v1, v2, v3) : CST.else_) =
  let v1 = token env v1 (* "else" *) in
  let v2 = List.map (map_statement env) v2 in
  let v3 =
    (match v3 with
    | Some x -> map_return_statement env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_elseif (env : env) ((v1, v2, v3, v4, v5) : CST.elseif) =
  let v1 = token env v1 (* "elseif" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* "then" *) in
  let v4 = List.map (map_statement env) v4 in
  let v5 =
    (match v5 with
    | Some x -> map_return_statement env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Spread tok -> token env tok (* "..." *)
  | `Prefix x -> map_prefix env x
  | `Next tok -> token env tok (* "next" *)
  | `Func_defi (v1, v2) ->
      let v1 = token env v1 (* "function" *) in
      let v2 = map_function_body env v2 in
      todo env (v1, v2)
  | `Table x -> map_table env x
  | `Bin_oper x -> map_binary_operation env x
  | `Un_oper (v1, v2) ->
      let v1 =
        (match v1 with
        | `Not tok -> token env tok (* "not" *)
        | `HASH tok -> token env tok (* "#" *)
        | `DASH tok -> token env tok (* "-" *)
        | `TILDE tok -> token env tok (* "~" *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Str tok -> token env tok (* string *)
  | `Num tok -> token env tok (* number *)
  | `Nil tok -> token env tok (* "nil" *)
  | `True tok -> token env tok (* "true" *)
  | `False tok -> token env tok (* "false" *)
  | `Id tok ->
      token env tok (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
  )

and map_field (env : env) (x : CST.field) =
  (match x with
  | `LBRACK_exp_RBRACK_EQ_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]" *) in
      let v4 = token env v4 (* "=" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Id_EQ_exp (v1, v2, v3) ->
      let v1 =
        token env v1 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp x -> map_expression env x
  )

and map_field_sequence (env : env) ((v1, v2, v3) : CST.field_sequence) =
  let v1 = map_field env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = map_field_sep env v1 in
      let v2 = map_field env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some x -> map_field_sep env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_function_body (env : env) ((v1, v2, v3, v4) : CST.function_body) =
  let v1 = map_parameters env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 =
    (match v3 with
    | Some x -> map_return_statement env x
    | None -> todo env ())
  in
  let v4 = token env v4 (* "end" *) in
  todo env (v1, v2, v3, v4)

and map_function_call_statement (env : env) (x : CST.function_call_statement) =
  (match x with
  | `Prefix_args (v1, v2) ->
      let v1 = map_prefix env v1 in
      let v2 = map_arguments env v2 in
      todo env (v1, v2)
  | `Prefix_COLON_id_args (v1, v2, v3, v4) ->
      let v1 = map_prefix env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 =
        token env v3 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      let v4 = map_arguments env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_in_loop_expression (env : env) ((v1, v2, v3, v4, v5) : CST.in_loop_expression) =
  let v1 =
    token env v1 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 =
        token env v2 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      todo env (v1, v2)
    ) v2
  in
  let v3 = token env v3 (* "in" *) in
  let v4 = map_expression env v4 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v5
  in
  todo env (v1, v2, v3, v4, v5)

and map_loop_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.loop_expression) =
  let v1 =
    token env v1 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
  in
  let v2 = token env v2 (* "=" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "," *) in
  let v5 = map_expression env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and map_prefix (env : env) (x : CST.prefix) =
  (match x with
  | `Self tok -> token env tok (* "self" *)
  | `Global_var x -> map_global_variable env x
  | `Var_decl x -> map_variable_declarator env x
  | `Func_call_stmt x -> map_function_call_statement env x
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = token env v1 (* "return" *) in
  let v2 =
    (match v2 with
    | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* ";" *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Exp x -> map_expression env x
  | `Var_decl (v1, v2, v3, v4, v5) ->
      let v1 = map_variable_declarator env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_variable_declarator env v2 in
          todo env (v1, v2)
        ) v2
      in
      let v3 = token env v3 (* "=" *) in
      let v4 = map_expression env v4 in
      let v5 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_expression env v2 in
          todo env (v1, v2)
        ) v5
      in
      todo env (v1, v2, v3, v4, v5)
  | `Local_var_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "local" *) in
      let v2 = map_local_variable_declarator env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_expression env v2 in
            let v3 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_expression env v2 in
                todo env (v1, v2)
              ) v3
            in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = List.map (map_statement env) v2 in
      let v3 =
        (match v3 with
        | Some x -> map_return_statement env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "end" *) in
      todo env (v1, v2, v3, v4)
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "then" *) in
      let v4 = List.map (map_statement env) v4 in
      let v5 =
        (match v5 with
        | Some x -> map_return_statement env x
        | None -> todo env ())
      in
      let v6 = List.map (map_elseif env) v6 in
      let v7 =
        (match v7 with
        | Some x -> map_else_ env x
        | None -> todo env ())
      in
      let v8 = token env v8 (* "end" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `While_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "do" *) in
      let v4 = List.map (map_statement env) v4 in
      let v5 =
        (match v5 with
        | Some x -> map_return_statement env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* "end" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Repeat_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "repeat" *) in
      let v2 = List.map (map_statement env) v2 in
      let v3 =
        (match v3 with
        | Some x -> map_return_statement env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "until" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `For_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = map_loop_expression env v2 in
      let v3 = token env v3 (* "do" *) in
      let v4 = List.map (map_statement env) v4 in
      let v5 =
        (match v5 with
        | Some x -> map_return_statement env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* "end" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `For_in_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = map_in_loop_expression env v2 in
      let v3 = token env v3 (* "do" *) in
      let v4 = List.map (map_statement env) v4 in
      let v5 =
        (match v5 with
        | Some x -> map_return_statement env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* "end" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Goto_stmt (v1, v2) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 =
        token env v2 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      todo env (v1, v2)
  | `Brk_stmt tok -> token env tok (* "break" *)
  | `Label_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "::" *) in
      let v2 =
        token env v2 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      let v3 = token env v3 (* "::" *) in
      todo env (v1, v2, v3)
  | `Empty_stmt tok -> token env tok (* ";" *)
  | `Func_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "function" *) in
      let v2 = map_function_name env v2 in
      let v3 = map_function_body env v3 in
      todo env (v1, v2, v3)
  | `Local_func_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "local" *) in
      let v2 = token env v2 (* "function" *) in
      let v3 =
        token env v3 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      let v4 = map_function_body env v4 in
      todo env (v1, v2, v3, v4)
  | `Func_call_stmt x -> map_function_call_statement env x
  )

and map_table (env : env) ((v1, v2, v3) : CST.table) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_field_sequence env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_variable_declarator (env : env) (x : CST.variable_declarator) =
  (match x with
  | `Id tok ->
      token env tok (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
  | `Prefix_LBRACK_exp_RBRACK (v1, v2, v3, v4) ->
      let v1 = map_prefix env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Field_exp (v1, v2, v3) ->
      let v1 = map_prefix env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 =
        token env v3 (* pattern \$?[a-zA-Z_][a-zA-Z0-9_]* *)
      in
      todo env (v1, v2, v3)
  )

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 = List.map (map_statement env) v1 in
  let v2 =
    (match v2 with
    | Some x -> map_return_statement env x
    | None -> todo env ())
  in
  todo env (v1, v2)
