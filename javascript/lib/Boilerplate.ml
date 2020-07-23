(**
   Boilerplate to be used as a template when mapping the javascript CST
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

let map_jsx_identifier (env : env) (tok : CST.jsx_identifier) =
  token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)

let map_jsx_text (env : env) (tok : CST.jsx_text) =
  token env tok (* pattern [^{}<>]+ *)

let map_template_chars (env : env) (tok : CST.template_chars) =
  token env tok (* template_chars *)

let map_number (env : env) (tok : CST.number) =
  token env tok (* number *)

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  token env tok (* escape_sequence *)

let map_meta_property (env : env) ((v1, v2, v3) : CST.meta_property) =
  let v1 = token env v1 (* "new" *) in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* "target" *) in
  todo env (v1, v2, v3)

let map_hash_bang_line (env : env) (tok : CST.hash_bang_line) =
  token env tok (* pattern #!.* *)

let map_regex_flags (env : env) (tok : CST.regex_flags) =
  token env tok (* pattern [a-z]+ *)

let map_regex_pattern (env : env) (tok : CST.regex_pattern) =
  token env tok (* regex_pattern *)

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  token env tok (* automatic_semicolon *)

let map_import (env : env) (tok : CST.import) =
  token env tok (* import *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* identifier *)

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Str_DQUOT_rep_choice_blank_DQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Blank () -> todo env ()
          | `Esc_seq tok -> token env tok (* escape_sequence *)
          )
        ) v2
      in
      let v3 = token env v3 (* "\"" *) in
      todo env (v1, v2, v3)
  | `Str_SQUOT_rep_choice_blank_SQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Blank () -> todo env ()
          | `Esc_seq tok -> token env tok (* escape_sequence *)
          )
        ) v2
      in
      let v3 = token env v3 (* "'" *) in
      todo env (v1, v2, v3)
  )

let map_regex (env : env) ((v1, v2, v3, v4) : CST.regex) =
  let v1 = token env v1 (* "/" *) in
  let v2 = token env v2 (* regex_pattern *) in
  let v3 = token env v3 (* "/" *) in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* pattern [a-z]+ *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

let map_debugger_statement (env : env) ((v1, v2) : CST.debugger_statement) =
  let v1 = token env v1 (* "debugger" *) in
  let v2 =
    (match v2 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2)

let rec map_nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 =
    (match v1 with
    | `Id tok -> token env tok (* identifier *)
    | `Nest_id x -> map_nested_identifier env x
    )
  in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_continue_statement (env : env) ((v1, v2, v3) : CST.continue_statement) =
  let v1 = token env v1 (* "continue" *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* identifier *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3)

let rec map_decorator_member_expression (env : env) ((v1, v2, v3) : CST.decorator_member_expression) =
  let v1 =
    (match v1 with
    | `Choice_id x ->
        (match x with
        | `Id tok -> token env tok (* identifier *)
        | `Choice_get x ->
            (match x with
            | `Get tok -> token env tok (* "get" *)
            | `Set tok -> token env tok (* "set" *)
            | `Async tok -> token env tok (* "async" *)
            | `Stat tok -> token env tok (* "static" *)
            )
        )
    | `Deco_memb_exp x -> map_decorator_member_expression env x
    )
  in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_import_export_specifier (env : env) ((v1, v2) : CST.import_export_specifier) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "as" *) in
        let v2 = token env v2 (* identifier *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_namespace_import (env : env) ((v1, v2, v3) : CST.namespace_import) =
  let v1 = token env v1 (* "*" *) in
  let v2 = token env v2 (* "as" *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_break_statement (env : env) ((v1, v2, v3) : CST.break_statement) =
  let v1 = token env v1 (* "break" *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* identifier *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3)

let map_jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 =
    (match v1 with
    | `Jsx_id tok ->
        token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
    | `Id tok -> token env tok (* identifier *)
    )
  in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    (match v3 with
    | `Jsx_id tok ->
        token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
    | `Id tok -> token env tok (* identifier *)
    )
  in
  todo env (v1, v2, v3)

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = token env v1 (* "from" *) in
  let v2 = map_string_ env v2 in
  todo env (v1, v2)

let map_export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_import_export_specifier env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_import_export_specifier env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

let map_named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_import_export_specifier env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_import_export_specifier env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

let map_jsx_closing_element (env : env) ((v1, v2, v3, v4) : CST.jsx_closing_element) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* "/" *) in
  let v3 =
    (match v3 with
    | `Choice_jsx_id x ->
        (match x with
        | `Jsx_id tok ->
            token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
        | `Id tok -> token env tok (* identifier *)
        )
    | `Nest_id x -> map_nested_identifier env x
    | `Jsx_name_name x -> map_jsx_namespace_name env x
    )
  in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

let map_import_clause (env : env) (x : CST.import_clause) =
  (match x with
  | `Impo_clau_name_impo x -> map_namespace_import env x
  | `Impo_clau_named_impors x -> map_named_imports env x
  | `Impo_clau_id_opt_COMMA_choice_name_impo (v1, v2) ->
      let v1 = token env v1 (* identifier *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Name_impo x -> map_namespace_import env x
              | `Named_impors x -> map_named_imports env x
              )
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2)
  )

let map_import_statement (env : env) ((v1, v2, v3) : CST.import_statement) =
  let v1 = token env v1 (* "import" *) in
  let v2 =
    (match v2 with
    | `Impo_clau_from_clau (v1, v2) ->
        let v1 = map_import_clause env v1 in
        let v2 = map_from_clause env v2 in
        todo env (v1, v2)
    | `Str x -> map_string_ env x
    )
  in
  let v3 =
    (match v3 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3)

let rec map_export_statement (env : env) (x : CST.export_statement) =
  (match x with
  | `Expo_stmt_expo_choice_STAR_from_clau_choice_auto_semi (v1, v2) ->
      let v1 = token env v1 (* "export" *) in
      let v2 =
        (match v2 with
        | `STAR_from_clau_choice_auto_semi (v1, v2, v3) ->
            let v1 = token env v1 (* "*" *) in
            let v2 = map_from_clause env v2 in
            let v3 =
              (match v3 with
              | `Auto_semi tok -> token env tok (* automatic_semicolon *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            todo env (v1, v2, v3)
        | `Expo_clau_from_clau_choice_auto_semi (v1, v2, v3) ->
            let v1 = map_export_clause env v1 in
            let v2 = map_from_clause env v2 in
            let v3 =
              (match v3 with
              | `Auto_semi tok -> token env tok (* automatic_semicolon *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            todo env (v1, v2, v3)
        | `Expo_clau_choice_auto_semi (v1, v2) ->
            let v1 = map_export_clause env v1 in
            let v2 =
              (match v2 with
              | `Auto_semi tok -> token env tok (* automatic_semicolon *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            todo env (v1, v2)
        )
      in
      todo env (v1, v2)
  | `Expo_stmt_rep_deco_expo_choice_decl (v1, v2, v3) ->
      let v1 = List.map (map_decorator env) v1 in
      let v2 = token env v2 (* "export" *) in
      let v3 =
        (match v3 with
        | `Decl x -> map_declaration env x
        | `Defa_exp_choice_auto_semi (v1, v2, v3) ->
            let v1 = token env v1 (* "default" *) in
            let v2 = map_expression env v2 in
            let v3 =
              (match v3 with
              | `Auto_semi tok -> token env tok (* automatic_semicolon *)
              | `SEMI tok -> token env tok (* ";" *)
              )
            in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2, v3)
  )


and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Decl_func_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "function" *) in
      let v3 = token env v3 (* identifier *) in
      let v4 = map_formal_parameters env v4 in
      let v5 = map_statement_block env v5 in
      let v6 =
        (match v6 with
        | Some tok -> token env tok (* automatic_semicolon *)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Decl_gene_func_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "function" *) in
      let v3 = token env v3 (* "*" *) in
      let v4 = token env v4 (* identifier *) in
      let v5 = map_formal_parameters env v5 in
      let v6 = map_statement_block env v6 in
      let v7 =
        (match v7 with
        | Some tok -> token env tok (* automatic_semicolon *)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Decl_class_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.map (map_decorator env) v1 in
      let v2 = token env v2 (* "class" *) in
      let v3 = token env v3 (* identifier *) in
      let v4 =
        (match v4 with
        | Some x -> map_class_heritage env x
        | None -> todo env ())
      in
      let v5 = map_class_body env v5 in
      let v6 =
        (match v6 with
        | Some tok -> token env tok (* automatic_semicolon *)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Decl_lexi_decl x -> map_lexical_declaration env x
  | `Decl_var_decl x -> map_variable_declaration env x
  )


and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 =
    (match v1 with
    | `Exp x -> map_expression env x
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v2 =
    (match v2 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2)


and map_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.variable_declaration) =
  let v1 = token env v1 (* "var" *) in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_variable_declarator env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3, v4)


and map_lexical_declaration (env : env) ((v1, v2, v3, v4) : CST.lexical_declaration) =
  let v1 =
    (match v1 with
    | `Let tok -> token env tok (* "let" *)
    | `Const tok -> token env tok (* "const" *)
    )
  in
  let v2 = map_variable_declarator env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_variable_declarator env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3, v4)


and map_variable_declarator (env : env) ((v1, v2) : CST.variable_declarator) =
  let v1 =
    (match v1 with
    | `Id tok -> token env tok (* identifier *)
    | `Choice_obj x ->
        (match x with
        | `Obj x -> map_object_ env x
        | `Array x -> map_array_ env x
        )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> map_initializer_ env x
    | None -> todo env ())
  in
  todo env (v1, v2)


and map_statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Expo_stmt x -> map_export_statement env x
      | `Impo_stmt x -> map_import_statement env x
      | `Debu_stmt x -> map_debugger_statement env x
      | `Exp_stmt x -> map_expression_statement env x
      | `Decl x -> map_declaration env x
      | `Stmt_blk x -> map_statement_block env x
      | `If_stmt x -> map_if_statement env x
      | `Swit_stmt x -> map_switch_statement env x
      | `For_stmt x -> map_for_statement env x
      | `For_in_stmt x -> map_for_in_statement env x
      | `While_stmt x -> map_while_statement env x
      | `Do_stmt x -> map_do_statement env x
      | `Try_stmt x -> map_try_statement env x
      | `With_stmt x -> map_with_statement env x
      | `Brk_stmt x -> map_break_statement env x
      | `Cont_stmt x -> map_continue_statement env x
      | `Ret_stmt x -> map_return_statement env x
      | `Throw_stmt x -> map_throw_statement env x
      | `Empty_stmt tok -> token env tok (* ";" *)
      | `Labe_stmt x -> map_labeled_statement env x
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)


and map_if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) =
  let v1 = token env v1 (* "if" *) in
  let v2 = map_parenthesized_expression env v2 in
  let v3 =
    (match v3 with
    | `Expo_stmt x -> map_export_statement env x
    | `Impo_stmt x -> map_import_statement env x
    | `Debu_stmt x -> map_debugger_statement env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Decl x -> map_declaration env x
    | `Stmt_blk x -> map_statement_block env x
    | `If_stmt x -> map_if_statement env x
    | `Swit_stmt x -> map_switch_statement env x
    | `For_stmt x -> map_for_statement env x
    | `For_in_stmt x -> map_for_in_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `Try_stmt x -> map_try_statement env x
    | `With_stmt x -> map_with_statement env x
    | `Brk_stmt x -> map_break_statement env x
    | `Cont_stmt x -> map_continue_statement env x
    | `Ret_stmt x -> map_return_statement env x
    | `Throw_stmt x -> map_throw_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    | `Labe_stmt x -> map_labeled_statement env x
    )
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "else" *) in
        let v2 =
          (match v2 with
          | `Expo_stmt x -> map_export_statement env x
          | `Impo_stmt x -> map_import_statement env x
          | `Debu_stmt x -> map_debugger_statement env x
          | `Exp_stmt x -> map_expression_statement env x
          | `Decl x -> map_declaration env x
          | `Stmt_blk x -> map_statement_block env x
          | `If_stmt x -> map_if_statement env x
          | `Swit_stmt x -> map_switch_statement env x
          | `For_stmt x -> map_for_statement env x
          | `For_in_stmt x -> map_for_in_statement env x
          | `While_stmt x -> map_while_statement env x
          | `Do_stmt x -> map_do_statement env x
          | `Try_stmt x -> map_try_statement env x
          | `With_stmt x -> map_with_statement env x
          | `Brk_stmt x -> map_break_statement env x
          | `Cont_stmt x -> map_continue_statement env x
          | `Ret_stmt x -> map_return_statement env x
          | `Throw_stmt x -> map_throw_statement env x
          | `Empty_stmt tok -> token env tok (* ";" *)
          | `Labe_stmt x -> map_labeled_statement env x
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)


and map_switch_statement (env : env) ((v1, v2, v3) : CST.switch_statement) =
  let v1 = token env v1 (* "switch" *) in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_switch_body env v3 in
  todo env (v1, v2, v3)


and map_for_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.for_statement) =
  let v1 = token env v1 (* "for" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 =
    (match v3 with
    | `Lexi_decl x -> map_lexical_declaration env x
    | `Var_decl x -> map_variable_declaration env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    )
  in
  let v4 =
    (match v4 with
    | `Exp_stmt x -> map_expression_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    )
  in
  let v5 =
    (match v5 with
    | Some x ->
        (match x with
        | `Exp x -> map_expression env x
        | `Seq_exp x -> map_sequence_expression env x
        )
    | None -> todo env ())
  in
  let v6 = token env v6 (* ")" *) in
  let v7 =
    (match v7 with
    | `Expo_stmt x -> map_export_statement env x
    | `Impo_stmt x -> map_import_statement env x
    | `Debu_stmt x -> map_debugger_statement env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Decl x -> map_declaration env x
    | `Stmt_blk x -> map_statement_block env x
    | `If_stmt x -> map_if_statement env x
    | `Swit_stmt x -> map_switch_statement env x
    | `For_stmt x -> map_for_statement env x
    | `For_in_stmt x -> map_for_in_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `Try_stmt x -> map_try_statement env x
    | `With_stmt x -> map_with_statement env x
    | `Brk_stmt x -> map_break_statement env x
    | `Cont_stmt x -> map_continue_statement env x
    | `Ret_stmt x -> map_return_statement env x
    | `Throw_stmt x -> map_throw_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    | `Labe_stmt x -> map_labeled_statement env x
    )
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)


and map_for_in_statement (env : env) ((v1, v2, v3, v4) : CST.for_in_statement) =
  let v1 = token env v1 (* "for" *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "await" *)
    | None -> todo env ())
  in
  let v3 = map_for_header env v3 in
  let v4 =
    (match v4 with
    | `Expo_stmt x -> map_export_statement env x
    | `Impo_stmt x -> map_import_statement env x
    | `Debu_stmt x -> map_debugger_statement env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Decl x -> map_declaration env x
    | `Stmt_blk x -> map_statement_block env x
    | `If_stmt x -> map_if_statement env x
    | `Swit_stmt x -> map_switch_statement env x
    | `For_stmt x -> map_for_statement env x
    | `For_in_stmt x -> map_for_in_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `Try_stmt x -> map_try_statement env x
    | `With_stmt x -> map_with_statement env x
    | `Brk_stmt x -> map_break_statement env x
    | `Cont_stmt x -> map_continue_statement env x
    | `Ret_stmt x -> map_return_statement env x
    | `Throw_stmt x -> map_throw_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    | `Labe_stmt x -> map_labeled_statement env x
    )
  in
  todo env (v1, v2, v3, v4)


and map_for_header (env : env) ((v1, v2, v3, v4, v5, v6) : CST.for_header) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Var tok -> token env tok (* "var" *)
        | `Let tok -> token env tok (* "let" *)
        | `Const tok -> token env tok (* "const" *)
        )
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Paren_exp x -> map_parenthesized_expression env x
    | `Choice_memb_exp x ->
        (match x with
        | `Memb_exp x -> map_member_expression env x
        | `Subs_exp x -> map_subscript_expression env x
        | `Id tok -> token env tok (* identifier *)
        | `Choice_get x ->
            (match x with
            | `Get tok -> token env tok (* "get" *)
            | `Set tok -> token env tok (* "set" *)
            | `Async tok -> token env tok (* "async" *)
            | `Stat tok -> token env tok (* "static" *)
            )
        | `Choice_obj x ->
            (match x with
            | `Obj x -> map_object_ env x
            | `Array x -> map_array_ env x
            )
        )
    )
  in
  let v4 =
    (match v4 with
    | `In tok -> token env tok (* "in" *)
    | `Of tok -> token env tok (* "of" *)
    )
  in
  let v5 =
    (match v5 with
    | `Exp x -> map_expression env x
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)


and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 = token env v1 (* "while" *) in
  let v2 = map_parenthesized_expression env v2 in
  let v3 =
    (match v3 with
    | `Expo_stmt x -> map_export_statement env x
    | `Impo_stmt x -> map_import_statement env x
    | `Debu_stmt x -> map_debugger_statement env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Decl x -> map_declaration env x
    | `Stmt_blk x -> map_statement_block env x
    | `If_stmt x -> map_if_statement env x
    | `Swit_stmt x -> map_switch_statement env x
    | `For_stmt x -> map_for_statement env x
    | `For_in_stmt x -> map_for_in_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `Try_stmt x -> map_try_statement env x
    | `With_stmt x -> map_with_statement env x
    | `Brk_stmt x -> map_break_statement env x
    | `Cont_stmt x -> map_continue_statement env x
    | `Ret_stmt x -> map_return_statement env x
    | `Throw_stmt x -> map_throw_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    | `Labe_stmt x -> map_labeled_statement env x
    )
  in
  todo env (v1, v2, v3)


and map_do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) =
  let v1 = token env v1 (* "do" *) in
  let v2 =
    (match v2 with
    | `Expo_stmt x -> map_export_statement env x
    | `Impo_stmt x -> map_import_statement env x
    | `Debu_stmt x -> map_debugger_statement env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Decl x -> map_declaration env x
    | `Stmt_blk x -> map_statement_block env x
    | `If_stmt x -> map_if_statement env x
    | `Swit_stmt x -> map_switch_statement env x
    | `For_stmt x -> map_for_statement env x
    | `For_in_stmt x -> map_for_in_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `Try_stmt x -> map_try_statement env x
    | `With_stmt x -> map_with_statement env x
    | `Brk_stmt x -> map_break_statement env x
    | `Cont_stmt x -> map_continue_statement env x
    | `Ret_stmt x -> map_return_statement env x
    | `Throw_stmt x -> map_throw_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    | `Labe_stmt x -> map_labeled_statement env x
    )
  in
  let v3 = token env v3 (* "while" *) in
  let v4 = map_parenthesized_expression env v4 in
  let v5 =
    (match v5 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3, v4, v5)


and map_try_statement (env : env) ((v1, v2, v3, v4) : CST.try_statement) =
  let v1 = token env v1 (* "try" *) in
  let v2 = map_statement_block env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_catch_clause env x
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_finally_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)


and map_with_statement (env : env) ((v1, v2, v3) : CST.with_statement) =
  let v1 = token env v1 (* "with" *) in
  let v2 = map_parenthesized_expression env v2 in
  let v3 =
    (match v3 with
    | `Expo_stmt x -> map_export_statement env x
    | `Impo_stmt x -> map_import_statement env x
    | `Debu_stmt x -> map_debugger_statement env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Decl x -> map_declaration env x
    | `Stmt_blk x -> map_statement_block env x
    | `If_stmt x -> map_if_statement env x
    | `Swit_stmt x -> map_switch_statement env x
    | `For_stmt x -> map_for_statement env x
    | `For_in_stmt x -> map_for_in_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `Try_stmt x -> map_try_statement env x
    | `With_stmt x -> map_with_statement env x
    | `Brk_stmt x -> map_break_statement env x
    | `Cont_stmt x -> map_continue_statement env x
    | `Ret_stmt x -> map_return_statement env x
    | `Throw_stmt x -> map_throw_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    | `Labe_stmt x -> map_labeled_statement env x
    )
  in
  todo env (v1, v2, v3)


and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = token env v1 (* "return" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Exp x -> map_expression env x
        | `Seq_exp x -> map_sequence_expression env x
        )
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3)


and map_throw_statement (env : env) ((v1, v2, v3) : CST.throw_statement) =
  let v1 = token env v1 (* "throw" *) in
  let v2 =
    (match v2 with
    | `Exp x -> map_expression env x
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v3 =
    (match v3 with
    | `Auto_semi tok -> token env tok (* automatic_semicolon *)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3)


and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 =
    (match v1 with
    | `Id tok -> token env tok (* identifier *)
    | `Choice_get x ->
        (match x with
        | `Get tok -> token env tok (* "get" *)
        | `Set tok -> token env tok (* "set" *)
        | `Async tok -> token env tok (* "async" *)
        | `Stat tok -> token env tok (* "static" *)
        )
    )
  in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    (match v3 with
    | `Expo_stmt x -> map_export_statement env x
    | `Impo_stmt x -> map_import_statement env x
    | `Debu_stmt x -> map_debugger_statement env x
    | `Exp_stmt x -> map_expression_statement env x
    | `Decl x -> map_declaration env x
    | `Stmt_blk x -> map_statement_block env x
    | `If_stmt x -> map_if_statement env x
    | `Swit_stmt x -> map_switch_statement env x
    | `For_stmt x -> map_for_statement env x
    | `For_in_stmt x -> map_for_in_statement env x
    | `While_stmt x -> map_while_statement env x
    | `Do_stmt x -> map_do_statement env x
    | `Try_stmt x -> map_try_statement env x
    | `With_stmt x -> map_with_statement env x
    | `Brk_stmt x -> map_break_statement env x
    | `Cont_stmt x -> map_continue_statement env x
    | `Ret_stmt x -> map_return_statement env x
    | `Throw_stmt x -> map_throw_statement env x
    | `Empty_stmt tok -> token env tok (* ";" *)
    | `Labe_stmt x -> map_labeled_statement env x
    )
  in
  todo env (v1, v2, v3)


and map_switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Swit_case x -> map_switch_case env x
      | `Swit_defa x -> map_switch_default env x
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 =
    (match v2 with
    | `Exp x -> map_expression env x
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v3 = token env v3 (* ":" *) in
  let v4 =
    List.map (fun x ->
      (match x with
      | `Expo_stmt x -> map_export_statement env x
      | `Impo_stmt x -> map_import_statement env x
      | `Debu_stmt x -> map_debugger_statement env x
      | `Exp_stmt x -> map_expression_statement env x
      | `Decl x -> map_declaration env x
      | `Stmt_blk x -> map_statement_block env x
      | `If_stmt x -> map_if_statement env x
      | `Swit_stmt x -> map_switch_statement env x
      | `For_stmt x -> map_for_statement env x
      | `For_in_stmt x -> map_for_in_statement env x
      | `While_stmt x -> map_while_statement env x
      | `Do_stmt x -> map_do_statement env x
      | `Try_stmt x -> map_try_statement env x
      | `With_stmt x -> map_with_statement env x
      | `Brk_stmt x -> map_break_statement env x
      | `Cont_stmt x -> map_continue_statement env x
      | `Ret_stmt x -> map_return_statement env x
      | `Throw_stmt x -> map_throw_statement env x
      | `Empty_stmt tok -> token env tok (* ";" *)
      | `Labe_stmt x -> map_labeled_statement env x
      )
    ) v4
  in
  todo env (v1, v2, v3, v4)


and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    List.map (fun x ->
      (match x with
      | `Expo_stmt x -> map_export_statement env x
      | `Impo_stmt x -> map_import_statement env x
      | `Debu_stmt x -> map_debugger_statement env x
      | `Exp_stmt x -> map_expression_statement env x
      | `Decl x -> map_declaration env x
      | `Stmt_blk x -> map_statement_block env x
      | `If_stmt x -> map_if_statement env x
      | `Swit_stmt x -> map_switch_statement env x
      | `For_stmt x -> map_for_statement env x
      | `For_in_stmt x -> map_for_in_statement env x
      | `While_stmt x -> map_while_statement env x
      | `Do_stmt x -> map_do_statement env x
      | `Try_stmt x -> map_try_statement env x
      | `With_stmt x -> map_with_statement env x
      | `Brk_stmt x -> map_break_statement env x
      | `Cont_stmt x -> map_continue_statement env x
      | `Ret_stmt x -> map_return_statement env x
      | `Throw_stmt x -> map_throw_statement env x
      | `Empty_stmt tok -> token env tok (* ";" *)
      | `Labe_stmt x -> map_labeled_statement env x
      )
    ) v3
  in
  todo env (v1, v2, v3)


and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "(" *) in
        let v2 =
          (match v2 with
          | `Id tok -> token env tok (* identifier *)
          | `Choice_obj x ->
              (match x with
              | `Obj x -> map_object_ env x
              | `Array x -> map_array_ env x
              )
          )
        in
        let v3 = token env v3 (* ")" *) in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = map_statement_block env v3 in
  todo env (v1, v2, v3)


and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = map_statement_block env v2 in
  todo env (v1, v2)


and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Exp x -> map_expression env x
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Exp_choice_this x ->
      (match x with
      | `This tok -> token env tok (* "this" *)
      | `Id tok -> token env tok (* identifier *)
      | `Choice_get x ->
          (match x with
          | `Get tok -> token env tok (* "get" *)
          | `Set tok -> token env tok (* "set" *)
          | `Async tok -> token env tok (* "async" *)
          | `Stat tok -> token env tok (* "static" *)
          )
      | `Num tok -> token env tok (* number *)
      | `Str x -> map_string_ env x
      | `Temp_str x -> map_template_string env x
      | `Regex x -> map_regex env x
      | `True tok -> token env tok (* "true" *)
      | `False tok -> token env tok (* "false" *)
      | `Null tok -> token env tok (* "null" *)
      | `Unde tok -> token env tok (* "undefined" *)
      | `Impo tok -> token env tok (* import *)
      | `Obj x -> map_object_ env x
      | `Array x -> map_array_ env x
      | `Func x -> map_function_ env x
      | `Arrow_func x -> map_arrow_function env x
      | `Gene_func x -> map_generator_function env x
      | `Class x -> map_class_ env x
      | `Paren_exp x -> map_parenthesized_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Memb_exp x -> map_member_expression env x
      | `Meta_prop x -> map_meta_property env x
      | `New_exp x -> map_new_expression env x
      )
  | `Exp_choice_jsx_elem x ->
      (match x with
      | `Jsx_elem x -> map_jsx_element env x
      | `Jsx_self_clos_elem x ->
          map_jsx_self_closing_element env x
      )
  | `Exp_jsx_frag x -> map_jsx_fragment env x
  | `Exp_assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Paren_exp x -> map_parenthesized_expression env x
        | `Choice_memb_exp x ->
            (match x with
            | `Memb_exp x -> map_member_expression env x
            | `Subs_exp x -> map_subscript_expression env x
            | `Id tok -> token env tok (* identifier *)
            | `Choice_get x ->
                (match x with
                | `Get tok -> token env tok (* "get" *)
                | `Set tok -> token env tok (* "set" *)
                | `Async tok -> token env tok (* "async" *)
                | `Stat tok -> token env tok (* "static" *)
                )
            | `Choice_obj x ->
                (match x with
                | `Obj x -> map_object_ env x
                | `Array x -> map_array_ env x
                )
            )
        )
      in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_augm_assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Memb_exp x -> map_member_expression env x
        | `Subs_exp x -> map_subscript_expression env x
        | `Choice_get x ->
            (match x with
            | `Get tok -> token env tok (* "get" *)
            | `Set tok -> token env tok (* "set" *)
            | `Async tok -> token env tok (* "async" *)
            | `Stat tok -> token env tok (* "static" *)
            )
        | `Id tok -> token env tok (* identifier *)
        | `Paren_exp x -> map_parenthesized_expression env x
        )
      in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> token env tok (* "+=" *)
        | `DASHEQ tok -> token env tok (* "-=" *)
        | `STAREQ tok -> token env tok (* "*=" *)
        | `SLASHEQ tok -> token env tok (* "/=" *)
        | `PERCEQ tok -> token env tok (* "%=" *)
        | `HATEQ tok -> token env tok (* "^=" *)
        | `AMPEQ tok -> token env tok (* "&=" *)
        | `BAREQ tok -> token env tok (* "|=" *)
        | `GTGTEQ tok -> token env tok (* ">>=" *)
        | `GTGTGTEQ tok -> token env tok (* ">>>=" *)
        | `LTLTEQ tok -> token env tok (* "<<=" *)
        | `STARSTAREQ tok -> token env tok (* "**=" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Exp_un_exp x -> map_unary_expression env x
  | `Exp_bin_exp x -> map_binary_expression env x
  | `Exp_tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ":" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Exp_upda_exp x -> map_update_expression env x
  | `Exp_call_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Exp x -> map_expression env x
        | `Super tok -> token env tok (* "super" *)
        | `Func x -> map_function_ env x
        )
      in
      let v2 =
        (match v2 with
        | `Args x -> map_arguments env x
        | `Temp_str x -> map_template_string env x
        )
      in
      todo env (v1, v2)
  | `Exp_yield_exp (v1, v2) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 =
        (match v2 with
        | `STAR_exp (v1, v2) ->
            let v1 = token env v1 (* "*" *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | `Opt_exp opt ->
            (match opt with
            | Some x -> map_expression env x
            | None -> todo env ())
        )
      in
      todo env (v1, v2)
  )


and map_object_ (env : env) ((v1, v2, v3) : CST.object_) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `Pair x -> map_pair env x
              | `Spre_elem x -> map_spread_element env x
              | `Meth_defi x -> map_method_definition env x
              | `Assign_pat x -> map_assignment_pattern env x
              | `Choice_id x ->
                  (match x with
                  | `Id tok -> token env tok (* identifier *)
                  | `Choice_get x ->
                      (match x with
                      | `Get tok -> token env tok (* "get" *)
                      | `Set tok -> token env tok (* "set" *)
                      | `Async tok -> token env tok (* "async" *)
                      | `Stat tok -> token env tok (* "static" *)
                      )
                  )
              )
          | None -> todo env ())
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | Some x ->
                  (match x with
                  | `Pair x -> map_pair env x
                  | `Spre_elem x -> map_spread_element env x
                  | `Meth_defi x -> map_method_definition env x
                  | `Assign_pat x -> map_assignment_pattern env x
                  | `Choice_id x ->
                      (match x with
                      | `Id tok -> token env tok (* identifier *)
                      | `Choice_get x ->
                          (match x with
                          | `Get tok -> token env tok (* "get" *)
                          | `Set tok -> token env tok (* "set" *)
                          | `Async tok -> token env tok (* "async" *)
                          | `Stat tok -> token env tok (* "static" *)
                          )
                      )
                  )
              | None -> todo env ())
            in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_assignment_pattern (env : env) ((v1, v2, v3) : CST.assignment_pattern) =
  let v1 =
    (match v1 with
    | `Choice_choice_get x ->
        (match x with
        | `Choice_get x ->
            (match x with
            | `Get tok -> token env tok (* "get" *)
            | `Set tok -> token env tok (* "set" *)
            | `Async tok -> token env tok (* "async" *)
            | `Stat tok -> token env tok (* "static" *)
            )
        | `Id tok -> token env tok (* identifier *)
        )
    | `Choice_obj x ->
        (match x with
        | `Obj x -> map_object_ env x
        | `Array x -> map_array_ env x
        )
    )
  in
  let v2 = token env v2 (* "=" *) in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)


and map_array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `Exp x -> map_expression env x
              | `Spre_elem x -> map_spread_element env x
              )
          | None -> todo env ())
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | Some x ->
                  (match x with
                  | `Exp x -> map_expression env x
                  | `Spre_elem x -> map_spread_element env x
                  )
              | None -> todo env ())
            in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)


and map_jsx_element (env : env) ((v1, v2, v3) : CST.jsx_element) =
  let v1 = map_jsx_opening_element env v1 in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Jsx_text tok -> token env tok (* pattern [^{}<>]+ *)
      | `Choice_jsx_elem x ->
          (match x with
          | `Jsx_elem x -> map_jsx_element env x
          | `Jsx_self_clos_elem x ->
              map_jsx_self_closing_element env x
          )
      | `Jsx_exp x -> map_jsx_expression env x
      )
    ) v2
  in
  let v3 = map_jsx_closing_element env v3 in
  todo env (v1, v2, v3)


and map_jsx_fragment (env : env) ((v1, v2, v3, v4, v5, v6) : CST.jsx_fragment) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* ">" *) in
  let v3 =
    List.map (fun x ->
      (match x with
      | `Jsx_text tok -> token env tok (* pattern [^{}<>]+ *)
      | `Choice_jsx_elem x ->
          (match x with
          | `Jsx_elem x -> map_jsx_element env x
          | `Jsx_self_clos_elem x ->
              map_jsx_self_closing_element env x
          )
      | `Jsx_exp x -> map_jsx_expression env x
      )
    ) v3
  in
  let v4 = token env v4 (* "<" *) in
  let v5 = token env v5 (* "/" *) in
  let v6 = token env v6 (* ">" *) in
  todo env (v1, v2, v3, v4, v5, v6)


and map_jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Exp x -> map_expression env x
        | `Seq_exp x -> map_sequence_expression env x
        | `Spre_elem x -> map_spread_element env x
        )
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_jsx_opening_element (env : env) ((v1, v2, v3, v4) : CST.jsx_opening_element) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | `Choice_jsx_id x ->
        (match x with
        | `Jsx_id tok ->
            token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
        | `Id tok -> token env tok (* identifier *)
        )
    | `Nest_id x -> map_nested_identifier env x
    | `Jsx_name_name x -> map_jsx_namespace_name env x
    )
  in
  let v3 =
    List.map (fun x ->
      (match x with
      | `Jsx_attr x -> map_jsx_attribute env x
      | `Jsx_exp x -> map_jsx_expression env x
      )
    ) v3
  in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)


and map_jsx_self_closing_element (env : env) ((v1, v2, v3, v4, v5) : CST.jsx_self_closing_element) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | `Choice_jsx_id x ->
        (match x with
        | `Jsx_id tok ->
            token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
        | `Id tok -> token env tok (* identifier *)
        )
    | `Nest_id x -> map_nested_identifier env x
    | `Jsx_name_name x -> map_jsx_namespace_name env x
    )
  in
  let v3 =
    List.map (fun x ->
      (match x with
      | `Jsx_attr x -> map_jsx_attribute env x
      | `Jsx_exp x -> map_jsx_expression env x
      )
    ) v3
  in
  let v4 = token env v4 (* "/" *) in
  let v5 = token env v5 (* ">" *) in
  todo env (v1, v2, v3, v4, v5)


and map_jsx_attribute (env : env) ((v1, v2) : CST.jsx_attribute) =
  let v1 =
    (match v1 with
    | `Choice_jsx_id x ->
        (match x with
        | `Jsx_id tok ->
            token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
        | `Id tok -> token env tok (* identifier *)
        )
    | `Jsx_name_name x -> map_jsx_namespace_name env x
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 =
          (match v2 with
          | `Str x -> map_string_ env x
          | `Jsx_exp x -> map_jsx_expression env x
          | `Choice_jsx_elem x ->
              (match x with
              | `Jsx_elem x -> map_jsx_element env x
              | `Jsx_self_clos_elem x ->
                  map_jsx_self_closing_element env x
              )
          | `Jsx_frag x -> map_jsx_fragment env x
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)


and map_class_ (env : env) ((v1, v2, v3, v4, v5) : CST.class_) =
  let v1 = List.map (map_decorator env) v1 in
  let v2 = token env v2 (* "class" *) in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* identifier *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_class_heritage env x
    | None -> todo env ())
  in
  let v5 = map_class_body env v5 in
  todo env (v1, v2, v3, v4, v5)


and map_class_heritage (env : env) ((v1, v2) : CST.class_heritage) =
  let v1 = token env v1 (* "extends" *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)


and map_function_ (env : env) ((v1, v2, v3, v4, v5) : CST.function_) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "function" *) in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* identifier *)
    | None -> todo env ())
  in
  let v4 = map_formal_parameters env v4 in
  let v5 = map_statement_block env v5 in
  todo env (v1, v2, v3, v4, v5)


and map_generator_function (env : env) ((v1, v2, v3, v4, v5, v6) : CST.generator_function) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "function" *) in
  let v3 = token env v3 (* "*" *) in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* identifier *)
    | None -> todo env ())
  in
  let v5 = map_formal_parameters env v5 in
  let v6 = map_statement_block env v6 in
  todo env (v1, v2, v3, v4, v5, v6)


and map_arrow_function (env : env) ((v1, v2, v3, v4) : CST.arrow_function) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Choice_choice_get x ->
        (match x with
        | `Choice_get x ->
            (match x with
            | `Get tok -> token env tok (* "get" *)
            | `Set tok -> token env tok (* "set" *)
            | `Async tok -> token env tok (* "async" *)
            | `Stat tok -> token env tok (* "static" *)
            )
        | `Id tok -> token env tok (* identifier *)
        )
    | `Form_params v1 -> map_formal_parameters env v1
    )
  in
  let v3 = token env v3 (* "=>" *) in
  let v4 =
    (match v4 with
    | `Exp x -> map_expression env x
    | `Stmt_blk x -> map_statement_block env x
    )
  in
  todo env (v1, v2, v3, v4)


and map_new_expression (env : env) ((v1, v2, v3) : CST.new_expression) =
  let v1 = token env v1 (* "new" *) in
  let v2 =
    (match v2 with
    | `This tok -> token env tok (* "this" *)
    | `Id tok -> token env tok (* identifier *)
    | `Choice_get x ->
        (match x with
        | `Get tok -> token env tok (* "get" *)
        | `Set tok -> token env tok (* "set" *)
        | `Async tok -> token env tok (* "async" *)
        | `Stat tok -> token env tok (* "static" *)
        )
    | `Num tok -> token env tok (* number *)
    | `Str x -> map_string_ env x
    | `Temp_str x -> map_template_string env x
    | `Regex x -> map_regex env x
    | `True tok -> token env tok (* "true" *)
    | `False tok -> token env tok (* "false" *)
    | `Null tok -> token env tok (* "null" *)
    | `Unde tok -> token env tok (* "undefined" *)
    | `Impo tok -> token env tok (* import *)
    | `Obj x -> map_object_ env x
    | `Array x -> map_array_ env x
    | `Func x -> map_function_ env x
    | `Arrow_func x -> map_arrow_function env x
    | `Gene_func x -> map_generator_function env x
    | `Class x -> map_class_ env x
    | `Paren_exp x -> map_parenthesized_expression env x
    | `Subs_exp x -> map_subscript_expression env x
    | `Memb_exp x -> map_member_expression env x
    | `Meta_prop x -> map_meta_property env x
    | `New_exp x -> map_new_expression env x
    )
  in
  let v3 =
    (match v3 with
    | Some x -> map_arguments env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> map_expression env x
    | `Id tok -> token env tok (* identifier *)
    | `Super tok -> token env tok (* "super" *)
    | `Choice_get x ->
        (match x with
        | `Get tok -> token env tok (* "get" *)
        | `Set tok -> token env tok (* "set" *)
        | `Async tok -> token env tok (* "async" *)
        | `Stat tok -> token env tok (* "static" *)
        )
    )
  in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)


and map_subscript_expression (env : env) ((v1, v2, v3, v4) : CST.subscript_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> map_expression env x
    | `Super tok -> token env tok (* "super" *)
    )
  in
  let v2 = token env v2 (* "[" *) in
  let v3 =
    (match v3 with
    | `Exp x -> map_expression env x
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)


and map_initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let v1 = token env v1 (* "=" *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)


and map_spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = token env v1 (* "..." *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)


and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Bin_exp_exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "**" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "===" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_inst_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "instanceof" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp_exp_in_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "in" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )


and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Un_exp_BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp_TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp_DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp_PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp_type_exp (v1, v2) ->
      let v1 = token env v1 (* "typeof" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp_void_exp (v1, v2) ->
      let v1 = token env v1 (* "void" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp_dele_exp (v1, v2) ->
      let v1 = token env v1 (* "delete" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )


and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSPLUS tok -> token env tok (* "++" *)
        | `DASHDASH tok -> token env tok (* "--" *)
        )
      in
      todo env (v1, v2)
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `PLUSPLUS tok -> token env tok (* "++" *)
        | `DASHDASH tok -> token env tok (* "--" *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )


and map_sequence_expression (env : env) ((v1, v2, v3) : CST.sequence_expression) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "," *) in
  let v3 =
    (match v3 with
    | `Seq_exp x -> map_sequence_expression env x
    | `Exp x -> map_expression env x
    )
  in
  todo env (v1, v2, v3)


and map_template_string (env : env) ((v1, v2, v3) : CST.template_string) =
  let v1 = token env v1 (* "`" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Temp_chars tok -> token env tok (* template_chars *)
      | `Esc_seq tok -> token env tok (* escape_sequence *)
      | `Temp_subs x -> map_template_substitution env x
      )
    ) v2
  in
  let v3 = token env v3 (* "`" *) in
  todo env (v1, v2, v3)


and map_template_substitution (env : env) ((v1, v2, v3) : CST.template_substitution) =
  let v1 = token env v1 (* "${" *) in
  let v2 =
    (match v2 with
    | `Exp x -> map_expression env x
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `Exp x -> map_expression env x
              | `Spre_elem x -> map_spread_element env x
              )
          | None -> todo env ())
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | Some x ->
                  (match x with
                  | `Exp x -> map_expression env x
                  | `Spre_elem x -> map_spread_element env x
                  )
              | None -> todo env ())
            in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and map_decorator (env : env) ((v1, v2) : CST.decorator) =
  let v1 = token env v1 (* "@" *) in
  let v2 =
    (match v2 with
    | `Choice_id x ->
        (match x with
        | `Id tok -> token env tok (* identifier *)
        | `Choice_get x ->
            (match x with
            | `Get tok -> token env tok (* "get" *)
            | `Set tok -> token env tok (* "set" *)
            | `Async tok -> token env tok (* "async" *)
            | `Stat tok -> token env tok (* "static" *)
            )
        )
    | `Deco_memb_exp x -> map_decorator_member_expression env x
    | `Deco_call_exp x -> map_decorator_call_expression env x
    )
  in
  todo env (v1, v2)


and map_decorator_call_expression (env : env) ((v1, v2) : CST.decorator_call_expression) =
  let v1 =
    (match v1 with
    | `Choice_id x ->
        (match x with
        | `Id tok -> token env tok (* identifier *)
        | `Choice_get x ->
            (match x with
            | `Get tok -> token env tok (* "get" *)
            | `Set tok -> token env tok (* "set" *)
            | `Async tok -> token env tok (* "async" *)
            | `Stat tok -> token env tok (* "static" *)
            )
        )
    | `Deco_memb_exp x -> map_decorator_member_expression env x
    )
  in
  let v2 = map_arguments env v2 in
  todo env (v1, v2)


and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Meth_defi_opt_SEMI (v1, v2) ->
          let v1 = map_method_definition env v1 in
          let v2 =
            (match v2 with
            | Some tok -> token env tok (* ";" *)
            | None -> todo env ())
          in
          todo env (v1, v2)
      | `Publ_field_defi_choice_auto_semi (v1, v2) ->
          let v1 = map_public_field_definition env v1 in
          let v2 =
            (match v2 with
            | `Auto_semi tok -> token env tok (* automatic_semicolon *)
            | `SEMI tok -> token env tok (* ";" *)
            )
          in
          todo env (v1, v2)
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)


and map_public_field_definition (env : env) ((v1, v2, v3) : CST.public_field_definition) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "static" *)
    | None -> todo env ())
  in
  let v2 = map_property_name env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_initializer_ env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)


and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 =
          (match v1 with
          | `Id tok -> token env tok (* identifier *)
          | `Choice_get x ->
              (match x with
              | `Get tok -> token env tok (* "get" *)
              | `Set tok -> token env tok (* "set" *)
              | `Async tok -> token env tok (* "async" *)
              | `Stat tok -> token env tok (* "static" *)
              )
          | `Choice_obj x ->
              (match x with
              | `Obj x -> map_object_ env x
              | `Array x -> map_array_ env x
              )
          | `Assign_pat x -> map_assignment_pattern env x
          | `Rest_param x -> map_rest_parameter env x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Id tok -> token env tok (* identifier *)
              | `Choice_get x ->
                  (match x with
                  | `Get tok -> token env tok (* "get" *)
                  | `Set tok -> token env tok (* "set" *)
                  | `Async tok -> token env tok (* "async" *)
                  | `Stat tok -> token env tok (* "static" *)
                  )
              | `Choice_obj x ->
                  (match x with
                  | `Obj x -> map_object_ env x
                  | `Array x -> map_array_ env x
                  )
              | `Assign_pat x -> map_assignment_pattern env x
              | `Rest_param x -> map_rest_parameter env x
              )
            in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)


and map_rest_parameter (env : env) ((v1, v2) : CST.rest_parameter) =
  let v1 = token env v1 (* "..." *) in
  let v2 =
    (match v2 with
    | `Id tok -> token env tok (* identifier *)
    | `Choice_obj x ->
        (match x with
        | `Obj x -> map_object_ env x
        | `Array x -> map_array_ env x
        )
    )
  in
  todo env (v1, v2)


and map_method_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.method_definition) =
  let v1 = List.map (map_decorator env) v1 in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "static" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `Get tok -> token env tok (* "get" *)
        | `Set tok -> token env tok (* "set" *)
        | `STAR tok -> token env tok (* "*" *)
        )
    | None -> todo env ())
  in
  let v5 = map_property_name env v5 in
  let v6 = map_formal_parameters env v6 in
  let v7 = map_statement_block env v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)


and map_pair (env : env) ((v1, v2, v3) : CST.pair) =
  let v1 = map_property_name env v1 in
  let v2 = token env v2 (* ":" *) in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)


and map_property_name (env : env) (x : CST.property_name) =
  (match x with
  | `Prop_name_choice_id x ->
      (match x with
      | `Id tok -> token env tok (* identifier *)
      | `Choice_get x ->
          (match x with
          | `Get tok -> token env tok (* "get" *)
          | `Set tok -> token env tok (* "set" *)
          | `Async tok -> token env tok (* "async" *)
          | `Stat tok -> token env tok (* "static" *)
          )
      )
  | `Prop_name_str x -> map_string_ env x
  | `Prop_name_num tok -> token env tok (* number *)
  | `Prop_name_comp_prop_name (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  )

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* pattern #!.* *)
    | None -> todo env ())
  in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Expo_stmt x -> map_export_statement env x
      | `Impo_stmt x -> map_import_statement env x
      | `Debu_stmt x -> map_debugger_statement env x
      | `Exp_stmt x -> map_expression_statement env x
      | `Decl x -> map_declaration env x
      | `Stmt_blk x -> map_statement_block env x
      | `If_stmt x -> map_if_statement env x
      | `Swit_stmt x -> map_switch_statement env x
      | `For_stmt x -> map_for_statement env x
      | `For_in_stmt x -> map_for_in_statement env x
      | `While_stmt x -> map_while_statement env x
      | `Do_stmt x -> map_do_statement env x
      | `Try_stmt x -> map_try_statement env x
      | `With_stmt x -> map_with_statement env x
      | `Brk_stmt x -> map_break_statement env x
      | `Cont_stmt x -> map_continue_statement env x
      | `Ret_stmt x -> map_return_statement env x
      | `Throw_stmt x -> map_throw_statement env x
      | `Empty_stmt tok -> token env tok (* ";" *)
      | `Labe_stmt x -> map_labeled_statement env x
      )
    ) v2
  in
  todo env (v1, v2)

