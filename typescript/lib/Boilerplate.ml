(**
   Boilerplate to be used as a template when mapping the typescript CST
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

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  token env tok (* escape_sequence *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* identifier *)

let map_jsx_identifier (env : env) (tok : CST.jsx_identifier) =
  token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)

let map_anon_choice_get_8fb02de (env : env) (x : CST.anon_choice_get_8fb02de) =
  (match x with
  | `Get tok -> token env tok (* "get" *)
  | `Set tok -> token env tok (* "set" *)
  | `STAR tok -> token env tok (* "*" *)
  )

let map_imm_tok_pat_3f3cd4d (env : env) (tok : CST.imm_tok_pat_3f3cd4d) =
  token env tok (* pattern "[^\"\\\\\\n]+|\\\\?\\r?\\n" *)

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  token env tok (* automatic_semicolon *)

let map_regex_pattern (env : env) (tok : CST.regex_pattern) =
  token env tok (* regex_pattern *)

let map_accessibility_modifier (env : env) (x : CST.accessibility_modifier) =
  (match x with
  | `Public tok -> token env tok (* "public" *)
  | `Priv tok -> token env tok (* "private" *)
  | `Prot tok -> token env tok (* "protected" *)
  )

let map_imm_tok_pat_a3af5dd (env : env) (tok : CST.imm_tok_pat_a3af5dd) =
  token env tok (* pattern "[^'\\\\\\n]+|\\\\?\\r?\\n" *)

let map_predefined_type (env : env) (x : CST.predefined_type) =
  (match x with
  | `Any tok -> token env tok (* "any" *)
  | `Num tok -> token env tok (* "number" *)
  | `Bool tok -> token env tok (* "boolean" *)
  | `Str tok -> token env tok (* "string" *)
  | `Symb tok -> token env tok (* "symbol" *)
  | `Void tok -> token env tok (* "void" *)
  )

let map_jsx_text (env : env) (tok : CST.jsx_text) =
  token env tok (* pattern [^{}<>]+ *)

let map_anon_choice_PLUSPLUS_e498e28 (env : env) (x : CST.anon_choice_PLUSPLUS_e498e28) =
  (match x with
  | `PLUSPLUS tok -> token env tok (* "++" *)
  | `DASHDASH tok -> token env tok (* "--" *)
  )

let map_hash_bang_line (env : env) (tok : CST.hash_bang_line) =
  token env tok (* pattern #!.* *)

let map_regex_flags (env : env) (tok : CST.regex_flags) =
  token env tok (* pattern [a-z]+ *)

let map_template_chars (env : env) (tok : CST.template_chars) =
  token env tok (* template_chars *)

let map_anon_choice_type_2b11f6b (env : env) (x : CST.anon_choice_type_2b11f6b) =
  (match x with
  | `Type tok -> token env tok (* "type" *)
  | `Typeof tok -> token env tok (* "typeof" *)
  )

let map_number (env : env) (tok : CST.number) =
  token env tok (* number *)

let map_import (env : env) (tok : CST.import) =
  token env tok (* import *)

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Decl tok -> token env tok (* "declare" *)
  | `Name tok -> token env tok (* "namespace" *)
  | `Type tok -> token env tok (* "type" *)
  | `Public tok -> token env tok (* "public" *)
  | `Priv tok -> token env tok (* "private" *)
  | `Prot tok -> token env tok (* "protected" *)
  | `Read tok -> token env tok (* "readonly" *)
  | `Module tok -> token env tok (* "module" *)
  | `Any tok -> token env tok (* "any" *)
  | `Num tok -> token env tok (* "number" *)
  | `Bool tok -> token env tok (* "boolean" *)
  | `Str tok -> token env tok (* "string" *)
  | `Symb tok -> token env tok (* "symbol" *)
  | `Export tok -> token env tok (* "export" *)
  | `Choice_get x ->
      (match x with
      | `Get tok -> token env tok (* "get" *)
      | `Set tok -> token env tok (* "set" *)
      | `Async tok -> token env tok (* "async" *)
      | `Static tok -> token env tok (* "static" *)
      )
  )

let map_semicolon (env : env) (x : CST.semicolon) =
  (match x with
  | `Auto_semi tok -> token env tok (* automatic_semicolon *)
  | `SEMI tok -> token env tok (* ";" *)
  )

let map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `DQUOT_rep_choice_imm_tok_pat_3f3cd4d_DQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Imm_tok_pat_3f3cd4d tok ->
              token env tok (* pattern "[^\"\\\\\\n]+|\\\\?\\r?\\n" *)
          | `Esc_seq tok -> token env tok (* escape_sequence *)
          )
        ) v2
      in
      let v3 = token env v3 (* "\"" *) in
      todo env (v1, v2, v3)
  | `SQUOT_rep_choice_imm_tok_pat_a3af5dd_SQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Imm_tok_pat_a3af5dd tok ->
              token env tok (* pattern "[^'\\\\\\n]+|\\\\?\\r?\\n" *)
          | `Esc_seq tok -> token env tok (* escape_sequence *)
          )
        ) v2
      in
      let v3 = token env v3 (* "'" *) in
      todo env (v1, v2, v3)
  )

let map_import_export_specifier (env : env) ((v1, v2, v3) : CST.import_export_specifier) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_type_2b11f6b env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* identifier *) in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "as" *) in
        let v2 = token env v2 (* identifier *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let rec map_anon_choice_type_id_42c0412 (env : env) (x : CST.anon_choice_type_id_42c0412) =
  (match x with
  | `Id tok -> token env tok (* identifier *)
  | `Nested_id x -> map_nested_identifier env x
  )

and map_nested_identifier (env : env) ((v1, v2, v3) : CST.nested_identifier) =
  let v1 = map_anon_choice_type_id_42c0412 env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_namespace_import (env : env) ((v1, v2, v3) : CST.namespace_import) =
  let v1 = token env v1 (* "*" *) in
  let v2 = token env v2 (* "as" *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_jsx_identifier_ (env : env) (x : CST.jsx_identifier_) =
  (match x with
  | `Jsx_id tok ->
      token env tok (* pattern [a-zA-Z_$][a-zA-Z\d_$]*-[a-zA-Z\d_$\-]* *)
  | `Id tok -> token env tok (* identifier *)
  )

let rec map_anon_choice_type_id_b8f8ced (env : env) (x : CST.anon_choice_type_id_b8f8ced) =
  (match x with
  | `Id tok -> token env tok (* identifier *)
  | `Deco_member_exp x ->
      map_decorator_member_expression env x
  )

and map_decorator_member_expression (env : env) ((v1, v2, v3) : CST.decorator_member_expression) =
  let v1 = map_anon_choice_type_id_b8f8ced env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_anon_choice_rese_id_515394d (env : env) (x : CST.anon_choice_rese_id_515394d) =
  (match x with
  | `Choice_decl x -> map_reserved_identifier env x
  | `Id tok -> token env tok (* identifier *)
  )

let map_anon_choice_type_id_dd17e7d (env : env) (x : CST.anon_choice_type_id_dd17e7d) =
  (match x with
  | `Id tok -> token env tok (* identifier *)
  | `Choice_decl x -> map_reserved_identifier env x
  )

let map_anon_choice_COMMA_5194cb4 (env : env) (x : CST.anon_choice_COMMA_5194cb4) =
  (match x with
  | `COMMA tok -> token env tok (* "," *)
  | `Choice_auto_semi x -> map_semicolon env x
  )

let map_from_clause (env : env) ((v1, v2) : CST.from_clause) =
  let v1 = token env v1 (* "from" *) in
  let v2 = map_string_ env v2 in
  todo env (v1, v2)

let map_import_require_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.import_require_clause) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* "=" *) in
  let v3 = token env v3 (* "require" *) in
  let v4 = token env v4 (* "(" *) in
  let v5 = map_string_ env v5 in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

let map_literal_type (env : env) (x : CST.literal_type) =
  (match x with
  | `Num_ (v1, v2) ->
      let v1 =
        (match v1 with
        | `DASH tok -> token env tok (* "-" *)
        | `PLUS tok -> token env tok (* "+" *)
        )
      in
      let v2 = token env v2 (* number *) in
      todo env (v1, v2)
  | `Num tok -> token env tok (* number *)
  | `Str x -> map_string_ env x
  | `True tok -> token env tok (* "true" *)
  | `False tok -> token env tok (* "false" *)
  )

let map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d (env : env) ((v1, v2) : CST.anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d) =
  let v1 = map_import_export_specifier env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_import_export_specifier env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let map_nested_type_identifier (env : env) ((v1, v2, v3) : CST.nested_type_identifier) =
  let v1 = map_anon_choice_type_id_42c0412 env v1 in
  let v2 = token env v2 (* "." *) in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

let map_jsx_namespace_name (env : env) ((v1, v2, v3) : CST.jsx_namespace_name) =
  let v1 = map_jsx_identifier_ env v1 in
  let v2 = token env v2 (* ":" *) in
  let v3 = map_jsx_identifier_ env v3 in
  todo env (v1, v2, v3)

let map_named_imports (env : env) ((v1, v2, v3, v4) : CST.named_imports) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x ->
        map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

let map_export_clause (env : env) ((v1, v2, v3, v4) : CST.export_clause) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x ->
        map_anon_import_export_spec_rep_COMMA_import_export_spec_3a1421d env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

let map_jsx_element_name (env : env) (x : CST.jsx_element_name) =
  (match x with
  | `Choice_jsx_id x -> map_jsx_identifier_ env x
  | `Nested_id x -> map_nested_identifier env x
  | `Jsx_name_name x -> map_jsx_namespace_name env x
  )

let map_jsx_attribute_name (env : env) (x : CST.jsx_attribute_name) =
  (match x with
  | `Choice_jsx_id x -> map_jsx_identifier_ env x
  | `Jsx_name_name x -> map_jsx_namespace_name env x
  )

let map_import_clause (env : env) (x : CST.import_clause) =
  (match x with
  | `Name_import x -> map_namespace_import env x
  | `Named_imports x -> map_named_imports env x
  | `Id_opt_COMMA_choice_name_import (v1, v2) ->
      let v1 = token env v1 (* identifier *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Name_import x -> map_namespace_import env x
              | `Named_imports x -> map_named_imports env x
              )
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2)
  )

let map_jsx_closing_element (env : env) ((v1, v2, v3, v4) : CST.jsx_closing_element) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* "/" *) in
  let v3 = map_jsx_element_name env v3 in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

let rec map_abstract_method_signature (env : env) ((v1, v2, v3, v4, v5, v6) : CST.abstract_method_signature) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "abstract" *) in
  let v3 =
    (match v3 with
    | Some x -> map_anon_choice_get_8fb02de env x
    | None -> todo env ())
  in
  let v4 = map_property_name env v4 in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* "?" *)
    | None -> todo env ())
  in
  let v6 = map_call_signature_ env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_anon_choice_choice_type_id_e16f95c (env : env) (x : CST.anon_choice_choice_type_id_e16f95c) =
  (match x with
  | `Choice_id x -> map_anon_choice_type_id_a85f573 env x
  | `Exp x -> map_expression env x
  )

and map_anon_choice_exp_6ded967 (env : env) (x : CST.anon_choice_exp_6ded967) =
  (match x with
  | `Exp x -> map_expression env x
  | `Choice_this x -> map_primary_expression env x
  )

and map_anon_choice_exp_9818c1b (env : env) (x : CST.anon_choice_exp_9818c1b) =
  (match x with
  | `Exp x -> map_expression env x
  | `Spread_elem x -> map_spread_element env x
  )

and map_anon_choice_export_stmt_f90d83f (env : env) (x : CST.anon_choice_export_stmt_f90d83f) =
  (match x with
  | `Export_stmt x -> map_export_statement env x
  | `Prop_sign (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some x -> map_accessibility_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "static" *)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some tok -> token env tok (* "readonly" *)
        | None -> todo env ())
      in
      let v4 = map_property_name env v4 in
      let v5 =
        (match v5 with
        | Some tok -> token env tok (* "?" *)
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Call_sign_ x -> map_call_signature_ env x
  | `Cons_sign (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let v2 =
        (match v2 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v3 = map_formal_parameters env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Index_sign x -> map_index_signature env x
  | `Meth_sign x -> map_method_signature env x
  )

and map_anon_choice_pair_bc93fa1 (env : env) (x : CST.anon_choice_pair_bc93fa1) =
  (match x with
  | `Pair (v1, v2, v3) ->
      let v1 = map_property_name env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Spread_elem x -> map_spread_element env x
  | `Meth_defi x -> map_method_definition env x
  | `Assign_pat (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Choice_choice_decl x ->
            map_anon_choice_rese_id_515394d env x
        | `Choice_obj x -> map_destructuring_pattern env x
        )
      in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Choice_id x -> map_anon_choice_type_id_dd17e7d env x
  )

and map_anon_choice_paren_exp_8725fb4 (env : env) (x : CST.anon_choice_paren_exp_8725fb4) =
  (match x with
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Choice_member_exp x -> map_lhs_expression env x
  )

and map_anon_choice_prop_name_6cc9e4b (env : env) (x : CST.anon_choice_prop_name_6cc9e4b) =
  (match x with
  | `Prop_name x -> map_property_name env x
  | `Enum_assign (v1, v2) ->
      let v1 = map_property_name env v1 in
      let v2 = map_initializer_ env v2 in
      todo env (v1, v2)
  )

and map_anon_choice_requ_param_1bd7580 (env : env) (x : CST.anon_choice_requ_param_1bd7580) =
  (match x with
  | `Requ_param (v1, v2, v3) ->
      let v1 = map_parameter_name env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Rest_param (v1, v2, v3) ->
      let v1 = token env v1 (* "..." *) in
      let v2 = token env v2 (* identifier *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Opt_param (v1, v2, v3, v4) ->
      let v1 = map_parameter_name env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  )

and map_anon_choice_type_id_21dd422 (env : env) (x : CST.anon_choice_type_id_21dd422) =
  (match x with
  | `Id tok -> token env tok (* identifier *)
  | `Choice_obj x -> map_destructuring_pattern env x
  )

and map_anon_choice_type_id_a85f573 (env : env) (x : CST.anon_choice_type_id_a85f573) =
  (match x with
  | `Id tok -> token env tok (* identifier *)
  | `Nested_type_id x -> map_nested_type_identifier env x
  | `Gene_type x -> map_generic_type env x
  )

and map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 (env : env) (opt : CST.anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4) =
  (match opt with
  | Some (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> map_anon_choice_exp_9818c1b env x
        | None -> todo env ())
      in
      let v2 = map_anon_rep_COMMA_opt_choice_exp_ca698a5 env v2 in
      todo env (v1, v2)
  | None -> todo env ())

and map_anon_rep_COMMA_opt_choice_exp_ca698a5 (env : env) (xs : CST.anon_rep_COMMA_opt_choice_exp_ca698a5) =
  List.map (fun (v1, v2) ->
    let v1 = token env v1 (* "," *) in
    let v2 =
      (match v2 with
      | Some x -> map_anon_choice_exp_9818c1b env x
      | None -> todo env ())
    in
    todo env (v1, v2)
  ) xs

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    map_anon_opt_opt_choice_exp_rep_COMMA_opt_choice_exp_208ebb4 env v2
  in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and map_array_type (env : env) (x : CST.array_type) =
  (match x with
  | `Read_prim_type_LBRACK_RBRACK (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "readonly" *) in
      let v2 = map_primary_type env v2 in
      let v3 = token env v3 (* "[" *) in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Prim_type_LBRACK_RBRACK (v1, v2, v3) ->
      let v1 = map_primary_type env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  )

and map_asserts (env : env) ((v1, v2, v3) : CST.asserts) =
  let v1 = token env v1 (* ":" *) in
  let v2 = token env v2 (* "asserts" *) in
  let v3 =
    (match v3 with
    | `Id tok -> token env tok (* identifier *)
    | `Id_is_type x -> map_type_predicate env x
    )
  in
  todo env (v1, v2, v3)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
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
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "**" *) in
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
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "===" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!==" *) in
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
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "instanceof" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_in_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "in" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Exp_opt_type_args_choice_args (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Args x -> map_arguments env x
        | `Temp_str x -> map_template_string env x
        )
      in
      todo env (v1, v2, v3)
  | `Choice_this_QMARKDOT_opt_type_args_args (v1, v2, v3, v4) ->
      let v1 = map_primary_expression env v1 in
      let v2 = token env v2 (* "?." *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v4 = map_arguments env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_call_signature (env : env) ((v1, v2, v3) : CST.call_signature) =
  let v1 =
    (match v1 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v2 = map_formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Type_anno x -> map_type_annotation env x
        | `Asserts x -> map_asserts env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_call_signature_ (env : env) (x : CST.call_signature_) =
  map_call_signature env x

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "(" *) in
        let v2 = map_anon_choice_type_id_21dd422 env v2 in
        let v3 = token env v3 (* ")" *) in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = map_statement_block env v3 in
  todo env (v1, v2, v3)

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Deco x -> map_decorator env x
      | `Meth_defi_opt_choice_auto_semi (v1, v2) ->
          let v1 = map_method_definition env v1 in
          let v2 =
            (match v2 with
            | Some x -> map_semicolon env x
            | None -> todo env ())
          in
          todo env (v1, v2)
      | `Choice_abst_meth_sign_choice_choice_auto_semi (v1, v2) ->
          let v1 =
            (match v1 with
            | `Abst_meth_sign x -> map_abstract_method_signature env x
            | `Index_sign x -> map_index_signature env x
            | `Meth_sign x -> map_method_signature env x
            | `Public_field_defi x -> map_public_field_definition env x
            )
          in
          let v2 =
            (match v2 with
            | `Choice_auto_semi x -> map_semicolon env x
            | `COMMA tok -> token env tok (* "," *)
            )
          in
          todo env (v1, v2)
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) =
  let v1 = List.map (map_decorator env) v1 in
  let v2 = token env v2 (* "class" *) in
  let v3 = token env v3 (* identifier *) in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_class_heritage env x
    | None -> todo env ())
  in
  let v6 = map_class_body env v6 in
  let v7 =
    (match v7 with
    | Some tok -> token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_class_heritage (env : env) (x : CST.class_heritage) =
  (match x with
  | `Extends_clause_opt_imples_clause (v1, v2) ->
      let v1 = map_extends_clause env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_implements_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Imples_clause x -> map_implements_clause env x
  )

and map_constraint_ (env : env) ((v1, v2) : CST.constraint_) =
  let v1 =
    (match v1 with
    | `Extends tok -> token env tok (* "extends" *)
    | `COLON tok -> token env tok (* ":" *)
    )
  in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Choice_func_decl x ->
      (match x with
      | `Func_decl x -> map_function_declaration env x
      | `Gene_func_decl x ->
          map_generator_function_declaration env x
      | `Class_decl x -> map_class_declaration env x
      | `Lexi_decl x -> map_lexical_declaration env x
      | `Var_decl x -> map_variable_declaration env x
      )
  | `Func_sign (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "function" *) in
      let v3 = token env v3 (* identifier *) in
      let v4 = map_call_signature_ env v4 in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Abst_class_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "abstract" *) in
      let v2 = token env v2 (* "class" *) in
      let v3 = token env v3 (* identifier *) in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_class_heritage env x
        | None -> todo env ())
      in
      let v6 = map_class_body env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Module (v1, v2) ->
      let v1 = token env v1 (* "module" *) in
      let v2 = map_module__ env v2 in
      todo env (v1, v2)
  | `Inte_module x -> map_internal_module env x
  | `Type_alias_decl (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "type" *) in
      let v2 = token env v2 (* identifier *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "=" *) in
      let v5 = map_type_ env v5 in
      let v6 = map_semicolon env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Enum_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "const" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "enum" *) in
      let v3 = token env v3 (* identifier *) in
      let v4 = map_enum_body env v4 in
      todo env (v1, v2, v3, v4)
  | `Inte_decl (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "interface" *) in
      let v2 = token env v2 (* identifier *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_extends_clause env x
        | None -> todo env ())
      in
      let v5 = map_object_type env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Import_alias (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "import" *) in
      let v2 = token env v2 (* identifier *) in
      let v3 = token env v3 (* "=" *) in
      let v4 = map_anon_choice_type_id_42c0412 env v4 in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Ambi_decl (v1, v2) ->
      let v1 = token env v1 (* "declare" *) in
      let v2 =
        (match v2 with
        | `Decl x -> map_declaration env x
        | `Global_stmt_blk (v1, v2) ->
            let v1 = token env v1 (* "global" *) in
            let v2 = map_statement_block env v2 in
            todo env (v1, v2)
        | `Module_DOT_id_COLON_type (v1, v2, v3, v4, v5) ->
            let v1 = token env v1 (* "module" *) in
            let v2 = token env v2 (* "." *) in
            let v3 = token env v3 (* identifier *) in
            let v4 = token env v4 (* ":" *) in
            let v5 = map_type_ env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      todo env (v1, v2)
  )

and map_decorator (env : env) ((v1, v2) : CST.decorator) =
  let v1 = token env v1 (* "@" *) in
  let v2 =
    (match v2 with
    | `Id tok -> token env tok (* identifier *)
    | `Deco_member_exp x ->
        map_decorator_member_expression env x
    | `Deco_call_exp x -> map_decorator_call_expression env x
    )
  in
  todo env (v1, v2)

and map_decorator_call_expression (env : env) ((v1, v2) : CST.decorator_call_expression) =
  let v1 = map_anon_choice_type_id_b8f8ced env v1 in
  let v2 = map_arguments env v2 in
  todo env (v1, v2)

and map_default_type (env : env) ((v1, v2) : CST.default_type) =
  let v1 = token env v1 (* "=" *) in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_destructuring_pattern (env : env) (x : CST.destructuring_pattern) =
  (match x with
  | `Obj x -> map_object_ env x
  | `Array x -> map_array_ env x
  )

and map_enum_body (env : env) ((v1, v2, v3) : CST.enum_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_anon_choice_prop_name_6cc9e4b env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_prop_name_6cc9e4b env v2 in
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
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_export_statement (env : env) (x : CST.export_statement) =
  (match x with
  | `Choice_export_choice_STAR_from_clause_choice_auto_semi x ->
      (match x with
      | `Export_choice_STAR_from_clause_choice_auto_semi (v1, v2) ->
          let v1 = token env v1 (* "export" *) in
          let v2 =
            (match v2 with
            | `STAR_from_clause_choice_auto_semi (v1, v2, v3) ->
                let v1 = token env v1 (* "*" *) in
                let v2 = map_from_clause env v2 in
                let v3 = map_semicolon env v3 in
                todo env (v1, v2, v3)
            | `Export_clause_from_clause_choice_auto_semi (v1, v2, v3) ->
                let v1 = map_export_clause env v1 in
                let v2 = map_from_clause env v2 in
                let v3 = map_semicolon env v3 in
                todo env (v1, v2, v3)
            | `Export_clause_choice_auto_semi (v1, v2) ->
                let v1 = map_export_clause env v1 in
                let v2 = map_semicolon env v2 in
                todo env (v1, v2)
            )
          in
          todo env (v1, v2)
      | `Rep_deco_export_choice_decl (v1, v2, v3) ->
          let v1 = List.map (map_decorator env) v1 in
          let v2 = token env v2 (* "export" *) in
          let v3 =
            (match v3 with
            | `Decl x -> map_declaration env x
            | `Defa_exp_choice_auto_semi (v1, v2, v3) ->
                let v1 = token env v1 (* "default" *) in
                let v2 = map_expression env v2 in
                let v3 = map_semicolon env v3 in
                todo env (v1, v2, v3)
            )
          in
          todo env (v1, v2, v3)
      )
  | `Export_EQ_id_choice_auto_semi (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "export" *) in
      let v2 = token env v2 (* "=" *) in
      let v3 = token env v3 (* identifier *) in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Export_as_name_id_choice_auto_semi (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "export" *) in
      let v2 = token env v2 (* "as" *) in
      let v3 = token env v3 (* "namespace" *) in
      let v4 = token env v4 (* identifier *) in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `As_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "as" *) in
      let v3 =
        (match v3 with
        | `Type x -> map_type_ env x
        | `Temp_str x -> map_template_string env x
        )
      in
      todo env (v1, v2, v3)
  | `Non_null_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!" *) in
      todo env (v1, v2)
  | `Inte_module x -> map_internal_module env x
  | `Super tok -> token env tok (* "super" *)
  | `Type_asse (v1, v2) ->
      let v1 = map_type_arguments env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Choice_this x -> map_primary_expression env x
  | `Assign_exp (v1, v2, v3) ->
      let v1 = map_anon_choice_paren_exp_8725fb4 env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Augm_assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Member_exp x -> map_member_expression env x
        | `Subs_exp x -> map_subscript_expression env x
        | `Choice_decl x -> map_reserved_identifier env x
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
        | `AMPAMPEQ tok -> token env tok (* "&&=" *)
        | `BARBAREQ tok -> token env tok (* "||=" *)
        | `QMARKQMARKEQ tok -> token env tok (* "??=" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Un_exp x -> map_unary_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ":" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Update_exp x -> map_update_expression env x
  | `New_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = map_primary_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Yield_exp (v1, v2) ->
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

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expressions env v1 in
  let v2 = map_semicolon env v2 in
  todo env (v1, v2)

and map_expressions (env : env) (x : CST.expressions) =
  (match x with
  | `Exp x -> map_expression env x
  | `Seq_exp x -> map_sequence_expression env x
  )

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = token env v1 (* "extends" *) in
  let v2 = map_anon_choice_choice_type_id_e16f95c env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_anon_choice_choice_type_id_e16f95c env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = map_statement_block env v2 in
  todo env (v1, v2)

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
  let v3 = map_anon_choice_paren_exp_8725fb4 env v3 in
  let v4 =
    (match v4 with
    | `In tok -> token env tok (* "in" *)
    | `Of tok -> token env tok (* "of" *)
    )
  in
  let v5 = map_expressions env v5 in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = List.map (map_decorator env) v1 in
        let v2 = map_anon_choice_requ_param_1bd7580 env v2 in
        let v3 =
          List.map (fun (v1, v2, v3) ->
            let v1 = token env v1 (* "," *) in
            let v2 = List.map (map_decorator env) v2 in
            let v3 = map_anon_choice_requ_param_1bd7580 env v3 in
            todo env (v1, v2, v3)
          ) v3
        in
        let v4 =
          (match v4 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "function" *) in
  let v3 = token env v3 (* identifier *) in
  let v4 = map_call_signature_ env v4 in
  let v5 = map_statement_block env v5 in
  let v6 =
    (match v6 with
    | Some tok -> token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and map_generator_function_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.generator_function_declaration) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "function" *) in
  let v3 = token env v3 (* "*" *) in
  let v4 = token env v4 (* identifier *) in
  let v5 = map_call_signature_ env v5 in
  let v6 = map_statement_block env v6 in
  let v7 =
    (match v7 with
    | Some tok -> token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> token env tok (* identifier *)
    | `Nested_type_id x -> map_nested_type_identifier env x
    )
  in
  let v2 = map_type_arguments env v2 in
  todo env (v1, v2)

and map_implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = token env v1 (* "implements" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_index_signature (env : env) ((v1, v2, v3, v4, v5) : CST.index_signature) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "readonly" *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "[" *) in
  let v3 =
    (match v3 with
    | `Choice_id_COLON_pred_type (v1, v2, v3) ->
        let v1 = map_anon_choice_type_id_dd17e7d env v1 in
        let v2 = token env v2 (* ":" *) in
        let v3 = map_predefined_type env v3 in
        todo env (v1, v2, v3)
    | `Mapped_type_clause x -> map_mapped_type_clause env x
    )
  in
  let v4 = token env v4 (* "]" *) in
  let v5 = map_type_annotation env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_initializer_ (env : env) ((v1, v2) : CST.initializer_) =
  let v1 = token env v1 (* "=" *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_internal_module (env : env) ((v1, v2) : CST.internal_module) =
  let v1 = token env v1 (* "namespace" *) in
  let v2 = map_module__ env v2 in
  todo env (v1, v2)

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
  let v4 = map_semicolon env v4 in
  todo env (v1, v2, v3, v4)

and map_lhs_expression (env : env) (x : CST.lhs_expression) =
  (match x with
  | `Member_exp x -> map_member_expression env x
  | `Subs_exp x -> map_subscript_expression env x
  | `Id tok -> token env tok (* identifier *)
  | `Choice_decl x -> map_reserved_identifier env x
  | `Choice_obj x -> map_destructuring_pattern env x
  )

and map_mapped_type_clause (env : env) ((v1, v2, v3) : CST.mapped_type_clause) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* "in" *) in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_member_expression (env : env) ((v1, v2, v3) : CST.member_expression) =
  let v1 = map_anon_choice_exp_6ded967 env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> token env tok (* "." *)
    | `QMARKDOT tok -> token env tok (* "?." *)
    )
  in
  let v3 = token env v3 (* identifier *) in
  todo env (v1, v2, v3)

and map_method_definition (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.method_definition) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "static" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "readonly" *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_anon_choice_get_8fb02de env x
    | None -> todo env ())
  in
  let v6 = map_property_name env v6 in
  let v7 =
    (match v7 with
    | Some tok -> token env tok (* "?" *)
    | None -> todo env ())
  in
  let v8 = map_call_signature_ env v8 in
  let v9 = map_statement_block env v9 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)

and map_method_signature (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.method_signature) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "static" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "readonly" *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_anon_choice_get_8fb02de env x
    | None -> todo env ())
  in
  let v6 = map_property_name env v6 in
  let v7 =
    (match v7 with
    | Some tok -> token env tok (* "?" *)
    | None -> todo env ())
  in
  let v8 = map_call_signature_ env v8 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and map_module__ (env : env) ((v1, v2) : CST.module__) =
  let v1 =
    (match v1 with
    | `Str x -> map_string_ env x
    | `Id tok -> token env tok (* identifier *)
    | `Nested_id x -> map_nested_identifier env x
    )
  in
  let v2 =
    (match v2 with
    | Some x -> map_statement_block env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_object_ (env : env) ((v1, v2, v3) : CST.object_) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_anon_choice_pair_bc93fa1 env x
          | None -> todo env ())
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 =
              (match v2 with
              | Some x -> map_anon_choice_pair_bc93fa1 env x
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

and map_object_type (env : env) ((v1, v2, v3) : CST.object_type) =
  let v1 =
    (match v1 with
    | `LCURL tok -> token env tok (* "{" *)
    | `LCURLBAR tok -> token env tok (* "{|" *)
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `COMMA tok -> token env tok (* "," *)
              | `SEMI tok -> token env tok (* ";" *)
              )
          | None -> todo env ())
        in
        let v2 = map_anon_choice_export_stmt_f90d83f env v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = map_anon_choice_COMMA_5194cb4 env v1 in
            let v2 = map_anon_choice_export_stmt_f90d83f env v2 in
            todo env (v1, v2)
          ) v3
        in
        let v4 =
          (match v4 with
          | Some x -> map_anon_choice_COMMA_5194cb4 env x
          | None -> todo env ())
        in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `RCURL tok -> token env tok (* "}" *)
    | `BARRCURL tok -> token env tok (* "|}" *)
    )
  in
  todo env (v1, v2, v3)

and map_parameter_name (env : env) ((v1, v2, v3) : CST.parameter_name) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "readonly" *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Id tok -> token env tok (* identifier *)
    | `Choice_decl x -> map_reserved_identifier env x
    | `Choice_obj x -> map_destructuring_pattern env x
    | `This tok -> token env tok (* "this" *)
    )
  in
  todo env (v1, v2, v3)

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Exp_opt_type_anno (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 =
          (match v2 with
          | Some x -> map_type_annotation env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Seq_exp x -> map_sequence_expression env x
    )
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `This tok -> token env tok (* "this" *)
  | `Super tok -> token env tok (* "super" *)
  | `Id tok -> token env tok (* identifier *)
  | `Choice_decl x -> map_reserved_identifier env x
  | `Num tok -> token env tok (* number *)
  | `Str x -> map_string_ env x
  | `Temp_str x -> map_template_string env x
  | `Regex (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "/" *) in
      let v2 = token env v2 (* regex_pattern *) in
      let v3 = token env v3 (* "/" *) in
      let v4 =
        (match v4 with
        | Some tok -> token env tok (* pattern [a-z]+ *)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `True tok -> token env tok (* "true" *)
  | `False tok -> token env tok (* "false" *)
  | `Null tok -> token env tok (* "null" *)
  | `Unde tok -> token env tok (* "undefined" *)
  | `Import tok -> token env tok (* import *)
  | `Obj x -> map_object_ env x
  | `Array x -> map_array_ env x
  | `Func (v1, v2, v3, v4, v5) ->
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
      let v4 = map_call_signature_ env v4 in
      let v5 = map_statement_block env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Arrow_func (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | `Choice_choice_decl x ->
            map_anon_choice_rese_id_515394d env x
        | `Call_sign x -> map_call_signature_ env x
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
  | `Gene_func (v1, v2, v3, v4, v5, v6) ->
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
      let v5 = map_call_signature_ env v5 in
      let v6 = map_statement_block env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Class (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.map (map_decorator env) v1 in
      let v2 = token env v2 (* "class" *) in
      let v3 =
        (match v3 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_class_heritage env x
        | None -> todo env ())
      in
      let v6 = map_class_body env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Subs_exp x -> map_subscript_expression env x
  | `Member_exp x -> map_member_expression env x
  | `Meta_prop (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* "target" *) in
      todo env (v1, v2, v3)
  | `Call_exp x -> map_call_expression env x
  )

and map_primary_type (env : env) (x : CST.primary_type) =
  (match x with
  | `Paren_type (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_ env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Pred_type x -> map_predefined_type env x
  | `Id tok -> token env tok (* identifier *)
  | `Nested_type_id x -> map_nested_type_identifier env x
  | `Gene_type x -> map_generic_type env x
  | `Type_pred x -> map_type_predicate env x
  | `Obj_type x -> map_object_type env x
  | `Array_type x -> map_array_type env x
  | `Tuple_type x -> map_tuple_type env x
  | `Flow_maybe_type (v1, v2) ->
      let v1 = token env v1 (* "?" *) in
      let v2 = map_primary_type env v2 in
      todo env (v1, v2)
  | `Type_query (v1, v2) ->
      let v1 = token env v1 (* "typeof" *) in
      let v2 = map_anon_choice_type_id_42c0412 env v2 in
      todo env (v1, v2)
  | `Index_type_query (v1, v2) ->
      let v1 = token env v1 (* "keyof" *) in
      let v2 =
        (match v2 with
        | `Gene_type x -> map_generic_type env x
        | `Id tok -> token env tok (* identifier *)
        | `Nested_type_id x -> map_nested_type_identifier env x
        )
      in
      todo env (v1, v2)
  | `This tok -> token env tok (* "this" *)
  | `Exis_type tok -> token env tok (* "*" *)
  | `Lit_type x -> map_literal_type env x
  | `Lookup_type (v1, v2, v3, v4) ->
      let v1 = map_primary_type env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_type_ env v3 in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  )

and map_property_name (env : env) (x : CST.property_name) =
  (match x with
  | `Choice_id x -> map_anon_choice_type_id_dd17e7d env x
  | `Str x -> map_string_ env x
  | `Num tok -> token env tok (* number *)
  | `Comp_prop_name (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  )

and map_public_field_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.public_field_definition) =
  let v1 =
    (match v1 with
    | Some x -> map_accessibility_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Opt_static_opt_read (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> token env tok (* "static" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> token env tok (* "readonly" *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_abst_opt_read (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> token env tok (* "abstract" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> token env tok (* "readonly" *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_read_opt_abst (v1, v2) ->
        let v1 =
          (match v1 with
          | Some tok -> token env tok (* "readonly" *)
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | Some tok -> token env tok (* "abstract" *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  in
  let v3 = map_property_name env v3 in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `QMARK tok -> token env tok (* "?" *)
        | `BANG tok -> token env tok (* "!" *)
        )
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_type_annotation env x
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_initializer_ env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

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

and map_spread_element (env : env) ((v1, v2) : CST.spread_element) =
  let v1 = token env v1 (* "..." *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Export_stmt x -> map_export_statement env x
  | `Import_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "import" *) in
      let v2 =
        (match v2 with
        | Some x -> map_anon_choice_type_2b11f6b env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Import_clause_from_clause (v1, v2) ->
            let v1 = map_import_clause env v1 in
            let v2 = map_from_clause env v2 in
            todo env (v1, v2)
        | `Import_requ_clause x -> map_import_require_clause env x
        | `Str x -> map_string_ env x
        )
      in
      let v4 = map_semicolon env v4 in
      todo env (v1, v2, v3, v4)
  | `Debu_stmt (v1, v2) ->
      let v1 = token env v1 (* "debugger" *) in
      let v2 = map_semicolon env v2 in
      todo env (v1, v2)
  | `Exp_stmt x -> map_expression_statement env x
  | `Decl x -> map_declaration env x
  | `Stmt_blk x -> map_statement_block env x
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "else" *) in
            let v2 = map_statement env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_body env v3 in
      todo env (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7) ->
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
        | Some x -> map_expressions env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* ")" *) in
      let v7 = map_statement env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `For_in_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "for" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "await" *)
        | None -> todo env ())
      in
      let v3 = map_for_header env v3 in
      let v4 = map_statement env v4 in
      todo env (v1, v2, v3, v4)
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = map_statement env v2 in
      let v3 = token env v3 (* "while" *) in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = map_semicolon env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Try_stmt (v1, v2, v3, v4) ->
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
  | `With_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "with" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expressions env x
        | None -> todo env ())
      in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = map_expressions env v2 in
      let v3 = map_semicolon env v3 in
      todo env (v1, v2, v3)
  | `Empty_stmt tok -> token env tok (* ";" *)
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = map_anon_choice_type_id_dd17e7d env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  )

and map_statement_block (env : env) ((v1, v2, v3, v4) : CST.statement_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* automatic_semicolon *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_subscript_expression (env : env) ((v1, v2, v3, v4, v5) : CST.subscript_expression) =
  let v1 = map_anon_choice_exp_6ded967 env v1 in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "?." *)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "[" *) in
  let v4 = map_expressions env v4 in
  let v5 = token env v5 (* "]" *) in
  todo env (v1, v2, v3, v4, v5)

and map_switch_body (env : env) ((v1, v2, v3) : CST.switch_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Switch_case x -> map_switch_case env x
      | `Switch_defa x -> map_switch_default env x
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = map_expressions env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 = List.map (map_statement env) v4 in
  todo env (v1, v2, v3, v4)

and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 = List.map (map_statement env) v3 in
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
  let v2 = map_expressions env v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_tuple_type (env : env) (x : CST.tuple_type) =
  (match x with
  | `Read_LBRACK_type_rep_COMMA_type_RBRACK (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "readonly" *) in
      let v2 = token env v2 (* "[" *) in
      let v3 = map_type_ env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_type_ env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 = token env v5 (* "]" *) in
      todo env (v1, v2, v3, v4, v5)
  | `LBRACK_type_rep_COMMA_type_RBRACK (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_type_ env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_type_ env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  )

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Prim_type x -> map_primary_type env x
  | `Union_type (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_type_ env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Inte_type (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_type_ env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Func_type (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v2 = map_formal_parameters env v2 in
      let v3 = token env v3 (* "=>" *) in
      let v4 = map_type_ env v4 in
      todo env (v1, v2, v3, v4)
  | `Cons_type (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "new" *) in
      let v2 =
        (match v2 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v3 = map_formal_parameters env v3 in
      let v4 = token env v4 (* "=>" *) in
      let v5 = map_type_ env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_type_annotation (env : env) ((v1, v2) : CST.type_annotation) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_type_arguments (env : env) ((v1, v2, v3, v4, v5) : CST.type_arguments) =
  let v1 = token env v1 (* "<" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ">" *) in
  todo env (v1, v2, v3, v4, v5)

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = token env v1 (* identifier *) in
  let v2 =
    (match v2 with
    | Some x -> map_constraint_ env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_default_type env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = token env v1 (* "<" *) in
  let v2 = map_type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ">" *) in
  todo env (v1, v2, v3, v4, v5)

and map_type_predicate (env : env) ((v1, v2, v3) : CST.type_predicate) =
  let v1 = token env v1 (* identifier *) in
  let v2 = token env v2 (* "is" *) in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Typeof_exp (v1, v2) ->
      let v1 = token env v1 (* "typeof" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Void_exp (v1, v2) ->
      let v1 = token env v1 (* "void" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Delete_exp (v1, v2) ->
      let v1 = token env v1 (* "delete" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_choice_PLUSPLUS (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_PLUSPLUS_e498e28 env v2 in
      todo env (v1, v2)
  | `Choice_PLUSPLUS_exp (v1, v2) ->
      let v1 = map_anon_choice_PLUSPLUS_e498e28 env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

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
  let v4 = map_semicolon env v4 in
  todo env (v1, v2, v3, v4)

and map_variable_declarator (env : env) (x : CST.variable_declarator) =
  (match x with
  | `Choice_id_opt_type_anno_opt_init (v1, v2, v3) ->
      let v1 = map_anon_choice_type_id_21dd422 env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_annotation env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_initializer_ env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Id_BANG_type_anno (v1, v2, v3) ->
      let v1 = token env v1 (* identifier *) in
      let v2 = token env v2 (* "!" *) in
      let v3 = map_type_annotation env v3 in
      todo env (v1, v2, v3)
  )

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* pattern #!.* *)
    | None -> todo env ())
  in
  let v2 = List.map (map_statement env) v2 in
  todo env (v1, v2)

let map_anon_choice_jsx_attr_name_b052322 (env : env) (x : CST.anon_choice_jsx_attr_name_b052322) =
  (match x with
  | `Choice_choice_jsx_id x -> map_jsx_attribute_name env x
  | `Choice_id_opt_type_args (v1, v2) ->
      let v1 = map_anon_choice_type_id_42c0412 env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  )

let map_jsx_expression (env : env) ((v1, v2, v3) : CST.jsx_expression) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Exp x -> map_expression env x
        | `Seq_exp x -> map_sequence_expression env x
        | `Spread_elem x -> map_spread_element env x
        )
    | None -> todo env ())
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

let rec map_jsx_attribute_ (env : env) (x : CST.jsx_attribute_) =
  (match x with
  | `Jsx_attr (v1, v2) ->
      let v1 = map_jsx_attribute_name env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_jsx_attribute_value env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Jsx_exp x -> map_jsx_expression env x
  )

and map_jsx_attribute_value (env : env) (x : CST.jsx_attribute_value) =
  (match x with
  | `Str x -> map_string_ env x
  | `Jsx_exp x -> map_jsx_expression env x
  | `Choice_jsx_elem x -> map_jsx_element_ env x
  | `Jsx_frag x -> map_jsx_fragment env x
  )

and map_jsx_child (env : env) (x : CST.jsx_child) =
  (match x with
  | `Jsx_text tok -> token env tok (* pattern [^{}<>]+ *)
  | `Choice_jsx_elem x -> map_jsx_element_ env x
  | `Jsx_frag x -> map_jsx_fragment env x
  | `Jsx_exp x -> map_jsx_expression env x
  )

and map_jsx_element_ (env : env) (x : CST.jsx_element_) =
  (match x with
  | `Jsx_elem (v1, v2, v3) ->
      let v1 = map_jsx_opening_element env v1 in
      let v2 = List.map (map_jsx_child env) v2 in
      let v3 = map_jsx_closing_element env v3 in
      todo env (v1, v2, v3)
  | `Jsx_self_clos_elem (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "<" *) in
      let v2 = map_anon_choice_jsx_attr_name_b052322 env v2 in
      let v3 = List.map (map_jsx_attribute_ env) v3 in
      let v4 = token env v4 (* "/" *) in
      let v5 = token env v5 (* ">" *) in
      todo env (v1, v2, v3, v4, v5)
  )

and map_jsx_fragment (env : env) ((v1, v2, v3, v4, v5, v6) : CST.jsx_fragment) =
  let v1 = token env v1 (* "<" *) in
  let v2 = token env v2 (* ">" *) in
  let v3 = List.map (map_jsx_child env) v3 in
  let v4 = token env v4 (* "<" *) in
  let v5 = token env v5 (* "/" *) in
  let v6 = token env v6 (* ">" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_jsx_opening_element (env : env) ((v1, v2, v3, v4) : CST.jsx_opening_element) =
  let v1 = token env v1 (* "<" *) in
  let v2 = map_anon_choice_jsx_attr_name_b052322 env v2 in
  let v3 = List.map (map_jsx_attribute_ env) v3 in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)
