(**
   Boilerplate to be used as a template when mapping the java CST
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

let map_octal_integer_literal (env : env) (tok : CST.octal_integer_literal) =
  token env tok (* octal_integer_literal *)

let map_integral_type (env : env) (x : CST.integral_type) =
  (match x with
  | `Byte tok -> token env tok (* "byte" *)
  | `Short tok -> token env tok (* "short" *)
  | `Int tok -> token env tok (* "int" *)
  | `Long tok -> token env tok (* "long" *)
  | `Char tok -> token env tok (* "char" *)
  )

let map_reserved_identifier (env : env) (x : CST.reserved_identifier) =
  (match x with
  | `Open tok -> token env tok (* "open" *)
  | `Module tok -> token env tok (* "module" *)
  )

let map_decimal_integer_literal (env : env) (tok : CST.decimal_integer_literal) =
  token env tok (* decimal_integer_literal *)

let map_character_literal (env : env) (tok : CST.character_literal) =
  token env tok (* character_literal *)

let map_requires_modifier (env : env) (x : CST.requires_modifier) =
  (match x with
  | `Tran tok -> token env tok (* "transitive" *)
  | `Static tok -> token env tok (* "static" *)
  )

let map_string_literal (env : env) (tok : CST.string_literal) =
  token env tok (* string_literal *)

let map_decimal_floating_point_literal (env : env) (tok : CST.decimal_floating_point_literal) =
  token env tok (* decimal_floating_point_literal *)

let map_binary_integer_literal (env : env) (tok : CST.binary_integer_literal) =
  token env tok (* binary_integer_literal *)

let map_floating_point_type (env : env) (x : CST.floating_point_type) =
  (match x with
  | `Float tok -> token env tok (* "float" *)
  | `Double tok -> token env tok (* "double" *)
  )

let map_hex_floating_point_literal (env : env) (tok : CST.hex_floating_point_literal) =
  token env tok (* hex_floating_point_literal *)

let map_hex_integer_literal (env : env) (tok : CST.hex_integer_literal) =
  token env tok (* hex_integer_literal *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* pattern [a-zA-Z_]\w* *)

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Deci_int_lit tok ->
      token env tok (* decimal_integer_literal *)
  | `Hex_int_lit tok ->
      token env tok (* hex_integer_literal *)
  | `Octal_int_lit tok ->
      token env tok (* octal_integer_literal *)
  | `Bin_int_lit tok ->
      token env tok (* binary_integer_literal *)
  | `Deci_floa_point_lit tok ->
      token env tok (* decimal_floating_point_literal *)
  | `Hex_floa_point_lit tok ->
      token env tok (* hex_floating_point_literal *)
  | `True tok -> token env tok (* "true" *)
  | `False tok -> token env tok (* "false" *)
  | `Char_lit tok -> token env tok (* character_literal *)
  | `Str_lit tok -> token env tok (* string_literal *)
  | `Null_lit tok -> token env tok (* "null" *)
  )

let map_anon_choice_id (env : env) (x : CST.anon_choice_id) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Choice_open x -> map_reserved_identifier env x
  )

let rec map_name (env : env) (x : CST.name) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Choice_open x -> map_reserved_identifier env x
  | `Scoped_id (v1, v2, v3) ->
      let v1 = map_name env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
      todo env (v1, v2, v3)
  )

let map_inferred_parameters (env : env) ((v1, v2, v3, v4) : CST.inferred_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

let map_module_directive (env : env) ((v1, v2) : CST.module_directive) =
  let v1 =
    (match v1 with
    | `Requis_rep_requis_modi_choice_id (v1, v2, v3) ->
        let v1 = token env v1 (* "requires" *) in
        let v2 = List.map (map_requires_modifier env) v2 in
        let v3 = map_name env v3 in
        todo env (v1, v2, v3)
    | `Exports_choice_id_opt_to_opt_choice_id_rep_COMMA_choice_id (v1, v2, v3, v4, v5) ->
        let v1 = token env v1 (* "exports" *) in
        let v2 = map_name env v2 in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "to" *)
          | None -> todo env ())
        in
        let v4 =
          (match v4 with
          | Some x -> map_name env x
          | None -> todo env ())
        in
        let v5 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_name env v2 in
            todo env (v1, v2)
          ) v5
        in
        todo env (v1, v2, v3, v4, v5)
    | `Opens_choice_id_opt_to_opt_choice_id_rep_COMMA_choice_id (v1, v2, v3, v4, v5) ->
        let v1 = token env v1 (* "opens" *) in
        let v2 = map_name env v2 in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "to" *)
          | None -> todo env ())
        in
        let v4 =
          (match v4 with
          | Some x -> map_name env x
          | None -> todo env ())
        in
        let v5 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_name env v2 in
            todo env (v1, v2)
          ) v5
        in
        todo env (v1, v2, v3, v4, v5)
    | `Uses_choice_id (v1, v2) ->
        let v1 = token env v1 (* "uses" *) in
        let v2 = map_name env v2 in
        todo env (v1, v2)
    | `Provis_choice_id_with_choice_id_rep_COMMA_choice_id (v1, v2, v3, v4, v5) ->
        let v1 = token env v1 (* "provides" *) in
        let v2 = map_name env v2 in
        let v3 = token env v3 (* "with" *) in
        let v4 = map_name env v4 in
        let v5 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_name env v2 in
            todo env (v1, v2)
          ) v5
        in
        todo env (v1, v2, v3, v4, v5)
    )
  in
  let v2 = token env v2 (* ";" *) in
  todo env (v1, v2)

let map_module_body (env : env) ((v1, v2, v3) : CST.module_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_module_directive env) v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

let rec map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_anon_choice_formal_param (env : env) (x : CST.anon_choice_formal_param) =
  (match x with
  | `Formal_param (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_id env v3 in
      todo env (v1, v2, v3)
  | `Spread_param (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = token env v3 (* "..." *) in
      let v4 = map_variable_declarator env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_anon_choice_type (env : env) (x : CST.anon_choice_type) =
  (match x with
  | `Type x -> map_type_ env x
  | `Wild (v1, v2, v3) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 =
        (match v3 with
        | Some x -> map_wildcard_bounds env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  )

and map_generic_type (env : env) ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | `Scoped_type_id x -> map_scoped_type_identifier env x
    )
  in
  let v2 = map_type_arguments env v2 in
  todo env (v1, v2)

and map_throws (env : env) ((v1, v2, v3) : CST.throws) =
  let v1 = token env v1 (* "throws" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
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
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%" *) in
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
  | `Exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_extends_interfaces (env : env) ((v1, v2) : CST.extends_interfaces) =
  let v1 = token env v1 (* "extends" *) in
  let v2 = map_interface_type_list env v2 in
  todo env (v1, v2)

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_program env v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_variable_declarator (env : env) ((v1, v2) : CST.variable_declarator) =
  let v1 = map_variable_declarator_id env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_variable_initializer env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_type env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_type env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ">" *) in
  todo env (v1, v2, v3)

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_class_body_declaration env) v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some x -> map_type_bound env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_catch_type (env : env) ((v1, v2) : CST.catch_type) =
  let v1 = map_unannotated_type env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "|" *) in
      let v2 = map_unannotated_type env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_interface_type_list (env : env) ((v1, v2) : CST.interface_type_list) =
  let v1 = map_type_ env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_local_variable_declaration (env : env) ((v1, v2, v3, v4) : CST.local_variable_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_variable_declarator_list env v3 in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

and map_primary (env : env) (x : CST.primary) =
  (match x with
  | `Lit x -> map_literal env x
  | `Class_lit (v1, v2, v3) ->
      let v1 = map_unannotated_type env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = token env v3 (* "class" *) in
      todo env (v1, v2, v3)
  | `This tok -> token env tok (* "this" *)
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Choice_open x -> map_reserved_identifier env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Obj_crea_exp x -> map_object_creation_expression env x
  | `Field_access x -> map_field_access env x
  | `Array_access x -> map_array_access env x
  | `Meth_invo (v1, v2) ->
      let v1 =
        (match v1 with
        | `Choice_id x -> map_anon_choice_id env x
        | `Choice_prim_DOT_opt_super_DOT_opt_type_args_choice_id (v1, v2, v3, v4, v5) ->
            let v1 = map_anon_choice_prim env v1 in
            let v2 = token env v2 (* "." *) in
            let v3 =
              (match v3 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* "super" *) in
                  let v2 = token env v2 (* "." *) in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            let v4 =
              (match v4 with
              | Some x -> map_type_arguments env x
              | None -> todo env ())
            in
            let v5 = map_anon_choice_id env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      let v2 = map_argument_list env v2 in
      todo env (v1, v2)
  | `Meth_ref (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | `Type x -> map_type_ env x
        | `Prim x -> map_primary env x
        | `Super tok -> token env tok (* "super" *)
        )
      in
      let v2 = token env v2 (* "::" *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | `New tok -> token env tok (* "new" *)
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        )
      in
      todo env (v1, v2, v3, v4)
  | `Array_crea_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = map_simple_type env v2 in
      let v3 =
        (match v3 with
        | `Rep1_dimens_expr_opt_dimens (v1, v2) ->
            let v1 = List.map (map_dimensions_expr env) v1 in
            let v2 =
              (match v2 with
              | Some x -> map_dimensions env x
              | None -> todo env ())
            in
            todo env (v1, v2)
        | `Dimens_array_init (v1, v2) ->
            let v1 = map_dimensions env v1 in
            let v2 = map_array_initializer env v2 in
            todo env (v1, v2)
        )
      in
      todo env (v1, v2, v3)
  )

and map_dimensions (env : env) (xs : CST.dimensions) =
  List.map (fun (v1, v2, v3) ->
    let v1 = List.map (map_annotation env) v1 in
    let v2 = token env v2 (* "[" *) in
    let v3 = token env v3 (* "]" *) in
    todo env (v1, v2, v3)
  ) xs

and map_variable_declarator_list (env : env) ((v1, v2) : CST.variable_declarator_list) =
  let v1 = map_variable_declarator env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_variable_declarator env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_switch_label (env : env) (x : CST.switch_label) =
  (match x with
  | `Case_exp_COLON (v1, v2, v3) ->
      let v1 = token env v1 (* "case" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ":" *) in
      todo env (v1, v2, v3)
  | `Defa_COLON (v1, v2) ->
      let v1 = token env v1 (* "default" *) in
      let v2 = token env v2 (* ":" *) in
      todo env (v1, v2)
  )

and map_array_initializer (env : env) ((v1, v2, v3, v4) : CST.array_initializer) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_variable_initializer env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_variable_initializer env v2 in
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

and map_simple_type (env : env) (x : CST.simple_type) =
  (match x with
  | `Void_type tok -> token env tok (* "void" *)
  | `Inte_type x -> map_integral_type env x
  | `Floa_point_type x -> map_floating_point_type env x
  | `Bool_type tok -> token env tok (* "boolean" *)
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  | `Gene_type x -> map_generic_type env x
  )

and map_interface_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.interface_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "interface" *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_extends_interfaces env x
    | None -> todo env ())
  in
  let v6 = map_interface_body env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_annotation_type_declaration (env : env) ((v1, v2, v3, v4) : CST.annotation_type_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "@interface" *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = map_annotation_type_body env v4 in
  todo env (v1, v2, v3, v4)

and map_interface_body (env : env) ((v1, v2, v3) : CST.interface_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Cst_decl x -> map_constant_declaration env x
      | `Enum_decl x -> map_enum_declaration env x
      | `Meth_decl x -> map_method_declaration env x
      | `Class_decl x -> map_class_declaration env x
      | `Inte_decl x -> map_interface_declaration env x
      | `Anno_type_decl x -> map_annotation_type_declaration env x
      | `SEMI tok -> token env tok (* ";" *)
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_catch_formal_parameter env v3 in
  let v4 = token env v4 (* ")" *) in
  let v5 = map_block env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_anon_exp_rep_COMMA_exp (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_scoped_type_identifier (env : env) ((v1, v2, v3, v4) : CST.scoped_type_identifier) =
  let v1 =
    (match v1 with
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | `Scoped_type_id x -> map_scoped_type_identifier env x
    | `Gene_type x -> map_generic_type env x
    )
  in
  let v2 = token env v2 (* "." *) in
  let v3 = List.map (map_annotation env) v3 in
  let v4 = token env v4 (* pattern [a-zA-Z_]\w* *) in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | `Choice_open x -> map_reserved_identifier env x
        | `Field_access x -> map_field_access env x
        | `Array_access x -> map_array_access env x
        )
      in
      let v2 =
        (match v2 with
        | `EQ tok -> token env tok (* "=" *)
        | `PLUSEQ tok -> token env tok (* "+=" *)
        | `DASHEQ tok -> token env tok (* "-=" *)
        | `STAREQ tok -> token env tok (* "*=" *)
        | `SLASHEQ tok -> token env tok (* "/=" *)
        | `AMPEQ tok -> token env tok (* "&=" *)
        | `BAREQ tok -> token env tok (* "|=" *)
        | `HATEQ tok -> token env tok (* "^=" *)
        | `PERCEQ tok -> token env tok (* "%=" *)
        | `LTLTEQ tok -> token env tok (* "<<=" *)
        | `GTGTEQ tok -> token env tok (* ">>=" *)
        | `GTGTGTEQ tok -> token env tok (* ">>>=" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp x -> map_binary_expression env x
  | `Inst_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "instanceof" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Lambda_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | `Formal_params x -> map_formal_parameters env x
        | `Infe_params x -> map_inferred_parameters env x
        )
      in
      let v2 = token env v2 (* "->" *) in
      let v3 =
        (match v3 with
        | `Exp x -> map_expression env x
        | `Blk x -> map_block env x
        )
      in
      todo env (v1, v2, v3)
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ":" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Update_exp x -> map_update_expression env x
  | `Prim x -> map_primary env x
  | `Un_exp x -> map_unary_expression env x
  | `Cast_exp (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_ env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "&" *) in
          let v2 = map_type_ env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* ")" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_annotation_type_element_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.annotation_type_element_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = token env v4 (* "(" *) in
  let v5 = token env v5 (* ")" *) in
  let v6 =
    (match v6 with
    | Some x -> map_dimensions env x
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | Some x -> map_default_value env x
    | None -> todo env ())
  in
  let v8 = token env v8 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

and map_class_body_declaration (env : env) (x : CST.class_body_declaration) =
  (match x with
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_list env v3 in
      let v4 = token env v4 (* ";" *) in
      todo env (v1, v2, v3, v4)
  | `Meth_decl x -> map_method_declaration env x
  | `Class_decl x -> map_class_declaration env x
  | `Inte_decl x -> map_interface_declaration env x
  | `Anno_type_decl x -> map_annotation_type_declaration env x
  | `Enum_decl x -> map_enum_declaration env x
  | `Blk x -> map_block env x
  | `Static_init (v1, v2) ->
      let v1 = token env v1 (* "static" *) in
      let v2 = map_block env v2 in
      todo env (v1, v2)
  | `Cons_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_constructor_declarator env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_throws env x
        | None -> todo env ())
      in
      let v4 = map_constructor_body env v4 in
      todo env (v1, v2, v3, v4)
  | `SEMI tok -> token env tok (* ";" *)
  )

and map_formal_parameters (env : env) ((v1, v2, v3, v4) : CST.formal_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x -> map_receiver_parameter env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_formal_param env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_formal_param env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_array_access (env : env) ((v1, v2, v3, v4) : CST.array_access) =
  let v1 = map_primary env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and map_modifiers (env : env) (xs : CST.modifiers) =
  List.map (fun x ->
    (match x with
    | `Anno x -> map_annotation env x
    | `Public tok -> token env tok (* "public" *)
    | `Prot tok -> token env tok (* "protected" *)
    | `Priv tok -> token env tok (* "private" *)
    | `Abst tok -> token env tok (* "abstract" *)
    | `Static tok -> token env tok (* "static" *)
    | `Final tok -> token env tok (* "final" *)
    | `Stri tok -> token env tok (* "strictfp" *)
    | `Defa tok -> token env tok (* "default" *)
    | `Sync tok -> token env tok (* "synchronized" *)
    | `Native tok -> token env tok (* "native" *)
    | `Tran tok -> token env tok (* "transient" *)
    | `Vola tok -> token env tok (* "volatile" *)
    )
  ) xs

and map_catch_formal_parameter (env : env) ((v1, v2, v3) : CST.catch_formal_parameter) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_catch_type env v2 in
  let v3 = map_variable_declarator_id env v3 in
  todo env (v1, v2, v3)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Decl x -> map_declaration env x
  | `Exp_stmt (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ";" *) in
      todo env (v1, v2)
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
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
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | `Local_var_decl x -> map_local_variable_declaration env x
        | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) ->
            let v1 =
              (match v1 with
              | Some x -> map_anon_exp_rep_COMMA_exp env x
              | None -> todo env ())
            in
            let v2 = token env v2 (* ";" *) in
            todo env (v1, v2)
        )
      in
      let v4 =
        (match v4 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v5 = token env v5 (* ";" *) in
      let v6 =
        (match v6 with
        | Some x -> map_anon_exp_rep_COMMA_exp env x
        | None -> todo env ())
      in
      let v7 = token env v7 (* ")" *) in
      let v8 = map_statement env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Enha_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v4 = map_unannotated_type env v4 in
      let v5 = map_variable_declarator_id env v5 in
      let v6 = token env v6 (* ":" *) in
      let v7 = map_expression env v7 in
      let v8 = token env v8 (* ")" *) in
      let v9 = map_statement env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Blk x -> map_block env x
  | `SEMI tok -> token env tok (* ";" *)
  | `Assert_stmt x -> map_assert_statement env x
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_switch_block env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = map_statement env v2 in
      let v3 = token env v3 (* "while" *) in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Sync_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "synchronized" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_block env v3 in
      todo env (v1, v2, v3)
  | `Local_var_decl x -> map_local_variable_declaration env x
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = map_block env v2 in
      let v3 =
        (match v3 with
        | `Rep1_catch_clause xs ->
            List.map (map_catch_clause env) xs
        | `Rep_catch_clause_fina_clause (v1, v2) ->
            let v1 = List.map (map_catch_clause env) v1 in
            let v2 = map_finally_clause env v2 in
            todo env (v1, v2)
        )
      in
      todo env (v1, v2, v3)
  | `Try_with_resous_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = map_resource_specification env v2 in
      let v3 = map_block env v3 in
      let v4 = List.map (map_catch_clause env) v4 in
      let v5 =
        (match v5 with
        | Some x -> map_finally_clause env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and map_enum_body_declarations (env : env) ((v1, v2) : CST.enum_body_declarations) =
  let v1 = token env v1 (* ";" *) in
  let v2 = List.map (map_class_body_declaration env) v2 in
  todo env (v1, v2)

and map_anon_choice_prim (env : env) (x : CST.anon_choice_prim) =
  (match x with
  | `Prim x -> map_primary env x
  | `Super tok -> token env tok (* "super" *)
  )

and map_unannotated_type (env : env) (x : CST.unannotated_type) =
  (match x with
  | `Choice_void_type x -> map_simple_type env x
  | `Array_type (v1, v2) ->
      let v1 = map_unannotated_type env v1 in
      let v2 = map_dimensions env v2 in
      todo env (v1, v2)
  )

and map_receiver_parameter (env : env) ((v1, v2, v3, v4) : CST.receiver_parameter) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 = map_unannotated_type env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
        let v2 = token env v2 (* "." *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "this" *) in
  todo env (v1, v2, v3, v4)

and map_annotation_argument_list (env : env) ((v1, v2, v3) : CST.annotation_argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Elem_value x -> map_element_value env x
    | `Opt_elem_value_pair_rep_COMMA_elem_value_pair opt ->
        (match opt with
        | Some (v1, v2) ->
            let v1 = map_element_value_pair env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_element_value_pair env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
    )
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_enum_constant (env : env) ((v1, v2, v3, v4) : CST.enum_constant) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some x -> map_argument_list env x
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_class_body env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_class_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "class" *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_superclass env x
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_super_interfaces env x
    | None -> todo env ())
  in
  let v7 = map_class_body env v7 in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_annotation (env : env) (x : CST.annotation) =
  (match x with
  | `Marker_anno (v1, v2) ->
      let v1 = token env v1 (* "@" *) in
      let v2 = map_name env v2 in
      todo env (v1, v2)
  | `Anno_ (v1, v2, v3) ->
      let v1 = token env v1 (* "@" *) in
      let v2 = map_name env v2 in
      let v3 = map_annotation_argument_list env v3 in
      todo env (v1, v2, v3)
  )

and map_annotation_type_body (env : env) ((v1, v2, v3) : CST.annotation_type_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Anno_type_elem_decl x ->
          map_annotation_type_element_declaration env x
      | `Cst_decl x -> map_constant_declaration env x
      | `Class_decl x -> map_class_declaration env x
      | `Inte_decl x -> map_interface_declaration env x
      | `Anno_type_decl x -> map_annotation_type_declaration env x
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_object_creation_expression (env : env) (x : CST.object_creation_expression) =
  (match x with
  | `Unqu_obj_crea_exp x ->
      map_unqualified_object_creation_expression env x
  | `Prim_DOT_unqu_obj_crea_exp (v1, v2, v3) ->
      let v1 = map_primary env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 =
        map_unqualified_object_creation_expression env v3
      in
      todo env (v1, v2, v3)
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "++" *) in
      todo env (v1, v2)
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "--" *) in
      todo env (v1, v2)
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "++" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token env v1 (* "--" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

and map_wildcard_bounds (env : env) (x : CST.wildcard_bounds) =
  (match x with
  | `Extends_type x -> map_superclass env x
  | `Super_type (v1, v2) ->
      let v1 = token env v1 (* "super" *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  )

and map_program (env : env) (xs : CST.program) =
  List.map (map_statement env) xs

and map_constant_declaration (env : env) ((v1, v2, v3, v4) : CST.constant_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_variable_declarator_list env v3 in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

and map_enum_body (env : env) ((v1, v2, v3, v4, v5) : CST.enum_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_enum_constant env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_enum_constant env v2 in
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
  let v4 =
    (match v4 with
    | Some x -> map_enum_body_declarations env x
    | None -> todo env ())
  in
  let v5 = token env v5 (* "}" *) in
  todo env (v1, v2, v3, v4, v5)

and map_variable_declarator_id (env : env) ((v1, v2) : CST.variable_declarator_id) =
  let v1 = map_anon_choice_id env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_dimensions env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_assert_statement (env : env) (x : CST.assert_statement) =
  (match x with
  | `Assert_exp_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "assert" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Assert_exp_COLON_exp_SEMI (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "assert" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ":" *) in
      let v4 = map_expression env v4 in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  )

and map_resource (env : env) (x : CST.resource) =
  (match x with
  | `Opt_modifs_unan_type_var_decl_id_EQ_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_unannotated_type env v2 in
      let v3 = map_variable_declarator_id env v3 in
      let v4 = token env v4 (* "=" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Field_access x -> map_field_access env x
  )

and map_superclass (env : env) ((v1, v2) : CST.superclass) =
  let v1 = token env v1 (* "extends" *) in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_element_value (env : env) (x : CST.element_value) =
  (match x with
  | `Exp x -> map_expression env x
  | `Elem_value_array_init (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "{" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = map_element_value env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_element_value env v2 in
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
  | `Anno x -> map_annotation env x
  )

and map_variable_initializer (env : env) (x : CST.variable_initializer) =
  (match x with
  | `Exp x -> map_expression env x
  | `Array_init x -> map_array_initializer env x
  )

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = map_block env v2 in
  todo env (v1, v2)

and map_constructor_body (env : env) ((v1, v2, v3, v4) : CST.constructor_body) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_explicit_constructor_invocation env x
    | None -> todo env ())
  in
  let v3 = map_program env v3 in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_unqualified_object_creation_expression (env : env) ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) =
  let v1 = token env v1 (* "new" *) in
  let v2 =
    (match v2 with
    | Some x -> map_type_arguments env x
    | None -> todo env ())
  in
  let v3 = map_simple_type env v3 in
  let v4 = map_argument_list env v4 in
  let v5 =
    (match v5 with
    | Some x -> map_class_body env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_switch_block (env : env) ((v1, v2, v3) : CST.switch_block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Switch_label x -> map_switch_label env x
      | `Stmt x -> map_statement env x
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_default_value (env : env) ((v1, v2) : CST.default_value) =
  let v1 = token env v1 (* "default" *) in
  let v2 = map_element_value env v2 in
  todo env (v1, v2)

and map_method_declaration (env : env) ((v1, v2, v3) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_method_header env v2 in
  let v3 =
    (match v3 with
    | `Blk x -> map_block env x
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3)

and map_field_access (env : env) ((v1, v2, v3, v4) : CST.field_access) =
  let v1 = map_anon_choice_prim env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "." *) in
        let v2 = token env v2 (* "super" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "." *) in
  let v4 =
    (match v4 with
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | `Choice_open x -> map_reserved_identifier env x
    | `This tok -> token env tok (* "this" *)
    )
  in
  todo env (v1, v2, v3, v4)

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x -> map_anon_exp_rep_COMMA_exp env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Unan_type x -> map_unannotated_type env x
  | `Anno_type (v1, v2) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 = map_unannotated_type env v2 in
      todo env (v1, v2)
  )

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = token env v1 (* "<" *) in
  let v2 = map_type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

and map_super_interfaces (env : env) ((v1, v2) : CST.super_interfaces) =
  let v1 = token env v1 (* "implements" *) in
  let v2 = map_interface_type_list env v2 in
  todo env (v1, v2)

and map_dimensions_expr (env : env) ((v1, v2, v3, v4) : CST.dimensions_expr) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and map_explicit_constructor_invocation (env : env) ((v1, v2, v3) : CST.explicit_constructor_invocation) =
  let v1 =
    (match v1 with
    | `Opt_type_args_choice_this (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_type_arguments env x
          | None -> todo env ())
        in
        let v2 =
          (match v2 with
          | `This tok -> token env tok (* "this" *)
          | `Super tok -> token env tok (* "super" *)
          )
        in
        todo env (v1, v2)
    | `Choice_prim_DOT_opt_type_args_super (v1, v2, v3, v4) ->
        let v1 =
          (match v1 with
          | `Prim x -> map_primary env x
          )
        in
        let v2 = token env v2 (* "." *) in
        let v3 =
          (match v3 with
          | Some x -> map_type_arguments env x
          | None -> todo env ())
        in
        let v4 = token env v4 (* "super" *) in
        todo env (v1, v2, v3, v4)
    )
  in
  let v2 = map_argument_list env v2 in
  let v3 = token env v3 (* ";" *) in
  todo env (v1, v2, v3)

and map_constructor_declarator (env : env) ((v1, v2, v3) : CST.constructor_declarator) =
  let v1 =
    (match v1 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_formal_parameters env v3 in
  todo env (v1, v2, v3)

and map_type_bound (env : env) ((v1, v2, v3) : CST.type_bound) =
  let v1 = token env v1 (* "extends" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "&" *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_method_declarator (env : env) ((v1, v2, v3) : CST.method_declarator) =
  let v1 = map_anon_choice_id env v1 in
  let v2 = map_formal_parameters env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_dimensions env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Module_decl (v1, v2, v3, v4, v5) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "open" *)
        | None -> todo env ())
      in
      let v3 = token env v3 (* "module" *) in
      let v4 = map_name env v4 in
      let v5 = map_module_body env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Pack_decl (v1, v2, v3, v4) ->
      let v1 = List.map (map_annotation env) v1 in
      let v2 = token env v2 (* "package" *) in
      let v3 = map_name env v3 in
      let v4 = token env v4 (* ";" *) in
      todo env (v1, v2, v3, v4)
  | `Import_decl (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "import" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "static" *)
        | None -> todo env ())
      in
      let v3 = map_name env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "." *) in
            let v2 = token env v2 (* "*" *) in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Class_decl x -> map_class_declaration env x
  | `Inte_decl x -> map_interface_declaration env x
  | `Anno_type_decl x -> map_annotation_type_declaration env x
  | `Enum_decl x -> map_enum_declaration env x
  )

and map_resource_specification (env : env) ((v1, v2, v3, v4, v5) : CST.resource_specification) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_resource env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* ";" *) in
      let v2 = map_resource env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* ";" *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ")" *) in
  todo env (v1, v2, v3, v4, v5)

and map_enum_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.enum_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "enum" *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> map_super_interfaces env x
    | None -> todo env ())
  in
  let v5 = map_enum_body env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_element_value_pair (env : env) ((v1, v2, v3) : CST.element_value_pair) =
  let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 = token env v2 (* "=" *) in
  let v3 = map_element_value env v3 in
  todo env (v1, v2, v3)

and map_method_header (env : env) ((v1, v2, v3, v4) : CST.method_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_type_parameters env v1 in
        let v2 = List.map (map_annotation env) v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = map_unannotated_type env v2 in
  let v3 = map_method_declarator env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_throws env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)
