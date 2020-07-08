(**
   Boilerplate to be used as a template when mapping the java CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let token (_tok : Tree_sitter_run.Token.t) = failwith "not implemented"
let blank () = failwith "not implemented"
let todo _ = failwith "not implemented"

let map_reserved_identifier (x : CST.reserved_identifier) =
  (match x with
  | `Rese_id_open tok -> token tok (* "open" *)
  | `Rese_id_modu tok -> token tok (* "module" *)
  )

let map_floating_point_type (x : CST.floating_point_type) =
  (match x with
  | `Floa_point_type_float tok -> token tok (* "float" *)
  | `Floa_point_type_doub tok -> token tok (* "double" *)
  )

let map_octal_integer_literal (tok : CST.octal_integer_literal) =
  token tok (* octal_integer_literal *)

let map_binary_integer_literal (tok : CST.binary_integer_literal) =
  token tok (* binary_integer_literal *)

let map_hex_integer_literal (tok : CST.hex_integer_literal) =
  token tok (* hex_integer_literal *)

let map_integral_type (x : CST.integral_type) =
  (match x with
  | `Inte_type_byte tok -> token tok (* "byte" *)
  | `Inte_type_short tok -> token tok (* "short" *)
  | `Inte_type_int tok -> token tok (* "int" *)
  | `Inte_type_long tok -> token tok (* "long" *)
  | `Inte_type_char tok -> token tok (* "char" *)
  )

let map_decimal_floating_point_literal (tok : CST.decimal_floating_point_literal) =
  token tok (* decimal_floating_point_literal *)

let map_character_literal (tok : CST.character_literal) =
  token tok (* character_literal *)

let map_string_literal (tok : CST.string_literal) =
  token tok (* string_literal *)

let map_identifier (tok : CST.identifier) =
  token tok (* pattern [a-zA-Z_]\w* *)

let map_hex_floating_point_literal (tok : CST.hex_floating_point_literal) =
  token tok (* hex_floating_point_literal *)

let map_decimal_integer_literal (tok : CST.decimal_integer_literal) =
  token tok (* decimal_integer_literal *)

let map_requires_modifier (x : CST.requires_modifier) =
  (match x with
  | `Requis_modi_tran tok -> token tok (* "transitive" *)
  | `Requis_modi_stat tok -> token tok (* "static" *)
  )

let rec map_name (x : CST.name) =
  (match x with
  | `Name_id tok -> token tok (* pattern [a-zA-Z_]\w* *)
  | `Name_rese_id x -> map_reserved_identifier x
  | `Name_scop_id (v1, v2, v3) ->
      let v1 = map_name v1 in
      let v2 = token v2 (* "." *) in
      let v3 = token v3 (* pattern [a-zA-Z_]\w* *) in
      todo (v1, v2, v3)
  )

let map_inferred_parameters ((v1, v2, v3, v4) : CST.inferred_parameters) =
  let v1 = token v1 (* "(" *) in
  let v2 = token v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* "," *) in
      let v2 = token v2 (* pattern [a-zA-Z_]\w* *) in
      todo (v1, v2)
    ) v3
  in
  let v4 = token v4 (* ")" *) in
  todo (v1, v2, v3, v4)

let map_literal (x : CST.literal) =
  (match x with
  | `Lit_deci_int_lit tok ->
      token tok (* decimal_integer_literal *)
  | `Lit_hex_int_lit tok ->
      token tok (* hex_integer_literal *)
  | `Lit_octal_int_lit tok ->
      token tok (* octal_integer_literal *)
  | `Lit_bin_int_lit tok ->
      token tok (* binary_integer_literal *)
  | `Lit_deci_floa_point_lit tok ->
      token tok (* decimal_floating_point_literal *)
  | `Lit_hex_floa_point_lit tok ->
      token tok (* hex_floating_point_literal *)
  | `Lit_true tok -> token tok (* "true" *)
  | `Lit_false tok -> token tok (* "false" *)
  | `Lit_char_lit tok -> token tok (* character_literal *)
  | `Lit_str_lit tok -> token tok (* string_literal *)
  | `Lit_null_lit tok -> token tok (* "null" *)
  )

let map_module_directive ((v1, v2) : CST.module_directive) =
  let v1 =
    (match v1 with
    | `Requis_rep_requis_modi_name (v1, v2, v3) ->
        let v1 = token v1 (* "requires" *) in
        let v2 = List.map map_requires_modifier v2 in
        let v3 = map_name v3 in
        todo (v1, v2, v3)
    | `Expors_name_opt_to_opt_name_rep_COMMA_name (v1, v2, v3, v4, v5) ->
        let v1 = token v1 (* "exports" *) in
        let v2 = map_name v2 in
        let v3 =
          (match v3 with
          | Some tok -> token tok (* "to" *)
          | None -> todo ())
        in
        let v4 =
          (match v4 with
          | Some x -> map_name x
          | None -> todo ())
        in
        let v5 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 = map_name v2 in
            todo (v1, v2)
          ) v5
        in
        todo (v1, v2, v3, v4, v5)
    | `Opens_name_opt_to_opt_name_rep_COMMA_name (v1, v2, v3, v4, v5) ->
        let v1 = token v1 (* "opens" *) in
        let v2 = map_name v2 in
        let v3 =
          (match v3 with
          | Some tok -> token tok (* "to" *)
          | None -> todo ())
        in
        let v4 =
          (match v4 with
          | Some x -> map_name x
          | None -> todo ())
        in
        let v5 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 = map_name v2 in
            todo (v1, v2)
          ) v5
        in
        todo (v1, v2, v3, v4, v5)
    | `Uses_name (v1, v2) ->
        let v1 = token v1 (* "uses" *) in
        let v2 = map_name v2 in
        todo (v1, v2)
    | `Provis_name_with_name_rep_COMMA_name (v1, v2, v3, v4, v5) ->
        let v1 = token v1 (* "provides" *) in
        let v2 = map_name v2 in
        let v3 = token v3 (* "with" *) in
        let v4 = map_name v4 in
        let v5 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 = map_name v2 in
            todo (v1, v2)
          ) v5
        in
        todo (v1, v2, v3, v4, v5)
    )
  in
  let v2 = token v2 (* ";" *) in
  todo (v1, v2)

let map_module_body ((v1, v2, v3) : CST.module_body) =
  let v1 = token v1 (* "{" *) in
  let v2 = List.map map_module_directive v2 in
  let v3 = token v3 (* "}" *) in
  todo (v1, v2, v3)

let rec map_expression (x : CST.expression) =
  (match x with
  | `Exp_assign_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
        | `Rese_id x -> map_reserved_identifier x
        | `Field_acce x -> map_field_access x
        | `Array_acce x -> map_array_access x
        )
      in
      let v2 =
        (match v2 with
        | `EQ tok -> token tok (* "=" *)
        | `PLUSEQ tok -> token tok (* "+=" *)
        | `DASHEQ tok -> token tok (* "-=" *)
        | `STAREQ tok -> token tok (* "*=" *)
        | `SLASHEQ tok -> token tok (* "/=" *)
        | `AMPEQ tok -> token tok (* "&=" *)
        | `BAREQ tok -> token tok (* "|=" *)
        | `HATEQ tok -> token tok (* "^=" *)
        | `PERCEQ tok -> token tok (* "%=" *)
        | `LTLTEQ tok -> token tok (* "<<=" *)
        | `GTGTEQ tok -> token tok (* ">>=" *)
        | `GTGTGTEQ tok -> token tok (* ">>>=" *)
        )
      in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Exp_bin_exp x -> map_binary_expression x
  | `Exp_inst_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "instanceof" *) in
      let v3 = map_type_ v3 in
      todo (v1, v2, v3)
  | `Exp_lamb_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
        | `Form_params x -> map_formal_parameters x
        | `Infe_params x -> map_inferred_parameters x
        )
      in
      let v2 = token v2 (* "->" *) in
      let v3 =
        (match v3 with
        | `Exp x -> map_expression x
        | `Blk x -> map_block x
        )
      in
      todo (v1, v2, v3)
  | `Exp_tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "?" *) in
      let v3 = map_expression v3 in
      let v4 = token v4 (* ":" *) in
      let v5 = map_expression v5 in
      todo (v1, v2, v3, v4, v5)
  | `Exp_upda_exp x -> map_update_expression x
  | `Exp_prim x -> map_primary x
  | `Exp_un_exp x -> map_unary_expression x
  | `Exp_cast_exp (v1, v2, v3, v4, v5) ->
      let v1 = token v1 (* "(" *) in
      let v2 = map_type_ v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token v1 (* "&" *) in
          let v2 = map_type_ v2 in
          todo (v1, v2)
        ) v3
      in
      let v4 = token v4 (* ")" *) in
      let v5 = map_expression v5 in
      todo (v1, v2, v3, v4, v5)
  )

and map_binary_expression (x : CST.binary_expression) =
  (match x with
  | `Bin_exp_exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* ">" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "<" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "==" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* ">=" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "<=" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "!=" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "&&" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "||" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "+" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "-" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "*" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "/" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "&" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "|" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "^" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "%" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "<<" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* ">>" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Bin_exp_exp_GTGTGT_exp (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* ">>>" *) in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  )

and map_unary_expression (x : CST.unary_expression) =
  (match x with
  | `Un_exp_PLUS_exp (v1, v2) ->
      let v1 = token v1 (* "+" *) in
      let v2 = map_expression v2 in
      todo (v1, v2)
  | `Un_exp_DASH_exp (v1, v2) ->
      let v1 = token v1 (* "-" *) in
      let v2 = map_expression v2 in
      todo (v1, v2)
  | `Un_exp_BANG_exp (v1, v2) ->
      let v1 = token v1 (* "!" *) in
      let v2 = map_expression v2 in
      todo (v1, v2)
  | `Un_exp_TILDE_exp (v1, v2) ->
      let v1 = token v1 (* "~" *) in
      let v2 = map_expression v2 in
      todo (v1, v2)
  )

and map_update_expression (x : CST.update_expression) =
  (match x with
  | `Exp_PLUSPLUS (v1, v2) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "++" *) in
      todo (v1, v2)
  | `Exp_DASHDASH (v1, v2) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* "--" *) in
      todo (v1, v2)
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token v1 (* "++" *) in
      let v2 = map_expression v2 in
      todo (v1, v2)
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token v1 (* "--" *) in
      let v2 = map_expression v2 in
      todo (v1, v2)
  )

and map_primary (x : CST.primary) =
  (match x with
  | `Prim_lit x -> map_literal x
  | `Prim_class_lit (v1, v2, v3) ->
      let v1 = map_unannotated_type v1 in
      let v2 = token v2 (* "." *) in
      let v3 = token v3 (* "class" *) in
      todo (v1, v2, v3)
  | `Prim_this tok -> token tok (* "this" *)
  | `Prim_id tok -> token tok (* pattern [a-zA-Z_]\w* *)
  | `Prim_rese_id x -> map_reserved_identifier x
  | `Prim_paren_exp x -> map_parenthesized_expression x
  | `Prim_obj_crea_exp x -> map_object_creation_expression x
  | `Prim_field_acce x -> map_field_access x
  | `Prim_array_acce x -> map_array_access x
  | `Prim_meth_invo (v1, v2) ->
      let v1 =
        (match v1 with
        | `Choice_id x ->
            (match x with
            | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
            | `Rese_id x -> map_reserved_identifier x
            )
        | `Choice_prim_DOT_opt_super_DOT_opt_type_args_choice_id (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | `Prim x -> map_primary x
              | `Super tok -> token tok (* "super" *)
              )
            in
            let v2 = token v2 (* "." *) in
            let v3 =
              (match v3 with
              | Some (v1, v2) ->
                  let v1 = token v1 (* "super" *) in
                  let v2 = token v2 (* "." *) in
                  todo (v1, v2)
              | None -> todo ())
            in
            let v4 =
              (match v4 with
              | Some x -> map_type_arguments x
              | None -> todo ())
            in
            let v5 =
              (match v5 with
              | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
              | `Rese_id x -> map_reserved_identifier x
              )
            in
            todo (v1, v2, v3, v4, v5)
        )
      in
      let v2 = map_argument_list v2 in
      todo (v1, v2)
  | `Prim_meth_ref (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | `Type x -> map_type_ x
        | `Prim x -> map_primary x
        | `Super tok -> token tok (* "super" *)
        )
      in
      let v2 = token v2 (* "::" *) in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments x
        | None -> todo ())
      in
      let v4 =
        (match v4 with
        | `New tok -> token tok (* "new" *)
        | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
        )
      in
      todo (v1, v2, v3, v4)
  | `Prim_array_crea_exp (v1, v2, v3) ->
      let v1 = token v1 (* "new" *) in
      let v2 = map_simple_type v2 in
      let v3 =
        (match v3 with
        | `Rep1_dimens_expr_opt_dimens (v1, v2) ->
            let v1 = List.map map_dimensions_expr v1 in
            let v2 =
              (match v2 with
              | Some x -> map_dimensions x
              | None -> todo ())
            in
            todo (v1, v2)
        | `Dimens_array_init (v1, v2) ->
            let v1 = map_dimensions v1 in
            let v2 = map_array_initializer v2 in
            todo (v1, v2)
        )
      in
      todo (v1, v2, v3)
  )

and map_dimensions_expr ((v1, v2, v3, v4) : CST.dimensions_expr) =
  let v1 = List.map map_annotation v1 in
  let v2 = token v2 (* "[" *) in
  let v3 = map_expression v3 in
  let v4 = token v4 (* "]" *) in
  todo (v1, v2, v3, v4)

and map_parenthesized_expression ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token v1 (* "(" *) in
  let v2 = map_expression v2 in
  let v3 = token v3 (* ")" *) in
  todo (v1, v2, v3)

and map_object_creation_expression (x : CST.object_creation_expression) =
  (match x with
  | `Obj_crea_exp_unqu_obj_crea_exp x ->
      map_unqualified_object_creation_expression x
  | `Obj_crea_exp_prim_DOT_unqu_obj_crea_exp (v1, v2, v3) ->
      let v1 = map_primary v1 in
      let v2 = token v2 (* "." *) in
      let v3 = map_unqualified_object_creation_expression v3 in
      todo (v1, v2, v3)
  )

and map_unqualified_object_creation_expression ((v1, v2, v3, v4, v5) : CST.unqualified_object_creation_expression) =
  let v1 = token v1 (* "new" *) in
  let v2 =
    (match v2 with
    | Some x -> map_type_arguments x
    | None -> todo ())
  in
  let v3 = map_simple_type v3 in
  let v4 = map_argument_list v4 in
  let v5 =
    (match v5 with
    | Some x -> map_class_body x
    | None -> todo ())
  in
  todo (v1, v2, v3, v4, v5)

and map_field_access ((v1, v2, v3, v4) : CST.field_access) =
  let v1 =
    (match v1 with
    | `Prim x -> map_primary x
    | `Super tok -> token tok (* "super" *)
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token v1 (* "." *) in
        let v2 = token v2 (* "super" *) in
        todo (v1, v2)
    | None -> todo ())
  in
  let v3 = token v3 (* "." *) in
  let v4 =
    (match v4 with
    | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
    | `Rese_id x -> map_reserved_identifier x
    | `This tok -> token tok (* "this" *)
    )
  in
  todo (v1, v2, v3, v4)

and map_array_access ((v1, v2, v3, v4) : CST.array_access) =
  let v1 = map_primary v1 in
  let v2 = token v2 (* "[" *) in
  let v3 = map_expression v3 in
  let v4 = token v4 (* "]" *) in
  todo (v1, v2, v3, v4)

and map_argument_list ((v1, v2, v3) : CST.argument_list) =
  let v1 = token v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_expression v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 = map_expression v2 in
            todo (v1, v2)
          ) v2
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v3 = token v3 (* ")" *) in
  todo (v1, v2, v3)

and map_type_arguments ((v1, v2, v3) : CST.type_arguments) =
  let v1 = token v1 (* "<" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | `Type x -> map_type_ x
          | `Wild x -> map_wildcard x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Type x -> map_type_ x
              | `Wild x -> map_wildcard x
              )
            in
            todo (v1, v2)
          ) v2
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v3 = token v3 (* ">" *) in
  todo (v1, v2, v3)

and map_wildcard ((v1, v2, v3) : CST.wildcard) =
  let v1 = List.map map_annotation v1 in
  let v2 = token v2 (* "?" *) in
  let v3 =
    (match v3 with
    | Some x -> map_wildcard_bounds x
    | None -> todo ())
  in
  todo (v1, v2, v3)

and map_wildcard_bounds (x : CST.wildcard_bounds) =
  (match x with
  | `Wild_bounds_extens_type (v1, v2) ->
      let v1 = token v1 (* "extends" *) in
      let v2 = map_type_ v2 in
      todo (v1, v2)
  | `Wild_bounds_super_type (v1, v2) ->
      let v1 = token v1 (* "super" *) in
      let v2 = map_type_ v2 in
      todo (v1, v2)
  )

and map_dimensions (xs : CST.dimensions) =
  List.map (fun (v1, v2, v3) ->
    let v1 = List.map map_annotation v1 in
    let v2 = token v2 (* "[" *) in
    let v3 = token v3 (* "]" *) in
    todo (v1, v2, v3)
  ) xs

and map_statement (x : CST.statement) =
  (match x with
  | `Stmt_decl x -> map_declaration x
  | `Stmt_exp_stmt (v1, v2) ->
      let v1 = map_expression v1 in
      let v2 = token v2 (* ";" *) in
      todo (v1, v2)
  | `Stmt_labe_stmt (v1, v2, v3) ->
      let v1 = token v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = token v2 (* ":" *) in
      let v3 = map_statement v3 in
      todo (v1, v2, v3)
  | `Stmt_if_stmt (v1, v2, v3, v4) ->
      let v1 = token v1 (* "if" *) in
      let v2 = map_parenthesized_expression v2 in
      let v3 = map_statement v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token v1 (* "else" *) in
            let v2 = map_statement v2 in
            todo (v1, v2)
        | None -> todo ())
      in
      todo (v1, v2, v3, v4)
  | `Stmt_while_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "while" *) in
      let v2 = map_parenthesized_expression v2 in
      let v3 = map_statement v3 in
      todo (v1, v2, v3)
  | `Stmt_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token v1 (* "for" *) in
      let v2 = token v2 (* "(" *) in
      let v3 =
        (match v3 with
        | `Local_var_decl x -> map_local_variable_declaration x
        | `Opt_exp_rep_COMMA_exp_SEMI (v1, v2) ->
            let v1 =
              (match v1 with
              | Some (v1, v2) ->
                  let v1 = map_expression v1 in
                  let v2 =
                    List.map (fun (v1, v2) ->
                      let v1 = token v1 (* "," *) in
                      let v2 = map_expression v2 in
                      todo (v1, v2)
                    ) v2
                  in
                  todo (v1, v2)
              | None -> todo ())
            in
            let v2 = token v2 (* ";" *) in
            todo (v1, v2)
        )
      in
      let v4 =
        (match v4 with
        | Some x -> map_expression x
        | None -> todo ())
      in
      let v5 = token v5 (* ";" *) in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = map_expression v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token v1 (* "," *) in
                let v2 = map_expression v2 in
                todo (v1, v2)
              ) v2
            in
            todo (v1, v2)
        | None -> todo ())
      in
      let v7 = token v7 (* ")" *) in
      let v8 = map_statement v8 in
      todo (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Stmt_enha_for_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token v1 (* "for" *) in
      let v2 = token v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some x -> map_modifiers x
        | None -> todo ())
      in
      let v4 = map_unannotated_type v4 in
      let v5 = map_variable_declarator_id v5 in
      let v6 = token v6 (* ":" *) in
      let v7 = map_expression v7 in
      let v8 = token v8 (* ")" *) in
      let v9 = map_statement v9 in
      todo (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Stmt_blk x -> map_block x
  | `Stmt_SEMI tok -> token tok (* ";" *)
  | `Stmt_asse_stmt x -> map_assert_statement x
  | `Stmt_swit_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "switch" *) in
      let v2 = map_parenthesized_expression v2 in
      let v3 = map_switch_block v3 in
      todo (v1, v2, v3)
  | `Stmt_do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token v1 (* "do" *) in
      let v2 = map_statement v2 in
      let v3 = token v3 (* "while" *) in
      let v4 = map_parenthesized_expression v4 in
      let v5 = token v5 (* ";" *) in
      todo (v1, v2, v3, v4, v5)
  | `Stmt_brk_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some tok -> token tok (* pattern [a-zA-Z_]\w* *)
        | None -> todo ())
      in
      let v3 = token v3 (* ";" *) in
      todo (v1, v2, v3)
  | `Stmt_cont_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some tok -> token tok (* pattern [a-zA-Z_]\w* *)
        | None -> todo ())
      in
      let v3 = token v3 (* ";" *) in
      todo (v1, v2, v3)
  | `Stmt_ret_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expression x
        | None -> todo ())
      in
      let v3 = token v3 (* ";" *) in
      todo (v1, v2, v3)
  | `Stmt_sync_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "synchronized" *) in
      let v2 = map_parenthesized_expression v2 in
      let v3 = map_block v3 in
      todo (v1, v2, v3)
  | `Stmt_local_var_decl x -> map_local_variable_declaration x
  | `Stmt_throw_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "throw" *) in
      let v2 = map_expression v2 in
      let v3 = token v3 (* ";" *) in
      todo (v1, v2, v3)
  | `Stmt_try_stmt (v1, v2, v3) ->
      let v1 = token v1 (* "try" *) in
      let v2 = map_block v2 in
      let v3 =
        (match v3 with
        | `Rep1_catch_clau xs -> List.map map_catch_clause xs
        | `Rep_catch_clau_fina_clau (v1, v2) ->
            let v1 = List.map map_catch_clause v1 in
            let v2 = map_finally_clause v2 in
            todo (v1, v2)
        )
      in
      todo (v1, v2, v3)
  | `Stmt_try_with_resous_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token v1 (* "try" *) in
      let v2 = map_resource_specification v2 in
      let v3 = map_block v3 in
      let v4 = List.map map_catch_clause v4 in
      let v5 =
        (match v5 with
        | Some x -> map_finally_clause x
        | None -> todo ())
      in
      todo (v1, v2, v3, v4, v5)
  )

and map_block ((v1, v2, v3) : CST.block) =
  let v1 = token v1 (* "{" *) in
  let v2 = List.map map_statement v2 in
  let v3 = token v3 (* "}" *) in
  todo (v1, v2, v3)

and map_assert_statement (x : CST.assert_statement) =
  (match x with
  | `Asse_stmt_asse_exp_SEMI (v1, v2, v3) ->
      let v1 = token v1 (* "assert" *) in
      let v2 = map_expression v2 in
      let v3 = token v3 (* ";" *) in
      todo (v1, v2, v3)
  | `Asse_stmt_asse_exp_COLON_exp_SEMI (v1, v2, v3, v4, v5) ->
      let v1 = token v1 (* "assert" *) in
      let v2 = map_expression v2 in
      let v3 = token v3 (* ":" *) in
      let v4 = map_expression v4 in
      let v5 = token v5 (* ";" *) in
      todo (v1, v2, v3, v4, v5)
  )

and map_switch_block ((v1, v2, v3) : CST.switch_block) =
  let v1 = token v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Swit_label x -> map_switch_label x
      | `Stmt x -> map_statement x
      )
    ) v2
  in
  let v3 = token v3 (* "}" *) in
  todo (v1, v2, v3)

and map_switch_label (x : CST.switch_label) =
  (match x with
  | `Swit_label_case_exp_COLON (v1, v2, v3) ->
      let v1 = token v1 (* "case" *) in
      let v2 = map_expression v2 in
      let v3 = token v3 (* ":" *) in
      todo (v1, v2, v3)
  | `Swit_label_defa_COLON (v1, v2) ->
      let v1 = token v1 (* "default" *) in
      let v2 = token v2 (* ":" *) in
      todo (v1, v2)
  )

and map_catch_clause ((v1, v2, v3, v4, v5) : CST.catch_clause) =
  let v1 = token v1 (* "catch" *) in
  let v2 = token v2 (* "(" *) in
  let v3 = map_catch_formal_parameter v3 in
  let v4 = token v4 (* ")" *) in
  let v5 = map_block v5 in
  todo (v1, v2, v3, v4, v5)

and map_catch_formal_parameter ((v1, v2, v3) : CST.catch_formal_parameter) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = map_catch_type v2 in
  let v3 = map_variable_declarator_id v3 in
  todo (v1, v2, v3)

and map_catch_type ((v1, v2) : CST.catch_type) =
  let v1 = map_unannotated_type v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* "|" *) in
      let v2 = map_unannotated_type v2 in
      todo (v1, v2)
    ) v2
  in
  todo (v1, v2)

and map_finally_clause ((v1, v2) : CST.finally_clause) =
  let v1 = token v1 (* "finally" *) in
  let v2 = map_block v2 in
  todo (v1, v2)

and map_resource_specification ((v1, v2, v3, v4, v5) : CST.resource_specification) =
  let v1 = token v1 (* "(" *) in
  let v2 = map_resource v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* ";" *) in
      let v2 = map_resource v2 in
      todo (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token tok (* ";" *)
    | None -> todo ())
  in
  let v5 = token v5 (* ")" *) in
  todo (v1, v2, v3, v4, v5)

and map_resource (x : CST.resource) =
  (match x with
  | `Reso_opt_modifs_unan_type_var_decl_id_EQ_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers x
        | None -> todo ())
      in
      let v2 = map_unannotated_type v2 in
      let v3 = map_variable_declarator_id v3 in
      let v4 = token v4 (* "=" *) in
      let v5 = map_expression v5 in
      todo (v1, v2, v3, v4, v5)
  | `Reso_id tok -> token tok (* pattern [a-zA-Z_]\w* *)
  | `Reso_field_acce x -> map_field_access x
  )

and map_annotation ((v1, v2, v3) : CST.annotation) =
  let v1 = token v1 (* "@" *) in
  let v2 = map_name v2 in
  let v3 = map_annotation_argument_list v3 in
  todo (v1, v2, v3)

and map_annotation_argument_list ((v1, v2, v3) : CST.annotation_argument_list) =
  let v1 = token v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Elem_value x -> map_element_value x
    | `Opt_elem_value_pair_rep_COMMA_elem_value_pair opt ->
        (match opt with
        | Some (v1, v2) ->
            let v1 = map_element_value_pair v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token v1 (* "," *) in
                let v2 = map_element_value_pair v2 in
                todo (v1, v2)
              ) v2
            in
            todo (v1, v2)
        | None -> todo ())
    )
  in
  let v3 = token v3 (* ")" *) in
  todo (v1, v2, v3)

and map_element_value_pair ((v1, v2, v3) : CST.element_value_pair) =
  let v1 = token v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 = token v2 (* "=" *) in
  let v3 = map_element_value v3 in
  todo (v1, v2, v3)

and map_element_value (x : CST.element_value) =
  (match x with
  | `Exp x -> map_expression x
  | `Elem_value_array_init (v1, v2, v3, v4) ->
      let v1 = token v1 (* "{" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = map_element_value v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token v1 (* "," *) in
                let v2 = map_element_value v2 in
                todo (v1, v2)
              ) v2
            in
            todo (v1, v2)
        | None -> todo ())
      in
      let v3 =
        (match v3 with
        | Some tok -> token tok (* "," *)
        | None -> todo ())
      in
      let v4 = token v4 (* "}" *) in
      todo (v1, v2, v3, v4)
  | `Anno x -> map_annotation x
  )

and map_declaration (x : CST.declaration) =
  (match x with
  | `Modu_decl (v1, v2, v3, v4, v5) ->
      let v1 = List.map map_annotation v1 in
      let v2 =
        (match v2 with
        | Some tok -> token tok (* "open" *)
        | None -> todo ())
      in
      let v3 = token v3 (* "module" *) in
      let v4 = map_name v4 in
      let v5 = map_module_body v5 in
      todo (v1, v2, v3, v4, v5)
  | `Pack_decl (v1, v2, v3, v4) ->
      let v1 = List.map map_annotation v1 in
      let v2 = token v2 (* "package" *) in
      let v3 = map_name v3 in
      let v4 = token v4 (* ";" *) in
      todo (v1, v2, v3, v4)
  | `Impo_decl (v1, v2, v3, v4, v5) ->
      let v1 = token v1 (* "import" *) in
      let v2 =
        (match v2 with
        | Some tok -> token tok (* "static" *)
        | None -> todo ())
      in
      let v3 = map_name v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token v1 (* "." *) in
            let v2 = token v2 (* "*" *) in
            todo (v1, v2)
        | None -> todo ())
      in
      let v5 = token v5 (* ";" *) in
      todo (v1, v2, v3, v4, v5)
  | `Class_decl x -> map_class_declaration x
  | `Inte_decl x -> map_interface_declaration x
  | `Anno_type_decl x -> map_annotation_type_declaration x
  | `Enum_decl x -> map_enum_declaration x
  )

and map_enum_declaration ((v1, v2, v3, v4, v5) : CST.enum_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = token v2 (* "enum" *) in
  let v3 = token v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> map_super_interfaces x
    | None -> todo ())
  in
  let v5 = map_enum_body v5 in
  todo (v1, v2, v3, v4, v5)

and map_enum_body ((v1, v2, v3, v4, v5) : CST.enum_body) =
  let v1 = token v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_enum_constant v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 = map_enum_constant v2 in
            todo (v1, v2)
          ) v2
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token tok (* "," *)
    | None -> todo ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_enum_body_declarations x
    | None -> todo ())
  in
  let v5 = token v5 (* "}" *) in
  todo (v1, v2, v3, v4, v5)

and map_enum_body_declarations ((v1, v2) : CST.enum_body_declarations) =
  let v1 = token v1 (* ";" *) in
  let v2 = List.map map_class_body_declaration v2 in
  todo (v1, v2)

and map_enum_constant ((v1, v2, v3, v4) : CST.enum_constant) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = token v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some x -> map_argument_list x
    | None -> todo ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_class_body x
    | None -> todo ())
  in
  todo (v1, v2, v3, v4)

and map_class_declaration ((v1, v2, v3, v4, v5, v6, v7) : CST.class_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = token v2 (* "class" *) in
  let v3 = token v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters x
    | None -> todo ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_superclass x
    | None -> todo ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_super_interfaces x
    | None -> todo ())
  in
  let v7 = map_class_body v7 in
  todo (v1, v2, v3, v4, v5, v6, v7)

and map_modifiers (xs : CST.modifiers) =
  List.map (fun x ->
    (match x with
    | `Anno x -> map_annotation x
    | `Publ tok -> token tok (* "public" *)
    | `Prot tok -> token tok (* "protected" *)
    | `Priv tok -> token tok (* "private" *)
    | `Abst tok -> token tok (* "abstract" *)
    | `Stat tok -> token tok (* "static" *)
    | `Final tok -> token tok (* "final" *)
    | `Stri tok -> token tok (* "strictfp" *)
    | `Defa tok -> token tok (* "default" *)
    | `Sync tok -> token tok (* "synchronized" *)
    | `Nati tok -> token tok (* "native" *)
    | `Tran tok -> token tok (* "transient" *)
    | `Vola tok -> token tok (* "volatile" *)
    )
  ) xs

and map_type_parameters ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = token v1 (* "<" *) in
  let v2 = map_type_parameter v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* "," *) in
      let v2 = map_type_parameter v2 in
      todo (v1, v2)
    ) v3
  in
  let v4 = token v4 (* ">" *) in
  todo (v1, v2, v3, v4)

and map_type_parameter ((v1, v2, v3) : CST.type_parameter) =
  let v1 = List.map map_annotation v1 in
  let v2 = token v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some x -> map_type_bound x
    | None -> todo ())
  in
  todo (v1, v2, v3)

and map_type_bound ((v1, v2, v3) : CST.type_bound) =
  let v1 = token v1 (* "extends" *) in
  let v2 = map_type_ v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* "&" *) in
      let v2 = map_type_ v2 in
      todo (v1, v2)
    ) v3
  in
  todo (v1, v2, v3)

and map_superclass ((v1, v2) : CST.superclass) =
  let v1 = token v1 (* "extends" *) in
  let v2 = map_type_ v2 in
  todo (v1, v2)

and map_super_interfaces ((v1, v2) : CST.super_interfaces) =
  let v1 = token v1 (* "implements" *) in
  let v2 = map_interface_type_list v2 in
  todo (v1, v2)

and map_interface_type_list ((v1, v2) : CST.interface_type_list) =
  let v1 = map_type_ v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* "," *) in
      let v2 = map_type_ v2 in
      todo (v1, v2)
    ) v2
  in
  todo (v1, v2)

and map_class_body ((v1, v2, v3) : CST.class_body) =
  let v1 = token v1 (* "{" *) in
  let v2 = List.map map_class_body_declaration v2 in
  let v3 = token v3 (* "}" *) in
  todo (v1, v2, v3)

and map_class_body_declaration (x : CST.class_body_declaration) =
  (match x with
  | `Class_body_decl_field_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers x
        | None -> todo ())
      in
      let v2 = map_unannotated_type v2 in
      let v3 = map_variable_declarator_list v3 in
      let v4 = token v4 (* ";" *) in
      todo (v1, v2, v3, v4)
  | `Class_body_decl_meth_decl x -> map_method_declaration x
  | `Class_body_decl_class_decl x -> map_class_declaration x
  | `Class_body_decl_inte_decl x ->
      map_interface_declaration x
  | `Class_body_decl_anno_type_decl x ->
      map_annotation_type_declaration x
  | `Class_body_decl_enum_decl x -> map_enum_declaration x
  | `Class_body_decl_blk x -> map_block x
  | `Class_body_decl_stat_init (v1, v2) ->
      let v1 = token v1 (* "static" *) in
      let v2 = map_block v2 in
      todo (v1, v2)
  | `Class_body_decl_cons_decl (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers x
        | None -> todo ())
      in
      let v2 = map_constructor_declarator v2 in
      let v3 =
        (match v3 with
        | Some x -> map_throws x
        | None -> todo ())
      in
      let v4 = map_constructor_body v4 in
      todo (v1, v2, v3, v4)
  | `Class_body_decl_SEMI tok -> token tok (* ";" *)
  )

and map_constructor_declarator ((v1, v2, v3) : CST.constructor_declarator) =
  let v1 =
    (match v1 with
    | Some x -> map_type_parameters x
    | None -> todo ())
  in
  let v2 = token v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_formal_parameters v3 in
  todo (v1, v2, v3)

and map_constructor_body ((v1, v2, v3, v4) : CST.constructor_body) =
  let v1 = token v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_explicit_constructor_invocation x
    | None -> todo ())
  in
  let v3 = List.map map_statement v3 in
  let v4 = token v4 (* "}" *) in
  todo (v1, v2, v3, v4)

and map_explicit_constructor_invocation ((v1, v2, v3) : CST.explicit_constructor_invocation) =
  let v1 =
    (match v1 with
    | `Opt_type_args_choice_this (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_type_arguments x
          | None -> todo ())
        in
        let v2 =
          (match v2 with
          | `This tok -> token tok (* "this" *)
          | `Super tok -> token tok (* "super" *)
          )
        in
        todo (v1, v2)
    | `Choice_prim_DOT_opt_type_args_super (v1, v2, v3, v4) ->
        let v1 = (match v1 with| `Prim x -> map_primary x) in
        let v2 = token v2 (* "." *) in
        let v3 =
          (match v3 with
          | Some x -> map_type_arguments x
          | None -> todo ())
        in
        let v4 = token v4 (* "super" *) in
        todo (v1, v2, v3, v4)
    )
  in
  let v2 = map_argument_list v2 in
  let v3 = token v3 (* ";" *) in
  todo (v1, v2, v3)

and map_annotation_type_declaration ((v1, v2, v3, v4) : CST.annotation_type_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = token v2 (* "@interface" *) in
  let v3 = token v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = map_annotation_type_body v4 in
  todo (v1, v2, v3, v4)

and map_annotation_type_body ((v1, v2, v3) : CST.annotation_type_body) =
  let v1 = token v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Anno_type_elem_decl x ->
          map_annotation_type_element_declaration x
      | `Cst_decl x -> map_constant_declaration x
      | `Class_decl x -> map_class_declaration x
      | `Inte_decl x -> map_interface_declaration x
      | `Anno_type_decl x -> map_annotation_type_declaration x
      )
    ) v2
  in
  let v3 = token v3 (* "}" *) in
  todo (v1, v2, v3)

and map_annotation_type_element_declaration ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.annotation_type_element_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = map_unannotated_type v2 in
  let v3 = token v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = token v4 (* "(" *) in
  let v5 = token v5 (* ")" *) in
  let v6 =
    (match v6 with
    | Some x -> map_dimensions x
    | None -> todo ())
  in
  let v7 =
    (match v7 with
    | Some x -> map_default_value x
    | None -> todo ())
  in
  let v8 = token v8 (* ";" *) in
  todo (v1, v2, v3, v4, v5, v6, v7, v8)

and map_default_value ((v1, v2) : CST.default_value) =
  let v1 = token v1 (* "default" *) in
  let v2 = map_element_value v2 in
  todo (v1, v2)

and map_interface_declaration ((v1, v2, v3, v4, v5, v6) : CST.interface_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = token v2 (* "interface" *) in
  let v3 = token v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters x
    | None -> todo ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_extends_interfaces x
    | None -> todo ())
  in
  let v6 = map_interface_body v6 in
  todo (v1, v2, v3, v4, v5, v6)

and map_extends_interfaces ((v1, v2) : CST.extends_interfaces) =
  let v1 = token v1 (* "extends" *) in
  let v2 = map_interface_type_list v2 in
  todo (v1, v2)

and map_interface_body ((v1, v2, v3) : CST.interface_body) =
  let v1 = token v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Cst_decl x -> map_constant_declaration x
      | `Enum_decl x -> map_enum_declaration x
      | `Meth_decl x -> map_method_declaration x
      | `Class_decl x -> map_class_declaration x
      | `Inte_decl x -> map_interface_declaration x
      | `Anno_type_decl x -> map_annotation_type_declaration x
      | `SEMI tok -> token tok (* ";" *)
      )
    ) v2
  in
  let v3 = token v3 (* "}" *) in
  todo (v1, v2, v3)

and map_constant_declaration ((v1, v2, v3, v4) : CST.constant_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = map_unannotated_type v2 in
  let v3 = map_variable_declarator_list v3 in
  let v4 = token v4 (* ";" *) in
  todo (v1, v2, v3, v4)

and map_variable_declarator_list ((v1, v2) : CST.variable_declarator_list) =
  let v1 = map_variable_declarator v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* "," *) in
      let v2 = map_variable_declarator v2 in
      todo (v1, v2)
    ) v2
  in
  todo (v1, v2)

and map_variable_declarator ((v1, v2) : CST.variable_declarator) =
  let v1 = map_variable_declarator_id v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token v1 (* "=" *) in
        let v2 = map_variable_initializer v2 in
        todo (v1, v2)
    | None -> todo ())
  in
  todo (v1, v2)

and map_variable_declarator_id ((v1, v2) : CST.variable_declarator_id) =
  let v1 =
    (match v1 with
    | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
    | `Rese_id x -> map_reserved_identifier x
    )
  in
  let v2 =
    (match v2 with
    | Some x -> map_dimensions x
    | None -> todo ())
  in
  todo (v1, v2)

and map_variable_initializer (x : CST.variable_initializer) =
  (match x with
  | `Var_init_exp x -> map_expression x
  | `Var_init_array_init x -> map_array_initializer x
  )

and map_array_initializer ((v1, v2, v3, v4) : CST.array_initializer) =
  let v1 = token v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_variable_initializer v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 = map_variable_initializer v2 in
            todo (v1, v2)
          ) v2
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token tok (* "," *)
    | None -> todo ())
  in
  let v4 = token v4 (* "}" *) in
  todo (v1, v2, v3, v4)

and map_type_ (x : CST.type_) =
  (match x with
  | `Type_unan_type x -> map_unannotated_type x
  | `Type_anno_type (v1, v2) ->
      let v1 = List.map map_annotation v1 in
      let v2 = map_unannotated_type v2 in
      todo (v1, v2)
  )

and map_unannotated_type (x : CST.unannotated_type) =
  (match x with
  | `Unan_type_simple_type x -> map_simple_type x
  | `Unan_type_array_type (v1, v2) ->
      let v1 = map_unannotated_type v1 in
      let v2 = map_dimensions v2 in
      todo (v1, v2)
  )

and map_simple_type (x : CST.simple_type) =
  (match x with
  | `Simple_type_void_type tok -> token tok (* "void" *)
  | `Simple_type_inte_type x -> map_integral_type x
  | `Simple_type_floa_point_type x ->
      map_floating_point_type x
  | `Simple_type_bool_type tok -> token tok (* "boolean" *)
  | `Simple_type_id tok ->
      token tok (* pattern [a-zA-Z_]\w* *)
  | `Simple_type_scop_type_id x ->
      map_scoped_type_identifier x
  | `Simple_type_gene_type x -> map_generic_type x
  )

and map_scoped_type_identifier ((v1, v2, v3, v4) : CST.scoped_type_identifier) =
  let v1 =
    (match v1 with
    | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
    | `Scop_type_id x -> map_scoped_type_identifier x
    | `Gene_type x -> map_generic_type x
    )
  in
  let v2 = token v2 (* "." *) in
  let v3 = List.map map_annotation v3 in
  let v4 = token v4 (* pattern [a-zA-Z_]\w* *) in
  todo (v1, v2, v3, v4)

and map_generic_type ((v1, v2) : CST.generic_type) =
  let v1 =
    (match v1 with
    | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
    | `Scop_type_id x -> map_scoped_type_identifier x
    )
  in
  let v2 = map_type_arguments v2 in
  todo (v1, v2)

and map_method_header ((v1, v2, v3, v4) : CST.method_header) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_type_parameters v1 in
        let v2 = List.map map_annotation v2 in
        todo (v1, v2)
    | None -> todo ())
  in
  let v2 = map_unannotated_type v2 in
  let v3 = map_method_declarator v3 in
  let v4 =
    (match v4 with
    | Some x -> map_throws x
    | None -> todo ())
  in
  todo (v1, v2, v3, v4)

and map_method_declarator ((v1, v2, v3) : CST.method_declarator) =
  let v1 =
    (match v1 with
    | `Id tok -> token tok (* pattern [a-zA-Z_]\w* *)
    | `Rese_id x -> map_reserved_identifier x
    )
  in
  let v2 = map_formal_parameters v2 in
  let v3 =
    (match v3 with
    | Some x -> map_dimensions x
    | None -> todo ())
  in
  todo (v1, v2, v3)

and map_formal_parameters ((v1, v2, v3, v4) : CST.formal_parameters) =
  let v1 = token v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x -> map_receiver_parameter x
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | `Form_param x -> map_formal_parameter x
          | `Spre_param x -> map_spread_parameter x
          )
        in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 (* "," *) in
            let v2 =
              (match v2 with
              | `Form_param x -> map_formal_parameter x
              | `Spre_param x -> map_spread_parameter x
              )
            in
            todo (v1, v2)
          ) v2
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v4 = token v4 (* ")" *) in
  todo (v1, v2, v3, v4)

and map_formal_parameter ((v1, v2, v3) : CST.formal_parameter) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = map_unannotated_type v2 in
  let v3 = map_variable_declarator_id v3 in
  todo (v1, v2, v3)

and map_receiver_parameter ((v1, v2, v3, v4) : CST.receiver_parameter) =
  let v1 = List.map map_annotation v1 in
  let v2 = map_unannotated_type v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token v1 (* pattern [a-zA-Z_]\w* *) in
        let v2 = token v2 (* "." *) in
        todo (v1, v2)
    | None -> todo ())
  in
  let v4 = token v4 (* "this" *) in
  todo (v1, v2, v3, v4)

and map_spread_parameter ((v1, v2, v3, v4) : CST.spread_parameter) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = map_unannotated_type v2 in
  let v3 = token v3 (* "..." *) in
  let v4 = map_variable_declarator v4 in
  todo (v1, v2, v3, v4)

and map_throws ((v1, v2, v3) : CST.throws) =
  let v1 = token v1 (* "throws" *) in
  let v2 = map_type_ v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 (* "," *) in
      let v2 = map_type_ v2 in
      todo (v1, v2)
    ) v3
  in
  todo (v1, v2, v3)

and map_local_variable_declaration ((v1, v2, v3, v4) : CST.local_variable_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = map_unannotated_type v2 in
  let v3 = map_variable_declarator_list v3 in
  let v4 = token v4 (* ";" *) in
  todo (v1, v2, v3, v4)

and map_method_declaration ((v1, v2, v3) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers x
    | None -> todo ())
  in
  let v2 = map_method_header v2 in
  let v3 =
    (match v3 with
    | `Blk x -> map_block x
    | `SEMI tok -> token tok (* ";" *)
    )
  in
  todo (v1, v2, v3)

let map_program (xs : CST.program) =
  List.map map_statement xs

