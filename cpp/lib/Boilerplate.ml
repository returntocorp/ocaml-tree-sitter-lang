(**
   Boilerplate to be used as a template when mapping the cpp CST
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

let map_type_qualifier (env : env) (x : CST.type_qualifier) =
  (match x with
  | `Choice_const x ->
      (match x with
      | `Const tok -> token env tok (* "const" *)
      | `Vola tok -> token env tok (* "volatile" *)
      | `Rest tok -> token env tok (* "restrict" *)
      | `X__Atomic tok -> token env tok (* "_Atomic" *)
      )
  | `Muta tok -> token env tok (* "mutable" *)
  | `Cons tok -> token env tok (* "constexpr" *)
  )

let map_system_lib_string (env : env) (tok : CST.system_lib_string) =
  token env tok (* system_lib_string *)

let map_number_literal (env : env) (tok : CST.number_literal) =
  token env tok (* number_literal *)

let map_anon_choice_AMP (env : env) (x : CST.anon_choice_AMP) =
  (match x with
  | `AMP tok -> token env tok (* "&" *)
  | `AMPAMP tok -> token env tok (* "&&" *)
  )

let map_anon_choice_public (env : env) (x : CST.anon_choice_public) =
  (match x with
  | `Public tok -> token env tok (* "public" *)
  | `Priv tok -> token env tok (* "private" *)
  | `Prot tok -> token env tok (* "protected" *)
  )

let map_ms_unaligned_ptr_modifier (env : env) (x : CST.ms_unaligned_ptr_modifier) =
  (match x with
  | `X__unal tok -> token env tok (* "_unaligned" *)
  | `X___unal tok -> token env tok (* "__unaligned" *)
  )

let map_anon_choice_DASHDASH (env : env) (x : CST.anon_choice_DASHDASH) =
  (match x with
  | `DASHDASH tok -> token env tok (* "--" *)
  | `PLUSPLUS tok -> token env tok (* "++" *)
  )

let map_ms_call_modifier (env : env) (x : CST.ms_call_modifier) =
  (match x with
  | `X___cdecl tok -> token env tok (* "__cdecl" *)
  | `X___clrc tok -> token env tok (* "__clrcall" *)
  | `X___stdc tok -> token env tok (* "__stdcall" *)
  | `X___fast tok -> token env tok (* "__fastcall" *)
  | `X___this tok -> token env tok (* "__thiscall" *)
  | `X___vect tok -> token env tok (* "__vectorcall" *)
  )

let map_operator_name (env : env) (tok : CST.operator_name) =
  token env tok (* operator_name *)

let map_preproc_directive (env : env) (tok : CST.preproc_directive) =
  token env tok (* pattern #[ \t]*[a-zA-Z]\w* *)

let map_anon_choice_DOT (env : env) (x : CST.anon_choice_DOT) =
  (match x with
  | `DOT tok -> token env tok (* "." *)
  | `DASHGT tok -> token env tok (* "->" *)
  )

let map_virtual_specifier (env : env) (x : CST.virtual_specifier) =
  (match x with
  | `Final tok -> token env tok (* "final" *)
  | `Over tok -> token env tok (* "override" *)
  )

let map_false_ (env : env) (tok : CST.false_) =
  token env tok (* false *)

let map_preproc_arg (env : env) (tok : CST.preproc_arg) =
  token env tok (* preproc_arg *)

let map_anon_choice_type (env : env) (x : CST.anon_choice_type) =
  (match x with
  | `Type tok -> token env tok (* "typename" *)
  | `Class tok -> token env tok (* "class" *)
  )

let map_anon_choice_BANG (env : env) (x : CST.anon_choice_BANG) =
  (match x with
  | `BANG tok -> token env tok (* "!" *)
  | `TILDE tok -> token env tok (* "~" *)
  | `DASH tok -> token env tok (* "-" *)
  | `PLUS tok -> token env tok (* "+" *)
  )

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  token env tok (* raw_string_literal *)

let map_true_ (env : env) (tok : CST.true_) =
  token env tok (* true *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* pattern [a-zA-Z_]\w* *)

let map_lambda_default_capture (env : env) (x : CST.lambda_default_capture) =
  (match x with
  | `EQ tok -> token env tok (* "=" *)
  | `AMP tok -> token env tok (* "&" *)
  )

let map_storage_class_specifier (env : env) (x : CST.storage_class_specifier) =
  (match x with
  | `Extern tok -> token env tok (* "extern" *)
  | `Static tok -> token env tok (* "static" *)
  | `Regi tok -> token env tok (* "register" *)
  | `Inline tok -> token env tok (* "inline" *)
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  token env tok (* escape_sequence *)

let map_virtual_function_specifier (env : env) (x : CST.virtual_function_specifier) =
  (match x with
  | `Virt tok -> token env tok (* "virtual" *)
  )

let map_primitive_type (env : env) (tok : CST.primitive_type) =
  token env tok (* primitive_type *)

let map_ms_pointer_modifier (env : env) (x : CST.ms_pointer_modifier) =
  (match x with
  | `Ms_unal_ptr_modi x -> map_ms_unaligned_ptr_modifier env x
  | `Ms_rest_modi tok -> token env tok (* "__restrict" *)
  | `Ms_unsi_ptr_modi tok -> token env tok (* "__uptr" *)
  | `Ms_signed_ptr_modi tok -> token env tok (* "__sptr" *)
  )

let map_preproc_call (env : env) ((v1, v2, v3) : CST.preproc_call) =
  let v1 = token env v1 (* pattern #[ \t]*[a-zA-Z]\w* *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* preproc_arg *)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "\n" *) in
  todo env (v1, v2, v3)

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 =
    (match v1 with
    | `LDQUOT tok -> token env tok (* "L\"" *)
    | `UDQUOT_c163aae tok -> token env tok (* "u\"" *)
    | `UDQUOT_df3447d tok -> token env tok (* "U\"" *)
    | `U8DQUOT tok -> token env tok (* "u8\"" *)
    | `DQUOT tok -> token env tok (* "\"" *)
    )
  in
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

let map_char_literal (env : env) ((v1, v2, v3) : CST.char_literal) =
  let v1 =
    (match v1 with
    | `LSQUOT tok -> token env tok (* "L'" *)
    | `USQUOT_d861d39 tok -> token env tok (* "u'" *)
    | `USQUOT_2701bdc tok -> token env tok (* "U'" *)
    | `U8SQUOT tok -> token env tok (* "u8'" *)
    | `SQUOT tok -> token env tok (* "'" *)
    )
  in
  let v2 =
    (match v2 with
    | `Esc_seq tok -> token env tok (* escape_sequence *)
    | `Blank () -> todo env ()
    )
  in
  let v3 = token env v3 (* "'" *) in
  todo env (v1, v2, v3)

let map_preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) =
  let v1 = blank env v1 in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* preproc_arg *)
    | None -> todo env ())
  in
  let v4 = token env v4 (* "\n" *) in
  todo env (v1, v2, v3, v4)

let map_ms_declspec_modifier (env : env) ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let v1 = token env v1 (* "__declspec" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

let map_type_parameter_declaration (env : env) ((v1, v2) : CST.type_parameter_declaration) =
  let v1 = map_anon_choice_type env v1 in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_sized_type_specifier (env : env) ((v1, v2) : CST.sized_type_specifier) =
  let v1 =
    List.map (fun x ->
      (match x with
      | `Signed tok -> token env tok (* "signed" *)
      | `Unsi tok -> token env tok (* "unsigned" *)
      | `Long tok -> token env tok (* "long" *)
      | `Short tok -> token env tok (* "short" *)
      )
    ) v1
  in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | `Prim_type tok -> token env tok (* primitive_type *)
        )
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_field_designator (env : env) ((v1, v2) : CST.field_designator) =
  let v1 = token env v1 (* "." *) in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  todo env (v1, v2)

let map_preproc_defined (env : env) (x : CST.preproc_defined) =
  (match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "defined" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Defi_id (v1, v2) ->
      let v1 = token env v1 (* "defined" *) in
      let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
      todo env (v1, v2)
  )

let map_destructor_name (env : env) ((v1, v2) : CST.destructor_name) =
  let v1 = token env v1 (* "~" *) in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  todo env (v1, v2)

let map_variadic_type_parameter_declaration (env : env) ((v1, v2, v3) : CST.variadic_type_parameter_declaration) =
  let v1 = map_anon_choice_type env v1 in
  let v2 = token env v2 (* "..." *) in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let map_anon_choice_stmt_id2 (env : env) (x : CST.anon_choice_stmt_id2) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `DOTDOTDOT tok -> token env tok (* "..." *)
  )

let map_variadic_declarator (env : env) ((v1, v2) : CST.variadic_declarator) =
  let v1 = token env v1 (* "..." *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | None -> todo env ())
  in
  todo env (v1, v2)

let map_anon_choice_raw_str_lit (env : env) (x : CST.anon_choice_raw_str_lit) =
  (match x with
  | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
  | `Str_lit x -> map_string_literal env x
  )

let rec map_preproc_binary_expression (env : env) (x : CST.preproc_binary_expression) =
  (match x with
  | `Prep_exp_PLUS_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_DASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_STAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_SLASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_PERC_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_BAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_HAT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_AMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_GT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_GTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_LTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_LT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_LTLT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_GTGT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_preproc_call_expression (env : env) ((v1, v2) : CST.preproc_call_expression) =
  let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 = map_preproc_argument_list env v2 in
  todo env (v1, v2)

and map_preproc_argument_list (env : env) ((v1, v2, v3) : CST.preproc_argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_preproc_expression env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_preproc_expression env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_preproc_expression (env : env) (x : CST.preproc_expression) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Prep_call_exp x -> map_preproc_call_expression env x
  | `Num_lit tok -> token env tok (* number_literal *)
  | `Char_lit x -> map_char_literal env x
  | `Prep_defi x -> map_preproc_defined env x
  | `Prep_un_exp (v1, v2) ->
      let v1 = map_anon_choice_BANG env v1 in
      let v2 = map_preproc_expression env v2 in
      todo env (v1, v2)
  | `Prep_bin_exp x -> map_preproc_binary_expression env x
  | `Prep_paren_exp (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_preproc_expression env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )

let map_anon_choice_stmt_id4 (env : env) (x : CST.anon_choice_stmt_id4) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Op_name tok -> token env tok (* operator_name *)
  | `Dest_name x -> map_destructor_name env x
  )

let map_preproc_params (env : env) ((v1, v2, v3) : CST.preproc_params) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_stmt_id2 env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_stmt_id2 env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

let map_variadic_reference_declarator (env : env) ((v1, v2) : CST.variadic_reference_declarator) =
  let v1 =
    (match v1 with
    | `AMPAMP tok -> token env tok (* "&&" *)
    | `AMP tok -> token env tok (* "&" *)
    )
  in
  let v2 = map_variadic_declarator env v2 in
  todo env (v1, v2)

let map_concatenated_string (env : env) ((v1, v2) : CST.concatenated_string) =
  let v1 = map_anon_choice_raw_str_lit env v1 in
  let v2 = List.map (map_anon_choice_raw_str_lit env) v2 in
  todo env (v1, v2)

let map_preproc_include (env : env) ((v1, v2, v3) : CST.preproc_include) =
  let v1 = blank env v1 in
  let v2 =
    (match v2 with
    | `Str_lit x -> map_string_literal env x
    | `System_lib_str tok ->
        token env tok (* system_lib_string *)
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | `Prep_call_exp x -> map_preproc_call_expression env x
    )
  in
  let v3 = token env v3 (* "\n" *) in
  todo env (v1, v2, v3)

let map_preproc_function_def (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_function_def) =
  let v1 = blank env v1 in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_preproc_params env v3 in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* preproc_arg *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* "\n" *) in
  todo env (v1, v2, v3, v4, v5)

let rec map_type_specifier (env : env) (x : CST.type_specifier) =
  (match x with
  | `Struct_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "struct" *) in
      let v2 =
        (match v2 with
        | Some x -> map_ms_declspec_modifier env x
        | None -> todo env ())
      in
      let v3 = map_anon_choice_class_name env v3 in
      todo env (v1, v2, v3)
  | `Union_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "union" *) in
      let v2 =
        (match v2 with
        | Some x -> map_ms_declspec_modifier env x
        | None -> todo env ())
      in
      let v3 = map_anon_choice_class_name env v3 in
      todo env (v1, v2, v3)
  | `Enum_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "enum" *) in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Class tok -> token env tok (* "class" *)
            | `Struct tok -> token env tok (* "struct" *)
            )
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Class_name_opt_enum_base_clause_opt_enum_list (v1, v2, v3) ->
            let v1 = map_class_name env v1 in
            let v2 =
              (match v2 with
              | Some x -> map_enum_base_clause env x
              | None -> todo env ())
            in
            let v3 =
              (match v3 with
              | Some x -> map_enumerator_list env x
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | `Enum_list x -> map_enumerator_list env x
        )
      in
      todo env (v1, v2, v3)
  | `Class_spec (v1, v2, v3) ->
      let v1 = token env v1 (* "class" *) in
      let v2 =
        (match v2 with
        | Some x -> map_ms_declspec_modifier env x
        | None -> todo env ())
      in
      let v3 = map_anon_choice_class_name env v3 in
      todo env (v1, v2, v3)
  | `Sized_type_spec x -> map_sized_type_specifier env x
  | `Prim_type tok -> token env tok (* primitive_type *)
  | `Temp_type x -> map_template_type env x
  | `Auto tok -> token env tok (* "auto" *)
  | `Depe_type (v1, v2) ->
      let v1 = token env v1 (* "typename" *) in
      let v2 = map_type_specifier env v2 in
      todo env (v1, v2)
  | `Decl (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "decltype" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Choice_scoped_type_id x ->
      (match x with
      | `Scoped_type_id x -> map_scoped_type_identifier env x
      | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
      )
  )

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_anon_choice_exp2 env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_anon_choice_type_desc (env : env) (x : CST.anon_choice_type_desc) =
  (match x with
  | `Type_desc x -> map_type_descriptor env x
  | `Type_param_pack_expa (v1, v2) ->
      let v1 = map_type_descriptor env v1 in
      let v2 = token env v2 (* "..." *) in
      todo env (v1, v2)
  | `Exp x -> map_expression env x
  )

and map_initializer_list (env : env) ((v1, v2, v3, v4) : CST.initializer_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_init_pair env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_init_pair env v2 in
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

and map_anon_choice_exp (env : env) (x : CST.anon_choice_exp) =
  (match x with
  | `Exp x -> map_expression env x
  | `Init_list x -> map_initializer_list env x
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
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
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
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
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
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
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
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
  )

and map_subscript_designator (env : env) ((v1, v2, v3) : CST.subscript_designator) =
  let v1 = token env v1 (* "[" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and map_base_class_clause (env : env) ((v1, v2, v3, v4, v5) : CST.base_class_clause) =
  let v1 = token env v1 (* ":" *) in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_public env x
    | None -> todo env ())
  in
  let v3 = map_class_name env v3 in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "..." *)
    | None -> todo env ())
  in
  let v5 =
    List.map (fun (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "," *) in
      let v2 =
        (match v2 with
        | Some x -> map_anon_choice_public env x
        | None -> todo env ())
      in
      let v3 = map_class_name env v3 in
      let v4 =
        (match v4 with
        | Some tok -> token env tok (* "..." *)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
    ) v5
  in
  todo env (v1, v2, v3, v4, v5)

and map_anon_choice_virt_func_spec (env : env) (x : CST.anon_choice_virt_func_spec) =
  (match x with
  | `Virt_func_spec x -> map_virtual_function_specifier env x
  | `Expl_func_spec x -> map_explicit_function_specifier env x
  )

and map_template_type (env : env) ((v1, v2) : CST.template_type) =
  let v1 =
    (match v1 with
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | `Scoped_type_id x -> map_scoped_type_identifier env x
    )
  in
  let v2 = map_template_argument_list env v2 in
  todo env (v1, v2)

and map_anon_choice_stor_class_spec_ (env : env) (x : CST.anon_choice_stor_class_spec_) =
  (match x with
  | `Stor_class_spec x -> map_storage_class_specifier env x
  | `Type_qual x -> map_type_qualifier env x
  | `Attr_spec x -> map_attribute_specifier env x
  | `Ms_decl_modi x -> map_ms_declspec_modifier env x
  )

and map_conditional_expression (env : env) ((v1, v2, v3, v4, v5) : CST.conditional_expression) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "?" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* ":" *) in
  let v5 = map_expression env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_preproc_ifdef_in_field_declaration_list (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_field_declaration_list) =
  let v1 =
    (match v1 with
    | `Blank () -> todo env ()
    )
  in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 =
    List.map (map_field_declaration_list_item env) v3
  in
  let v4 =
    (match v4 with
    | Some x ->
        map_anon_choice_prep_else_in_field_decl_list env x
    | None -> todo env ())
  in
  let v5 = blank env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_field_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.field_declaration) =
  let v1 = List.map (map_attribute env) v1 in
  let v2 =
    (match v2 with
    | Some x -> map_virtual_function_specifier env x
    | None -> todo env ())
  in
  let v3 = map_declaration_specifiers env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = map_field_declarator env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_field_declarator env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x ->
        (match x with
        | `Bitf_clause x -> map_bitfield_clause env x
        | `Init_list x -> map_initializer_list env x
        | `EQ_choice_exp (v1, v2) ->
            let v1 = token env v1 (* "=" *) in
            let v2 = map_anon_choice_exp env v2 in
            todo env (v1, v2)
        )
    | None -> todo env ())
  in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_assignment_expression (env : env) ((v1, v2, v3) : CST.assignment_expression) =
  let v1 = map_assignment_left_expression env v1 in
  let v2 =
    (match v2 with
    | `EQ tok -> token env tok (* "=" *)
    | `STAREQ tok -> token env tok (* "*=" *)
    | `SLASHEQ tok -> token env tok (* "/=" *)
    | `PERCEQ tok -> token env tok (* "%=" *)
    | `PLUSEQ tok -> token env tok (* "+=" *)
    | `DASHEQ tok -> token env tok (* "-=" *)
    | `LTLTEQ tok -> token env tok (* "<<=" *)
    | `GTGTEQ tok -> token env tok (* ">>=" *)
    | `AMPEQ tok -> token env tok (* "&=" *)
    | `HATEQ tok -> token env tok (* "^=" *)
    | `BAREQ tok -> token env tok (* "|=" *)
    )
  in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_static_assert_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.static_assert_declaration) =
  let v1 = token env v1 (* "static_assert" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_expression env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "," *) in
        let v2 =
          (match v2 with
          | `Str_lit x -> map_string_literal env x
          | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
          | `Conc_str x -> map_concatenated_string env x
          )
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ")" *) in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_template_argument_list (env : env) ((v1, v2, v3) : CST.template_argument_list) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_type_desc env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_type_desc env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = blank env v3 in
  todo env (v1, v2, v3)

and map_type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) =
  let v1 = List.map (map_type_qualifier env) v1 in
  let v2 = map_type_specifier env v2 in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    (match v4 with
    | Some x -> map_abstract_declarator env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_anon_choice_param_decl (env : env) (x : CST.anon_choice_param_decl) =
  (match x with
  | `Param_decl x -> map_parameter_declaration env x
  | `Opt_param_decl x ->
      map_optional_parameter_declaration env x
  | `Vari_param_decl x ->
      map_variadic_parameter_declaration env x
  | `DOTDOTDOT tok -> token env tok (* "..." *)
  )

and map_anon_choice_param_decl_ (env : env) (x : CST.anon_choice_param_decl_) =
  (match x with
  | `Param_decl x -> map_parameter_declaration env x
  | `Opt_param_decl x ->
      map_optional_parameter_declaration env x
  | `Type_param_decl x -> map_type_parameter_declaration env x
  | `Vari_param_decl x ->
      map_variadic_parameter_declaration env x
  | `Vari_type_param_decl x ->
      map_variadic_type_parameter_declaration env x
  | `Opt_type_param_decl x ->
      map_optional_type_parameter_declaration env x
  | `Temp_temp_param_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "template" *) in
      let v2 = map_template_parameter_list env v2 in
      let v3 =
        (match v3 with
        | `Type_param_decl x -> map_type_parameter_declaration env x
        | `Vari_type_param_decl x ->
            map_variadic_type_parameter_declaration env x
        | `Opt_type_param_decl x ->
            map_optional_type_parameter_declaration env x
        )
      in
      todo env (v1, v2, v3)
  )

and map_anon_choice_comp_stmt (env : env) (x : CST.anon_choice_comp_stmt) =
  (match x with
  | `Comp_stmt x -> map_compound_statement env x
  | `Defa_meth_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "=" *) in
      let v2 = token env v2 (* "default" *) in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Delete_meth_clause (v1, v2, v3) ->
      let v1 = token env v1 (* "=" *) in
      let v2 = token env v2 (* "delete" *) in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  )

and map_anon_choice_decl (env : env) (x : CST.anon_choice_decl) =
  (match x with
  | `Decl x -> map_declarator env x
  | `Init_decl x -> map_init_declarator env x
  )

and map_enum_base_clause (env : env) ((v1, v2) : CST.enum_base_clause) =
  let v1 = token env v1 (* ":" *) in
  let v2 =
    (match v2 with
    | `Scoped_type_id x -> map_scoped_type_identifier env x
    | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | `Sized_type_spec x -> map_sized_type_specifier env x
    )
  in
  todo env (v1, v2)

and map_anon_choice_type_qual (env : env) (x : CST.anon_choice_type_qual) =
  (match x with
  | `Type_qual x -> map_type_qualifier env x
  | `Virt_spec x -> map_virtual_specifier env x
  | `Noex x -> map_noexcept env x
  | `Throw_spec x -> map_throw_specifier env x
  | `Trai_ret_type x -> map_trailing_return_type env x
  )

and map_subscript_expression (env : env) ((v1, v2, v3, v4) : CST.subscript_expression) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "]" *) in
  todo env (v1, v2, v3, v4)

and map_operator_cast_declaration (env : env) ((v1, v2, v3, v4) : CST.operator_cast_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_virt_func_spec env x
    | None -> todo env ())
  in
  let v2 = map_operator_cast env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

and map_empty_declaration (env : env) ((v1, v2) : CST.empty_declaration) =
  let v1 = map_type_specifier env v1 in
  let v2 = token env v2 (* ";" *) in
  todo env (v1, v2)

and map_case_statement (env : env) ((v1, v2, v3) : CST.case_statement) =
  let v1 =
    (match v1 with
    | `Case_exp (v1, v2) ->
        let v1 = token env v1 (* "case" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | `Defa tok -> token env tok (* "default" *)
    )
  in
  let v2 = token env v2 (* ":" *) in
  let v3 =
    List.map (fun x ->
      (match x with
      | `Choice_labe_stmt x -> map_non_case_statement env x
      | `Decl x -> map_declaration env x
      | `Type_defi x -> map_type_definition env x
      )
    ) v3
  in
  todo env (v1, v2, v3)

and map_new_declarator (env : env) (x : CST.new_declarator) =
  (match x with
  | `Rectype (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "]" *) in
      let v4 =
        (match v4 with
        | Some x -> map_new_declarator env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  )

and map_template_declaration (env : env) ((v1, v2, v3) : CST.template_declaration) =
  let v1 = token env v1 (* "template" *) in
  let v2 = map_template_parameter_list env v2 in
  let v3 =
    (match v3 with
    | `Empty_decl x -> map_empty_declaration env x
    | `Alias_decl x -> map_alias_declaration env x
    | `Decl x -> map_declaration env x
    | `Temp_decl x -> map_template_declaration env x
    | `Func_defi x -> map_function_definition env x
    | `Cons_or_dest_defi x ->
        map_constructor_or_destructor_definition env x
    | `Op_cast_defi x -> map_operator_cast_definition env x
    )
  in
  todo env (v1, v2, v3)

and map_operator_cast_definition (env : env) ((v1, v2, v3, v4) : CST.operator_cast_definition) =
  let v1 =
    List.map (map_anon_choice_stor_class_spec env) v1
  in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_virt_func_spec env x
    | None -> todo env ())
  in
  let v3 = map_operator_cast env v3 in
  let v4 = map_anon_choice_comp_stmt env v4 in
  todo env (v1, v2, v3, v4)

and map_non_case_statement (env : env) (x : CST.non_case_statement) =
  (match x with
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Comp_stmt x -> map_compound_statement env x
  | `Exp_stmt x -> map_expression_statement env x
  | `If_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "if" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "constexpr" *)
        | None -> todo env ())
      in
      let v3 = map_condition_clause env v3 in
      let v4 = map_statement env v4 in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "else" *) in
            let v2 = map_statement env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = map_condition_clause env v2 in
      let v3 = map_compound_statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = map_statement env v2 in
      let v3 = token env v3 (* "while" *) in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_condition_clause env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | `Decl x -> map_declaration env x
        | `Opt_choice_exp_SEMI x -> map_expression_statement env x
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
        | Some x -> map_anon_choice_exp2 env x
        | None -> todo env ())
      in
      let v7 = token env v7 (* ")" *) in
      let v8 = map_statement env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Ret_stmt x -> map_return_statement env x
  | `Brk_stmt (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 = token env v2 (* ";" *) in
      todo env (v1, v2)
  | `Cont_stmt (v1, v2) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 = token env v2 (* ";" *) in
      todo env (v1, v2)
  | `Goto_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "goto" *) in
      let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  )

and map_call_expression (env : env) (x : CST.call_expression) =
  (match x with
  | `Exp_arg_list (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_argument_list env v2 in
      todo env (v1, v2)
  | `Prim_type_arg_list (v1, v2) ->
      let v1 = token env v1 (* primitive_type *) in
      let v2 = map_argument_list env v2 in
      todo env (v1, v2)
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_exp2 env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* ";" *) in
  todo env (v1, v2)

and map_optional_type_parameter_declaration (env : env) ((v1, v2, v3, v4) : CST.optional_type_parameter_declaration) =
  let v1 = map_anon_choice_type env v1 in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* pattern [a-zA-Z_]\w* *)
    | None -> todo env ())
  in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_type_specifier env v4 in
  todo env (v1, v2, v3, v4)

and map_trailing_return_type (env : env) ((v1, v2, v3, v4) : CST.trailing_return_type) =
  let v1 = token env v1 (* "->" *) in
  let v2 =
    (match v2 with
    | Some x -> map_type_qualifier env x
    | None -> todo env ())
  in
  let v3 = map_type_specifier env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_abstract_declarator env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_catch_clause (env : env) ((v1, v2, v3) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 = map_parameter_list env v2 in
  let v3 = map_compound_statement env v3 in
  todo env (v1, v2, v3)

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_type_descriptor env v2 in
  let v3 = token env v3 (* ")" *) in
  let v4 = map_expression env v4 in
  todo env (v1, v2, v3, v4)

and map_pointer_expression (env : env) ((v1, v2) : CST.pointer_expression) =
  let v1 =
    (match v1 with
    | `STAR tok -> token env tok (* "*" *)
    | `AMP tok -> token env tok (* "&" *)
    )
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_stmt_id3 env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "::" *) in
  let v3 = map_anon_choice_stmt_id4 env v3 in
  todo env (v1, v2, v3)

and map_anon_choice_stmt_id (env : env) (x : CST.anon_choice_stmt_id) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Scoped_field_id (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_anon_choice_stmt_id3 env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "::" *) in
      let v3 = map_anon_choice_stmt_id4 env v3 in
      todo env (v1, v2, v3)
  )

and map_abstract_declarator (env : env) (x : CST.abstract_declarator) =
  (match x with
  | `Choice_abst_poin_decl x ->
      (match x with
      | `Abst_poin_decl x -> map_abstract_pointer_declarator env x
      | `Abst_func_decl x ->
          map_abstract_function_declarator env x
      | `Abst_array_decl x -> map_abstract_array_declarator env x
      | `Abst_paren_decl x ->
          map_abstract_parenthesized_declarator env x
      )
  | `Abst_ref_decl (v1, v2) ->
      let v1 = map_anon_choice_AMP env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_abstract_declarator env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  )

and map_variadic_parameter_declaration (env : env) ((v1, v2) : CST.variadic_parameter_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 =
    (match v2 with
    | `Vari_decl x -> map_variadic_declarator env x
    | `Vari_ref_decl x ->
        map_variadic_reference_declarator env x
    )
  in
  todo env (v1, v2)

and map_template_method (env : env) ((v1, v2) : CST.template_method) =
  let v1 = map_anon_choice_stmt_id env v1 in
  let v2 = map_template_argument_list env v2 in
  todo env (v1, v2)

and map_preproc_if_in_field_declaration_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_field_declaration_list) =
  let v1 = blank env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = token env v3 (* "\n" *) in
  let v4 =
    List.map (map_field_declaration_list_item env) v4
  in
  let v5 =
    (match v5 with
    | Some x ->
        map_anon_choice_prep_else_in_field_decl_list env x
    | None -> todo env ())
  in
  let v6 = blank env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_type_declarator (env : env) (x : CST.type_declarator) =
  (match x with
  | `Poin_type_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_ms_based_modifier env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "*" *) in
      let v3 = List.map (map_ms_pointer_modifier env) v3 in
      let v4 = List.map (map_type_qualifier env) v4 in
      let v5 = map_type_declarator env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Func_type_decl (v1, v2) ->
      let v1 = map_type_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      todo env (v1, v2)
  | `Array_type_decl (v1, v2, v3, v4, v5) ->
      let v1 = map_type_declarator env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 = List.map (map_type_qualifier env) v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_exp_ env x
        | None -> todo env ())
      in
      let v5 = token env v5 (* "]" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Paren_type_decl (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_declarator env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  )

and map_init_declarator (env : env) (x : CST.init_declarator) =
  (match x with
  | `Decl_EQ_choice_init_list (v1, v2, v3) ->
      let v1 = map_declarator env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 =
        (match v3 with
        | `Init_list x -> map_initializer_list env x
        | `Exp x -> map_expression env x
        )
      in
      todo env (v1, v2, v3)
  | `Decl_choice_arg_list (v1, v2) ->
      let v1 = map_declarator env v1 in
      let v2 = map_anon_choice_arg_list env v2 in
      todo env (v1, v2)
  )

and map_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.enumerator_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_enumerator env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_enumerator env v2 in
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

and map_scoped_type_identifier (env : env) ((v1, v2, v3) : CST.scoped_type_identifier) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_stmt_id3 env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "::" *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  todo env (v1, v2, v3)

and map_template_function (env : env) ((v1, v2) : CST.template_function) =
  let v1 = map_anon_choice_stmt_id_ env v1 in
  let v2 = map_template_argument_list env v2 in
  todo env (v1, v2)

and map_linkage_specification (env : env) ((v1, v2, v3) : CST.linkage_specification) =
  let v1 = token env v1 (* "extern" *) in
  let v2 = map_string_literal env v2 in
  let v3 =
    (match v3 with
    | `Func_defi x -> map_function_definition env x
    | `Decl x -> map_declaration env x
    | `Decl_list x -> map_declaration_list env x
    )
  in
  todo env (v1, v2, v3)

and map_noexcept (env : env) ((v1, v2) : CST.noexcept) =
  let v1 = token env v1 (* "noexcept" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "(" *) in
        let v2 =
          (match v2 with
          | Some x -> map_expression env x
          | None -> todo env ())
        in
        let v3 = token env v3 (* ")" *) in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_cond_exp x ->
      (match x with
      | `Cond_exp x -> map_conditional_expression env x
      | `Assign_exp x -> map_assignment_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Un_exp x -> map_unary_expression env x
      | `Update_exp x -> map_update_expression env x
      | `Cast_exp x -> map_cast_expression env x
      | `Poin_exp x -> map_pointer_expression env x
      | `Sizeof_exp x -> map_sizeof_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Call_exp x -> map_call_expression env x
      | `Field_exp x -> map_field_expression env x
      | `Comp_lit_exp x -> map_compound_literal_expression env x
      | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
      | `Num_lit tok -> token env tok (* number_literal *)
      | `Str_lit x -> map_string_literal env x
      | `True tok -> token env tok (* true *)
      | `False tok -> token env tok (* false *)
      | `Null tok -> token env tok (* "NULL" *)
      | `Conc_str x -> map_concatenated_string env x
      | `Char_lit x -> map_char_literal env x
      | `Paren_exp x -> map_parenthesized_expression env x
      )
  | `Temp_func x -> map_template_function env x
  | `Scoped_id x -> map_scoped_identifier env x
  | `New_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "::" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "new" *) in
      let v3 =
        (match v3 with
        | Some x -> map_argument_list env x
        | None -> todo env ())
      in
      let v4 = map_type_specifier env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_new_declarator env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_anon_choice_arg_list env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Delete_exp (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "::" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "delete" *) in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "[" *) in
            let v2 = token env v2 (* "]" *) in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 = map_expression env v4 in
      todo env (v1, v2, v3, v4)
  | `Lambda_exp (v1, v2, v3) ->
      let v1 = map_lambda_capture_specifier env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_abstract_function_declarator env x
        | None -> todo env ())
      in
      let v3 = map_compound_statement env v3 in
      todo env (v1, v2, v3)
  | `Param_pack_expa (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "..." *) in
      todo env (v1, v2)
  | `Null tok -> token env tok (* "nullptr" *)
  | `This tok -> token env tok (* "this" *)
  | `Raw_str_lit tok -> token env tok (* raw_string_literal *)
  )

and map_field_expression (env : env) (x : CST.field_expression) =
  (match x with
  | `Exp_choice_DOT_id (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DOT env v2 in
      let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
      todo env (v1, v2, v3)
  | `Exp_choice_DOT_choice_dest_name (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DOT env v2 in
      let v3 =
        (match v3 with
        | `Dest_name x -> map_destructor_name env x
        | `Temp_meth x -> map_template_method env x
        )
      in
      todo env (v1, v2, v3)
  )

and map_declaration_specifiers (env : env) ((v1, v2, v3) : CST.declaration_specifiers) =
  let v1 =
    List.map (map_anon_choice_stor_class_spec_ env) v1
  in
  let v2 = map_type_specifier env v2 in
  let v3 =
    List.map (map_anon_choice_stor_class_spec_ env) v3
  in
  todo env (v1, v2, v3)

and map_unary_expression (env : env) ((v1, v2) : CST.unary_expression) =
  let v1 = map_anon_choice_BANG env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_template_parameter_list (env : env) ((v1, v2, v3) : CST.template_parameter_list) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl_ env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_param_decl_ env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = blank env v3 in
  todo env (v1, v2, v3)

and map_condition_declaration (env : env) ((v1, v2, v3) : CST.condition_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 = map_declarator env v2 in
  let v3 =
    (match v3 with
    | `EQ_exp (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | `Init_list x -> map_initializer_list env x
    )
  in
  todo env (v1, v2, v3)

and map_throw_specifier (env : env) ((v1, v2, v3, v4) : CST.throw_specifier) =
  let v1 = token env v1 (* "throw" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = map_type_descriptor env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_type_descriptor env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_definition) =
  let v1 = token env v1 (* "typedef" *) in
  let v2 = List.map (map_type_qualifier env) v2 in
  let v3 = map_type_specifier env v3 in
  let v4 = map_type_declarator env v4 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_declarator env v2 in
      todo env (v1, v2)
    ) v5
  in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_translation_unit env v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_parenthesized_declarator (env : env) ((v1, v2, v3) : CST.parenthesized_declarator) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_array_field_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.array_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    (match v4 with
    | Some x -> map_anon_choice_exp_ env x
    | None -> todo env ())
  in
  let v5 = token env v5 (* "]" *) in
  todo env (v1, v2, v3, v4, v5)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_case_stmt x ->
      (match x with
      | `Case_stmt x -> map_case_statement env x
      | `Choice_labe_stmt x -> map_non_case_statement env x
      )
  | `For_range_loop (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_declaration_specifiers env v3 in
      let v4 = map_declarator env v4 in
      let v5 = token env v5 (* ":" *) in
      let v6 = map_anon_choice_exp env v6 in
      let v7 = token env v7 (* ")" *) in
      let v8 = map_statement env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = map_compound_statement env v2 in
      let v3 = List.map (map_catch_clause env) v3 in
      todo env (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  )

and map_field_declaration_list_item (env : env) (x : CST.field_declaration_list_item) =
  (match x with
  | `Choice_field_decl x ->
      (match x with
      | `Field_decl x -> map_field_declaration env x
      | `Prep_def x -> map_preproc_def env x
      | `Prep_func_def x -> map_preproc_function_def env x
      | `Prep_call x -> map_preproc_call env x
      | `Prep_if_in_field_decl_list x ->
          map_preproc_if_in_field_declaration_list env x
      | `Prep_ifdef_in_field_decl_list x ->
          map_preproc_ifdef_in_field_declaration_list env x
      )
  | `Temp_decl x -> map_template_declaration env x
  | `Inline_meth_defi (v1, v2, v3, v4, v5) ->
      let v1 = List.map (map_attribute env) v1 in
      let v2 =
        (match v2 with
        | Some x -> map_virtual_function_specifier env x
        | None -> todo env ())
      in
      let v3 = map_declaration_specifiers env v3 in
      let v4 = map_field_declarator env v4 in
      let v5 = map_anon_choice_comp_stmt env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Cons_or_dest_defi x ->
      map_constructor_or_destructor_definition env x
  | `Cons_or_dest_decl (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_anon_choice_virt_func_spec env x
        | None -> todo env ())
      in
      let v2 = map_function_declarator env v2 in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Op_cast_defi x -> map_operator_cast_definition env x
  | `Op_cast_decl x -> map_operator_cast_declaration env x
  | `Friend_decl (v1, v2) ->
      let v1 = token env v1 (* "friend" *) in
      let v2 =
        (match v2 with
        | `Decl x -> map_declaration env x
        | `Func_defi x -> map_function_definition env x
        | `Opt_choice_class_class_name_SEMI (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | Some x ->
                  (match x with
                  | `Class tok -> token env tok (* "class" *)
                  | `Struct tok -> token env tok (* "struct" *)
                  | `Union tok -> token env tok (* "union" *)
                  )
              | None -> todo env ())
            in
            let v2 = map_class_name env v2 in
            let v3 = token env v3 (* ";" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Access_spec (v1, v2) ->
      let v1 = map_anon_choice_public env v1 in
      let v2 = token env v2 (* ":" *) in
      todo env (v1, v2)
  | `Alias_decl x -> map_alias_declaration env x
  | `Using_decl x -> map_using_declaration env x
  | `Type_defi x -> map_type_definition env x
  | `Static_assert_decl x ->
      map_static_assert_declaration env x
  )

and map_enumerator (env : env) ((v1, v2) : CST.enumerator) =
  let v1 = token env v1 (* pattern [a-zA-Z_]\w* *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_using_declaration (env : env) ((v1, v2, v3, v4) : CST.using_declaration) =
  let v1 = token env v1 (* "using" *) in
  let v2 =
    (match v2 with
    | Some tok -> token env tok (* "namespace" *)
    | None -> todo env ())
  in
  let v3 = map_anon_choice_stmt_id_ env v3 in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

and map_function_definition (env : env) ((v1, v2, v3, v4, v5) : CST.function_definition) =
  let v1 = List.map (map_attribute env) v1 in
  let v2 =
    (match v2 with
    | Some x -> map_ms_call_modifier env x
    | None -> todo env ())
  in
  let v3 = map_declaration_specifiers env v3 in
  let v4 = map_declarator env v4 in
  let v5 = map_compound_statement env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_array_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.array_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = token env v2 (* "[" *) in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    (match v4 with
    | Some x -> map_anon_choice_exp_ env x
    | None -> todo env ())
  in
  let v5 = token env v5 (* "]" *) in
  todo env (v1, v2, v3, v4, v5)

and map_anon_choice_stmt_id_ (env : env) (x : CST.anon_choice_stmt_id_) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Scoped_id x -> map_scoped_identifier env x
  )

and map_anon_choice_prep_else (env : env) (x : CST.anon_choice_prep_else) =
  (match x with
  | `Prep_else (v1, v2) ->
      let v1 = blank env v1 in
      let v2 = map_translation_unit env v2 in
      todo env (v1, v2)
  | `Prep_elif (v1, v2, v3, v4, v5) ->
      let v1 = blank env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = token env v3 (* "\n" *) in
      let v4 = map_translation_unit env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_anon_choice_prep_else env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and map_constructor_or_destructor_definition (env : env) ((v1, v2, v3, v4, v5) : CST.constructor_or_destructor_definition) =
  let v1 =
    List.map (map_anon_choice_stor_class_spec env) v1
  in
  let v2 =
    (match v2 with
    | Some x -> map_anon_choice_virt_func_spec env x
    | None -> todo env ())
  in
  let v3 = map_function_declarator env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_field_initializer_list env x
    | None -> todo env ())
  in
  let v5 = map_anon_choice_comp_stmt env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_anon_choice_class_name (env : env) (x : CST.anon_choice_class_name) =
  (match x with
  | `Class_name x -> map_class_name env x
  | `Opt_class_name_opt_virt_spec_opt_base_class_clause_field_decl_list (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some x -> map_class_name env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x -> map_virtual_specifier env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_base_class_clause env x
        | None -> todo env ())
      in
      let v4 = map_field_declaration_list env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Choice_DASHDASH_exp (v1, v2) ->
      let v1 = map_anon_choice_DASHDASH env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Exp_choice_DASHDASH (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DASHDASH env v2 in
      todo env (v1, v2)
  )

and map_abstract_pointer_declarator (env : env) ((v1, v2, v3) : CST.abstract_pointer_declarator) =
  let v1 = token env v1 (* "*" *) in
  let v2 = List.map (map_type_qualifier env) v2 in
  let v3 =
    (match v3 with
    | Some x -> map_abstract_declarator env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_alias_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.alias_declaration) =
  let v1 = token env v1 (* "using" *) in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_type_descriptor env v4 in
  let v5 = token env v5 (* ";" *) in
  todo env (v1, v2, v3, v4, v5)

and map_anon_choice_exp2 (env : env) (x : CST.anon_choice_exp2) =
  (match x with
  | `Exp x -> map_expression env x
  | `Comma_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "," *) in
      let v3 = map_anon_choice_exp2 env v3 in
      todo env (v1, v2, v3)
  )

and map_class_name (env : env) (x : CST.class_name) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Scoped_type_id x -> map_scoped_type_identifier env x
  | `Temp_type x -> map_template_type env x
  )

and map_parameter_declaration (env : env) ((v1, v2, v3) : CST.parameter_declaration) =
  let v1 = List.map (map_attribute env) v1 in
  let v2 = map_declaration_specifiers env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `Decl x -> map_declarator env x
        | `Abst_decl x -> map_abstract_declarator env x
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_pointer_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.pointer_declarator) =
  let v1 =
    (match v1 with
    | Some x -> map_ms_based_modifier env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "*" *) in
  let v3 = List.map (map_ms_pointer_modifier env) v3 in
  let v4 = List.map (map_type_qualifier env) v4 in
  let v5 = map_declarator env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let v1 = token env v1 (* "__based" *) in
  let v2 = map_argument_list env v2 in
  todo env (v1, v2)

and map_pointer_field_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.pointer_field_declarator) =
  let v1 =
    (match v1 with
    | Some x -> map_ms_based_modifier env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "*" *) in
  let v3 = List.map (map_ms_pointer_modifier env) v3 in
  let v4 = List.map (map_type_qualifier env) v4 in
  let v5 = map_field_declarator env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_anon_choice_prep_else_in_field_decl_list (env : env) (x : CST.anon_choice_prep_else_in_field_decl_list) =
  (match x with
  | `Prep_else_in_field_decl_list (v1, v2) ->
      let v1 = blank env v1 in
      let v2 =
        List.map (map_field_declaration_list_item env) v2
      in
      todo env (v1, v2)
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let v1 = blank env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = token env v3 (* "\n" *) in
      let v4 =
        List.map (map_field_declaration_list_item env) v4
      in
      let v5 =
        (match v5 with
        | Some x ->
            map_anon_choice_prep_else_in_field_decl_list env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and map_abstract_parenthesized_declarator (env : env) ((v1, v2, v3) : CST.abstract_parenthesized_declarator) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_abstract_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_compound_literal_expression (env : env) (x : CST.compound_literal_expression) =
  (match x with
  | `LPAR_type_desc_RPAR_init_list (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_type_descriptor env v2 in
      let v3 = token env v3 (* ")" *) in
      let v4 = map_initializer_list env v4 in
      todo env (v1, v2, v3, v4)
  | `Choice_id_init_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | `Temp_type x -> map_template_type env x
        | `Scoped_type_id x -> map_scoped_type_identifier env x
        )
      in
      let v2 = map_initializer_list env v2 in
      todo env (v1, v2)
  )

and map_field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 = map_anon_choice_stmt_id env v1 in
  let v2 =
    (match v2 with
    | `Init_list x -> map_initializer_list env x
    | `Arg_list x -> map_argument_list env x
    )
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "..." *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_attribute_specifier (env : env) ((v1, v2, v3, v4) : CST.attribute_specifier) =
  let v1 = token env v1 (* "__attribute__" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_argument_list env v3 in
  let v4 = token env v4 (* ")" *) in
  todo env (v1, v2, v3, v4)

and map_field_declarator (env : env) (x : CST.field_declarator) =
  (match x with
  | `Choice_poin_field_decl x ->
      (match x with
      | `Poin_field_decl x -> map_pointer_field_declarator env x
      | `Func_field_decl x -> map_function_field_declarator env x
      | `Array_field_decl x -> map_array_field_declarator env x
      | `Paren_field_decl x ->
          map_parenthesized_field_declarator env x
      | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
      )
  | `Ref_field_decl (v1, v2) ->
      let v1 = map_anon_choice_AMP env v1 in
      let v2 = map_field_declarator env v2 in
      todo env (v1, v2)
  | `Temp_meth x -> map_template_method env x
  | `Op_name tok -> token env tok (* operator_name *)
  )

and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (map_field_declaration_list_item env) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_explicit_function_specifier (env : env) (x : CST.explicit_function_specifier) =
  (match x with
  | `Expl tok -> token env tok (* "explicit" *)
  | `Expl_LPAR_exp_RPAR (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "explicit" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  )

and map_bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_top_level_item (env : env) (x : CST.top_level_item) =
  (match x with
  | `Choice_func_defi x ->
      (match x with
      | `Func_defi x -> map_function_definition env x
      | `Link_spec x -> map_linkage_specification env x
      | `Decl x -> map_declaration env x
      | `Choice_choice_case_stmt x -> map_statement env x
      | `Type_defi x -> map_type_definition env x
      | `Empty_decl x -> map_empty_declaration env x
      | `Prep_if x -> map_preproc_if env x
      | `Prep_ifdef x -> map_preproc_ifdef env x
      | `Prep_incl x -> map_preproc_include env x
      | `Prep_def x -> map_preproc_def env x
      | `Prep_func_def x -> map_preproc_function_def env x
      | `Prep_call x -> map_preproc_call env x
      )
  | `Name_defi (v1, v2, v3) ->
      let v1 = token env v1 (* "namespace" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern [a-zA-Z_]\w* *)
        | None -> todo env ())
      in
      let v3 = map_declaration_list env v3 in
      todo env (v1, v2, v3)
  | `Using_decl x -> map_using_declaration env x
  | `Alias_decl x -> map_alias_declaration env x
  | `Static_assert_decl x ->
      map_static_assert_declaration env x
  | `Temp_decl x -> map_template_declaration env x
  | `Temp_inst (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "template" *) in
      let v2 =
        (match v2 with
        | Some x -> map_declaration_specifiers env x
        | None -> todo env ())
      in
      let v3 = map_declarator env v3 in
      let v4 = token env v4 (* ";" *) in
      todo env (v1, v2, v3, v4)
  | `Cons_or_dest_defi x ->
      map_constructor_or_destructor_definition env x
  | `Op_cast_defi x -> map_operator_cast_definition env x
  | `Op_cast_decl x -> map_operator_cast_declaration env x
  )

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_translation_unit env v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_condition_clause (env : env) ((v1, v2, v3) : CST.condition_clause) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | `Opt_choice_decl_choice_exp (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x ->
              (match x with
              | `Decl x -> map_declaration env x
              | `Exp_stmt x -> map_expression_statement env x
              )
          | None -> todo env ())
        in
        let v2 = map_anon_choice_exp2 env v2 in
        todo env (v1, v2)
    | `Cond_decl x -> map_condition_declaration env x
    )
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_return_statement (env : env) (x : CST.return_statement) =
  (match x with
  | `Ret_opt_choice_exp_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> map_anon_choice_exp2 env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Ret_init_list_SEMI (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = map_initializer_list env v2 in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  )

and map_optional_parameter_declaration (env : env) ((v1, v2, v3, v4) : CST.optional_parameter_declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_declarator env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* "=" *) in
  let v4 = map_expression env v4 in
  todo env (v1, v2, v3, v4)

and map_anon_choice_stor_class_spec (env : env) (x : CST.anon_choice_stor_class_spec) =
  (match x with
  | `Stor_class_spec x -> map_storage_class_specifier env x
  | `Type_qual x -> map_type_qualifier env x
  | `Attr_spec x -> map_attribute_specifier env x
  )

and map_function_field_declarator (env : env) ((v1, v2, v3) : CST.function_field_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 = map_parameter_list env v2 in
  let v3 = List.map (map_anon_choice_type_qual env) v3 in
  todo env (v1, v2, v3)

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_exp env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_exp env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_attribute (env : env) ((v1, v2, v3, v4) : CST.attribute) =
  let v1 = token env v1 (* "[[" *) in
  let v2 = map_expression env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* "]]" *) in
  todo env (v1, v2, v3, v4)

and map_anon_choice_stmt_id3 (env : env) (x : CST.anon_choice_stmt_id3) =
  (match x with
  | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
  | `Temp_type x -> map_template_type env x
  | `Scoped_name_id x -> map_scoped_namespace_identifier env x
  )

and map_declarator (env : env) (x : CST.declarator) =
  (match x with
  | `Choice_poin_decl x ->
      (match x with
      | `Poin_decl x -> map_pointer_declarator env x
      | `Func_decl x -> map_function_declarator env x
      | `Array_decl x -> map_array_declarator env x
      | `Paren_decl x -> map_parenthesized_declarator env x
      | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
      )
  | `Ref_decl (v1, v2) ->
      let v1 = map_anon_choice_AMP env v1 in
      let v2 = map_declarator env v2 in
      todo env (v1, v2)
  | `Scoped_id x -> map_scoped_identifier env x
  | `Temp_func x -> map_template_function env x
  | `Op_name tok -> token env tok (* operator_name *)
  | `Dest_name x -> map_destructor_name env x
  | `Stru_bind_decl (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "[" *) in
      let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  )

and map_anon_choice_arg_list (env : env) (x : CST.anon_choice_arg_list) =
  (match x with
  | `Arg_list x -> map_argument_list env x
  | `Init_list x -> map_initializer_list env x
  )

and map_anon_choice_exp_ (env : env) (x : CST.anon_choice_exp_) =
  (match x with
  | `Exp x -> map_expression env x
  | `STAR tok -> token env tok (* "*" *)
  )

and map_translation_unit (env : env) (xs : CST.translation_unit) =
  List.map (map_top_level_item env) xs

and map_sizeof_expression (env : env) (x : CST.sizeof_expression) =
  (match x with
  | `Sizeof_choice_exp (v1, v2) ->
      let v1 = token env v1 (* "sizeof" *) in
      let v2 =
        (match v2 with
        | `Exp x -> map_expression env x
        | `LPAR_type_desc_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_type_descriptor env v2 in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Sizeof_DOTDOTDOT_LPAR_id_RPAR (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "sizeof" *) in
      let v2 = token env v2 (* "..." *) in
      let v3 = token env v3 (* "(" *) in
      let v4 = token env v4 (* pattern [a-zA-Z_]\w* *) in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  )

and map_assignment_left_expression (env : env) (x : CST.assignment_left_expression) =
  (match x with
  | `Choice_id x ->
      (match x with
      | `Id tok -> token env tok (* pattern [a-zA-Z_]\w* *)
      | `Call_exp x -> map_call_expression env x
      | `Field_exp x -> map_field_expression env x
      | `Poin_exp x -> map_pointer_expression env x
      | `Subs_exp x -> map_subscript_expression env x
      | `Paren_exp x -> map_parenthesized_expression env x
      )
  | `Scoped_name_id x -> map_scoped_namespace_identifier env x
  )

and map_preproc_ifdef (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) =
  let v1 =
    (match v1 with
    | `Blank () -> todo env ()
    )
  in
  let v2 = token env v2 (* pattern [a-zA-Z_]\w* *) in
  let v3 = map_translation_unit env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_anon_choice_prep_else env x
    | None -> todo env ())
  in
  let v5 = blank env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_anon_choice_param_decl env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_abstract_function_declarator (env : env) ((v1, v2, v3, v4) : CST.abstract_function_declarator) =
  let v1 =
    (match v1 with
    | Some x -> map_abstract_declarator env x
    | None -> todo env ())
  in
  let v2 = map_parameter_list env v2 in
  let v3 =
    List.map (fun x ->
      (match x with
      | `Type_qual x -> map_type_qualifier env x
      | `Noex x -> map_noexcept env x
      | `Throw_spec x -> map_throw_specifier env x
      )
    ) v3
  in
  let v4 =
    (match v4 with
    | Some x -> map_trailing_return_type env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_abstract_array_declarator (env : env) ((v1, v2, v3, v4, v5) : CST.abstract_array_declarator) =
  let v1 =
    (match v1 with
    | Some x -> map_abstract_declarator env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "[" *) in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    (match v4 with
    | Some x -> map_anon_choice_exp_ env x
    | None -> todo env ())
  in
  let v5 = token env v5 (* "]" *) in
  todo env (v1, v2, v3, v4, v5)

and map_operator_cast (env : env) ((v1, v2, v3, v4) : CST.operator_cast) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_anon_choice_stmt_id3 env x
          | None -> todo env ())
        in
        let v2 = token env v2 (* "::" *) in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "operator" *) in
  let v3 = map_declaration_specifiers env v3 in
  let v4 = map_abstract_declarator env v4 in
  todo env (v1, v2, v3, v4)

and map_anon_choice_init_pair (env : env) (x : CST.anon_choice_init_pair) =
  (match x with
  | `Init_pair (v1, v2, v3) ->
      let v1 =
        List.map (fun x ->
          (match x with
          | `Subs_desi x -> map_subscript_designator env x
          | `Field_desi x -> map_field_designator env x
          )
        ) v1
      in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_anon_choice_exp env v3 in
      todo env (v1, v2, v3)
  | `Exp x -> map_expression env x
  | `Init_list x -> map_initializer_list env x
  )

and map_preproc_if (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) =
  let v1 = blank env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = token env v3 (* "\n" *) in
  let v4 = map_translation_unit env v4 in
  let v5 =
    (match v5 with
    | Some x -> map_anon_choice_prep_else env x
    | None -> todo env ())
  in
  let v6 = blank env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_function_declarator (env : env) ((v1, v2, v3, v4) : CST.function_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = map_parameter_list env v2 in
  let v3 = List.map (map_attribute_specifier env) v3 in
  let v4 = List.map (map_anon_choice_type_qual env) v4 in
  todo env (v1, v2, v3, v4)

and map_declaration (env : env) ((v1, v2, v3, v4, v5) : CST.declaration) =
  let v1 = List.map (map_attribute env) v1 in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_anon_choice_decl env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_anon_choice_decl env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = token env v5 (* ";" *) in
  todo env (v1, v2, v3, v4, v5)

and map_lambda_capture_specifier (env : env) ((v1, v2, v3) : CST.lambda_capture_specifier) =
  let v1 = token env v1 (* "[" *) in
  let v2 =
    (match v2 with
    | `Lambda_defa_capt x -> map_lambda_default_capture env x
    | `Opt_exp_rep_COMMA_exp opt ->
        (match opt with
        | Some (v1, v2) ->
            let v1 = map_expression env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_expression env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
    | `Lambda_defa_capt_COMMA_exp_rep_COMMA_exp (v1, v2, v3, v4) ->
        let v1 = map_lambda_default_capture env v1 in
        let v2 = token env v2 (* "," *) in
        let v3 = map_expression env v3 in
        let v4 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
          ) v4
        in
        todo env (v1, v2, v3, v4)
    )
  in
  let v3 = token env v3 (* "]" *) in
  todo env (v1, v2, v3)

and map_field_initializer_list (env : env) ((v1, v2, v3) : CST.field_initializer_list) =
  let v1 = token env v1 (* ":" *) in
  let v2 = map_field_initializer env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_field_initializer env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_scoped_namespace_identifier (env : env) ((v1, v2, v3) : CST.scoped_namespace_identifier) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_stmt_id3 env x
    | None -> todo env ())
  in
  let v2 = token env v2 (* "::" *) in
  let v3 = token env v3 (* pattern [a-zA-Z_]\w* *) in
  todo env (v1, v2, v3)

and map_parenthesized_field_declarator (env : env) ((v1, v2, v3) : CST.parenthesized_field_declarator) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_field_declarator env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)
