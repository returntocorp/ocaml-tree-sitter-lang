(**
   Boilerplate to be used as a template when mapping the ruby CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let token (_tok : Tree_sitter_run.Token.t) = failwith "not implemented"
let blank () = failwith "not implemented"
let todo _ = failwith "not implemented"

let map_uninterpreted (tok : CST.uninterpreted) =
  token tok

let map_binary_star (tok : CST.binary_star) =
  token tok

let map_singleton_class_left_angle_left_langle (tok : CST.singleton_class_left_angle_left_langle) =
  token tok

let map_instance_variable (tok : CST.instance_variable) =
  token tok

let map_binary_minus (tok : CST.binary_minus) =
  token tok

let map_simple_symbol (tok : CST.simple_symbol) =
  token tok

let map_complex (tok : CST.complex) =
  token tok

let map_character (tok : CST.character) =
  token tok

let map_escape_sequence (tok : CST.escape_sequence) =
  token tok

let map_false_ (x : CST.false_) =
  (match x with
  | `False_false tok -> token tok
  | `False_FALSE tok -> token tok
  )

let map_subshell_start (tok : CST.subshell_start) =
  token tok

let map_regex_start (tok : CST.regex_start) =
  token tok

let map_constant (tok : CST.constant) =
  token tok

let map_symbol_start (tok : CST.symbol_start) =
  token tok

let map_unary_minus (tok : CST.unary_minus) =
  token tok

let map_block_ampersand (tok : CST.block_ampersand) =
  token tok

let map_class_variable (tok : CST.class_variable) =
  token tok

let map_string_array_start (tok : CST.string_array_start) =
  token tok

let map_splat_star (tok : CST.splat_star) =
  token tok

let map_integer (tok : CST.integer) =
  token tok

let map_heredoc_content (tok : CST.heredoc_content) =
  token tok

let map_string_end (tok : CST.string_end) =
  token tok

let map_line_break (tok : CST.line_break) =
  token tok

let map_identifier (tok : CST.identifier) =
  token tok

let map_string_content (tok : CST.string_content) =
  token tok

let map_heredoc_end (tok : CST.heredoc_end) =
  token tok

let map_nil (x : CST.nil) =
  (match x with
  | `Nil_nil tok -> token tok
  | `Nil_NIL tok -> token tok
  )

let map_heredoc_beginning (tok : CST.heredoc_beginning) =
  token tok

let map_float_ (tok : CST.float_) =
  token tok

let map_global_variable (tok : CST.global_variable) =
  token tok

let map_symbol_array_start (tok : CST.symbol_array_start) =
  token tok

let map_heredoc_body_start (tok : CST.heredoc_body_start) =
  token tok

let map_operator (x : CST.operator) =
  (match x with
  | `Op_DOTDOT tok -> token tok
  | `Op_BAR tok -> token tok
  | `Op_HAT tok -> token tok
  | `Op_AMP tok -> token tok
  | `Op_LTEQGT tok -> token tok
  | `Op_EQEQ tok -> token tok
  | `Op_EQEQEQ tok -> token tok
  | `Op_EQTILDE tok -> token tok
  | `Op_GT tok -> token tok
  | `Op_GTEQ tok -> token tok
  | `Op_LT tok -> token tok
  | `Op_LTEQ tok -> token tok
  | `Op_PLUS tok -> token tok
  | `Op_DASH tok -> token tok
  | `Op_STAR tok -> token tok
  | `Op_SLASH tok -> token tok
  | `Op_PERC tok -> token tok
  | `Op_BANG tok -> token tok
  | `Op_BANGTILDE tok -> token tok
  | `Op_STARSTAR tok -> token tok
  | `Op_LTLT tok -> token tok
  | `Op_GTGT tok -> token tok
  | `Op_TILDE tok -> token tok
  | `Op_PLUSAT tok -> token tok
  | `Op_DASHAT tok -> token tok
  | `Op_LBRACKRBRACK tok -> token tok
  | `Op_LBRACKRBRACKEQ tok -> token tok
  | `Op_BQUOT tok -> token tok
  )

let map_true_ (x : CST.true_) =
  (match x with
  | `True_true tok -> token tok
  | `True_TRUE tok -> token tok
  )

let map_string_start (tok : CST.string_start) =
  token tok

let map_identifier_hash_key (tok : CST.identifier_hash_key) =
  token tok

let map_terminator (x : CST.terminator) =
  (match x with
  | `Term_line_brk tok -> token tok
  | `Term_SEMI tok -> token tok
  )

let map_variable (x : CST.variable) =
  (match x with
  | `Self tok -> token tok
  | `Super tok -> token tok
  | `Inst_var tok -> token tok
  | `Class_var tok -> token tok
  | `Glob_var tok -> token tok
  | `Id tok -> token tok
  | `Cst tok -> token tok
  )

let map_do_ (x : CST.do_) =
  (match x with
  | `Do_do tok -> token tok
  | `Do_term x -> map_terminator x
  )

let rec map_statements (x : CST.statements) =
  (match x with
  | `Stmts_rep1_choice_stmt_term_opt_stmt (v1, v2) ->
      let v1 =
        List.map (fun x ->
          (match x with
          | `Stmt_term (v1, v2) ->
              let v1 = map_statement v1 in
              let v2 = map_terminator v2 in
              todo (v1, v2)
          | `Empty_stmt tok -> token tok
          )
        ) v1
      in
      let v2 =
        (match v2 with
        | Some x -> map_statement x
        | None -> todo ())
      in
      todo (v1, v2)
  | `Stmts_stmt x -> map_statement x
  )

and map_statement (x : CST.statement) =
  (match x with
  | `Stmt_undef (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 = map_method_name v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token v1 in
          let v2 = map_method_name v2 in
          todo (v1, v2)
        ) v3
      in
      todo (v1, v2, v3)
  | `Stmt_alias (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 = map_method_name v2 in
      let v3 = map_method_name v3 in
      todo (v1, v2, v3)
  | `Stmt_if_modi (v1, v2, v3) ->
      let v1 = map_statement v1 in
      let v2 = token v2 in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Stmt_unle_modi (v1, v2, v3) ->
      let v1 = map_statement v1 in
      let v2 = token v2 in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Stmt_while_modi (v1, v2, v3) ->
      let v1 = map_statement v1 in
      let v2 = token v2 in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Stmt_until_modi (v1, v2, v3) ->
      let v1 = map_statement v1 in
      let v2 = token v2 in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Stmt_resc_modi (v1, v2, v3) ->
      let v1 = map_statement v1 in
      let v2 = token v2 in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Stmt_begin_blk (v1, v2, v3, v4) ->
      let v1 = token v1 in
      let v2 = token v2 in
      let v3 =
        (match v3 with
        | Some x -> map_statements x
        | None -> todo ())
      in
      let v4 = token v4 in
      todo (v1, v2, v3, v4)
  | `Stmt_end_blk (v1, v2, v3, v4) ->
      let v1 = token v1 in
      let v2 = token v2 in
      let v3 =
        (match v3 with
        | Some x -> map_statements x
        | None -> todo ())
      in
      let v4 = token v4 in
      todo (v1, v2, v3, v4)
  | `Stmt_exp x -> map_expression x
  )

and map_method_rest ((v1, v2, v3) : CST.method_rest) =
  let v1 = map_method_name v1 in
  let v2 =
    (match v2 with
    | `Params_opt_term (v1, v2) ->
        let v1 = map_parameters v1 in
        let v2 =
          (match v2 with
          | Some x -> map_terminator x
          | None -> todo ())
        in
        todo (v1, v2)
    | `Opt_bare_params_term (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_bare_parameters x
          | None -> todo ())
        in
        let v2 = map_terminator v2 in
        todo (v1, v2)
    )
  in
  let v3 = map_body_statement v3 in
  todo (v1, v2, v3)

and map_parameters ((v1, v2, v3) : CST.parameters) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_formal_parameter v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 in
            let v2 = map_formal_parameter v2 in
            todo (v1, v2)
          ) v2
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v3 = token v3 in
  todo (v1, v2, v3)

and map_bare_parameters ((v1, v2) : CST.bare_parameters) =
  let v1 = map_simple_formal_parameter v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_formal_parameter v2 in
      todo (v1, v2)
    ) v2
  in
  todo (v1, v2)

and map_block_parameters ((v1, v2, v3, v4, v5) : CST.block_parameters) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_formal_parameter v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 in
            let v2 = map_formal_parameter v2 in
            todo (v1, v2)
          ) v2
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token tok
    | None -> todo ())
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) ->
        let v1 = token v1 in
        let v2 = token v2 in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = token v1 in
            let v2 = token v2 in
            todo (v1, v2)
          ) v3
        in
        todo (v1, v2, v3)
    | None -> todo ())
  in
  let v5 = token v5 in
  todo (v1, v2, v3, v4, v5)

and map_formal_parameter (x : CST.formal_parameter) =
  (match x with
  | `Form_param_simple_form_param x ->
      map_simple_formal_parameter x
  | `Form_param_params x -> map_parameters x
  )

and map_simple_formal_parameter (x : CST.simple_formal_parameter) =
  (match x with
  | `Simple_form_param_id tok -> token tok
  | `Simple_form_param_splat_param (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some tok -> token tok
        | None -> todo ())
      in
      todo (v1, v2)
  | `Simple_form_param_hash_splat_param (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some tok -> token tok
        | None -> todo ())
      in
      todo (v1, v2)
  | `Simple_form_param_blk_param (v1, v2) ->
      let v1 = token v1 in
      let v2 = token v2 in
      todo (v1, v2)
  | `Simple_form_param_kw_param (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 = token v2 in
      let v3 =
        (match v3 with
        | Some x -> map_arg x
        | None -> todo ())
      in
      todo (v1, v2, v3)
  | `Simple_form_param_opt_param (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  )

and map_superclass ((v1, v2) : CST.superclass) =
  let v1 = token v1 in
  let v2 = map_arg v2 in
  todo (v1, v2)

and map_in_ ((v1, v2) : CST.in_) =
  let v1 = token v1 in
  let v2 = map_arg v2 in
  todo (v1, v2)

and map_when_ ((v1, v2, v3, v4) : CST.when_) =
  let v1 = token v1 in
  let v2 = map_pattern v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_pattern v2 in
      todo (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | `Term x -> map_terminator x
    | `Then x -> map_then_ x
    )
  in
  todo (v1, v2, v3, v4)

and map_pattern (x : CST.pattern) =
  (match x with
  | `Pat_arg x -> map_arg x
  | `Pat_splat_arg x -> map_splat_argument x
  )

and map_elsif ((v1, v2, v3, v4) : CST.elsif) =
  let v1 = token v1 in
  let v2 = map_statement v2 in
  let v3 =
    (match v3 with
    | `Term x -> map_terminator x
    | `Then x -> map_then_ x
    )
  in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `Else x -> map_else_ x
        | `Elsif x -> map_elsif x
        )
    | None -> todo ())
  in
  todo (v1, v2, v3, v4)

and map_else_ ((v1, v2, v3) : CST.else_) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_terminator x
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_statements x
    | None -> todo ())
  in
  todo (v1, v2, v3)

and map_then_ (x : CST.then_) =
  (match x with
  | `Then_term_stmts (v1, v2) ->
      let v1 = map_terminator v1 in
      let v2 = map_statements v2 in
      todo (v1, v2)
  | `Then_opt_term_then_opt_stmts (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_terminator x
        | None -> todo ())
      in
      let v2 = token v2 in
      let v3 =
        (match v3 with
        | Some x -> map_statements x
        | None -> todo ())
      in
      todo (v1, v2, v3)
  )

and map_ensure ((v1, v2) : CST.ensure) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_statements x
    | None -> todo ())
  in
  todo (v1, v2)

and map_rescue ((v1, v2, v3, v4) : CST.rescue) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_exceptions x
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_exception_variable x
    | None -> todo ())
  in
  let v4 =
    (match v4 with
    | `Term x -> map_terminator x
    | `Then x -> map_then_ x
    )
  in
  todo (v1, v2, v3, v4)

and map_exceptions ((v1, v2) : CST.exceptions) =
  let v1 =
    (match v1 with
    | `Arg x -> map_arg x
    | `Splat_arg x -> map_splat_argument x
    )
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | `Arg x -> map_arg x
        | `Splat_arg x -> map_splat_argument x
        )
      in
      todo (v1, v2)
    ) v2
  in
  todo (v1, v2)

and map_exception_variable ((v1, v2) : CST.exception_variable) =
  let v1 = token v1 in
  let v2 = map_lhs v2 in
  todo (v1, v2)

and map_body_statement ((v1, v2, v3) : CST.body_statement) =
  let v1 =
    (match v1 with
    | Some x -> map_statements x
    | None -> todo ())
  in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Resc x -> map_rescue x
      | `Else x -> map_else_ x
      | `Ensu x -> map_ensure x
      )
    ) v2
  in
  let v3 = token v3 in
  todo (v1, v2, v3)

and map_expression (x : CST.expression) =
  (match x with
  | `Exp_cmd_bin (v1, v2, v3) ->
      let v1 = map_expression v1 in
      let v2 =
        (match v2 with
        | `Or tok -> token tok
        | `And tok -> token tok
        )
      in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Exp_cmd_assign x -> map_command_assignment x
  | `Exp_cmd_op_assign (v1, v2, v3) ->
      let v1 = map_lhs v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> token tok
        | `DASHEQ tok -> token tok
        | `STAREQ tok -> token tok
        | `STARSTAREQ tok -> token tok
        | `SLASHEQ tok -> token tok
        | `BARBAREQ tok -> token tok
        | `BAREQ tok -> token tok
        | `AMPAMPEQ tok -> token tok
        | `AMPEQ tok -> token tok
        | `PERCEQ tok -> token tok
        | `GTGTEQ tok -> token tok
        | `LTLTEQ tok -> token tok
        | `HATEQ tok -> token tok
        )
      in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  | `Exp_cmd_call x -> map_command_call x
  | `Exp_ret_cmd (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_command_argument_list v2 in
      todo (v1, v2)
  | `Exp_yield_cmd (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_command_argument_list v2 in
      todo (v1, v2)
  | `Exp_brk_cmd (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_command_argument_list v2 in
      todo (v1, v2)
  | `Exp_next_cmd (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_command_argument_list v2 in
      todo (v1, v2)
  | `Exp_arg x -> map_arg x
  )

and map_arg (x : CST.arg) =
  (match x with
  | `Arg_prim x -> map_primary x
  | `Arg_assign x -> map_assignment x
  | `Arg_op_assign (v1, v2, v3) ->
      let v1 = map_lhs v1 in
      let v2 =
        (match v2 with
        | `PLUSEQ tok -> token tok
        | `DASHEQ tok -> token tok
        | `STAREQ tok -> token tok
        | `STARSTAREQ tok -> token tok
        | `SLASHEQ tok -> token tok
        | `BARBAREQ tok -> token tok
        | `BAREQ tok -> token tok
        | `AMPAMPEQ tok -> token tok
        | `AMPEQ tok -> token tok
        | `PERCEQ tok -> token tok
        | `GTGTEQ tok -> token tok
        | `LTLTEQ tok -> token tok
        | `HATEQ tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Arg_cond (v1, v2, v3, v4, v5) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      let v4 = token v4 in
      let v5 = map_arg v5 in
      todo (v1, v2, v3, v4, v5)
  | `Arg_range (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> token tok
        | `DOTDOTDOT tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Arg_bin x -> map_binary x
  | `Arg_un x -> map_unary x
  )

and map_primary (x : CST.primary) =
  (match x with
  | `Prim_paren_stmts x -> map_parenthesized_statements x
  | `Prim_lhs x -> map_lhs x
  | `Prim_array (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list_with_trailing_comma x
        | None -> todo ())
      in
      let v3 = token v3 in
      todo (v1, v2, v3)
  | `Prim_str_array (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some () -> todo ()
        | None -> todo ())
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = map_literal_contents v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = blank v1 in
                let v2 = map_literal_contents v2 in
                todo (v1, v2)
              ) v2
            in
            todo (v1, v2)
        | None -> todo ())
      in
      let v4 =
        (match v4 with
        | Some () -> todo ()
        | None -> todo ())
      in
      let v5 = token v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_symb_array (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some () -> todo ()
        | None -> todo ())
      in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = map_literal_contents v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = blank v1 in
                let v2 = map_literal_contents v2 in
                todo (v1, v2)
              ) v2
            in
            todo (v1, v2)
        | None -> todo ())
      in
      let v4 =
        (match v4 with
        | Some () -> todo ()
        | None -> todo ())
      in
      let v5 = token v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_hash (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) ->
            let v1 =
              (match v1 with
              | `Pair x -> map_pair x
              | `Hash_splat_arg x -> map_hash_splat_argument x
              )
            in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token v1 in
                let v2 =
                  (match v2 with
                  | `Pair x -> map_pair x
                  | `Hash_splat_arg x -> map_hash_splat_argument x
                  )
                in
                todo (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> token tok
              | None -> todo ())
            in
            todo (v1, v2, v3)
        | None -> todo ())
      in
      let v3 = token v3 in
      todo (v1, v2, v3)
  | `Prim_subs (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_literal_contents x
        | None -> todo ())
      in
      let v3 = token v3 in
      todo (v1, v2, v3)
  | `Prim_symb x -> map_symbol x
  | `Prim_int tok -> token tok
  | `Prim_float tok -> token tok
  | `Prim_comp tok -> token tok
  | `Prim_rati (v1, v2) ->
      let v1 = token v1 in
      let v2 = token v2 in
      todo (v1, v2)
  | `Prim_str x -> map_string_ x
  | `Prim_char tok -> token tok
  | `Prim_chai_str (v1, v2) ->
      let v1 = map_string_ v1 in
      let v2 = List.map map_string_ v2 in
      todo (v1, v2)
  | `Prim_regex (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_literal_contents x
        | None -> todo ())
      in
      let v3 = token v3 in
      todo (v1, v2, v3)
  | `Prim_lamb (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Params x -> map_parameters x
            | `Bare_params x -> map_bare_parameters x
            )
        | None -> todo ())
      in
      let v3 =
        (match v3 with
        | `Blk x -> map_block x
        | `Do_blk x -> map_do_block x
        )
      in
      todo (v1, v2, v3)
  | `Prim_meth (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_method_rest v2 in
      todo (v1, v2)
  | `Prim_sing_meth (v1, v2, v3, v4) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | `Var x -> map_variable x
        | `LPAR_arg_RPAR (v1, v2, v3) ->
            let v1 = token v1 in
            let v2 = map_arg v2 in
            let v3 = token v3 in
            todo (v1, v2, v3)
        )
      in
      let v3 =
        (match v3 with
        | `DOT tok -> token tok
        | `COLONCOLON tok -> token tok
        )
      in
      let v4 = map_method_rest v4 in
      todo (v1, v2, v3, v4)
  | `Prim_class (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | `Cst tok -> token tok
        | `Scope_resol x -> map_scope_resolution x
        )
      in
      let v3 =
        (match v3 with
        | Some x -> map_superclass x
        | None -> todo ())
      in
      let v4 = map_terminator v4 in
      let v5 = map_body_statement v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_sing_class (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      let v4 = map_terminator v4 in
      let v5 = map_body_statement v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_modu (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | `Cst tok -> token tok
        | `Scope_resol x -> map_scope_resolution x
        )
      in
      let v3 =
        (match v3 with
        | `Term_body_stmt (v1, v2) ->
            let v1 = map_terminator v1 in
            let v2 = map_body_statement v2 in
            todo (v1, v2)
        | `End tok -> token tok
        )
      in
      todo (v1, v2, v3)
  | `Prim_begin (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_terminator x
        | None -> todo ())
      in
      let v3 = map_body_statement v3 in
      todo (v1, v2, v3)
  | `Prim_while (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 = map_arg v2 in
      let v3 = map_do_ v3 in
      let v4 =
        (match v4 with
        | Some x -> map_statements x
        | None -> todo ())
      in
      let v5 = token v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_until (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 = map_arg v2 in
      let v3 = map_do_ v3 in
      let v4 =
        (match v4 with
        | Some x -> map_statements x
        | None -> todo ())
      in
      let v5 = token v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_if (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 = map_statement v2 in
      let v3 =
        (match v3 with
        | `Term x -> map_terminator x
        | `Then x -> map_then_ x
        )
      in
      let v4 =
        (match v4 with
        | Some x ->
            (match x with
            | `Else x -> map_else_ x
            | `Elsif x -> map_elsif x
            )
        | None -> todo ())
      in
      let v5 = token v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_unle (v1, v2, v3, v4, v5) ->
      let v1 = token v1 in
      let v2 = map_statement v2 in
      let v3 =
        (match v3 with
        | `Term x -> map_terminator x
        | `Then x -> map_then_ x
        )
      in
      let v4 =
        (match v4 with
        | Some x ->
            (match x with
            | `Else x -> map_else_ x
            | `Elsif x -> map_elsif x
            )
        | None -> todo ())
      in
      let v5 = token v5 in
      todo (v1, v2, v3, v4, v5)
  | `Prim_for (v1, v2, v3, v4, v5, v6) ->
      let v1 = token v1 in
      let v2 = map_mlhs v2 in
      let v3 = map_in_ v3 in
      let v4 = map_do_ v4 in
      let v5 =
        (match v5 with
        | Some x -> map_statements x
        | None -> todo ())
      in
      let v6 = token v6 in
      todo (v1, v2, v3, v4, v5, v6)
  | `Prim_case (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_arg x
        | None -> todo ())
      in
      let v3 = map_terminator v3 in
      let v4 = List.map token v4 in
      let v5 = List.map map_when_ v5 in
      let v6 =
        (match v6 with
        | Some x -> map_else_ x
        | None -> todo ())
      in
      let v7 = token v7 in
      todo (v1, v2, v3, v4, v5, v6, v7)
  | `Prim_ret (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list x
        | None -> todo ())
      in
      todo (v1, v2)
  | `Prim_yield (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list x
        | None -> todo ())
      in
      todo (v1, v2)
  | `Prim_brk (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list x
        | None -> todo ())
      in
      todo (v1, v2)
  | `Prim_next (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list x
        | None -> todo ())
      in
      todo (v1, v2)
  | `Prim_redo (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list x
        | None -> todo ())
      in
      todo (v1, v2)
  | `Prim_retry (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list x
        | None -> todo ())
      in
      todo (v1, v2)
  | `Prim_paren_un (v1, v2) ->
      let v1 =
        (match v1 with
        | `Defi tok -> token tok
        | `Not tok -> token tok
        )
      in
      let v2 = map_parenthesized_statements v2 in
      todo (v1, v2)
  | `Prim_un_lit (v1, v2) ->
      let v1 =
        (match v1 with
        | `Un_minus tok -> token tok
        | `PLUS tok -> token tok
        )
      in
      let v2 =
        (match v2 with
        | `Int tok -> token tok
        | `Float tok -> token tok
        )
      in
      todo (v1, v2)
  | `Prim_here_begin tok -> token tok
  )

and map_parenthesized_statements ((v1, v2, v3) : CST.parenthesized_statements) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_statements x
    | None -> todo ())
  in
  let v3 = token v3 in
  todo (v1, v2, v3)

and map_scope_resolution ((v1, v2) : CST.scope_resolution) =
  let v1 =
    (match v1 with
    | `COLONCOLON tok -> token tok
    | `Prim_COLONCOLON (v1, v2) ->
        let v1 = map_primary v1 in
        let v2 = token v2 in
        todo (v1, v2)
    )
  in
  let v2 =
    (match v2 with
    | `Id tok -> token tok
    | `Cst tok -> token tok
    )
  in
  todo (v1, v2)

and map_call ((v1, v2, v3) : CST.call) =
  let v1 = map_primary v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> token tok
    | `AMPDOT tok -> token tok
    )
  in
  let v3 =
    (match v3 with
    | `Id tok -> token tok
    | `Op x -> map_operator x
    | `Cst tok -> token tok
    | `Arg_list x -> map_argument_list x
    )
  in
  todo (v1, v2, v3)

and map_command_call (x : CST.command_call) =
  (match x with
  | `Cmd_call_choice_var_cmd_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_command_argument_list v2 in
      todo (v1, v2)
  | `Cmd_call_choice_var_cmd_arg_list_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_command_argument_list v2 in
      let v3 = map_block v3 in
      todo (v1, v2, v3)
  | `Cmd_call_choice_var_cmd_arg_list_do_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_command_argument_list v2 in
      let v3 = map_do_block v3 in
      todo (v1, v2, v3)
  )

and map_method_call (x : CST.method_call) =
  (match x with
  | `Meth_call_choice_var_arg_list (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_argument_list v2 in
      todo (v1, v2)
  | `Meth_call_choice_var_arg_list_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_argument_list v2 in
      let v3 = map_block v3 in
      todo (v1, v2, v3)
  | `Meth_call_choice_var_arg_list_do_blk (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_argument_list v2 in
      let v3 = map_do_block v3 in
      todo (v1, v2, v3)
  | `Meth_call_choice_var_blk (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_block v2 in
      todo (v1, v2)
  | `Meth_call_choice_var_do_blk (v1, v2) ->
      let v1 =
        (match v1 with
        | `Var x -> map_variable x
        | `Scope_resol x -> map_scope_resolution x
        | `Call x -> map_call x
        )
      in
      let v2 = map_do_block v2 in
      todo (v1, v2)
  )

and map_command_argument_list (x : CST.command_argument_list) =
  (match x with
  | `Cmd_arg_list_arg_rep_COMMA_arg (v1, v2) ->
      let v1 = map_argument v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 = token v1 in
          let v2 = map_argument v2 in
          todo (v1, v2)
        ) v2
      in
      todo (v1, v2)
  | `Cmd_arg_list_cmd_call x -> map_command_call x
  )

and map_argument_list ((v1, v2, v3) : CST.argument_list) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_argument_list_with_trailing_comma x
    | None -> todo ())
  in
  let v3 = token v3 in
  todo (v1, v2, v3)

and map_argument_list_with_trailing_comma ((v1, v2, v3) : CST.argument_list_with_trailing_comma) =
  let v1 = map_argument v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_argument v2 in
      todo (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token tok
    | None -> todo ())
  in
  todo (v1, v2, v3)

and map_argument (x : CST.argument) =
  (match x with
  | `Arg_arg x -> map_arg x
  | `Arg_splat_arg x -> map_splat_argument x
  | `Arg_hash_splat_arg x -> map_hash_splat_argument x
  | `Arg_blk_arg (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_arg v2 in
      todo (v1, v2)
  | `Arg_pair x -> map_pair x
  )

and map_splat_argument ((v1, v2) : CST.splat_argument) =
  let v1 = token v1 in
  let v2 = map_arg v2 in
  todo (v1, v2)

and map_hash_splat_argument ((v1, v2) : CST.hash_splat_argument) =
  let v1 = token v1 in
  let v2 = map_arg v2 in
  todo (v1, v2)

and map_do_block ((v1, v2, v3, v4) : CST.do_block) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_terminator x
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = map_block_parameters v1 in
        let v2 =
          (match v2 with
          | Some x -> map_terminator x
          | None -> todo ())
        in
        todo (v1, v2)
    | None -> todo ())
  in
  let v4 = map_body_statement v4 in
  todo (v1, v2, v3, v4)

and map_block ((v1, v2, v3, v4) : CST.block) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_block_parameters x
    | None -> todo ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_statements x
    | None -> todo ())
  in
  let v4 = token v4 in
  todo (v1, v2, v3, v4)

and map_assignment (x : CST.assignment) =
  (match x with
  | `Choice_lhs_EQ_choice_arg (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Lhs x -> map_lhs x
        | `Left_assign_list x -> map_left_assignment_list x
        )
      in
      let v2 = token v2 in
      let v3 =
        (match v3 with
        | `Arg x -> map_arg x
        | `Splat_arg x -> map_splat_argument x
        | `Right_assign_list x -> map_right_assignment_list x
        )
      in
      todo (v1, v2, v3)
  )

and map_command_assignment (x : CST.command_assignment) =
  (match x with
  | `Choice_lhs_EQ_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Lhs x -> map_lhs x
        | `Left_assign_list x -> map_left_assignment_list x
        )
      in
      let v2 = token v2 in
      let v3 = map_expression v3 in
      todo (v1, v2, v3)
  )

and map_binary (x : CST.binary) =
  (match x with
  | `Bin_arg_and_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_or_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_BARBAR_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_AMPAMP_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_choice_LTLT_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> token tok
        | `GTGT tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_choice_LT_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 =
        (match v2 with
        | `LT tok -> token tok
        | `LTEQ tok -> token tok
        | `GT tok -> token tok
        | `GTEQ tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_AMP_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_choice_HAT_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 =
        (match v2 with
        | `HAT tok -> token tok
        | `BAR tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_choice_PLUS_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> token tok
        | `Bin_minus tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_choice_SLASH_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 =
        (match v2 with
        | `SLASH tok -> token tok
        | `PERC tok -> token tok
        | `Bin_star tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_choice_EQEQ_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> token tok
        | `BANGEQ tok -> token tok
        | `EQEQEQ tok -> token tok
        | `LTEQGT tok -> token tok
        | `EQTILDE tok -> token tok
        | `BANGTILDE tok -> token tok
        )
      in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Bin_arg_STARSTAR_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  )

and map_unary (x : CST.unary) =
  (match x with
  | `Un_defi_arg (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_arg v2 in
      todo (v1, v2)
  | `Un_not_arg (v1, v2) ->
      let v1 = token v1 in
      let v2 = map_arg v2 in
      todo (v1, v2)
  | `Un_choice_un_minus_arg (v1, v2) ->
      let v1 =
        (match v1 with
        | `Un_minus tok -> token tok
        | `PLUS tok -> token tok
        )
      in
      let v2 = map_arg v2 in
      todo (v1, v2)
  | `Un_choice_BANG_arg (v1, v2) ->
      let v1 =
        (match v1 with
        | `BANG tok -> token tok
        | `TILDE tok -> token tok
        )
      in
      let v2 = map_arg v2 in
      todo (v1, v2)
  )

and map_right_assignment_list ((v1, v2) : CST.right_assignment_list) =
  let v1 =
    (match v1 with
    | `Arg x -> map_arg x
    | `Splat_arg x -> map_splat_argument x
    )
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | `Arg x -> map_arg x
        | `Splat_arg x -> map_splat_argument x
        )
      in
      todo (v1, v2)
    ) v2
  in
  todo (v1, v2)

and map_left_assignment_list (x : CST.left_assignment_list) =
  map_mlhs x

and map_mlhs ((v1, v2, v3) : CST.mlhs) =
  let v1 =
    (match v1 with
    | `Lhs x -> map_lhs x
    | `Rest_assign x -> map_rest_assignment x
    | `Dest_left_assign x -> map_destructured_left_assignment x
    )
  in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | `Lhs x -> map_lhs x
        | `Rest_assign x -> map_rest_assignment x
        | `Dest_left_assign x -> map_destructured_left_assignment x
        )
      in
      todo (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token tok
    | None -> todo ())
  in
  todo (v1, v2, v3)

and map_destructured_left_assignment ((v1, v2, v3) : CST.destructured_left_assignment) =
  let v1 = token v1 in
  let v2 = map_mlhs v2 in
  let v3 = token v3 in
  todo (v1, v2, v3)

and map_rest_assignment ((v1, v2) : CST.rest_assignment) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_lhs x
    | None -> todo ())
  in
  todo (v1, v2)

and map_lhs (x : CST.lhs) =
  (match x with
  | `Var x -> map_variable x
  | `True x -> map_true_ x
  | `False x -> map_false_ x
  | `Nil x -> map_nil x
  | `Scope_resol x -> map_scope_resolution x
  | `Elem_ref (v1, v2, v3, v4) ->
      let v1 = map_primary v1 in
      let v2 = token v2 in
      let v3 =
        (match v3 with
        | Some x -> map_argument_list_with_trailing_comma x
        | None -> todo ())
      in
      let v4 = token v4 in
      todo (v1, v2, v3, v4)
  | `Call x -> map_call x
  | `Meth_call x -> map_method_call x
  )

and map_method_name (x : CST.method_name) =
  (match x with
  | `Meth_name_id tok -> token tok
  | `Meth_name_cst tok -> token tok
  | `Meth_name_sett (v1, v2) ->
      let v1 = token v1 in
      let v2 = token v2 in
      todo (v1, v2)
  | `Meth_name_symb x -> map_symbol x
  | `Meth_name_op x -> map_operator x
  | `Meth_name_inst_var tok -> token tok
  | `Meth_name_class_var tok -> token tok
  | `Meth_name_glob_var tok -> token tok
  )

and map_interpolation ((v1, v2, v3) : CST.interpolation) =
  let v1 = token v1 in
  let v2 = map_statement v2 in
  let v3 = token v3 in
  todo (v1, v2, v3)

and map_string_ ((v1, v2, v3) : CST.string_) =
  let v1 = token v1 in
  let v2 =
    (match v2 with
    | Some x -> map_literal_contents x
    | None -> todo ())
  in
  let v3 = token v3 in
  todo (v1, v2, v3)

and map_symbol (x : CST.symbol) =
  (match x with
  | `Symb_simple_symb tok -> token tok
  | `Symb_symb_start_opt_lit_content_str_end (v1, v2, v3) ->
      let v1 = token v1 in
      let v2 =
        (match v2 with
        | Some x -> map_literal_contents x
        | None -> todo ())
      in
      let v3 = token v3 in
      todo (v1, v2, v3)
  )

and map_literal_contents (xs : CST.literal_contents) =
  List.map (fun x ->
    (match x with
    | `Str_content tok -> token tok
    | `Interp x -> map_interpolation x
    | `Esc_seq tok -> token tok
    )
  ) xs

and map_pair (x : CST.pair) =
  (match x with
  | `Pair_arg_EQGT_arg (v1, v2, v3) ->
      let v1 = map_arg v1 in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  | `Pair_choice_id_hash_key_COLON_arg (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id_hash_key tok -> token tok
        | `Id tok -> token tok
        | `Cst tok -> token tok
        | `Str x -> map_string_ x
        )
      in
      let v2 = token v2 in
      let v3 = map_arg v3 in
      todo (v1, v2, v3)
  )

let map_program ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some x -> map_statements x
    | None -> todo ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token v1 in
        let v2 = token v2 in
        let v3 = token v3 in
        todo (v1, v2, v3)
    | None -> todo ())
  in
  todo (v1, v2)

