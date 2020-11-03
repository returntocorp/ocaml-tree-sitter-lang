(**
   Boilerplate to be used as a template when mapping the ruby CST
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

let map_heredoc_content (env : env) (tok : CST.heredoc_content) =
  token env tok (* heredoc_content *)

let map_string_array_start (env : env) (tok : CST.string_array_start) =
  token env tok (* string_array_start *)

let map_true_ (env : env) (x : CST.true_) =
  (match x with
  | `True tok -> token env tok (* "true" *)
  | `TRUE tok -> token env tok (* "TRUE" *)
  )

let map_heredoc_beginning (env : env) (tok : CST.heredoc_beginning) =
  token env tok (* heredoc_beginning *)

let map_line_break (env : env) (tok : CST.line_break) =
  token env tok (* line_break *)

let map_global_variable (env : env) (tok : CST.global_variable) =
  token env tok (* pattern "\\$-?(([!@&`'+~=/\\\\,;.<>*$?:\"])|([0-9]*\
  )|([a-zA-Z_][a-zA-Z0-9_]*\
  ))" *)

let map_anon_choice_PLUSEQ_6a24756 (env : env) (x : CST.anon_choice_PLUSEQ_6a24756) =
  (match x with
  | `PLUSEQ tok -> token env tok (* "+=" *)
  | `DASHEQ tok -> token env tok (* "-=" *)
  | `STAREQ tok -> token env tok (* "*=" *)
  | `STARSTAREQ tok -> token env tok (* "**=" *)
  | `SLASHEQ tok -> token env tok (* "/=" *)
  | `BARBAREQ tok -> token env tok (* "||=" *)
  | `BAREQ tok -> token env tok (* "|=" *)
  | `AMPAMPEQ tok -> token env tok (* "&&=" *)
  | `AMPEQ tok -> token env tok (* "&=" *)
  | `PERCEQ tok -> token env tok (* "%=" *)
  | `GTGTEQ tok -> token env tok (* ">>=" *)
  | `LTLTEQ tok -> token env tok (* "<<=" *)
  | `HATEQ tok -> token env tok (* "^=" *)
  )

let map_identifier_hash_key (env : env) (tok : CST.identifier_hash_key) =
  token env tok (* identifier_hash_key *)

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  token env tok (* heredoc_end *)

let map_string_end (env : env) (tok : CST.string_end) =
  token env tok (* string_end *)

let map_subshell_start (env : env) (tok : CST.subshell_start) =
  token env tok (* subshell_start *)

let map_symbol_start (env : env) (tok : CST.symbol_start) =
  token env tok (* symbol_start *)

let map_splat_star (env : env) (tok : CST.splat_star) =
  token env tok (* splat_star *)

let map_binary_star (env : env) (tok : CST.binary_star) =
  token env tok (* binary_star *)

let map_block_ampersand (env : env) (tok : CST.block_ampersand) =
  token env tok (* block_ampersand *)

let map_instance_variable (env : env) (tok : CST.instance_variable) =
  token env tok (* instance_variable *)

let map_binary_minus (env : env) (tok : CST.binary_minus) =
  token env tok (* binary_minus *)

let map_float_ (env : env) (tok : CST.float_) =
  token env tok (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* identifier *)

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  token env tok (* escape_sequence *)

let map_heredoc_body_start (env : env) (tok : CST.heredoc_body_start) =
  token env tok (* heredoc_body_start *)

let map_nil (env : env) (x : CST.nil) =
  (match x with
  | `Nil tok -> token env tok (* "nil" *)
  | `NIL tok -> token env tok (* "NIL" *)
  )

let map_false_ (env : env) (x : CST.false_) =
  (match x with
  | `False tok -> token env tok (* "false" *)
  | `FALSE tok -> token env tok (* "FALSE" *)
  )

let map_uninterpreted (env : env) (tok : CST.uninterpreted) =
  token env tok (* pattern (.|\s)* *)

let map_simple_symbol (env : env) (tok : CST.simple_symbol) =
  token env tok (* simple_symbol *)

let map_unary_minus (env : env) (tok : CST.unary_minus) =
  token env tok (* unary_minus *)

let map_regex_start (env : env) (tok : CST.regex_start) =
  token env tok (* regex_start *)

let map_complex (env : env) (tok : CST.complex) =
  token env tok (* pattern (\d+)?(\+|-)?(\d+)i *)

let map_singleton_class_left_angle_left_langle (env : env) (tok : CST.singleton_class_left_angle_left_langle) =
  token env tok (* singleton_class_left_angle_left_langle *)

let map_constant (env : env) (tok : CST.constant) =
  token env tok (* constant *)

let map_integer (env : env) (tok : CST.integer) =
  token env tok (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)

let map_string_content (env : env) (tok : CST.string_content) =
  token env tok (* string_content *)

let map_operator (env : env) (x : CST.operator) =
  (match x with
  | `DOTDOT tok -> token env tok (* ".." *)
  | `BAR tok -> token env tok (* "|" *)
  | `HAT tok -> token env tok (* "^" *)
  | `AMP tok -> token env tok (* "&" *)
  | `LTEQGT tok -> token env tok (* "<=>" *)
  | `EQEQ tok -> token env tok (* "==" *)
  | `EQEQEQ tok -> token env tok (* "===" *)
  | `EQTILDE tok -> token env tok (* "=~" *)
  | `GT tok -> token env tok (* ">" *)
  | `GTEQ tok -> token env tok (* ">=" *)
  | `LT tok -> token env tok (* "<" *)
  | `LTEQ tok -> token env tok (* "<=" *)
  | `PLUS tok -> token env tok (* "+" *)
  | `DASH tok -> token env tok (* "-" *)
  | `STAR tok -> token env tok (* "*" *)
  | `SLASH tok -> token env tok (* "/" *)
  | `PERC tok -> token env tok (* "%" *)
  | `BANG tok -> token env tok (* "!" *)
  | `BANGTILDE tok -> token env tok (* "!~" *)
  | `STARSTAR tok -> token env tok (* "**" *)
  | `LTLT tok -> token env tok (* "<<" *)
  | `GTGT tok -> token env tok (* ">>" *)
  | `TILDE tok -> token env tok (* "~" *)
  | `PLUSAT tok -> token env tok (* "+@" *)
  | `DASHAT tok -> token env tok (* "-@" *)
  | `LBRACKRBRACK tok -> token env tok (* "[]" *)
  | `LBRACKRBRACKEQ tok -> token env tok (* "[]=" *)
  | `BQUOT tok -> token env tok (* "`" *)
  )

let map_character (env : env) (tok : CST.character) =
  token env tok (* pattern \?(\\\S({[0-9]*}|[0-9]*|-\S([MC]-\S)?)?|\S) *)

let map_class_variable (env : env) (tok : CST.class_variable) =
  token env tok (* class_variable *)

let map_symbol_array_start (env : env) (tok : CST.symbol_array_start) =
  token env tok (* symbol_array_start *)

let map_pat_3d340f6 (env : env) (tok : CST.pat_3d340f6) =
  token env tok (* pattern \s+ *)

let map_string_start (env : env) (tok : CST.string_start) =
  token env tok (* string_start *)

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `Line_brk tok -> token env tok (* line_break *)
  | `SEMI tok -> token env tok (* ";" *)
  )

let map_anon_choice_un_minus_157a1bc (env : env) (x : CST.anon_choice_un_minus_157a1bc) =
  (match x with
  | `Un_minus tok -> token env tok (* unary_minus *)
  | `PLUS tok -> token env tok (* "+" *)
  )

let map_variable (env : env) (x : CST.variable) =
  (match x with
  | `Self tok -> token env tok (* "self" *)
  | `Super tok -> token env tok (* "super" *)
  | `Inst_var tok -> token env tok (* instance_variable *)
  | `Class_var tok -> token env tok (* class_variable *)
  | `Global_var tok ->
      token env tok (* pattern "\\$-?(([!@&`'+~=/\\\\,;.<>*$?:\"])|([0-9]*\
  )|([a-zA-Z_][a-zA-Z0-9_]*\
  ))" *)
  | `Id tok -> token env tok (* identifier *)
  | `Cst tok -> token env tok (* constant *)
  )

let map_do_ (env : env) (x : CST.do_) =
  (match x with
  | `Do tok -> token env tok (* "do" *)
  | `Term x -> map_terminator env x
  )

let rec map_anon_choice_cst_c1a97cb (env : env) (x : CST.anon_choice_cst_c1a97cb) =
  (match x with
  | `Cst tok -> token env tok (* constant *)
  | `Scope_resol x -> map_scope_resolution env x
  )

and map_anon_choice_else_4cfa13b (env : env) (x : CST.anon_choice_else_4cfa13b) =
  (match x with
  | `Else x -> map_else_ env x
  | `Elsif (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "elsif" *) in
      let v2 = map_statement env v2 in
      let v3 = map_anon_choice_term_b9e1843 env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_else_4cfa13b env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  )

and map_anon_choice_lhs_3a98eae (env : env) (x : CST.anon_choice_lhs_3a98eae) =
  (match x with
  | `Lhs x -> map_lhs env x
  | `Rest_assign (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 =
        (match v2 with
        | Some x -> map_lhs env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Dest_left_assign (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_left_assignment_list env v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  )

and map_anon_choice_lhs_6f12f8f (env : env) (x : CST.anon_choice_lhs_6f12f8f) =
  (match x with
  | `Lhs x -> map_lhs env x
  | `Left_assign_list x -> map_left_assignment_list env x
  )

and map_anon_choice_pair_a4f33e2 (env : env) (x : CST.anon_choice_pair_a4f33e2) =
  (match x with
  | `Pair x -> map_pair env x
  | `Hash_splat_arg x -> map_hash_splat_argument env x
  )

and map_anon_choice_term_b9e1843 (env : env) (x : CST.anon_choice_term_b9e1843) =
  (match x with
  | `Term x -> map_terminator env x
  | `Then x -> map_then_ env x
  )

and map_anon_choice_var_56bd3f5 (env : env) (x : CST.anon_choice_var_56bd3f5) =
  (match x with
  | `Var x -> map_variable env x
  | `Scope_resol x -> map_scope_resolution env x
  | `Call x -> map_call env x
  )

and map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 (env : env) ((v1, v2) : CST.anon_formal_param_rep_COMMA_formal_param_fcb57c2) =
  let v1 = map_formal_parameter env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_formal_parameter env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e (env : env) ((v1, v2) : CST.anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e) =
  let v1 = map_literal_contents env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* pattern \s+ *) in
      let v2 = map_literal_contents env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_arg (env : env) (x : CST.arg) =
  (match x with
  | `Prim x -> map_primary env x
  | `Assign x -> map_assignment env x
  | `Op_assign (v1, v2, v3) ->
      let v1 = map_lhs env v1 in
      let v2 = map_anon_choice_PLUSEQ_6a24756 env v2 in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Cond (v1, v2, v3, v4, v5) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 = map_arg env v3 in
      let v4 = token env v4 (* ":" *) in
      let v5 = map_arg env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Range (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `DOTDOT tok -> token env tok (* ".." *)
        | `DOTDOTDOT tok -> token env tok (* "..." *)
        )
      in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Bin x -> map_binary env x
  | `Un x -> map_unary env x
  )

and map_argument (env : env) (x : CST.argument) =
  (match x with
  | `Arg x -> map_arg env x
  | `Splat_arg x -> map_splat_argument env x
  | `Hash_splat_arg x -> map_hash_splat_argument env x
  | `Blk_arg (v1, v2) ->
      let v1 = token env v1 (* block_ampersand *) in
      let v2 = map_arg env v2 in
      todo env (v1, v2)
  | `Pair x -> map_pair env x
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x -> map_argument_list_with_trailing_comma env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_argument_list_with_trailing_comma (env : env) ((v1, v2, v3) : CST.argument_list_with_trailing_comma) =
  let v1 = map_argument env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_argument env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_assignment (env : env) (x : CST.assignment) =
  (match x with
  | `Choice_lhs_EQ_choice_arg (v1, v2, v3) ->
      let v1 = map_anon_choice_lhs_6f12f8f env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 =
        (match v3 with
        | `Arg x -> map_arg env x
        | `Splat_arg x -> map_splat_argument env x
        | `Right_assign_list x -> map_right_assignment_list env x
        )
      in
      todo env (v1, v2, v3)
  )

and map_bare_parameters (env : env) ((v1, v2) : CST.bare_parameters) =
  let v1 = map_simple_formal_parameter env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_formal_parameter env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_binary (env : env) (x : CST.binary) =
  (match x with
  | `Arg_and_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "and" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_or_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "or" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_BARBAR_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_AMPAMP_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_choice_LTLT_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `LTLT tok -> token env tok (* "<<" *)
        | `GTGT tok -> token env tok (* ">>" *)
        )
      in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_choice_LT_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `LT tok -> token env tok (* "<" *)
        | `LTEQ tok -> token env tok (* "<=" *)
        | `GT tok -> token env tok (* ">" *)
        | `GTEQ tok -> token env tok (* ">=" *)
        )
      in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_AMP_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_choice_HAT_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `HAT tok -> token env tok (* "^" *)
        | `BAR tok -> token env tok (* "|" *)
        )
      in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_choice_PLUS_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> token env tok (* "+" *)
        | `Bin_minus tok -> token env tok (* binary_minus *)
        )
      in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_choice_SLASH_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `SLASH tok -> token env tok (* "/" *)
        | `PERC tok -> token env tok (* "%" *)
        | `Bin_star tok -> token env tok (* binary_star *)
        )
      in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_choice_EQEQ_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 =
        (match v2 with
        | `EQEQ tok -> token env tok (* "==" *)
        | `BANGEQ tok -> token env tok (* "!=" *)
        | `EQEQEQ tok -> token env tok (* "===" *)
        | `LTEQGT tok -> token env tok (* "<=>" *)
        | `EQTILDE tok -> token env tok (* "=~" *)
        | `BANGTILDE tok -> token env tok (* "!~" *)
        )
      in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Arg_STARSTAR_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "**" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  )

and map_block (env : env) ((v1, v2, v3, v4) : CST.block) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    (match v2 with
    | Some x -> map_block_parameters env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

and map_block_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.block_parameters) =
  let v1 = token env v1 (* "|" *) in
  let v2 =
    (match v2 with
    | Some x ->
        map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* ";" *) in
        let v2 = token env v2 (* identifier *) in
        let v3 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = token env v2 (* identifier *) in
            todo env (v1, v2)
          ) v3
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v5 = token env v5 (* "|" *) in
  todo env (v1, v2, v3, v4, v5)

and map_body_statement (env : env) ((v1, v2, v3) : CST.body_statement) =
  let v1 =
    (match v1 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Rescue x -> map_rescue env x
      | `Else x -> map_else_ env x
      | `Ensure x -> map_ensure env x
      )
    ) v2
  in
  let v3 = token env v3 (* "end" *) in
  todo env (v1, v2, v3)

and map_call (env : env) ((v1, v2, v3) : CST.call) =
  let v1 = map_primary env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> token env tok (* "." *)
    | `AMPDOT tok -> token env tok (* "&." *)
    )
  in
  let v3 =
    (match v3 with
    | `Id tok -> token env tok (* identifier *)
    | `Op x -> map_operator env x
    | `Cst tok -> token env tok (* constant *)
    | `Arg_list x -> map_argument_list env x
    )
  in
  todo env (v1, v2, v3)

and map_command_argument_list (env : env) (x : CST.command_argument_list) =
  (match x with
  | `Arg_rep_COMMA_arg (v1, v2) ->
      let v1 = map_argument env v1 in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_argument env v2 in
          todo env (v1, v2)
        ) v2
      in
      todo env (v1, v2)
  | `Cmd_call x -> map_command_call env x
  )

and map_command_assignment (env : env) (x : CST.command_assignment) =
  (match x with
  | `Choice_lhs_EQ_exp (v1, v2, v3) ->
      let v1 = map_anon_choice_lhs_6f12f8f env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_command_call (env : env) (x : CST.command_call) =
  (match x with
  | `Choice_var_cmd_arg_list (v1, v2) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_command_argument_list env v2 in
      todo env (v1, v2)
  | `Choice_var_cmd_arg_list_blk (v1, v2, v3) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_command_argument_list env v2 in
      let v3 = map_block env v3 in
      todo env (v1, v2, v3)
  | `Choice_var_cmd_arg_list_do_blk (v1, v2, v3) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_command_argument_list env v2 in
      let v3 = map_do_block env v3 in
      todo env (v1, v2, v3)
  )

and map_do_block (env : env) ((v1, v2, v3, v4) : CST.do_block) =
  let v1 = token env v1 (* "do" *) in
  let v2 =
    (match v2 with
    | Some x -> map_terminator env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = map_block_parameters env v1 in
        let v2 =
          (match v2 with
          | Some x -> map_terminator env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = map_body_statement env v4 in
  todo env (v1, v2, v3, v4)

and map_else_ (env : env) ((v1, v2, v3) : CST.else_) =
  let v1 = token env v1 (* "else" *) in
  let v2 =
    (match v2 with
    | Some x -> map_terminator env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_ensure (env : env) ((v1, v2) : CST.ensure) =
  let v1 = token env v1 (* "ensure" *) in
  let v2 =
    (match v2 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_exception_variable (env : env) ((v1, v2) : CST.exception_variable) =
  let v1 = token env v1 (* "=>" *) in
  let v2 = map_lhs env v2 in
  todo env (v1, v2)

and map_exceptions (env : env) ((v1, v2) : CST.exceptions) =
  let v1 = map_pattern env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_pattern env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Cmd_bin (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `Or tok -> token env tok (* "or" *)
        | `And tok -> token env tok (* "and" *)
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Cmd_assign x -> map_command_assignment env x
  | `Cmd_op_assign (v1, v2, v3) ->
      let v1 = map_lhs env v1 in
      let v2 = map_anon_choice_PLUSEQ_6a24756 env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Cmd_call x -> map_command_call env x
  | `Ret_cmd (v1, v2) ->
      let v1 = token env v1 (* "return" *) in
      let v2 = map_command_argument_list env v2 in
      todo env (v1, v2)
  | `Yield_cmd (v1, v2) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 = map_command_argument_list env v2 in
      todo env (v1, v2)
  | `Brk_cmd (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 = map_command_argument_list env v2 in
      todo env (v1, v2)
  | `Next_cmd (v1, v2) ->
      let v1 = token env v1 (* "next" *) in
      let v2 = map_command_argument_list env v2 in
      todo env (v1, v2)
  | `Arg x -> map_arg env x
  )

and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  (match x with
  | `Simple_formal_param x ->
      map_simple_formal_parameter env x
  | `Params x -> map_parameters env x
  )

and map_hash_splat_argument (env : env) ((v1, v2) : CST.hash_splat_argument) =
  let v1 = token env v1 (* "**" *) in
  let v2 = map_arg env v2 in
  todo env (v1, v2)

and map_in_ (env : env) ((v1, v2) : CST.in_) =
  let v1 = token env v1 (* "in" *) in
  let v2 = map_arg env v2 in
  todo env (v1, v2)

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) =
  let v1 = token env v1 (* "#{" *) in
  let v2 = map_statement env v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_left_assignment_list (env : env) (x : CST.left_assignment_list) =
  map_mlhs env x

and map_lhs (env : env) (x : CST.lhs) =
  (match x with
  | `Var x -> map_variable env x
  | `True x -> map_true_ env x
  | `False x -> map_false_ env x
  | `Nil x -> map_nil env x
  | `Scope_resol x -> map_scope_resolution env x
  | `Elem_ref (v1, v2, v3, v4) ->
      let v1 = map_primary env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 =
        (match v3 with
        | Some x -> map_argument_list_with_trailing_comma env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Call x -> map_call env x
  | `Meth_call x -> map_method_call env x
  )

and map_literal_contents (env : env) (xs : CST.literal_contents) =
  List.map (fun x ->
    (match x with
    | `Str_content tok -> token env tok (* string_content *)
    | `Interp x -> map_interpolation env x
    | `Esc_seq tok -> token env tok (* escape_sequence *)
    )
  ) xs

and map_method_call (env : env) (x : CST.method_call) =
  (match x with
  | `Choice_var_arg_list (v1, v2) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_argument_list env v2 in
      todo env (v1, v2)
  | `Choice_var_arg_list_blk (v1, v2, v3) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_argument_list env v2 in
      let v3 = map_block env v3 in
      todo env (v1, v2, v3)
  | `Choice_var_arg_list_do_blk (v1, v2, v3) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_argument_list env v2 in
      let v3 = map_do_block env v3 in
      todo env (v1, v2, v3)
  | `Choice_var_blk (v1, v2) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_block env v2 in
      todo env (v1, v2)
  | `Choice_var_do_blk (v1, v2) ->
      let v1 = map_anon_choice_var_56bd3f5 env v1 in
      let v2 = map_do_block env v2 in
      todo env (v1, v2)
  )

and map_method_name (env : env) (x : CST.method_name) =
  (match x with
  | `Id tok -> token env tok (* identifier *)
  | `Cst tok -> token env tok (* constant *)
  | `Setter (v1, v2) ->
      let v1 = token env v1 (* identifier *) in
      let v2 = token env v2 (* "=" *) in
      todo env (v1, v2)
  | `Symb x -> map_symbol env x
  | `Op x -> map_operator env x
  | `Inst_var tok -> token env tok (* instance_variable *)
  | `Class_var tok -> token env tok (* class_variable *)
  | `Global_var tok ->
      token env tok (* pattern "\\$-?(([!@&`'+~=/\\\\,;.<>*$?:\"])|([0-9]*\
  )|([a-zA-Z_][a-zA-Z0-9_]*\
  ))" *)
  )

and map_method_rest (env : env) ((v1, v2, v3) : CST.method_rest) =
  let v1 = map_method_name env v1 in
  let v2 =
    (match v2 with
    | `Params_opt_term (v1, v2) ->
        let v1 = map_parameters env v1 in
        let v2 =
          (match v2 with
          | Some x -> map_terminator env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_bare_params_term (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_bare_parameters env x
          | None -> todo env ())
        in
        let v2 = map_terminator env v2 in
        todo env (v1, v2)
    )
  in
  let v3 = map_body_statement env v3 in
  todo env (v1, v2, v3)

and map_mlhs (env : env) ((v1, v2, v3) : CST.mlhs) =
  let v1 = map_anon_choice_lhs_3a98eae env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_anon_choice_lhs_3a98eae env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_pair (env : env) (x : CST.pair) =
  (match x with
  | `Arg_EQGT_arg (v1, v2, v3) ->
      let v1 = map_arg env v1 in
      let v2 = token env v2 (* "=>" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  | `Choice_id_hash_key_imm_tok_COLON_arg (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Id_hash_key tok ->
            token env tok (* identifier_hash_key *)
        | `Id tok -> token env tok (* identifier *)
        | `Cst tok -> token env tok (* constant *)
        | `Str x -> map_string_ env x
        )
      in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  )

and map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x ->
        map_anon_formal_param_rep_COMMA_formal_param_fcb57c2 env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_parenthesized_statements (env : env) ((v1, v2, v3) : CST.parenthesized_statements) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_pattern (env : env) (x : CST.pattern) =
  (match x with
  | `Arg x -> map_arg env x
  | `Splat_arg x -> map_splat_argument env x
  )

and map_primary (env : env) (x : CST.primary) =
  (match x with
  | `Paren_stmts x -> map_parenthesized_statements env x
  | `Lhs x -> map_lhs env x
  | `Array (v1, v2, v3) ->
      let v1 = token env v1 (* "[" *) in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list_with_trailing_comma env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "]" *) in
      todo env (v1, v2, v3)
  | `Str_array (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* string_array_start *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern \s+ *)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x ->
            map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some tok -> token env tok (* pattern \s+ *)
        | None -> todo env ())
      in
      let v5 = token env v5 (* string_end *) in
      todo env (v1, v2, v3, v4, v5)
  | `Symb_array (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* symbol_array_start *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* pattern \s+ *)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x ->
            map_anon_lit_content_rep_pat_3d340f6_lit_content_3d2b44e env x
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some tok -> token env tok (* pattern \s+ *)
        | None -> todo env ())
      in
      let v5 = token env v5 (* string_end *) in
      todo env (v1, v2, v3, v4, v5)
  | `Hash (v1, v2, v3) ->
      let v1 = token env v1 (* "{" *) in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_anon_choice_pair_a4f33e2 env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_anon_choice_pair_a4f33e2 env v2 in
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
  | `Subs (v1, v2, v3) ->
      let v1 = token env v1 (* subshell_start *) in
      let v2 =
        (match v2 with
        | Some x -> map_literal_contents env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* string_end *) in
      todo env (v1, v2, v3)
  | `Symb x -> map_symbol env x
  | `Int tok ->
      token env tok (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)
  | `Float tok ->
      token env tok (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *)
  | `Comp tok ->
      token env tok (* pattern (\d+)?(\+|-)?(\d+)i *)
  | `Rati (v1, v2) ->
      let v1 =
        token env v1 (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)
      in
      let v2 = token env v2 (* "r" *) in
      todo env (v1, v2)
  | `Str x -> map_string_ env x
  | `Char tok ->
      token env tok (* pattern \?(\\\S({[0-9]*}|[0-9]*|-\S([MC]-\S)?)?|\S) *)
  | `Chai_str (v1, v2) ->
      let v1 = map_string_ env v1 in
      let v2 = List.map (map_string_ env) v2 in
      todo env (v1, v2)
  | `Regex (v1, v2, v3) ->
      let v1 = token env v1 (* regex_start *) in
      let v2 =
        (match v2 with
        | Some x -> map_literal_contents env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* string_end *) in
      todo env (v1, v2, v3)
  | `Lambda (v1, v2, v3) ->
      let v1 = token env v1 (* "->" *) in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Params x -> map_parameters env x
            | `Bare_params x -> map_bare_parameters env x
            )
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Blk x -> map_block env x
        | `Do_blk x -> map_do_block env x
        )
      in
      todo env (v1, v2, v3)
  | `Meth (v1, v2) ->
      let v1 = token env v1 (* "def" *) in
      let v2 = map_method_rest env v2 in
      todo env (v1, v2)
  | `Sing_meth (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "def" *) in
      let v2 =
        (match v2 with
        | `Var x -> map_variable env x
        | `LPAR_arg_RPAR (v1, v2, v3) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_arg env v2 in
            let v3 = token env v3 (* ")" *) in
            todo env (v1, v2, v3)
        )
      in
      let v3 =
        (match v3 with
        | `DOT tok -> token env tok (* "." *)
        | `COLONCOLON tok -> token env tok (* "::" *)
        )
      in
      let v4 = map_method_rest env v4 in
      todo env (v1, v2, v3, v4)
  | `Class (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "class" *) in
      let v2 = map_anon_choice_cst_c1a97cb env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_superclass env x
        | None -> todo env ())
      in
      let v4 = map_terminator env v4 in
      let v5 = map_body_statement env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Sing_class (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "class" *) in
      let v2 =
        token env v2 (* singleton_class_left_angle_left_langle *)
      in
      let v3 = map_arg env v3 in
      let v4 = map_terminator env v4 in
      let v5 = map_body_statement env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Module (v1, v2, v3) ->
      let v1 = token env v1 (* "module" *) in
      let v2 = map_anon_choice_cst_c1a97cb env v2 in
      let v3 =
        (match v3 with
        | `Term_body_stmt (v1, v2) ->
            let v1 = map_terminator env v1 in
            let v2 = map_body_statement env v2 in
            todo env (v1, v2)
        | `End tok -> token env tok (* "end" *)
        )
      in
      todo env (v1, v2, v3)
  | `Begin (v1, v2, v3) ->
      let v1 = token env v1 (* "begin" *) in
      let v2 =
        (match v2 with
        | Some x -> map_terminator env x
        | None -> todo env ())
      in
      let v3 = map_body_statement env v3 in
      todo env (v1, v2, v3)
  | `While (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_arg env v2 in
      let v3 = map_do_ env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_statements env x
        | None -> todo env ())
      in
      let v5 = token env v5 (* "end" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Until (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "until" *) in
      let v2 = map_arg env v2 in
      let v3 = map_do_ env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_statements env x
        | None -> todo env ())
      in
      let v5 = token env v5 (* "end" *) in
      todo env (v1, v2, v3, v4, v5)
  | `If (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = map_statement env v2 in
      let v3 = map_anon_choice_term_b9e1843 env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_else_4cfa13b env x
        | None -> todo env ())
      in
      let v5 = token env v5 (* "end" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Unless (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "unless" *) in
      let v2 = map_statement env v2 in
      let v3 = map_anon_choice_term_b9e1843 env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_else_4cfa13b env x
        | None -> todo env ())
      in
      let v5 = token env v5 (* "end" *) in
      todo env (v1, v2, v3, v4, v5)
  | `For (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = map_left_assignment_list env v2 in
      let v3 = map_in_ env v3 in
      let v4 = map_do_ env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_statements env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* "end" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Case (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "case" *) in
      let v2 =
        (match v2 with
        | Some x -> map_arg env x
        | None -> todo env ())
      in
      let v3 = map_terminator env v3 in
      let v4 = List.map (token env) (* ";" *) v4 in
      let v5 = List.map (map_when_ env) v5 in
      let v6 =
        (match v6 with
        | Some x -> map_else_ env x
        | None -> todo env ())
      in
      let v7 = token env v7 (* "end" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Ret (v1, v2) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Yield (v1, v2) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Brk (v1, v2) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Next (v1, v2) ->
      let v1 = token env v1 (* "next" *) in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Redo (v1, v2) ->
      let v1 = token env v1 (* "redo" *) in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Retry (v1, v2) ->
      let v1 = token env v1 (* "retry" *) in
      let v2 =
        (match v2 with
        | Some x -> map_argument_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Paren_un (v1, v2) ->
      let v1 =
        (match v1 with
        | `Defi tok -> token env tok (* "defined?" *)
        | `Not tok -> token env tok (* "not" *)
        )
      in
      let v2 = map_parenthesized_statements env v2 in
      todo env (v1, v2)
  | `Un_lit (v1, v2) ->
      let v1 = map_anon_choice_un_minus_157a1bc env v1 in
      let v2 =
        (match v2 with
        | `Int tok ->
            token env tok (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)
        | `Float tok ->
            token env tok (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *)
        )
      in
      todo env (v1, v2)
  | `Here_begin tok -> token env tok (* heredoc_beginning *)
  )

and map_rescue (env : env) ((v1, v2, v3, v4) : CST.rescue) =
  let v1 = token env v1 (* "rescue" *) in
  let v2 =
    (match v2 with
    | Some x -> map_exceptions env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_exception_variable env x
    | None -> todo env ())
  in
  let v4 = map_anon_choice_term_b9e1843 env v4 in
  todo env (v1, v2, v3, v4)

and map_right_assignment_list (env : env) ((v1, v2) : CST.right_assignment_list) =
  let v1 = map_pattern env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_pattern env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_scope_resolution (env : env) ((v1, v2) : CST.scope_resolution) =
  let v1 =
    (match v1 with
    | `COLONCOLON tok -> token env tok (* "::" *)
    | `Prim_imm_tok_COLONCOLON (v1, v2) ->
        let v1 = map_primary env v1 in
        let v2 = token env v2 (* "::" *) in
        todo env (v1, v2)
    )
  in
  let v2 =
    (match v2 with
    | `Id tok -> token env tok (* identifier *)
    | `Cst tok -> token env tok (* constant *)
    )
  in
  todo env (v1, v2)

and map_simple_formal_parameter (env : env) (x : CST.simple_formal_parameter) =
  (match x with
  | `Id tok -> token env tok (* identifier *)
  | `Splat_param (v1, v2) ->
      let v1 = token env v1 (* "*" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Hash_splat_param (v1, v2) ->
      let v1 = token env v1 (* "**" *) in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* identifier *)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Blk_param (v1, v2) ->
      let v1 = token env v1 (* "&" *) in
      let v2 = token env v2 (* identifier *) in
      todo env (v1, v2)
  | `Kw_param (v1, v2, v3) ->
      let v1 = token env v1 (* identifier *) in
      let v2 = token env v2 (* ":" *) in
      let v3 =
        (match v3 with
        | Some x -> map_arg env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Opt_param (v1, v2, v3) ->
      let v1 = token env v1 (* identifier *) in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_arg env v3 in
      todo env (v1, v2, v3)
  )

and map_splat_argument (env : env) ((v1, v2) : CST.splat_argument) =
  let v1 = token env v1 (* splat_star *) in
  let v2 = map_arg env v2 in
  todo env (v1, v2)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Undef (v1, v2, v3) ->
      let v1 = token env v1 (* "undef" *) in
      let v2 = map_method_name env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_method_name env v2 in
          todo env (v1, v2)
        ) v3
      in
      todo env (v1, v2, v3)
  | `Alias (v1, v2, v3) ->
      let v1 = token env v1 (* "alias" *) in
      let v2 = map_method_name env v2 in
      let v3 = map_method_name env v3 in
      todo env (v1, v2, v3)
  | `If_modi (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 = token env v2 (* "if" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Unless_modi (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 = token env v2 (* "unless" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `While_modi (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 = token env v2 (* "while" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Until_modi (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 = token env v2 (* "until" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Rescue_modi (v1, v2, v3) ->
      let v1 = map_statement env v1 in
      let v2 = token env v2 (* "rescue" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Begin_blk (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "BEGIN" *) in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        (match v3 with
        | Some x -> map_statements env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "}" *) in
      todo env (v1, v2, v3, v4)
  | `End_blk (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "END" *) in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        (match v3 with
        | Some x -> map_statements env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "}" *) in
      todo env (v1, v2, v3, v4)
  | `Exp x -> map_expression env x
  )

and map_statements (env : env) (x : CST.statements) =
  (match x with
  | `Rep1_choice_stmt_term_opt_stmt (v1, v2) ->
      let v1 =
        List.map (fun x ->
          (match x with
          | `Stmt_term (v1, v2) ->
              let v1 = map_statement env v1 in
              let v2 = map_terminator env v2 in
              todo env (v1, v2)
          | `Empty_stmt tok -> token env tok (* ";" *)
          )
        ) v1
      in
      let v2 =
        (match v2 with
        | Some x -> map_statement env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Stmt x -> map_statement env x
  )

and map_string_ (env : env) ((v1, v2, v3) : CST.string_) =
  let v1 = token env v1 (* string_start *) in
  let v2 =
    (match v2 with
    | Some x -> map_literal_contents env x
    | None -> todo env ())
  in
  let v3 = token env v3 (* string_end *) in
  todo env (v1, v2, v3)

and map_superclass (env : env) ((v1, v2) : CST.superclass) =
  let v1 = token env v1 (* "<" *) in
  let v2 = map_arg env v2 in
  todo env (v1, v2)

and map_symbol (env : env) (x : CST.symbol) =
  (match x with
  | `Simple_symb tok -> token env tok (* simple_symbol *)
  | `Symb_start_opt_lit_content_str_end (v1, v2, v3) ->
      let v1 = token env v1 (* symbol_start *) in
      let v2 =
        (match v2 with
        | Some x -> map_literal_contents env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* string_end *) in
      todo env (v1, v2, v3)
  )

and map_then_ (env : env) (x : CST.then_) =
  (match x with
  | `Term_stmts (v1, v2) ->
      let v1 = map_terminator env v1 in
      let v2 = map_statements env v2 in
      todo env (v1, v2)
  | `Opt_term_then_opt_stmts (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_terminator env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "then" *) in
      let v3 =
        (match v3 with
        | Some x -> map_statements env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  )

and map_unary (env : env) (x : CST.unary) =
  (match x with
  | `Defi_arg (v1, v2) ->
      let v1 = token env v1 (* "defined?" *) in
      let v2 = map_arg env v2 in
      todo env (v1, v2)
  | `Not_arg (v1, v2) ->
      let v1 = token env v1 (* "not" *) in
      let v2 = map_arg env v2 in
      todo env (v1, v2)
  | `Choice_un_minus_arg (v1, v2) ->
      let v1 = map_anon_choice_un_minus_157a1bc env v1 in
      let v2 = map_arg env v2 in
      todo env (v1, v2)
  | `Choice_BANG_arg (v1, v2) ->
      let v1 =
        (match v1 with
        | `BANG tok -> token env tok (* "!" *)
        | `TILDE tok -> token env tok (* "~" *)
        )
      in
      let v2 = map_arg env v2 in
      todo env (v1, v2)
  )

and map_when_ (env : env) ((v1, v2, v3, v4) : CST.when_) =
  let v1 = token env v1 (* "when" *) in
  let v2 = map_pattern env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_pattern env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = map_anon_choice_term_b9e1843 env v4 in
  todo env (v1, v2, v3, v4)

let map_program (env : env) ((v1, v2) : CST.program) =
  let v1 =
    (match v1 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* "__END__" *) in
        let v2 = token env v2 (* line_break *) in
        let v3 = token env v3 (* pattern (.|\s)* *) in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  todo env (v1, v2)
