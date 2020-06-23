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

let map_self (tok : CST.self) =
  token tok

let map_binary_star (tok : CST.binary_star) =
  token tok

let map_singleton_class_left_angle_left_langle (tok : CST.singleton_class_left_angle_left_langle) =
  token tok

let map_instance_variable (tok : CST.instance_variable) =
  token tok

let map_empty_statement (tok : CST.empty_statement) =
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
  match x with
  | `False_false tok -> todo tok
  | `False_FALSE tok -> todo tok

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

let map_comment (tok : CST.comment) =
  token tok

let map_string_array_start (tok : CST.string_array_start) =
  token tok

let map_splat_star (tok : CST.splat_star) =
  token tok

let map_integer (tok : CST.integer) =
  token tok

let map_heredoc_content (tok : CST.heredoc_content) =
  token tok

let map_super (tok : CST.super) =
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
  match x with
  | `Nil_nil tok -> todo tok
  | `Nil_NIL tok -> todo tok

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
  match x with
  | `Op_DOTDOT tok -> todo tok
  | `Op_BAR tok -> todo tok
  | `Op_HAT tok -> todo tok
  | `Op_AMP tok -> todo tok
  | `Op_LTEQGT tok -> todo tok
  | `Op_EQEQ tok -> todo tok
  | `Op_EQEQEQ tok -> todo tok
  | `Op_EQTILDE tok -> todo tok
  | `Op_GT tok -> todo tok
  | `Op_GTEQ tok -> todo tok
  | `Op_LT tok -> todo tok
  | `Op_LTEQ tok -> todo tok
  | `Op_PLUS tok -> todo tok
  | `Op_DASH tok -> todo tok
  | `Op_STAR tok -> todo tok
  | `Op_SLASH tok -> todo tok
  | `Op_PERC tok -> todo tok
  | `Op_BANG tok -> todo tok
  | `Op_BANGTILDE tok -> todo tok
  | `Op_STARSTAR tok -> todo tok
  | `Op_LTLT tok -> todo tok
  | `Op_GTGT tok -> todo tok
  | `Op_TILDE tok -> todo tok
  | `Op_PLUSAT tok -> todo tok
  | `Op_DASHAT tok -> todo tok
  | `Op_LBRACKRBRACK tok -> todo tok
  | `Op_LBRACKRBRACKEQ tok -> todo tok
  | `Op_BQUOT tok -> todo tok

let map_true_ (x : CST.true_) =
  match x with
  | `True_true tok -> todo tok
  | `True_TRUE tok -> todo tok

let map_string_start (tok : CST.string_start) =
  token tok

let map_identifier_hash_key (tok : CST.identifier_hash_key) =
  token tok

let map_rational ((v1, v2) : CST.rational) =
  todo (v1, v2)

let map_terminator (x : CST.terminator) =
  match x with
  | `Term_line_brk x -> todo x
  | `Term_SEMI tok -> todo tok

let map_hash_splat_parameter ((v1, v2) : CST.hash_splat_parameter) =
  todo (v1, v2)

let map_splat_parameter ((v1, v2) : CST.splat_parameter) =
  todo (v1, v2)

let map_setter ((v1, v2) : CST.setter) =
  todo (v1, v2)

let map_block_parameter ((v1, v2) : CST.block_parameter) =
  todo (v1, v2)

let map_unary_literal ((v1, v2) : CST.unary_literal) =
  todo (v1, v2)

let map_variable (x : CST.variable) =
  match x with
  | `Self x -> todo x
  | `Super x -> todo x
  | `Inst_var x -> todo x
  | `Class_var x -> todo x
  | `Glob_var x -> todo x
  | `Id x -> todo x
  | `Cst x -> todo x

let map_do_ (x : CST.do_) =
  match x with
  | `Do_do tok -> todo tok
  | `Do_term x -> todo x

let rec map_statements (x : CST.statements) =
  match x with
  | `Stmts_rep1_choice_stmt_term_opt_stmt (v1, v2) -> todo (v1, v2)
  | `Stmts_stmt x -> todo x

and map_begin_block ((v1, v2, v3, v4) : CST.begin_block) =
  todo (v1, v2, v3, v4)

and map_end_block ((v1, v2, v3, v4) : CST.end_block) =
  todo (v1, v2, v3, v4)

and map_statement (x : CST.statement) =
  match x with
  | `Stmt_undef x -> todo x
  | `Stmt_alias x -> todo x
  | `Stmt_if_modi x -> todo x
  | `Stmt_unle_modi x -> todo x
  | `Stmt_while_modi x -> todo x
  | `Stmt_until_modi x -> todo x
  | `Stmt_resc_modi x -> todo x
  | `Stmt_begin_blk x -> todo x
  | `Stmt_end_blk x -> todo x
  | `Stmt_exp x -> todo x

and map_method_ ((v1, v2) : CST.method_) =
  todo (v1, v2)

and map_singleton_method ((v1, v2, v3, v4) : CST.singleton_method) =
  todo (v1, v2, v3, v4)

and map_method_rest ((v1, v2, v3) : CST.method_rest) =
  todo (v1, v2, v3)

and map_parameters ((v1, v2, v3) : CST.parameters) =
  todo (v1, v2, v3)

and map_bare_parameters ((v1, v2) : CST.bare_parameters) =
  todo (v1, v2)

and map_block_parameters ((v1, v2, v3, v4, v5) : CST.block_parameters) =
  todo (v1, v2, v3, v4, v5)

and map_formal_parameter (x : CST.formal_parameter) =
  match x with
  | `Form_param_simple_form_param x -> todo x
  | `Form_param_params x -> todo x

and map_simple_formal_parameter (x : CST.simple_formal_parameter) =
  match x with
  | `Simple_form_param_id x -> todo x
  | `Simple_form_param_splat_param x -> todo x
  | `Simple_form_param_hash_splat_param x -> todo x
  | `Simple_form_param_blk_param x -> todo x
  | `Simple_form_param_kw_param x -> todo x
  | `Simple_form_param_opt_param x -> todo x

and map_keyword_parameter ((v1, v2, v3) : CST.keyword_parameter) =
  todo (v1, v2, v3)

and map_optional_parameter ((v1, v2, v3) : CST.optional_parameter) =
  todo (v1, v2, v3)

and map_class_ ((v1, v2, v3, v4, v5) : CST.class_) =
  todo (v1, v2, v3, v4, v5)

and map_superclass ((v1, v2) : CST.superclass) =
  todo (v1, v2)

and map_singleton_class ((v1, v2, v3, v4, v5) : CST.singleton_class) =
  todo (v1, v2, v3, v4, v5)

and map_module_ ((v1, v2, v3) : CST.module_) =
  todo (v1, v2, v3)

and map_return_command ((v1, v2) : CST.return_command) =
  todo (v1, v2)

and map_yield_command ((v1, v2) : CST.yield_command) =
  todo (v1, v2)

and map_break_command ((v1, v2) : CST.break_command) =
  todo (v1, v2)

and map_next_command ((v1, v2) : CST.next_command) =
  todo (v1, v2)

and map_return ((v1, v2) : CST.return) =
  todo (v1, v2)

and map_yield ((v1, v2) : CST.yield) =
  todo (v1, v2)

and map_break ((v1, v2) : CST.break) =
  todo (v1, v2)

and map_next ((v1, v2) : CST.next) =
  todo (v1, v2)

and map_redo ((v1, v2) : CST.redo) =
  todo (v1, v2)

and map_retry ((v1, v2) : CST.retry) =
  todo (v1, v2)

and map_if_modifier ((v1, v2, v3) : CST.if_modifier) =
  todo (v1, v2, v3)

and map_unless_modifier ((v1, v2, v3) : CST.unless_modifier) =
  todo (v1, v2, v3)

and map_while_modifier ((v1, v2, v3) : CST.while_modifier) =
  todo (v1, v2, v3)

and map_until_modifier ((v1, v2, v3) : CST.until_modifier) =
  todo (v1, v2, v3)

and map_rescue_modifier ((v1, v2, v3) : CST.rescue_modifier) =
  todo (v1, v2, v3)

and map_while_ ((v1, v2, v3, v4, v5) : CST.while_) =
  todo (v1, v2, v3, v4, v5)

and map_until ((v1, v2, v3, v4, v5) : CST.until) =
  todo (v1, v2, v3, v4, v5)

and map_for_ ((v1, v2, v3, v4, v5, v6) : CST.for_) =
  todo (v1, v2, v3, v4, v5, v6)

and map_in_ ((v1, v2) : CST.in_) =
  todo (v1, v2)

and map_case ((v1, v2, v3, v4, v5, v6, v7) : CST.case) =
  todo (v1, v2, v3, v4, v5, v6, v7)

and map_when_ ((v1, v2, v3, v4) : CST.when_) =
  todo (v1, v2, v3, v4)

and map_pattern (x : CST.pattern) =
  match x with
  | `Pat_arg x -> todo x
  | `Pat_splat_arg x -> todo x

and map_if_ ((v1, v2, v3, v4, v5) : CST.if_) =
  todo (v1, v2, v3, v4, v5)

and map_unless ((v1, v2, v3, v4, v5) : CST.unless) =
  todo (v1, v2, v3, v4, v5)

and map_elsif ((v1, v2, v3, v4) : CST.elsif) =
  todo (v1, v2, v3, v4)

and map_else_ ((v1, v2, v3) : CST.else_) =
  todo (v1, v2, v3)

and map_then_ (x : CST.then_) =
  match x with
  | `Then_term_stmts (v1, v2) -> todo (v1, v2)
  | `Then_opt_term_then_opt_stmts (v1, v2, v3) -> todo (v1, v2, v3)

and map_begin_ ((v1, v2, v3) : CST.begin_) =
  todo (v1, v2, v3)

and map_ensure ((v1, v2) : CST.ensure) =
  todo (v1, v2)

and map_rescue ((v1, v2, v3, v4) : CST.rescue) =
  todo (v1, v2, v3, v4)

and map_exceptions ((v1, v2) : CST.exceptions) =
  todo (v1, v2)

and map_exception_variable ((v1, v2) : CST.exception_variable) =
  todo (v1, v2)

and map_body_statement ((v1, v2, v3) : CST.body_statement) =
  todo (v1, v2, v3)

and map_expression (x : CST.expression) =
  match x with
  | `Exp_cmd_bin x -> todo x
  | `Exp_cmd_assign x -> todo x
  | `Exp_cmd_op_assign x -> todo x
  | `Exp_cmd_call x -> todo x
  | `Exp_ret_cmd x -> todo x
  | `Exp_yield_cmd x -> todo x
  | `Exp_brk_cmd x -> todo x
  | `Exp_next_cmd x -> todo x
  | `Exp_arg x -> todo x

and map_arg (x : CST.arg) =
  match x with
  | `Arg_prim x -> todo x
  | `Arg_assign x -> todo x
  | `Arg_op_assign x -> todo x
  | `Arg_cond x -> todo x
  | `Arg_range x -> todo x
  | `Arg_bin x -> todo x
  | `Arg_un x -> todo x

and map_primary (x : CST.primary) =
  match x with
  | `Prim_paren_stmts x -> todo x
  | `Prim_lhs x -> todo x
  | `Prim_array x -> todo x
  | `Prim_str_array x -> todo x
  | `Prim_symb_array x -> todo x
  | `Prim_hash x -> todo x
  | `Prim_subs x -> todo x
  | `Prim_symb x -> todo x
  | `Prim_int x -> todo x
  | `Prim_float x -> todo x
  | `Prim_comp x -> todo x
  | `Prim_rati x -> todo x
  | `Prim_str x -> todo x
  | `Prim_char x -> todo x
  | `Prim_chai_str x -> todo x
  | `Prim_regex x -> todo x
  | `Prim_lamb x -> todo x
  | `Prim_meth x -> todo x
  | `Prim_sing_meth x -> todo x
  | `Prim_class x -> todo x
  | `Prim_sing_class x -> todo x
  | `Prim_modu x -> todo x
  | `Prim_begin x -> todo x
  | `Prim_while x -> todo x
  | `Prim_until x -> todo x
  | `Prim_if x -> todo x
  | `Prim_unle x -> todo x
  | `Prim_for x -> todo x
  | `Prim_case x -> todo x
  | `Prim_ret x -> todo x
  | `Prim_yield x -> todo x
  | `Prim_brk x -> todo x
  | `Prim_next x -> todo x
  | `Prim_redo x -> todo x
  | `Prim_retry x -> todo x
  | `Prim_paren_un x -> todo x
  | `Prim_un_lit x -> todo x
  | `Prim_here_begin x -> todo x

and map_parenthesized_statements ((v1, v2, v3) : CST.parenthesized_statements) =
  todo (v1, v2, v3)

and map_element_reference ((v1, v2, v3, v4) : CST.element_reference) =
  todo (v1, v2, v3, v4)

and map_scope_resolution ((v1, v2) : CST.scope_resolution) =
  todo (v1, v2)

and map_call ((v1, v2, v3) : CST.call) =
  todo (v1, v2, v3)

and map_command_call (x : CST.command_call) =
  match x with
  | `Cmd_call_choice_var_cmd_arg_list (v1, v2) -> todo (v1, v2)
  | `Cmd_call_choice_var_cmd_arg_list_blk (v1, v2, v3) -> todo (v1, v2, v3)
  | `Cmd_call_choice_var_cmd_arg_list_do_blk (v1, v2, v3) -> todo (v1, v2, v3)

and map_method_call (x : CST.method_call) =
  match x with
  | `Meth_call_choice_var_arg_list (v1, v2) -> todo (v1, v2)
  | `Meth_call_choice_var_arg_list_blk (v1, v2, v3) -> todo (v1, v2, v3)
  | `Meth_call_choice_var_arg_list_do_blk (v1, v2, v3) -> todo (v1, v2, v3)
  | `Meth_call_choice_var_blk (v1, v2) -> todo (v1, v2)
  | `Meth_call_choice_var_do_blk (v1, v2) -> todo (v1, v2)

and map_command_argument_list (x : CST.command_argument_list) =
  match x with
  | `Cmd_arg_list_arg_rep_COMMA_arg (v1, v2) -> todo (v1, v2)
  | `Cmd_arg_list_cmd_call x -> todo x

and map_argument_list ((v1, v2, v3) : CST.argument_list) =
  todo (v1, v2, v3)

and map_argument_list_with_trailing_comma ((v1, v2, v3) : CST.argument_list_with_trailing_comma) =
  todo (v1, v2, v3)

and map_argument (x : CST.argument) =
  match x with
  | `Arg_arg x -> todo x
  | `Arg_splat_arg x -> todo x
  | `Arg_hash_splat_arg x -> todo x
  | `Arg_blk_arg x -> todo x
  | `Arg_pair x -> todo x

and map_splat_argument ((v1, v2) : CST.splat_argument) =
  todo (v1, v2)

and map_hash_splat_argument ((v1, v2) : CST.hash_splat_argument) =
  todo (v1, v2)

and map_block_argument ((v1, v2) : CST.block_argument) =
  todo (v1, v2)

and map_do_block ((v1, v2, v3, v4) : CST.do_block) =
  todo (v1, v2, v3, v4)

and map_block ((v1, v2, v3, v4) : CST.block) =
  todo (v1, v2, v3, v4)

and map_assignment (x : CST.assignment) =
  match x with
  | `Choice_lhs_EQ_choice_arg (v1, v2, v3) -> todo (v1, v2, v3)

and map_command_assignment (x : CST.command_assignment) =
  match x with
  | `Choice_lhs_EQ_exp (v1, v2, v3) -> todo (v1, v2, v3)

and map_operator_assignment ((v1, v2, v3) : CST.operator_assignment) =
  todo (v1, v2, v3)

and map_command_operator_assignment ((v1, v2, v3) : CST.command_operator_assignment) =
  todo (v1, v2, v3)

and map_conditional ((v1, v2, v3, v4, v5) : CST.conditional) =
  todo (v1, v2, v3, v4, v5)

and map_range ((v1, v2, v3) : CST.range) =
  todo (v1, v2, v3)

and map_binary (x : CST.binary) =
  match x with
  | `Bin_arg_and_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_or_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_BARBAR_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_AMPAMP_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_choice_LTLT_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_choice_LT_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_AMP_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_choice_HAT_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_choice_PLUS_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_choice_SLASH_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_choice_EQEQ_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Bin_arg_STARSTAR_arg (v1, v2, v3) -> todo (v1, v2, v3)

and map_command_binary ((v1, v2, v3) : CST.command_binary) =
  todo (v1, v2, v3)

and map_unary (x : CST.unary) =
  match x with
  | `Un_defi_arg (v1, v2) -> todo (v1, v2)
  | `Un_not_arg (v1, v2) -> todo (v1, v2)
  | `Un_choice_un_minus_arg (v1, v2) -> todo (v1, v2)
  | `Un_choice_BANG_arg (v1, v2) -> todo (v1, v2)

and map_parenthesized_unary ((v1, v2) : CST.parenthesized_unary) =
  todo (v1, v2)

and map_right_assignment_list ((v1, v2) : CST.right_assignment_list) =
  todo (v1, v2)

and map_left_assignment_list (x : CST.left_assignment_list) =
  map_mlhs x

and map_mlhs ((v1, v2, v3) : CST.mlhs) =
  todo (v1, v2, v3)

and map_destructured_left_assignment ((v1, v2, v3) : CST.destructured_left_assignment) =
  todo (v1, v2, v3)

and map_rest_assignment ((v1, v2) : CST.rest_assignment) =
  todo (v1, v2)

and map_lhs (x : CST.lhs) =
  match x with
  | `Var x -> todo x
  | `True x -> todo x
  | `False x -> todo x
  | `Nil x -> todo x
  | `Scope_resol x -> todo x
  | `Elem_ref x -> todo x
  | `Call x -> todo x
  | `Meth_call x -> todo x

and map_method_name (x : CST.method_name) =
  match x with
  | `Meth_name_id x -> todo x
  | `Meth_name_cst x -> todo x
  | `Meth_name_sett x -> todo x
  | `Meth_name_symb x -> todo x
  | `Meth_name_op x -> todo x
  | `Meth_name_inst_var x -> todo x
  | `Meth_name_class_var x -> todo x
  | `Meth_name_glob_var x -> todo x

and map_undef ((v1, v2, v3) : CST.undef) =
  todo (v1, v2, v3)

and map_alias ((v1, v2, v3) : CST.alias) =
  todo (v1, v2, v3)

and map_chained_string ((v1, v2) : CST.chained_string) =
  todo (v1, v2)

and map_interpolation ((v1, v2, v3) : CST.interpolation) =
  todo (v1, v2, v3)

and map_string_ ((v1, v2, v3) : CST.string_) =
  todo (v1, v2, v3)

and map_subshell ((v1, v2, v3) : CST.subshell) =
  todo (v1, v2, v3)

and map_string_array ((v1, v2, v3, v4, v5) : CST.string_array) =
  todo (v1, v2, v3, v4, v5)

and map_symbol_array ((v1, v2, v3, v4, v5) : CST.symbol_array) =
  todo (v1, v2, v3, v4, v5)

and map_symbol (x : CST.symbol) =
  match x with
  | `Symb_simple_symb x -> todo x
  | `Symb_symb_start_opt_lit_content_str_end (v1, v2, v3) -> todo (v1, v2, v3)

and map_regex ((v1, v2, v3) : CST.regex) =
  todo (v1, v2, v3)

and map_literal_contents (xs : CST.literal_contents) =
  List.map (fun x -> todo x) xs

and map_array_ ((v1, v2, v3) : CST.array_) =
  todo (v1, v2, v3)

and map_hash ((v1, v2, v3) : CST.hash) =
  todo (v1, v2, v3)

and map_pair (x : CST.pair) =
  match x with
  | `Pair_arg_EQGT_arg (v1, v2, v3) -> todo (v1, v2, v3)
  | `Pair_choice_id_hash_key_COLON_arg (v1, v2, v3) -> todo (v1, v2, v3)

and map_lambda ((v1, v2, v3) : CST.lambda) =
  todo (v1, v2, v3)

let map_program ((v1, v2) : CST.program) =
  todo (v1, v2)

let map_heredoc_body ((v1, v2, v3) : CST.heredoc_body) =
  todo (v1, v2, v3)

