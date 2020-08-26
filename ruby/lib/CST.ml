(* Generated by ocaml-tree-sitter. *)
(*
   ruby grammar

   entrypoint: program
*)

open! Sexplib.Conv
open Tree_sitter_run

type simple_symbol = Token.t
[@@deriving sexp_of]

type binary_minus = Token.t
[@@deriving sexp_of]

type regex_start = Token.t
[@@deriving sexp_of]

type binary_star = Token.t
[@@deriving sexp_of]

type pat_3d340f6 = Token.t (* pattern \s+ *)
[@@deriving sexp_of]

type heredoc_content = Token.t
[@@deriving sexp_of]

type identifier_hash_key = Token.t
[@@deriving sexp_of]

type string_start = Token.t
[@@deriving sexp_of]

type anon_choice_PLUSEQ = [
    `PLUSEQ of Token.t (* "+=" *)
  | `DASHEQ of Token.t (* "-=" *)
  | `STAREQ of Token.t (* "*=" *)
  | `STARSTAREQ of Token.t (* "**=" *)
  | `SLASHEQ of Token.t (* "/=" *)
  | `BARBAREQ of Token.t (* "||=" *)
  | `BAREQ of Token.t (* "|=" *)
  | `AMPAMPEQ of Token.t (* "&&=" *)
  | `AMPEQ of Token.t (* "&=" *)
  | `PERCEQ of Token.t (* "%=" *)
  | `GTGTEQ of Token.t (* ">>=" *)
  | `LTLTEQ of Token.t (* "<<=" *)
  | `HATEQ of Token.t (* "^=" *)
]
[@@deriving sexp_of]

type heredoc_end = Token.t
[@@deriving sexp_of]

type singleton_class_left_angle_left_langle = Token.t
[@@deriving sexp_of]

type escape_sequence = Token.t
[@@deriving sexp_of]

type instance_variable = Token.t
[@@deriving sexp_of]

type identifier = Token.t
[@@deriving sexp_of]

type uninterpreted = Token.t (* pattern (.|\s)* *)
[@@deriving sexp_of]

type string_content = Token.t
[@@deriving sexp_of]

type string_array_start = Token.t
[@@deriving sexp_of]

type operator = [
    `DOTDOT of Token.t (* ".." *)
  | `BAR of Token.t (* "|" *)
  | `HAT of Token.t (* "^" *)
  | `AMP of Token.t (* "&" *)
  | `LTEQGT of Token.t (* "<=>" *)
  | `EQEQ of Token.t (* "==" *)
  | `EQEQEQ of Token.t (* "===" *)
  | `EQTILDE of Token.t (* "=~" *)
  | `GT of Token.t (* ">" *)
  | `GTEQ of Token.t (* ">=" *)
  | `LT of Token.t (* "<" *)
  | `LTEQ of Token.t (* "<=" *)
  | `PLUS of Token.t (* "+" *)
  | `DASH of Token.t (* "-" *)
  | `STAR of Token.t (* "*" *)
  | `SLASH of Token.t (* "/" *)
  | `PERC of Token.t (* "%" *)
  | `BANG of Token.t (* "!" *)
  | `BANGTILDE of Token.t (* "!~" *)
  | `STARSTAR of Token.t (* "**" *)
  | `LTLT of Token.t (* "<<" *)
  | `GTGT of Token.t (* ">>" *)
  | `TILDE of Token.t (* "~" *)
  | `PLUSAT of Token.t (* "+@" *)
  | `DASHAT of Token.t (* "-@" *)
  | `LBRACKRBRACK of Token.t (* "[]" *)
  | `LBRACKRBRACKEQ of Token.t (* "[]=" *)
  | `BQUOT of Token.t (* "`" *)
]
[@@deriving sexp_of]

type heredoc_body_start = Token.t
[@@deriving sexp_of]

type subshell_start = Token.t
[@@deriving sexp_of]

type complex = Token.t (* pattern (\d+)?(\+|-)?(\d+)i *)
[@@deriving sexp_of]

type float_ =
  Token.t (* pattern \d(_?\d)*(\.\d)?(_?\d)*([eE][\+-]?\d(_?\d)*\
  )? *)
[@@deriving sexp_of]

type string_end = Token.t
[@@deriving sexp_of]

type symbol_start = Token.t
[@@deriving sexp_of]

type heredoc_beginning = Token.t
[@@deriving sexp_of]

type unary_minus = Token.t
[@@deriving sexp_of]

type class_variable = Token.t
[@@deriving sexp_of]

type line_break = Token.t
[@@deriving sexp_of]

type symbol_array_start = Token.t
[@@deriving sexp_of]

type constant = Token.t
[@@deriving sexp_of]

type integer =
  Token.t (* pattern 0[bB][01](_?[01])*|0[oO]?[0-7](_?[0-7])*|(0[dD])?\d(_?\d)*|0x[0-9a-fA-F](_?[0-9a-fA-F])* *)
[@@deriving sexp_of]

type true_ = [
    `True of Token.t (* "true" *)
  | `TRUE of Token.t (* "TRUE" *)
]
[@@deriving sexp_of]

type character =
  Token.t (* pattern \?(\\\S({[0-9]*}|[0-9]*|-\S([MC]-\S)?)?|\S) *)
[@@deriving sexp_of]

type global_variable =
  Token.t (* pattern "\\$-?(([!@&`'+~=\\/\\\\,;.<>*$?:\"])|([0-9]*\
  )|([a-zA-Z_][a-zA-Z0-9_]*\
  ))" *)
[@@deriving sexp_of]

type block_ampersand = Token.t
[@@deriving sexp_of]

type nil = [ `Nil of Token.t (* "nil" *) | `NIL of Token.t (* "NIL" *) ]
[@@deriving sexp_of]

type splat_star = Token.t
[@@deriving sexp_of]

type false_ = [
    `False of Token.t (* "false" *)
  | `FALSE of Token.t (* "FALSE" *)
]
[@@deriving sexp_of]

type anon_choice_un_minus = [
    `Un_minus of unary_minus (*tok*)
  | `PLUS of Token.t (* "+" *)
]
[@@deriving sexp_of]

type terminator = [
    `Line_brk of line_break (*tok*)
  | `SEMI of Token.t (* ";" *)
]
[@@deriving sexp_of]

type variable = [
    `Self of Token.t (* "self" *)
  | `Super of Token.t (* "super" *)
  | `Inst_var of instance_variable (*tok*)
  | `Class_var of class_variable (*tok*)
  | `Global_var of global_variable (*tok*)
  | `Id of identifier (*tok*)
  | `Cst of constant (*tok*)
]
[@@deriving sexp_of]

type do_ = [ `Do of Token.t (* "do" *) | `Term of terminator ]
[@@deriving sexp_of]

type anon_formal_param_rep_COMMA_formal_param = (
    formal_parameter
  * (Token.t (* "," *) * formal_parameter) list (* zero or more *)
)

and else_ = (Token.t (* "else" *) * terminator option * statements option)

and block = (
    Token.t (* "{" *)
  * block_parameters option
  * statements option
  * Token.t (* "}" *)
)

and in_ = (Token.t (* "in" *) * arg)

and scope_resolution = (
    [
        `COLONCOLON of Token.t (* "::" *)
      | `Prim_imm_tok_COLONCOLON of (primary * Token.t (* "::" *))
    ]
  * [ `Id of identifier (*tok*) | `Cst of constant (*tok*) ]
)

and argument_list_with_trailing_comma = (
    argument
  * (Token.t (* "," *) * argument) list (* zero or more *)
  * Token.t (* "," *) option
)

and mlhs = (
    anon_choice_lhs_
  * (Token.t (* "," *) * anon_choice_lhs_) list (* zero or more *)
  * Token.t (* "," *) option
)

and statements = [
    `Rep1_choice_stmt_term_opt_stmt of (
        [
            `Stmt_term of (statement * terminator)
          | `Empty_stmt of Token.t (* ";" *)
        ]
          list (* one or more *)
      * statement option
    )
  | `Stmt of statement
]

and call = (
    primary
  * [ `DOT of Token.t (* "." *) | `AMPDOT of Token.t (* "&." *) ]
  * [
        `Id of identifier (*tok*)
      | `Op of operator
      | `Cst of constant (*tok*)
      | `Arg_list of argument_list
    ]
)

and anon_choice_lhs = [
    `Lhs of lhs
  | `Left_assign_list of left_assignment_list
]

and method_call = [
    `Choice_var_arg_list of (anon_choice_var * argument_list)
  | `Choice_var_arg_list_blk of (anon_choice_var * argument_list * block)
  | `Choice_var_arg_list_do_blk of (
        anon_choice_var * argument_list * do_block
    )
  | `Choice_var_blk of (anon_choice_var * block)
  | `Choice_var_do_blk of (anon_choice_var * do_block)
]

and method_rest = (
    method_name
  * [
        `Params_opt_term of (parameters * terminator option)
      | `Opt_bare_params_term of (bare_parameters option * terminator)
    ]
  * body_statement
)

and rescue = (
    Token.t (* "rescue" *)
  * exceptions option
  * exception_variable option
  * anon_choice_term
)

and anon_choice_var = [
    `Var of variable
  | `Scope_resol of scope_resolution
  | `Call of call
]

and anon_choice_pair = [
    `Pair of pair
  | `Hash_splat_arg of hash_splat_argument
]

and primary = [
    `Paren_stmts of parenthesized_statements
  | `Lhs of lhs
  | `Array of (
        Token.t (* "[" *)
      * argument_list_with_trailing_comma option
      * Token.t (* "]" *)
    )
  | `Str_array of (
        string_array_start (*tok*)
      * pat_3d340f6 (*tok*) option
      * anon_lit_content_rep_pat_3d340f6_lit_content option
      * pat_3d340f6 (*tok*) option
      * string_end (*tok*)
    )
  | `Symb_array of (
        symbol_array_start (*tok*)
      * pat_3d340f6 (*tok*) option
      * anon_lit_content_rep_pat_3d340f6_lit_content option
      * pat_3d340f6 (*tok*) option
      * string_end (*tok*)
    )
  | `Hash of (
        Token.t (* "{" *)
      * (
            anon_choice_pair
          * (Token.t (* "," *) * anon_choice_pair) list (* zero or more *)
          * Token.t (* "," *) option
        )
          option
      * Token.t (* "}" *)
    )
  | `Subs of (
        subshell_start (*tok*)
      * literal_contents option
      * string_end (*tok*)
    )
  | `Symb of symbol
  | `Int of integer (*tok*)
  | `Float of float_ (*tok*)
  | `Comp of complex (*tok*)
  | `Rati of (integer (*tok*) * Token.t (* "r" *))
  | `Str of string_
  | `Char of character (*tok*)
  | `Chai_str of (string_ * string_ list (* one or more *))
  | `Regex of (
        regex_start (*tok*)
      * literal_contents option
      * string_end (*tok*)
    )
  | `Lambda of (
        Token.t (* "->" *)
      * [ `Params of parameters | `Bare_params of bare_parameters ] option
      * [ `Blk of block | `Do_blk of do_block ]
    )
  | `Meth of (Token.t (* "def" *) * method_rest)
  | `Sing_meth of (
        Token.t (* "def" *)
      * [
            `Var of variable
          | `LPAR_arg_RPAR of (Token.t (* "(" *) * arg * Token.t (* ")" *))
        ]
      * [ `DOT of Token.t (* "." *) | `COLONCOLON of Token.t (* "::" *) ]
      * method_rest
    )
  | `Class of (
        Token.t (* "class" *)
      * anon_choice_cst
      * superclass option
      * terminator
      * body_statement
    )
  | `Sing_class of (
        Token.t (* "class" *)
      * singleton_class_left_angle_left_langle (*tok*) * arg * terminator
      * body_statement
    )
  | `Module of (
        Token.t (* "module" *)
      * anon_choice_cst
      * [
            `Term_body_stmt of (terminator * body_statement)
          | `End of Token.t (* "end" *)
        ]
    )
  | `Begin of (Token.t (* "begin" *) * terminator option * body_statement)
  | `While of (
        Token.t (* "while" *)
      * arg
      * do_
      * statements option
      * Token.t (* "end" *)
    )
  | `Until of (
        Token.t (* "until" *)
      * arg
      * do_
      * statements option
      * Token.t (* "end" *)
    )
  | `If of (
        Token.t (* "if" *)
      * statement
      * anon_choice_term
      * anon_choice_else option
      * Token.t (* "end" *)
    )
  | `Unless of (
        Token.t (* "unless" *)
      * statement
      * anon_choice_term
      * anon_choice_else option
      * Token.t (* "end" *)
    )
  | `For of (
        Token.t (* "for" *)
      * left_assignment_list
      * in_
      * do_
      * statements option
      * Token.t (* "end" *)
    )
  | `Case of (
        Token.t (* "case" *)
      * arg option
      * terminator
      * Token.t (* ";" *) list (* zero or more *)
      * when_ list (* zero or more *)
      * else_ option
      * Token.t (* "end" *)
    )
  | `Ret of (Token.t (* "return" *) * argument_list option)
  | `Yield of (Token.t (* "yield" *) * argument_list option)
  | `Brk of (Token.t (* "break" *) * argument_list option)
  | `Next of (Token.t (* "next" *) * argument_list option)
  | `Redo of (Token.t (* "redo" *) * argument_list option)
  | `Retry of (Token.t (* "retry" *) * argument_list option)
  | `Paren_un of (
        [ `Defi of Token.t (* "defined?" *) | `Not of Token.t (* "not" *) ]
      * parenthesized_statements
    )
  | `Un_lit of (
        anon_choice_un_minus
      * [ `Int of integer (*tok*) | `Float of float_ (*tok*) ]
    )
  | `Here_begin of heredoc_beginning (*tok*)
]

and argument = [
    `Arg of arg
  | `Splat_arg of splat_argument
  | `Hash_splat_arg of hash_splat_argument
  | `Blk_arg of (block_ampersand (*tok*) * arg)
  | `Pair of pair
]

and parenthesized_statements = (
    Token.t (* "(" *)
  * statements option
  * Token.t (* ")" *)
)

and string_ = (
    string_start (*tok*)
  * literal_contents option
  * string_end (*tok*)
)

and body_statement = (
    statements option
  * [ `Rescue of rescue | `Else of else_ | `Ensure of ensure ]
      list (* zero or more *)
  * Token.t (* "end" *)
)

and binary = [
    `Arg_and_arg of (arg * Token.t (* "and" *) * arg)
  | `Arg_or_arg of (arg * Token.t (* "or" *) * arg)
  | `Arg_BARBAR_arg of (arg * Token.t (* "||" *) * arg)
  | `Arg_AMPAMP_arg of (arg * Token.t (* "&&" *) * arg)
  | `Arg_choice_LTLT_arg of (
        arg
      * [ `LTLT of Token.t (* "<<" *) | `GTGT of Token.t (* ">>" *) ]
      * arg
    )
  | `Arg_choice_LT_arg of (
        arg
      * [
            `LT of Token.t (* "<" *)
          | `LTEQ of Token.t (* "<=" *)
          | `GT of Token.t (* ">" *)
          | `GTEQ of Token.t (* ">=" *)
        ]
      * arg
    )
  | `Arg_AMP_arg of (arg * Token.t (* "&" *) * arg)
  | `Arg_choice_HAT_arg of (
        arg
      * [ `HAT of Token.t (* "^" *) | `BAR of Token.t (* "|" *) ]
      * arg
    )
  | `Arg_choice_PLUS_arg of (
        arg
      * [ `PLUS of Token.t (* "+" *) | `Bin_minus of binary_minus (*tok*) ]
      * arg
    )
  | `Arg_choice_SLASH_arg of (
        arg
      * [
            `SLASH of Token.t (* "/" *)
          | `PERC of Token.t (* "%" *)
          | `Bin_star of binary_star (*tok*)
        ]
      * arg
    )
  | `Arg_choice_EQEQ_arg of (
        arg
      * [
            `EQEQ of Token.t (* "==" *)
          | `BANGEQ of Token.t (* "!=" *)
          | `EQEQEQ of Token.t (* "===" *)
          | `LTEQGT of Token.t (* "<=>" *)
          | `EQTILDE of Token.t (* "=~" *)
          | `BANGTILDE of Token.t (* "!~" *)
        ]
      * arg
    )
  | `Arg_STARSTAR_arg of (arg * Token.t (* "**" *) * arg)
]

and then_ = [
    `Term_stmts of (terminator * statements)
  | `Opt_term_then_opt_stmts of (
        terminator option
      * Token.t (* "then" *)
      * statements option
    )
]

and lhs = [
    `Var of variable
  | `True of true_
  | `False of false_
  | `Nil of nil
  | `Scope_resol of scope_resolution
  | `Elem_ref of (
        primary
      * Token.t (* "[" *)
      * argument_list_with_trailing_comma option
      * Token.t (* "]" *)
    )
  | `Call of call
  | `Meth_call of method_call
]

and unary = [
    `Defi_arg of (Token.t (* "defined?" *) * arg)
  | `Not_arg of (Token.t (* "not" *) * arg)
  | `Choice_un_minus_arg of (anon_choice_un_minus * arg)
  | `Choice_BANG_arg of (
        [ `BANG of Token.t (* "!" *) | `TILDE of Token.t (* "~" *) ]
      * arg
    )
]

and expression = [
    `Cmd_bin of (
        expression
      * [ `Or of Token.t (* "or" *) | `And of Token.t (* "and" *) ]
      * expression
    )
  | `Cmd_assign of command_assignment
  | `Cmd_op_assign of (lhs * anon_choice_PLUSEQ * expression)
  | `Cmd_call of command_call
  | `Ret_cmd of (Token.t (* "return" *) * command_argument_list)
  | `Yield_cmd of (Token.t (* "yield" *) * command_argument_list)
  | `Brk_cmd of (Token.t (* "break" *) * command_argument_list)
  | `Next_cmd of (Token.t (* "next" *) * command_argument_list)
  | `Arg of arg
]

and arg = [
    `Prim of primary
  | `Assign of assignment
  | `Op_assign of (lhs * anon_choice_PLUSEQ * arg)
  | `Cond of (arg * Token.t (* "?" *) * arg * Token.t (* ":" *) * arg)
  | `Range of (
        arg
      * [ `DOTDOT of Token.t (* ".." *) | `DOTDOTDOT of Token.t (* "..." *) ]
      * arg
    )
  | `Bin of binary
  | `Un of unary
]

and right_assignment_list = (
    pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
)

and do_block = (
    Token.t (* "do" *)
  * terminator option
  * (block_parameters * terminator option) option
  * body_statement
)

and pair = [
    `Arg_EQGT_arg of (arg * Token.t (* "=>" *) * arg)
  | `Choice_id_hash_key_imm_tok_COLON_arg of (
        [
            `Id_hash_key of identifier_hash_key (*tok*)
          | `Id of identifier (*tok*)
          | `Cst of constant (*tok*)
          | `Str of string_
        ]
      * Token.t (* ":" *)
      * arg
    )
]

and assignment = [
  `Choice_lhs_EQ_choice_arg of (
      anon_choice_lhs
    * Token.t (* "=" *)
    * [
          `Arg of arg
        | `Splat_arg of splat_argument
        | `Right_assign_list of right_assignment_list
      ]
  )
]

and literal_contents =
  [
      `Str_content of string_content (*tok*)
    | `Interp of interpolation
    | `Esc_seq of escape_sequence (*tok*)
  ]
    list (* one or more *)

and when_ = (
    Token.t (* "when" *)
  * pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
  * anon_choice_term
)

and bare_parameters = (
    simple_formal_parameter
  * (Token.t (* "," *) * formal_parameter) list (* zero or more *)
)

and statement = [
    `Undef of (
        Token.t (* "undef" *)
      * method_name
      * (Token.t (* "," *) * method_name) list (* zero or more *)
    )
  | `Alias of (Token.t (* "alias" *) * method_name * method_name)
  | `If_modi of (statement * Token.t (* "if" *) * expression)
  | `Unless_modi of (statement * Token.t (* "unless" *) * expression)
  | `While_modi of (statement * Token.t (* "while" *) * expression)
  | `Until_modi of (statement * Token.t (* "until" *) * expression)
  | `Rescue_modi of (statement * Token.t (* "rescue" *) * expression)
  | `Begin_blk of (
        Token.t (* "BEGIN" *)
      * Token.t (* "{" *)
      * statements option
      * Token.t (* "}" *)
    )
  | `End_blk of (
        Token.t (* "END" *)
      * Token.t (* "{" *)
      * statements option
      * Token.t (* "}" *)
    )
  | `Exp of expression
]

and anon_choice_cst = [
    `Cst of constant (*tok*)
  | `Scope_resol of scope_resolution
]

and command_assignment = [
  `Choice_lhs_EQ_exp of (anon_choice_lhs * Token.t (* "=" *) * expression)
]

and splat_argument = (splat_star (*tok*) * arg)

and exceptions = (
    pattern
  * (Token.t (* "," *) * pattern) list (* zero or more *)
)

and symbol = [
    `Simple_symb of simple_symbol (*tok*)
  | `Symb_start_opt_lit_content_str_end of (
        symbol_start (*tok*)
      * literal_contents option
      * string_end (*tok*)
    )
]

and ensure = (Token.t (* "ensure" *) * statements option)

and command_call = [
    `Choice_var_cmd_arg_list of (anon_choice_var * command_argument_list)
  | `Choice_var_cmd_arg_list_blk of (
        anon_choice_var * command_argument_list * block
    )
  | `Choice_var_cmd_arg_list_do_blk of (
        anon_choice_var * command_argument_list * do_block
    )
]

and hash_splat_argument = (Token.t (* "**" *) * arg)

and exception_variable = (Token.t (* "=>" *) * lhs)

and method_name = [
    `Id of identifier (*tok*)
  | `Cst of constant (*tok*)
  | `Setter of (identifier (*tok*) * Token.t (* "=" *))
  | `Symb of symbol
  | `Op of operator
  | `Inst_var of instance_variable (*tok*)
  | `Class_var of class_variable (*tok*)
  | `Global_var of global_variable (*tok*)
]

and block_parameters = (
    Token.t (* "|" *)
  * anon_formal_param_rep_COMMA_formal_param option
  * Token.t (* "," *) option
  * (
        Token.t (* ";" *)
      * identifier (*tok*)
      * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
    )
      option
  * Token.t (* "|" *)
)

and superclass = (Token.t (* "<" *) * arg)

and anon_choice_lhs_ = [
    `Lhs of lhs
  | `Rest_assign of (Token.t (* "*" *) * lhs option)
  | `Dest_left_assign of (
        Token.t (* "(" *) * left_assignment_list * Token.t (* ")" *)
    )
]

and pattern = [ `Arg of arg | `Splat_arg of splat_argument ]

and command_argument_list = [
    `Arg_rep_COMMA_arg of (
        argument
      * (Token.t (* "," *) * argument) list (* zero or more *)
    )
  | `Cmd_call of command_call
]

and anon_lit_content_rep_pat_3d340f6_lit_content = (
    literal_contents
  * (pat_3d340f6 (*tok*) * literal_contents) list (* zero or more *)
)

and argument_list = (
    Token.t (* "(" *)
  * argument_list_with_trailing_comma option
  * Token.t (* ")" *)
)

and left_assignment_list = mlhs

and parameters = (
    Token.t (* "(" *)
  * anon_formal_param_rep_COMMA_formal_param option
  * Token.t (* ")" *)
)

and anon_choice_term = [ `Term of terminator | `Then of then_ ]

and simple_formal_parameter = [
    `Id of identifier (*tok*)
  | `Splat_param of (Token.t (* "*" *) * identifier (*tok*) option)
  | `Hash_splat_param of (Token.t (* "**" *) * identifier (*tok*) option)
  | `Blk_param of (Token.t (* "&" *) * identifier (*tok*))
  | `Kw_param of (identifier (*tok*) * Token.t (* ":" *) * arg option)
  | `Opt_param of (identifier (*tok*) * Token.t (* "=" *) * arg)
]

and interpolation = (Token.t (* "#{" *) * statement * Token.t (* "}" *))

and formal_parameter = [
    `Simple_formal_param of simple_formal_parameter
  | `Params of parameters
]

and anon_choice_else = [
    `Else of else_
  | `Elsif of (
        Token.t (* "elsif" *)
      * statement
      * anon_choice_term
      * anon_choice_else option
    )
]
[@@deriving sexp_of]

type program = (
    statements option
  * (Token.t (* "__END__" *) * line_break (*tok*) * uninterpreted (*tok*))
      option
)
[@@deriving sexp_of]

type imm_tok_LPAR (* inlined *) = Token.t (* "(" *)
[@@deriving sexp_of]

type self (* inlined *) = Token.t (* "self" *)
[@@deriving sexp_of]

type empty_statement (* inlined *) = Token.t (* ";" *)
[@@deriving sexp_of]

type imm_tok_LBRACK (* inlined *) = Token.t (* "[" *)
[@@deriving sexp_of]

type imm_tok_COLONCOLON (* inlined *) = Token.t (* "::" *)
[@@deriving sexp_of]

type imm_tok_COLON (* inlined *) = Token.t (* ":" *)
[@@deriving sexp_of]

type comment (* inlined *) = Token.t
[@@deriving sexp_of]

type super (* inlined *) = Token.t (* "super" *)
[@@deriving sexp_of]

type splat_parameter (* inlined *) = (
    Token.t (* "*" *)
  * identifier (*tok*) option
)
[@@deriving sexp_of]

type setter (* inlined *) = (identifier (*tok*) * Token.t (* "=" *))
[@@deriving sexp_of]

type block_parameter (* inlined *) = (Token.t (* "&" *) * identifier (*tok*))
[@@deriving sexp_of]

type hash_splat_parameter (* inlined *) = (
    Token.t (* "**" *)
  * identifier (*tok*) option
)
[@@deriving sexp_of]

type rational (* inlined *) = (integer (*tok*) * Token.t (* "r" *))
[@@deriving sexp_of]

type unary_literal (* inlined *) = (
    anon_choice_un_minus
  * [ `Int of integer (*tok*) | `Float of float_ (*tok*) ]
)
[@@deriving sexp_of]

type while_modifier (* inlined *) = (
    statement * Token.t (* "while" *) * expression
)
[@@deriving sexp_of]

type singleton_method (* inlined *) = (
    Token.t (* "def" *)
  * [
        `Var of variable
      | `LPAR_arg_RPAR of (Token.t (* "(" *) * arg * Token.t (* ")" *))
    ]
  * [ `DOT of Token.t (* "." *) | `COLONCOLON of Token.t (* "::" *) ]
  * method_rest
)
[@@deriving sexp_of]

type hash (* inlined *) = (
    Token.t (* "{" *)
  * (
        anon_choice_pair
      * (Token.t (* "," *) * anon_choice_pair) list (* zero or more *)
      * Token.t (* "," *) option
    )
      option
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type yield_command (* inlined *) = (
    Token.t (* "yield" *) * command_argument_list
)
[@@deriving sexp_of]

type yield (* inlined *) = (Token.t (* "yield" *) * argument_list option)
[@@deriving sexp_of]

type undef (* inlined *) = (
    Token.t (* "undef" *)
  * method_name
  * (Token.t (* "," *) * method_name) list (* zero or more *)
)
[@@deriving sexp_of]

type command_binary (* inlined *) = (
    expression
  * [ `Or of Token.t (* "or" *) | `And of Token.t (* "and" *) ]
  * expression
)
[@@deriving sexp_of]

type next_command (* inlined *) = (
    Token.t (* "next" *) * command_argument_list
)
[@@deriving sexp_of]

type until (* inlined *) = (
    Token.t (* "until" *)
  * arg
  * do_
  * statements option
  * Token.t (* "end" *)
)
[@@deriving sexp_of]

type unless (* inlined *) = (
    Token.t (* "unless" *)
  * statement
  * anon_choice_term
  * anon_choice_else option
  * Token.t (* "end" *)
)
[@@deriving sexp_of]

type break (* inlined *) = (Token.t (* "break" *) * argument_list option)
[@@deriving sexp_of]

type next (* inlined *) = (Token.t (* "next" *) * argument_list option)
[@@deriving sexp_of]

type begin_block (* inlined *) = (
    Token.t (* "BEGIN" *)
  * Token.t (* "{" *)
  * statements option
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type return (* inlined *) = (Token.t (* "return" *) * argument_list option)
[@@deriving sexp_of]

type parenthesized_unary (* inlined *) = (
    [ `Defi of Token.t (* "defined?" *) | `Not of Token.t (* "not" *) ]
  * parenthesized_statements
)
[@@deriving sexp_of]

type range (* inlined *) = (
    arg
  * [ `DOTDOT of Token.t (* ".." *) | `DOTDOTDOT of Token.t (* "..." *) ]
  * arg
)
[@@deriving sexp_of]

type element_reference (* inlined *) = (
    primary
  * Token.t (* "[" *)
  * argument_list_with_trailing_comma option
  * Token.t (* "]" *)
)
[@@deriving sexp_of]

type module_ (* inlined *) = (
    Token.t (* "module" *)
  * anon_choice_cst
  * [
        `Term_body_stmt of (terminator * body_statement)
      | `End of Token.t (* "end" *)
    ]
)
[@@deriving sexp_of]

type case (* inlined *) = (
    Token.t (* "case" *)
  * arg option
  * terminator
  * Token.t (* ";" *) list (* zero or more *)
  * when_ list (* zero or more *)
  * else_ option
  * Token.t (* "end" *)
)
[@@deriving sexp_of]

type elsif (* inlined *) = (
    Token.t (* "elsif" *)
  * statement
  * anon_choice_term
  * anon_choice_else option
)
[@@deriving sexp_of]

type unless_modifier (* inlined *) = (
    statement * Token.t (* "unless" *) * expression
)
[@@deriving sexp_of]

type operator_assignment (* inlined *) = (lhs * anon_choice_PLUSEQ * arg)
[@@deriving sexp_of]

type subshell (* inlined *) = (
    subshell_start (*tok*)
  * literal_contents option
  * string_end (*tok*)
)
[@@deriving sexp_of]

type lambda (* inlined *) = (
    Token.t (* "->" *)
  * [ `Params of parameters | `Bare_params of bare_parameters ] option
  * [ `Blk of block | `Do_blk of do_block ]
)
[@@deriving sexp_of]

type chained_string (* inlined *) = (
    string_
  * string_ list (* one or more *)
)
[@@deriving sexp_of]

type method_ (* inlined *) = (Token.t (* "def" *) * method_rest)
[@@deriving sexp_of]

type conditional (* inlined *) = (
    arg * Token.t (* "?" *) * arg * Token.t (* ":" *) * arg
)
[@@deriving sexp_of]

type retry (* inlined *) = (Token.t (* "retry" *) * argument_list option)
[@@deriving sexp_of]

type array_ (* inlined *) = (
    Token.t (* "[" *)
  * argument_list_with_trailing_comma option
  * Token.t (* "]" *)
)
[@@deriving sexp_of]

type rescue_modifier (* inlined *) = (
    statement * Token.t (* "rescue" *) * expression
)
[@@deriving sexp_of]

type class_ (* inlined *) = (
    Token.t (* "class" *)
  * anon_choice_cst
  * superclass option
  * terminator
  * body_statement
)
[@@deriving sexp_of]

type until_modifier (* inlined *) = (
    statement * Token.t (* "until" *) * expression
)
[@@deriving sexp_of]

type for_ (* inlined *) = (
    Token.t (* "for" *)
  * left_assignment_list
  * in_
  * do_
  * statements option
  * Token.t (* "end" *)
)
[@@deriving sexp_of]

type optional_parameter (* inlined *) = (
    identifier (*tok*) * Token.t (* "=" *) * arg
)
[@@deriving sexp_of]

type singleton_class (* inlined *) = (
    Token.t (* "class" *) * singleton_class_left_angle_left_langle (*tok*)
  * arg * terminator * body_statement
)
[@@deriving sexp_of]

type rest_assignment (* inlined *) = (Token.t (* "*" *) * lhs option)
[@@deriving sexp_of]

type alias (* inlined *) = (
    Token.t (* "alias" *) * method_name * method_name
)
[@@deriving sexp_of]

type if_modifier (* inlined *) = (
    statement * Token.t (* "if" *) * expression
)
[@@deriving sexp_of]

type return_command (* inlined *) = (
    Token.t (* "return" *) * command_argument_list
)
[@@deriving sexp_of]

type symbol_array (* inlined *) = (
    symbol_array_start (*tok*)
  * pat_3d340f6 (*tok*) option
  * anon_lit_content_rep_pat_3d340f6_lit_content option
  * pat_3d340f6 (*tok*) option
  * string_end (*tok*)
)
[@@deriving sexp_of]

type redo (* inlined *) = (Token.t (* "redo" *) * argument_list option)
[@@deriving sexp_of]

type break_command (* inlined *) = (
    Token.t (* "break" *) * command_argument_list
)
[@@deriving sexp_of]

type block_argument (* inlined *) = (block_ampersand (*tok*) * arg)
[@@deriving sexp_of]

type regex (* inlined *) = (
    regex_start (*tok*)
  * literal_contents option
  * string_end (*tok*)
)
[@@deriving sexp_of]

type command_operator_assignment (* inlined *) = (
    lhs * anon_choice_PLUSEQ * expression
)
[@@deriving sexp_of]

type end_block (* inlined *) = (
    Token.t (* "END" *)
  * Token.t (* "{" *)
  * statements option
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type string_array (* inlined *) = (
    string_array_start (*tok*)
  * pat_3d340f6 (*tok*) option
  * anon_lit_content_rep_pat_3d340f6_lit_content option
  * pat_3d340f6 (*tok*) option
  * string_end (*tok*)
)
[@@deriving sexp_of]

type while_ (* inlined *) = (
    Token.t (* "while" *)
  * arg
  * do_
  * statements option
  * Token.t (* "end" *)
)
[@@deriving sexp_of]

type if_ (* inlined *) = (
    Token.t (* "if" *)
  * statement
  * anon_choice_term
  * anon_choice_else option
  * Token.t (* "end" *)
)
[@@deriving sexp_of]

type keyword_parameter (* inlined *) = (
    identifier (*tok*)
  * Token.t (* ":" *)
  * arg option
)
[@@deriving sexp_of]

type destructured_left_assignment (* inlined *) = (
    Token.t (* "(" *) * left_assignment_list * Token.t (* ")" *)
)
[@@deriving sexp_of]

type begin_ (* inlined *) = (
    Token.t (* "begin" *)
  * terminator option
  * body_statement
)
[@@deriving sexp_of]

type heredoc_body (* inlined *) = (
    heredoc_body_start (*tok*)
  * [
        `Here_content of heredoc_content (*tok*)
      | `Interp of interpolation
      | `Esc_seq of escape_sequence (*tok*)
    ]
      list (* zero or more *)
  * heredoc_end (*tok*)
)
[@@deriving sexp_of]

let dump_tree root =
  sexp_of_program root
  |> Print_sexp.to_stdout
