(* Generated by ocaml-tree-sitter. *)
(*
   go grammar

   entrypoint: source_file
*)

open! Sexplib.Conv
open Tree_sitter_run

type identifier = Token.t
[@@deriving sexp_of]

type float_literal = Token.t
[@@deriving sexp_of]

type raw_string_literal = Token.t
[@@deriving sexp_of]

type anon_choice_EQ = [
    `EQ of Token.t (* "=" *)
  | `COLONEQ of Token.t (* ":=" *)
]
[@@deriving sexp_of]

type anon_choice_LF = [
    `LF of Token.t (* "\n" *)
  | `SEMI of Token.t (* ";" *)
]
[@@deriving sexp_of]

type imm_tok_pat_101b4f2 = Token.t (* pattern "[^\"\\n\\\\]+" *)
[@@deriving sexp_of]

type imaginary_literal = Token.t
[@@deriving sexp_of]

type int_literal = Token.t
[@@deriving sexp_of]

type escape_sequence = Token.t
[@@deriving sexp_of]

type rune_literal = Token.t
[@@deriving sexp_of]

type anon_choice_new = [
    `New of Token.t (* "new" *)
  | `Make of Token.t (* "make" *)
]
[@@deriving sexp_of]

type qualified_type = (
    identifier (*tok*) * Token.t (* "." *) * identifier (*tok*)
)
[@@deriving sexp_of]

type empty_labeled_statement = (identifier (*tok*) * Token.t (* ":" *))
[@@deriving sexp_of]

type field_name_list = (
    identifier (*tok*)
  * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
)
[@@deriving sexp_of]

type string_literal = [
    `Raw_str_lit of raw_string_literal (*tok*)
  | `Inte_str_lit of (
        Token.t (* "\"" *)
      * [
            `Imm_tok_pat_101b4f2 of imm_tok_pat_101b4f2 (*tok*)
          | `Esc_seq of escape_sequence (*tok*)
        ]
          list (* zero or more *)
      * Token.t (* "\"" *)
    )
]
[@@deriving sexp_of]

type import_spec = (
    [
        `Dot of Token.t (* "." *)
      | `Blank_id of Token.t (* "_" *)
      | `Id of identifier (*tok*)
    ]
      option
  * string_literal
)
[@@deriving sexp_of]

type type_case = (
    Token.t (* "case" *)
  * type_
  * (Token.t (* "," *) * type_) list (* zero or more *)
  * Token.t (* ":" *)
  * statement_list option
)

and simple_statement = [
    `Exp of expression
  | `Send_stmt of send_statement
  | `Inc_stmt of (expression * Token.t (* "++" *))
  | `Dec_stmt of (expression * Token.t (* "--" *))
  | `Assign_stmt of (
        expression_list
      * [
            `STAREQ of Token.t (* "*=" *)
          | `SLASHEQ of Token.t (* "/=" *)
          | `PERCEQ of Token.t (* "%=" *)
          | `LTLTEQ of Token.t (* "<<=" *)
          | `GTGTEQ of Token.t (* ">>=" *)
          | `AMPEQ of Token.t (* "&=" *)
          | `AMPHATEQ of Token.t (* "&^=" *)
          | `PLUSEQ of Token.t (* "+=" *)
          | `DASHEQ of Token.t (* "-=" *)
          | `BAREQ of Token.t (* "|=" *)
          | `HATEQ of Token.t (* "^=" *)
          | `EQ of Token.t (* "=" *)
        ]
      * expression_list
    )
  | `Short_var_decl of (
        expression_list * Token.t (* ":=" *) * expression_list
    )
]

and anon_choice_exp = [
    `Exp of expression
  | `Vari_arg of (expression * Token.t (* "..." *))
]

and binary_expression = [
    `Exp_choice_STAR_exp of (
        expression
      * [
            `STAR of Token.t (* "*" *)
          | `SLASH of Token.t (* "/" *)
          | `PERC of Token.t (* "%" *)
          | `LTLT of Token.t (* "<<" *)
          | `GTGT of Token.t (* ">>" *)
          | `AMP of Token.t (* "&" *)
          | `AMPHAT of Token.t (* "&^" *)
        ]
      * expression
    )
  | `Exp_choice_PLUS_exp of (
        expression
      * [
            `PLUS of Token.t (* "+" *)
          | `DASH of Token.t (* "-" *)
          | `BAR of Token.t (* "|" *)
          | `HAT of Token.t (* "^" *)
        ]
      * expression
    )
  | `Exp_choice_EQEQ_exp of (
        expression
      * [
            `EQEQ of Token.t (* "==" *)
          | `BANGEQ of Token.t (* "!=" *)
          | `LT of Token.t (* "<" *)
          | `LTEQ of Token.t (* "<=" *)
          | `GT of Token.t (* ">" *)
          | `GTEQ of Token.t (* ">=" *)
        ]
      * expression
    )
  | `Exp_AMPAMP_exp of (expression * Token.t (* "&&" *) * expression)
  | `Exp_BARBAR_exp of (expression * Token.t (* "||" *) * expression)
]

and anon_choice_type_id = [
    `Id of identifier (*tok*)
  | `Qual_type of qualified_type
  | `Meth_spec of (
        identifier (*tok*)
      * parameter_list
      * anon_choice_param_list option
    )
]

and block = (Token.t (* "{" *) * statement_list option * Token.t (* "}" *))

and receive_statement = (
    (expression_list * anon_choice_EQ) option
  * expression
)

and field_declaration = (
    [
        `Id_rep_COMMA_id_choice_simple_type of (
            identifier (*tok*)
          * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
          * type_
        )
      | `Opt_STAR_choice_id of (
            Token.t (* "*" *) option
          * [ `Id of identifier (*tok*) | `Qual_type of qualified_type ]
        )
    ]
  * string_literal option
)

and special_argument_list = (
    Token.t (* "(" *)
  * type_
  * (Token.t (* "," *) * expression) list (* zero or more *)
  * Token.t (* "," *) option
  * Token.t (* ")" *)
)

and for_clause = (
    simple_statement option
  * Token.t (* ";" *)
  * expression option
  * Token.t (* ";" *)
  * simple_statement option
)

and anon_choice_param_decl = [
    `Param_decl of (field_name_list option * type_)
  | `Vari_param_decl of (
        identifier (*tok*) option
      * Token.t (* "..." *)
      * type_
    )
]

and method_spec_list = (
    Token.t (* "{" *)
  * (
        anon_choice_type_id
      * (anon_choice_LF * anon_choice_type_id) list (* zero or more *)
      * anon_choice_LF option
    )
      option
  * Token.t (* "}" *)
)

and array_type = (Token.t (* "[" *) * expression * Token.t (* "]" *) * type_)

and struct_type = (Token.t (* "struct" *) * field_declaration_list)

and anon_choice_param_list = [
    `Param_list of parameter_list
  | `Simple_type of simple_type
]

and simple_type = [
    `Id of identifier (*tok*)
  | `Qual_type of qualified_type
  | `Poin_type of (Token.t (* "*" *) * type_)
  | `Struct_type of struct_type
  | `Inte_type of (Token.t (* "interface" *) * method_spec_list)
  | `Array_type of array_type
  | `Slice_type of slice_type
  | `Map_type of map_type
  | `Chan_type of channel_type
  | `Func_type of (
        Token.t (* "func" *)
      * parameter_list
      * anon_choice_param_list option
    )
]

and call_expression = [
    `Choice_new_spec_arg_list of (anon_choice_new * special_argument_list)
  | `Exp_arg_list of (expression * argument_list)
]

and default_case = (
    Token.t (* "default" *)
  * Token.t (* ":" *)
  * statement_list option
)

and slice_type = (Token.t (* "[" *) * Token.t (* "]" *) * type_)

and expression_list = (
    expression
  * (Token.t (* "," *) * expression) list (* zero or more *)
)

and expression = [
    `Un_exp of (
        [
            `PLUS of Token.t (* "+" *)
          | `DASH of Token.t (* "-" *)
          | `BANG of Token.t (* "!" *)
          | `HAT of Token.t (* "^" *)
          | `STAR of Token.t (* "*" *)
          | `AMP of Token.t (* "&" *)
          | `LTDASH of Token.t (* "<-" *)
        ]
      * expression
    )
  | `Bin_exp of binary_expression
  | `Sele_exp of (expression * Token.t (* "." *) * identifier (*tok*))
  | `Index_exp of (
        expression * Token.t (* "[" *) * expression * Token.t (* "]" *)
    )
  | `Slice_exp of (
        expression
      * Token.t (* "[" *)
      * [
            `Opt_exp_COLON_opt_exp of (
                expression option
              * Token.t (* ":" *)
              * expression option
            )
          | `Opt_exp_COLON_exp_COLON_exp of (
                expression option
              * Token.t (* ":" *)
              * expression
              * Token.t (* ":" *)
              * expression
            )
        ]
      * Token.t (* "]" *)
    )
  | `Call_exp of call_expression
  | `Type_asse_exp of (
        expression * Token.t (* "." *) * Token.t (* "(" *) * type_
      * Token.t (* ")" *)
    )
  | `Type_conv_exp of (
        type_
      * Token.t (* "(" *)
      * expression
      * Token.t (* "," *) option
      * Token.t (* ")" *)
    )
  | `Id of identifier (*tok*)
  | `Choice_new of anon_choice_new
  | `Comp_lit of (
        [
            `Map_type of map_type
          | `Slice_type of slice_type
          | `Array_type of array_type
          | `Impl_len_array_type of implicit_length_array_type
          | `Struct_type of struct_type
          | `Id of identifier (*tok*)
          | `Qual_type of qualified_type
        ]
      * literal_value
    )
  | `Func_lit of (
        Token.t (* "func" *)
      * parameter_list
      * anon_choice_param_list option
      * block
    )
  | `Choice_raw_str_lit of string_literal
  | `Int_lit of int_literal (*tok*)
  | `Float_lit of float_literal (*tok*)
  | `Imag_lit of imaginary_literal (*tok*)
  | `Rune_lit of rune_literal (*tok*)
  | `Nil of Token.t (* "nil" *)
  | `True of Token.t (* "true" *)
  | `False of Token.t (* "false" *)
  | `Paren_exp of (Token.t (* "(" *) * expression * Token.t (* ")" *))
]

and type_switch_header = (
    (simple_statement * Token.t (* ";" *)) option
  * (expression_list * Token.t (* ":=" *)) option
  * expression
  * Token.t (* "." *)
  * Token.t (* "(" *)
  * Token.t (* "type" *)
  * Token.t (* ")" *)
)

and type_alias = (identifier (*tok*) * Token.t (* "=" *) * type_)

and if_statement = (
    Token.t (* "if" *)
  * (simple_statement * Token.t (* ";" *)) option
  * expression
  * block
  * (Token.t (* "else" *) * [ `Blk of block | `If_stmt of if_statement ])
      option
)

and statement = [
    `Decl of declaration
  | `Simple_stmt of simple_statement
  | `Ret_stmt of (Token.t (* "return" *) * expression_list option)
  | `Go_stmt of (Token.t (* "go" *) * expression)
  | `Defer_stmt of (Token.t (* "defer" *) * expression)
  | `If_stmt of if_statement
  | `For_stmt of (
        Token.t (* "for" *)
      * [
            `Exp of expression
          | `For_clause of for_clause
          | `Range_clause of range_clause
        ]
          option
      * block
    )
  | `Exp_switch_stmt of (
        Token.t (* "switch" *)
      * (simple_statement * Token.t (* ";" *)) option
      * expression option
      * Token.t (* "{" *)
      * [ `Exp_case of expression_case | `Defa_case of default_case ]
          list (* zero or more *)
      * Token.t (* "}" *)
    )
  | `Type_switch_stmt of (
        Token.t (* "switch" *)
      * type_switch_header
      * Token.t (* "{" *)
      * [ `Type_case of type_case | `Defa_case of default_case ]
          list (* zero or more *)
      * Token.t (* "}" *)
    )
  | `Select_stmt of (
        Token.t (* "select" *)
      * Token.t (* "{" *)
      * [ `Comm_case of communication_case | `Defa_case of default_case ]
          list (* zero or more *)
      * Token.t (* "}" *)
    )
  | `Labe_stmt of (identifier (*tok*) * Token.t (* ":" *) * statement)
  | `Fall_stmt of Token.t (* "fallthrough" *)
  | `Brk_stmt of (Token.t (* "break" *) * identifier (*tok*) option)
  | `Cont_stmt of (Token.t (* "continue" *) * identifier (*tok*) option)
  | `Goto_stmt of (Token.t (* "goto" *) * identifier (*tok*))
  | `Blk of block
  | `Empty_stmt of Token.t (* ";" *)
]

and range_clause = (
    (expression_list * anon_choice_EQ) option
  * Token.t (* "range" *)
  * expression
)

and send_statement = (expression * Token.t (* "<-" *) * expression)

and field_declaration_list = (
    Token.t (* "{" *)
  * (
        field_declaration
      * (anon_choice_LF * field_declaration) list (* zero or more *)
      * anon_choice_LF option
    )
      option
  * Token.t (* "}" *)
)

and map_type = (
    Token.t (* "map" *) * Token.t (* "[" *) * type_ * Token.t (* "]" *)
  * type_
)

and implicit_length_array_type = (
    Token.t (* "[" *) * Token.t (* "..." *) * Token.t (* "]" *) * type_
)

and expression_case = (
    Token.t (* "case" *)
  * expression_list
  * Token.t (* ":" *)
  * statement_list option
)

and argument_list = (
    Token.t (* "(" *)
  * (
        anon_choice_exp
      * (Token.t (* "," *) * anon_choice_exp) list (* zero or more *)
      * Token.t (* "," *) option
    )
      option
  * Token.t (* ")" *)
)

and type_ = [
    `Simple_type of simple_type
  | `Paren_type of (Token.t (* "(" *) * type_ * Token.t (* ")" *))
]

and const_spec = (
    identifier (*tok*)
  * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
  * (type_ option * Token.t (* "=" *) * expression_list) option
)

and anon_choice_elem = [
    `Elem of element
  | `Keyed_elem of (
        [
            `Exp_COLON of (expression * Token.t (* ":" *))
          | `Lit_value_COLON of (literal_value * Token.t (* ":" *))
          | `Id_COLON of empty_labeled_statement
        ]
      * element
    )
]

and type_spec = (identifier (*tok*) * type_)

and channel_type = [
    `Chan_choice_simple_type of (Token.t (* "chan" *) * type_)
  | `Chan_LTDASH_choice_simple_type of (
        Token.t (* "chan" *) * Token.t (* "<-" *) * type_
    )
  | `LTDASH_chan_choice_simple_type of (
        Token.t (* "<-" *) * Token.t (* "chan" *) * type_
    )
]

and parameter_list = (
    Token.t (* "(" *)
  * (
        (
            anon_choice_param_decl
          * (Token.t (* "," *) * anon_choice_param_decl)
              list (* zero or more *)
        )
          option
      * Token.t (* "," *) option
    )
      option
  * Token.t (* ")" *)
)

and element = [ `Exp of expression | `Lit_value of literal_value ]

and var_spec = (
    identifier (*tok*)
  * (Token.t (* "," *) * identifier (*tok*)) list (* zero or more *)
  * [
        `Choice_simple_type_opt_EQ_exp_list of (
            type_
          * (Token.t (* "=" *) * expression_list) option
        )
      | `EQ_exp_list of (Token.t (* "=" *) * expression_list)
    ]
)

and declaration = [
    `Const_decl of (
        Token.t (* "const" *)
      * [
            `Const_spec of const_spec
          | `LPAR_rep_const_spec_choice_LF_RPAR of (
                Token.t (* "(" *)
              * (const_spec * anon_choice_LF) list (* zero or more *)
              * Token.t (* ")" *)
            )
        ]
    )
  | `Type_decl of (
        Token.t (* "type" *)
      * [
            `Type_spec of type_spec
          | `Type_alias of type_alias
          | `LPAR_rep_choice_type_spec_choice_LF_RPAR of (
                Token.t (* "(" *)
              * (
                    [ `Type_spec of type_spec | `Type_alias of type_alias ]
                  * anon_choice_LF
                )
                  list (* zero or more *)
              * Token.t (* ")" *)
            )
        ]
    )
  | `Var_decl of (
        Token.t (* "var" *)
      * [
            `Var_spec of var_spec
          | `LPAR_rep_var_spec_choice_LF_RPAR of (
                Token.t (* "(" *)
              * (var_spec * anon_choice_LF) list (* zero or more *)
              * Token.t (* ")" *)
            )
        ]
    )
]

and statement_list = [
    `Stmt_rep_choice_LF_stmt_opt_choice_LF_opt_empty_labe_stmt of (
        statement
      * (anon_choice_LF * statement) list (* zero or more *)
      * (anon_choice_LF * empty_labeled_statement option) option
    )
  | `Empty_labe_stmt of empty_labeled_statement
]

and communication_case = (
    Token.t (* "case" *)
  * [ `Send_stmt of send_statement | `Rece_stmt of receive_statement ]
  * Token.t (* ":" *)
  * statement_list option
)

and literal_value = (
    Token.t (* "{" *)
  * (
        anon_choice_elem
      * (Token.t (* "," *) * anon_choice_elem) list (* zero or more *)
      * Token.t (* "," *) option
    )
      option
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type import_spec_list = (
    Token.t (* "(" *)
  * (import_spec * anon_choice_LF) list (* zero or more *)
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type top_level_declaration = [
    `Pack_clause of (Token.t (* "package" *) * identifier (*tok*))
  | `Func_decl of (
        Token.t (* "func" *)
      * identifier (*tok*)
      * parameter_list
      * anon_choice_param_list option
      * block option
    )
  | `Meth_decl of (
        Token.t (* "func" *)
      * parameter_list
      * identifier (*tok*)
      * parameter_list
      * anon_choice_param_list option
      * block option
    )
  | `Import_decl of (
        Token.t (* "import" *)
      * [
            `Import_spec of import_spec
          | `Import_spec_list of import_spec_list
        ]
    )
]
[@@deriving sexp_of]

type source_file =
  [
      `Stmt_choice_LF of (statement * anon_choice_LF)
    | `Choice_pack_clause_opt_choice_LF of (
          top_level_declaration
        * anon_choice_LF option
      )
  ]
    list (* zero or more *)
[@@deriving sexp_of]

type empty_statement (* inlined *) = Token.t (* ";" *)
[@@deriving sexp_of]

type blank_identifier (* inlined *) = Token.t (* "_" *)
[@@deriving sexp_of]

type dot (* inlined *) = Token.t (* "." *)
[@@deriving sexp_of]

type comment (* inlined *) = Token.t
[@@deriving sexp_of]

type false_ (* inlined *) = Token.t (* "false" *)
[@@deriving sexp_of]

type nil (* inlined *) = Token.t (* "nil" *)
[@@deriving sexp_of]

type fallthrough_statement (* inlined *) = Token.t (* "fallthrough" *)
[@@deriving sexp_of]

type true_ (* inlined *) = Token.t (* "true" *)
[@@deriving sexp_of]

type field_identifier (* inlined *) = identifier (*tok*)
[@@deriving sexp_of]

type type_identifier (* inlined *) = identifier (*tok*)
[@@deriving sexp_of]

type package_identifier (* inlined *) = identifier (*tok*)
[@@deriving sexp_of]

type interpreted_string_literal (* inlined *) = (
    Token.t (* "\"" *)
  * [
        `Imm_tok_pat_101b4f2 of imm_tok_pat_101b4f2 (*tok*)
      | `Esc_seq of escape_sequence (*tok*)
    ]
      list (* zero or more *)
  * Token.t (* "\"" *)
)
[@@deriving sexp_of]

type goto_statement (* inlined *) = (
    Token.t (* "goto" *) * identifier (*tok*)
)
[@@deriving sexp_of]

type continue_statement (* inlined *) = (
    Token.t (* "continue" *)
  * identifier (*tok*) option
)
[@@deriving sexp_of]

type package_clause (* inlined *) = (
    Token.t (* "package" *) * identifier (*tok*)
)
[@@deriving sexp_of]

type break_statement (* inlined *) = (
    Token.t (* "break" *)
  * identifier (*tok*) option
)
[@@deriving sexp_of]

type parenthesized_expression (* inlined *) = (
    Token.t (* "(" *) * expression * Token.t (* ")" *)
)
[@@deriving sexp_of]

type method_spec (* inlined *) = (
    identifier (*tok*)
  * parameter_list
  * anon_choice_param_list option
)
[@@deriving sexp_of]

type dec_statement (* inlined *) = (expression * Token.t (* "--" *))
[@@deriving sexp_of]

type labeled_statement (* inlined *) = (
    identifier (*tok*) * Token.t (* ":" *) * statement
)
[@@deriving sexp_of]

type type_conversion_expression (* inlined *) = (
    type_
  * Token.t (* "(" *)
  * expression
  * Token.t (* "," *) option
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type composite_literal (* inlined *) = (
    [
        `Map_type of map_type
      | `Slice_type of slice_type
      | `Array_type of array_type
      | `Impl_len_array_type of implicit_length_array_type
      | `Struct_type of struct_type
      | `Id of identifier (*tok*)
      | `Qual_type of qualified_type
    ]
  * literal_value
)
[@@deriving sexp_of]

type pointer_type (* inlined *) = (Token.t (* "*" *) * type_)
[@@deriving sexp_of]

type inc_statement (* inlined *) = (expression * Token.t (* "++" *))
[@@deriving sexp_of]

type type_assertion_expression (* inlined *) = (
    expression * Token.t (* "." *) * Token.t (* "(" *) * type_
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type var_declaration (* inlined *) = (
    Token.t (* "var" *)
  * [
        `Var_spec of var_spec
      | `LPAR_rep_var_spec_choice_LF_RPAR of (
            Token.t (* "(" *)
          * (var_spec * anon_choice_LF) list (* zero or more *)
          * Token.t (* ")" *)
        )
    ]
)
[@@deriving sexp_of]

type select_statement (* inlined *) = (
    Token.t (* "select" *)
  * Token.t (* "{" *)
  * [ `Comm_case of communication_case | `Defa_case of default_case ]
      list (* zero or more *)
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type keyed_element (* inlined *) = (
    [
        `Exp_COLON of (expression * Token.t (* ":" *))
      | `Lit_value_COLON of (literal_value * Token.t (* ":" *))
      | `Id_COLON of empty_labeled_statement
    ]
  * element
)
[@@deriving sexp_of]

type const_declaration (* inlined *) = (
    Token.t (* "const" *)
  * [
        `Const_spec of const_spec
      | `LPAR_rep_const_spec_choice_LF_RPAR of (
            Token.t (* "(" *)
          * (const_spec * anon_choice_LF) list (* zero or more *)
          * Token.t (* ")" *)
        )
    ]
)
[@@deriving sexp_of]

type variadic_argument (* inlined *) = (expression * Token.t (* "..." *))
[@@deriving sexp_of]

type assignment_statement (* inlined *) = (
    expression_list
  * [
        `STAREQ of Token.t (* "*=" *)
      | `SLASHEQ of Token.t (* "/=" *)
      | `PERCEQ of Token.t (* "%=" *)
      | `LTLTEQ of Token.t (* "<<=" *)
      | `GTGTEQ of Token.t (* ">>=" *)
      | `AMPEQ of Token.t (* "&=" *)
      | `AMPHATEQ of Token.t (* "&^=" *)
      | `PLUSEQ of Token.t (* "+=" *)
      | `DASHEQ of Token.t (* "-=" *)
      | `BAREQ of Token.t (* "|=" *)
      | `HATEQ of Token.t (* "^=" *)
      | `EQ of Token.t (* "=" *)
    ]
  * expression_list
)
[@@deriving sexp_of]

type variadic_parameter_declaration (* inlined *) = (
    identifier (*tok*) option
  * Token.t (* "..." *)
  * type_
)
[@@deriving sexp_of]

type expression_switch_statement (* inlined *) = (
    Token.t (* "switch" *)
  * (simple_statement * Token.t (* ";" *)) option
  * expression option
  * Token.t (* "{" *)
  * [ `Exp_case of expression_case | `Defa_case of default_case ]
      list (* zero or more *)
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type unary_expression (* inlined *) = (
    [
        `PLUS of Token.t (* "+" *)
      | `DASH of Token.t (* "-" *)
      | `BANG of Token.t (* "!" *)
      | `HAT of Token.t (* "^" *)
      | `STAR of Token.t (* "*" *)
      | `AMP of Token.t (* "&" *)
      | `LTDASH of Token.t (* "<-" *)
    ]
  * expression
)
[@@deriving sexp_of]

type short_var_declaration (* inlined *) = (
    expression_list * Token.t (* ":=" *) * expression_list
)
[@@deriving sexp_of]

type parenthesized_type (* inlined *) = (
    Token.t (* "(" *) * type_ * Token.t (* ")" *)
)
[@@deriving sexp_of]

type defer_statement (* inlined *) = (Token.t (* "defer" *) * expression)
[@@deriving sexp_of]

type type_switch_statement (* inlined *) = (
    Token.t (* "switch" *)
  * type_switch_header
  * Token.t (* "{" *)
  * [ `Type_case of type_case | `Defa_case of default_case ]
      list (* zero or more *)
  * Token.t (* "}" *)
)
[@@deriving sexp_of]

type parameter_declaration (* inlined *) = (field_name_list option * type_)
[@@deriving sexp_of]

type type_declaration (* inlined *) = (
    Token.t (* "type" *)
  * [
        `Type_spec of type_spec
      | `Type_alias of type_alias
      | `LPAR_rep_choice_type_spec_choice_LF_RPAR of (
            Token.t (* "(" *)
          * (
                [ `Type_spec of type_spec | `Type_alias of type_alias ]
              * anon_choice_LF
            )
              list (* zero or more *)
          * Token.t (* ")" *)
        )
    ]
)
[@@deriving sexp_of]

type func_literal (* inlined *) = (
    Token.t (* "func" *)
  * parameter_list
  * anon_choice_param_list option
  * block
)
[@@deriving sexp_of]

type return_statement (* inlined *) = (
    Token.t (* "return" *)
  * expression_list option
)
[@@deriving sexp_of]

type slice_expression (* inlined *) = (
    expression
  * Token.t (* "[" *)
  * [
        `Opt_exp_COLON_opt_exp of (
            expression option
          * Token.t (* ":" *)
          * expression option
        )
      | `Opt_exp_COLON_exp_COLON_exp of (
            expression option
          * Token.t (* ":" *)
          * expression
          * Token.t (* ":" *)
          * expression
        )
    ]
  * Token.t (* "]" *)
)
[@@deriving sexp_of]

type index_expression (* inlined *) = (
    expression * Token.t (* "[" *) * expression * Token.t (* "]" *)
)
[@@deriving sexp_of]

type for_statement (* inlined *) = (
    Token.t (* "for" *)
  * [
        `Exp of expression
      | `For_clause of for_clause
      | `Range_clause of range_clause
    ]
      option
  * block
)
[@@deriving sexp_of]

type go_statement (* inlined *) = (Token.t (* "go" *) * expression)
[@@deriving sexp_of]

type selector_expression (* inlined *) = (
    expression * Token.t (* "." *) * identifier (*tok*)
)
[@@deriving sexp_of]

type interface_type (* inlined *) = (
    Token.t (* "interface" *) * method_spec_list
)
[@@deriving sexp_of]

type function_type (* inlined *) = (
    Token.t (* "func" *)
  * parameter_list
  * anon_choice_param_list option
)
[@@deriving sexp_of]

type function_declaration (* inlined *) = (
    Token.t (* "func" *)
  * identifier (*tok*)
  * parameter_list
  * anon_choice_param_list option
  * block option
)
[@@deriving sexp_of]

type method_declaration (* inlined *) = (
    Token.t (* "func" *)
  * parameter_list
  * identifier (*tok*)
  * parameter_list
  * anon_choice_param_list option
  * block option
)
[@@deriving sexp_of]

type import_declaration (* inlined *) = (
    Token.t (* "import" *)
  * [ `Import_spec of import_spec | `Import_spec_list of import_spec_list ]
)
[@@deriving sexp_of]

let dump_tree root =
  sexp_of_source_file root
  |> Print_sexp.to_stdout
