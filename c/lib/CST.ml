(* Generated by ocaml-tree-sitter. *)
(*
   c grammar

   entrypoint: translation_unit
*)

open! Sexplib.Conv
open Tree_sitter_run

type anon_choice_BANG = [
    `BANG of Token.t (* "!" *)
  | `TILDE of Token.t (* "~" *)
  | `DASH of Token.t (* "-" *)
  | `PLUS of Token.t (* "+" *)
]
[@@deriving sexp_of]

type false_ = Token.t
[@@deriving sexp_of]

type preproc_arg = Token.t
[@@deriving sexp_of]

type pat_ca8830e = Token.t (* pattern #[ 	]*include *)
[@@deriving sexp_of]

type type_qualifier = [
    `Const of Token.t (* "const" *)
  | `Vola of Token.t (* "volatile" *)
  | `Rest of Token.t (* "restrict" *)
  | `X__Atomic of Token.t (* "_Atomic" *)
]
[@@deriving sexp_of]

type imm_tok_pat_36637e2 = Token.t (* pattern "[^\\n']" *)
[@@deriving sexp_of]

type pat_3df6e71 = Token.t (* pattern #[ 	]*if *)
[@@deriving sexp_of]

type primitive_type = Token.t
[@@deriving sexp_of]

type pat_25b90ba = Token.t (* pattern #[ 	]*ifdef *)
[@@deriving sexp_of]

type pat_c46d1b2 = Token.t (* pattern #[ 	]*endif *)
[@@deriving sexp_of]

type escape_sequence = Token.t
[@@deriving sexp_of]

type preproc_directive = Token.t (* pattern #[ \t]*[a-zA-Z]\w* *)
[@@deriving sexp_of]

type number_literal = Token.t
[@@deriving sexp_of]

type anon_choice_DASHDASH = [
    `DASHDASH of Token.t (* "--" *)
  | `PLUSPLUS of Token.t (* "++" *)
]
[@@deriving sexp_of]

type pat_bfeb4bb = Token.t (* pattern #[ 	]*elif *)
[@@deriving sexp_of]

type imm_tok_pat_c7f65b4 = Token.t (* pattern "[^\\\\\"\\n]+" *)
[@@deriving sexp_of]

type pat_c3ea183 = Token.t (* pattern #[ 	]*define *)
[@@deriving sexp_of]

type storage_class_specifier = [
    `Extern of Token.t (* "extern" *)
  | `Static of Token.t (* "static" *)
  | `Auto of Token.t (* "auto" *)
  | `Regi of Token.t (* "register" *)
  | `Inline of Token.t (* "inline" *)
]
[@@deriving sexp_of]

type pat_9d92f6a = Token.t (* pattern #[ 	]*ifndef *)
[@@deriving sexp_of]

type system_lib_string = Token.t
[@@deriving sexp_of]

type identifier = Token.t (* pattern [a-zA-Z_]\w* *)
[@@deriving sexp_of]

type pat_56631e5 = Token.t (* pattern #[ 	]*else *)
[@@deriving sexp_of]

type true_ = Token.t
[@@deriving sexp_of]

type ms_unaligned_ptr_modifier = [
    `X__unal of Token.t (* "_unaligned" *)
  | `X___unal of Token.t (* "__unaligned" *)
]
[@@deriving sexp_of]

type ms_call_modifier = [
    `X___cdecl of Token.t (* "__cdecl" *)
  | `X___clrc of Token.t (* "__clrcall" *)
  | `X___stdc of Token.t (* "__stdcall" *)
  | `X___fast of Token.t (* "__fastcall" *)
  | `X___this of Token.t (* "__thiscall" *)
  | `X___vect of Token.t (* "__vectorcall" *)
]
[@@deriving sexp_of]

type char_literal = (
    [
        `LSQUOT of Token.t (* "L'" *)
      | `USQUOT_d861d39 of Token.t (* "u'" *)
      | `USQUOT_2701bdc of Token.t (* "U'" *)
      | `U8SQUOT of Token.t (* "u8'" *)
      | `SQUOT of Token.t (* "'" *)
    ]
  * [
        `Esc_seq of escape_sequence (*tok*)
      | `Imm_tok_pat_36637e2 of imm_tok_pat_36637e2 (*tok*)
    ]
  * Token.t (* "'" *)
)
[@@deriving sexp_of]

type preproc_call = (
    preproc_directive (*tok*)
  * preproc_arg (*tok*) option
  * Token.t (* "\n" *)
)
[@@deriving sexp_of]

type string_literal = (
    [
        `LDQUOT of Token.t (* "L\"" *)
      | `UDQUOT_c163aae of Token.t (* "u\"" *)
      | `UDQUOT_df3447d of Token.t (* "U\"" *)
      | `U8DQUOT of Token.t (* "u8\"" *)
      | `DQUOT of Token.t (* "\"" *)
    ]
  * [
        `Imm_tok_pat_c7f65b4 of imm_tok_pat_c7f65b4 (*tok*)
      | `Esc_seq of escape_sequence (*tok*)
    ]
      list (* zero or more *)
  * Token.t (* "\"" *)
)
[@@deriving sexp_of]

type anon_choice_pat_25b90ba = [
    `Pat_25b90ba of pat_25b90ba (*tok*)
  | `Pat_9d92f6a of pat_9d92f6a (*tok*)
]
[@@deriving sexp_of]

type ms_pointer_modifier = [
    `Ms_unal_ptr_modi of ms_unaligned_ptr_modifier
  | `Ms_rest_modi of Token.t (* "__restrict" *)
  | `Ms_unsi_ptr_modi of Token.t (* "__uptr" *)
  | `Ms_signed_ptr_modi of Token.t (* "__sptr" *)
]
[@@deriving sexp_of]

type preproc_def = (
    pat_c3ea183 (*tok*)
  * identifier (*tok*)
  * preproc_arg (*tok*) option
  * Token.t (* "\n" *)
)
[@@deriving sexp_of]

type ms_declspec_modifier = (
    Token.t (* "__declspec" *) * Token.t (* "(" *) * identifier (*tok*)
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type field_designator = (Token.t (* "." *) * identifier (*tok*))
[@@deriving sexp_of]

type preproc_defined = [
    `Defi_LPAR_id_RPAR of (
        Token.t (* "defined" *) * Token.t (* "(" *) * identifier (*tok*)
      * Token.t (* ")" *)
    )
  | `Defi_id of (Token.t (* "defined" *) * identifier (*tok*))
]
[@@deriving sexp_of]

type anon_choice_field_id = [
    `Id of identifier (*tok*)
  | `DOTDOTDOT of Token.t (* "..." *)
]
[@@deriving sexp_of]

type preproc_binary_expression = [
    `Prep_exp_PLUS_prep_exp of (
        preproc_expression * Token.t (* "+" *) * preproc_expression
    )
  | `Prep_exp_DASH_prep_exp of (
        preproc_expression * Token.t (* "-" *) * preproc_expression
    )
  | `Prep_exp_STAR_prep_exp of (
        preproc_expression * Token.t (* "*" *) * preproc_expression
    )
  | `Prep_exp_SLASH_prep_exp of (
        preproc_expression * Token.t (* "/" *) * preproc_expression
    )
  | `Prep_exp_PERC_prep_exp of (
        preproc_expression * Token.t (* "%" *) * preproc_expression
    )
  | `Prep_exp_BARBAR_prep_exp of (
        preproc_expression * Token.t (* "||" *) * preproc_expression
    )
  | `Prep_exp_AMPAMP_prep_exp of (
        preproc_expression * Token.t (* "&&" *) * preproc_expression
    )
  | `Prep_exp_BAR_prep_exp of (
        preproc_expression * Token.t (* "|" *) * preproc_expression
    )
  | `Prep_exp_HAT_prep_exp of (
        preproc_expression * Token.t (* "^" *) * preproc_expression
    )
  | `Prep_exp_AMP_prep_exp of (
        preproc_expression * Token.t (* "&" *) * preproc_expression
    )
  | `Prep_exp_EQEQ_prep_exp of (
        preproc_expression * Token.t (* "==" *) * preproc_expression
    )
  | `Prep_exp_BANGEQ_prep_exp of (
        preproc_expression * Token.t (* "!=" *) * preproc_expression
    )
  | `Prep_exp_GT_prep_exp of (
        preproc_expression * Token.t (* ">" *) * preproc_expression
    )
  | `Prep_exp_GTEQ_prep_exp of (
        preproc_expression * Token.t (* ">=" *) * preproc_expression
    )
  | `Prep_exp_LTEQ_prep_exp of (
        preproc_expression * Token.t (* "<=" *) * preproc_expression
    )
  | `Prep_exp_LT_prep_exp of (
        preproc_expression * Token.t (* "<" *) * preproc_expression
    )
  | `Prep_exp_LTLT_prep_exp of (
        preproc_expression * Token.t (* "<<" *) * preproc_expression
    )
  | `Prep_exp_GTGT_prep_exp of (
        preproc_expression * Token.t (* ">>" *) * preproc_expression
    )
]

and preproc_call_expression = (identifier (*tok*) * preproc_argument_list)

and preproc_argument_list = (
    Token.t (* "(" *)
  * (
        preproc_expression
      * (Token.t (* "," *) * preproc_expression) list (* zero or more *)
    )
      option
  * Token.t (* ")" *)
)

and preproc_expression = [
    `Id of identifier (*tok*)
  | `Prep_call_exp of preproc_call_expression
  | `Num_lit of number_literal (*tok*)
  | `Char_lit of char_literal
  | `Prep_defi of preproc_defined
  | `Prep_un_exp of (anon_choice_BANG * preproc_expression)
  | `Prep_bin_exp of preproc_binary_expression
  | `Prep_paren_exp of (
        Token.t (* "(" *) * preproc_expression * Token.t (* ")" *)
    )
]
[@@deriving sexp_of]

type preproc_params = (
    Token.t (* "(" *)
  * (
        anon_choice_field_id
      * (Token.t (* "," *) * anon_choice_field_id) list (* zero or more *)
    )
      option
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type preproc_function_def = (
    pat_c3ea183 (*tok*)
  * identifier (*tok*)
  * preproc_params
  * preproc_arg (*tok*) option
  * Token.t (* "\n" *)
)
[@@deriving sexp_of]

type type_specifier = [
    `Struct_spec of (
        Token.t (* "struct" *)
      * ms_declspec_modifier option
      * anon_choice_field_id_opt_field_decl_list
    )
  | `Union_spec of (
        Token.t (* "union" *)
      * ms_declspec_modifier option
      * anon_choice_field_id_opt_field_decl_list
    )
  | `Enum_spec of (
        Token.t (* "enum" *)
      * [
            `Id_opt_enum_list of (identifier (*tok*) * enumerator_list option)
          | `Enum_list of enumerator_list
        ]
    )
  | `Macro_type_spec of (
        identifier (*tok*) * Token.t (* "(" *) * type_descriptor
      * Token.t (* ")" *)
    )
  | `Sized_type_spec of (
        [
            `Signed of Token.t (* "signed" *)
          | `Unsi of Token.t (* "unsigned" *)
          | `Long of Token.t (* "long" *)
          | `Short of Token.t (* "short" *)
        ]
          list (* one or more *)
      * [ `Id of identifier (*tok*) | `Prim_type of primitive_type (*tok*) ]
          option
    )
  | `Prim_type of primitive_type (*tok*)
  | `Id of identifier (*tok*)
]

and parenthesized_expression = (
    Token.t (* "(" *) * anon_choice_exp_ * Token.t (* ")" *)
)

and initializer_list = (
    Token.t (* "{" *)
  * (
        anon_choice_init_pair
      * (Token.t (* "," *) * anon_choice_init_pair) list (* zero or more *)
    )
      option
  * Token.t (* "," *) option
  * Token.t (* "}" *)
)

and anon_choice_exp = [ `Exp of expression | `STAR of Token.t (* "*" *) ]

and binary_expression = [
    `Exp_PLUS_exp of (expression * Token.t (* "+" *) * expression)
  | `Exp_DASH_exp of (expression * Token.t (* "-" *) * expression)
  | `Exp_STAR_exp of (expression * Token.t (* "*" *) * expression)
  | `Exp_SLASH_exp of (expression * Token.t (* "/" *) * expression)
  | `Exp_PERC_exp of (expression * Token.t (* "%" *) * expression)
  | `Exp_BARBAR_exp of (expression * Token.t (* "||" *) * expression)
  | `Exp_AMPAMP_exp of (expression * Token.t (* "&&" *) * expression)
  | `Exp_BAR_exp of (expression * Token.t (* "|" *) * expression)
  | `Exp_HAT_exp of (expression * Token.t (* "^" *) * expression)
  | `Exp_AMP_exp of (expression * Token.t (* "&" *) * expression)
  | `Exp_EQEQ_exp of (expression * Token.t (* "==" *) * expression)
  | `Exp_BANGEQ_exp of (expression * Token.t (* "!=" *) * expression)
  | `Exp_GT_exp of (expression * Token.t (* ">" *) * expression)
  | `Exp_GTEQ_exp of (expression * Token.t (* ">=" *) * expression)
  | `Exp_LTEQ_exp of (expression * Token.t (* "<=" *) * expression)
  | `Exp_LT_exp of (expression * Token.t (* "<" *) * expression)
  | `Exp_LTLT_exp of (expression * Token.t (* "<<" *) * expression)
  | `Exp_GTGT_exp of (expression * Token.t (* ">>" *) * expression)
]

and subscript_designator = (
    Token.t (* "[" *) * expression * Token.t (* "]" *)
)

and type_descriptor = (
    type_qualifier list (* zero or more *)
  * type_specifier
  * type_qualifier list (* zero or more *)
  * abstract_declarator option
)

and anon_choice_param_decl = [
    `Param_decl of (
        declaration_specifiers
      * [ `Decl of declarator | `Abst_decl of abstract_declarator ] option
    )
  | `DOTDOTDOT of Token.t (* "..." *)
]

and subscript_expression = (
    expression * Token.t (* "[" *) * expression * Token.t (* "]" *)
)

and call_expression = (expression * argument_list)

and pointer_expression = (
    [ `STAR of Token.t (* "*" *) | `AMP of Token.t (* "&" *) ]
  * expression
)

and abstract_declarator = [
    `Abst_poin_decl of (
        Token.t (* "*" *)
      * type_qualifier list (* zero or more *)
      * abstract_declarator option
    )
  | `Abst_func_decl of (abstract_declarator option * parameter_list)
  | `Abst_array_decl of (
        abstract_declarator option
      * Token.t (* "[" *)
      * type_qualifier list (* zero or more *)
      * anon_choice_exp option
      * Token.t (* "]" *)
    )
  | `Abst_paren_decl of (
        Token.t (* "(" *) * abstract_declarator * Token.t (* ")" *)
    )
]

and enumerator_list = (
    Token.t (* "{" *)
  * (enumerator * (Token.t (* "," *) * enumerator) list (* zero or more *))
      option
  * Token.t (* "," *) option
  * Token.t (* "}" *)
)

and expression = [
    `Cond_exp of (
        expression * Token.t (* "?" *) * expression * Token.t (* ":" *)
      * expression
    )
  | `Assign_exp of (
        assignment_left_expression
      * [
            `EQ of Token.t (* "=" *)
          | `STAREQ of Token.t (* "*=" *)
          | `SLASHEQ of Token.t (* "/=" *)
          | `PERCEQ of Token.t (* "%=" *)
          | `PLUSEQ of Token.t (* "+=" *)
          | `DASHEQ of Token.t (* "-=" *)
          | `LTLTEQ of Token.t (* "<<=" *)
          | `GTGTEQ of Token.t (* ">>=" *)
          | `AMPEQ of Token.t (* "&=" *)
          | `HATEQ of Token.t (* "^=" *)
          | `BAREQ of Token.t (* "|=" *)
        ]
      * expression
    )
  | `Bin_exp of binary_expression
  | `Un_exp of (anon_choice_BANG * expression)
  | `Update_exp of update_expression
  | `Cast_exp of (
        Token.t (* "(" *) * type_descriptor * Token.t (* ")" *) * expression
    )
  | `Poin_exp of pointer_expression
  | `Sizeof_exp of (
        Token.t (* "sizeof" *)
      * [
            `Exp of expression
          | `LPAR_type_desc_RPAR of (
                Token.t (* "(" *) * type_descriptor * Token.t (* ")" *)
            )
        ]
    )
  | `Subs_exp of subscript_expression
  | `Call_exp of call_expression
  | `Field_exp of field_expression
  | `Comp_lit_exp of (
        Token.t (* "(" *) * type_descriptor * Token.t (* ")" *)
      * initializer_list
    )
  | `Id of identifier (*tok*)
  | `Num_lit of number_literal (*tok*)
  | `Str_lit of string_literal
  | `True of true_ (*tok*)
  | `False of false_ (*tok*)
  | `Null of Token.t (* "NULL" *)
  | `Conc_str of (string_literal * string_literal list (* one or more *))
  | `Char_lit of char_literal
  | `Paren_exp of parenthesized_expression
]

and field_expression = (
    expression
  * [ `DOT of Token.t (* "." *) | `DASHGT of Token.t (* "->" *) ]
  * identifier (*tok*)
)

and declaration_specifiers = (
    anon_choice_stor_class_spec list (* zero or more *)
  * type_specifier
  * anon_choice_stor_class_spec list (* zero or more *)
)

and anon_choice_field_id_opt_field_decl_list = [
    `Id_opt_field_decl_list of (
        identifier (*tok*)
      * field_declaration_list option
    )
  | `Field_decl_list of field_declaration_list
]

and field_declaration_list_item = [
    `Field_decl of (
        declaration_specifiers
      * (
            field_declarator
          * (Token.t (* "," *) * field_declarator) list (* zero or more *)
        )
          option
      * bitfield_clause option
      * Token.t (* ";" *)
    )
  | `Prep_def of preproc_def
  | `Prep_func_def of preproc_function_def
  | `Prep_call of preproc_call
  | `Prep_if_in_field_decl_list of (
        pat_3df6e71 (*tok*)
      * preproc_expression
      * Token.t (* "\n" *)
      * field_declaration_list_item list (* zero or more *)
      * anon_choice_prep_else_in_field_decl_list option
      * pat_c46d1b2 (*tok*)
    )
  | `Prep_ifdef_in_field_decl_list of (
        anon_choice_pat_25b90ba
      * identifier (*tok*)
      * field_declaration_list_item list (* zero or more *)
      * anon_choice_prep_else_in_field_decl_list option
      * pat_c46d1b2 (*tok*)
    )
]

and enumerator = (
    identifier (*tok*)
  * (Token.t (* "=" *) * expression) option
)

and update_expression = [
    `Choice_DASHDASH_exp of (anon_choice_DASHDASH * expression)
  | `Exp_choice_DASHDASH of (expression * anon_choice_DASHDASH)
]

and ms_based_modifier = (Token.t (* "__based" *) * argument_list)

and anon_choice_prep_else_in_field_decl_list = [
    `Prep_else_in_field_decl_list of (
        pat_56631e5 (*tok*)
      * field_declaration_list_item list (* zero or more *)
    )
  | `Prep_elif_in_field_decl_list of (
        pat_bfeb4bb (*tok*)
      * preproc_expression
      * Token.t (* "\n" *)
      * field_declaration_list_item list (* zero or more *)
      * anon_choice_prep_else_in_field_decl_list option
    )
]

and attribute_specifier = (
    Token.t (* "__attribute__" *) * Token.t (* "(" *) * argument_list
  * Token.t (* ")" *)
)

and field_declarator = [
    `Poin_field_decl of (
        ms_based_modifier option
      * Token.t (* "*" *)
      * ms_pointer_modifier list (* zero or more *)
      * type_qualifier list (* zero or more *)
      * field_declarator
    )
  | `Func_field_decl of (field_declarator * parameter_list)
  | `Array_field_decl of (
        field_declarator
      * Token.t (* "[" *)
      * type_qualifier list (* zero or more *)
      * anon_choice_exp option
      * Token.t (* "]" *)
    )
  | `Paren_field_decl of (
        Token.t (* "(" *) * field_declarator * Token.t (* ")" *)
    )
  | `Id of identifier (*tok*)
]

and field_declaration_list = (
    Token.t (* "{" *)
  * field_declaration_list_item list (* zero or more *)
  * Token.t (* "}" *)
)

and bitfield_clause = (Token.t (* ":" *) * expression)

and anon_choice_stor_class_spec = [
    `Stor_class_spec of storage_class_specifier
  | `Type_qual of type_qualifier
  | `Attr_spec of attribute_specifier
  | `Ms_decl_modi of ms_declspec_modifier
]

and argument_list = (
    Token.t (* "(" *)
  * (expression * (Token.t (* "," *) * expression) list (* zero or more *))
      option
  * Token.t (* ")" *)
)

and declarator = [
    `Poin_decl of (
        ms_based_modifier option
      * Token.t (* "*" *)
      * ms_pointer_modifier list (* zero or more *)
      * type_qualifier list (* zero or more *)
      * declarator
    )
  | `Func_decl of (
        declarator
      * parameter_list
      * attribute_specifier list (* zero or more *)
    )
  | `Array_decl of (
        declarator
      * Token.t (* "[" *)
      * type_qualifier list (* zero or more *)
      * anon_choice_exp option
      * Token.t (* "]" *)
    )
  | `Paren_decl of (Token.t (* "(" *) * declarator * Token.t (* ")" *))
  | `Id of identifier (*tok*)
]

and anon_choice_exp_ = [
    `Exp of expression
  | `Comma_exp of (expression * Token.t (* "," *) * anon_choice_exp_)
]

and assignment_left_expression = [
    `Id of identifier (*tok*)
  | `Call_exp of call_expression
  | `Field_exp of field_expression
  | `Poin_exp of pointer_expression
  | `Subs_exp of subscript_expression
  | `Paren_exp of parenthesized_expression
]

and parameter_list = (
    Token.t (* "(" *)
  * (
        anon_choice_param_decl
      * (Token.t (* "," *) * anon_choice_param_decl) list (* zero or more *)
    )
      option
  * Token.t (* ")" *)
)

and anon_choice_init_pair = [
    `Init_pair of (
        [
            `Subs_desi of subscript_designator
          | `Field_desi of field_designator
        ]
          list (* one or more *)
      * Token.t (* "=" *)
      * [ `Exp of expression | `Init_list of initializer_list ]
    )
  | `Exp of expression
  | `Init_list of initializer_list
]
[@@deriving sexp_of]

type expression_statement = (anon_choice_exp_ option * Token.t (* ";" *))
[@@deriving sexp_of]

type type_declarator = [
    `Poin_type_decl of (
        ms_based_modifier option
      * Token.t (* "*" *)
      * ms_pointer_modifier list (* zero or more *)
      * type_qualifier list (* zero or more *)
      * type_declarator
    )
  | `Func_type_decl of (type_declarator * parameter_list)
  | `Array_type_decl of (
        type_declarator
      * Token.t (* "[" *)
      * type_qualifier list (* zero or more *)
      * anon_choice_exp option
      * Token.t (* "]" *)
    )
  | `Paren_type_decl of (
        Token.t (* "(" *) * type_declarator * Token.t (* ")" *)
    )
  | `Id of identifier (*tok*)
]
[@@deriving sexp_of]

type anon_choice_decl = [
    `Decl of declarator
  | `Init_decl of (
        declarator
      * Token.t (* "=" *)
      * [ `Init_list of initializer_list | `Exp of expression ]
    )
]
[@@deriving sexp_of]

type type_definition = (
    Token.t (* "typedef" *)
  * type_qualifier list (* zero or more *)
  * type_specifier
  * type_declarator
  * (Token.t (* "," *) * type_declarator) list (* zero or more *)
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type declaration = (
    declaration_specifiers
  * anon_choice_decl
  * (Token.t (* "," *) * anon_choice_decl) list (* zero or more *)
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type non_case_statement = [
    `Labe_stmt of (identifier (*tok*) * Token.t (* ":" *) * statement)
  | `Comp_stmt of compound_statement
  | `Exp_stmt of expression_statement
  | `If_stmt of (
        Token.t (* "if" *)
      * parenthesized_expression
      * statement
      * (Token.t (* "else" *) * statement) option
    )
  | `Switch_stmt of (
        Token.t (* "switch" *) * parenthesized_expression
      * compound_statement
    )
  | `Do_stmt of (
        Token.t (* "do" *) * statement * Token.t (* "while" *)
      * parenthesized_expression * Token.t (* ";" *)
    )
  | `While_stmt of (
        Token.t (* "while" *) * parenthesized_expression * statement
    )
  | `For_stmt of (
        Token.t (* "for" *)
      * Token.t (* "(" *)
      * [
            `Decl of declaration
          | `Opt_choice_exp_SEMI of expression_statement
        ]
      * expression option
      * Token.t (* ";" *)
      * anon_choice_exp_ option
      * Token.t (* ")" *)
      * statement
    )
  | `Ret_stmt of (
        Token.t (* "return" *)
      * anon_choice_exp_ option
      * Token.t (* ";" *)
    )
  | `Brk_stmt of (Token.t (* "break" *) * Token.t (* ";" *))
  | `Cont_stmt of (Token.t (* "continue" *) * Token.t (* ";" *))
  | `Goto_stmt of (
        Token.t (* "goto" *) * identifier (*tok*) * Token.t (* ";" *)
    )
]

and declaration_list = (
    Token.t (* "{" *) * translation_unit * Token.t (* "}" *)
)

and statement = [
    `Case_stmt of (
        [
            `Case_exp of (Token.t (* "case" *) * expression)
          | `Defa of Token.t (* "default" *)
        ]
      * Token.t (* ":" *)
      * [
            `Choice_labe_stmt of non_case_statement
          | `Decl of declaration
          | `Type_defi of type_definition
        ]
          list (* zero or more *)
    )
  | `Choice_labe_stmt of non_case_statement
]

and function_definition = (
    ms_call_modifier option
  * declaration_specifiers
  * declarator
  * compound_statement
)

and anon_choice_prep_else = [
    `Prep_else of (pat_56631e5 (*tok*) * translation_unit)
  | `Prep_elif of (
        pat_bfeb4bb (*tok*)
      * preproc_expression
      * Token.t (* "\n" *)
      * translation_unit
      * anon_choice_prep_else option
    )
]

and top_level_item = [
    `Func_defi of function_definition
  | `Link_spec of (
        Token.t (* "extern" *)
      * string_literal
      * [
            `Func_defi of function_definition
          | `Decl of declaration
          | `Decl_list of declaration_list
        ]
    )
  | `Decl of declaration
  | `Choice_case_stmt of statement
  | `Type_defi of type_definition
  | `Empty_decl of (type_specifier * Token.t (* ";" *))
  | `Prep_if of (
        pat_3df6e71 (*tok*)
      * preproc_expression
      * Token.t (* "\n" *)
      * translation_unit
      * anon_choice_prep_else option
      * pat_c46d1b2 (*tok*)
    )
  | `Prep_ifdef of (
        anon_choice_pat_25b90ba
      * identifier (*tok*)
      * translation_unit
      * anon_choice_prep_else option
      * pat_c46d1b2 (*tok*)
    )
  | `Prep_incl of (
        pat_ca8830e (*tok*)
      * [
            `Str_lit of string_literal
          | `System_lib_str of system_lib_string (*tok*)
          | `Id of identifier (*tok*)
          | `Prep_call_exp of preproc_call_expression
        ]
      * Token.t (* "\n" *)
    )
  | `Prep_def of preproc_def
  | `Prep_func_def of preproc_function_def
  | `Prep_call of preproc_call
]

and compound_statement = (
    Token.t (* "{" *) * translation_unit * Token.t (* "}" *)
)

and translation_unit = top_level_item list (* zero or more *)
[@@deriving sexp_of]

type ms_restrict_modifier (* inlined *) = Token.t (* "__restrict" *)
[@@deriving sexp_of]

type ms_signed_ptr_modifier (* inlined *) = Token.t (* "__sptr" *)
[@@deriving sexp_of]

type break_statement (* inlined *) = (
    Token.t (* "break" *) * Token.t (* ";" *)
)
[@@deriving sexp_of]

type comment (* inlined *) = Token.t
[@@deriving sexp_of]

type null (* inlined *) = Token.t (* "NULL" *)
[@@deriving sexp_of]

type continue_statement (* inlined *) = (
    Token.t (* "continue" *) * Token.t (* ";" *)
)
[@@deriving sexp_of]

type imm_tok_LPAR (* inlined *) = Token.t (* "(" *)
[@@deriving sexp_of]

type ms_unsigned_ptr_modifier (* inlined *) = Token.t (* "__uptr" *)
[@@deriving sexp_of]

type statement_identifier (* inlined *) = identifier (*tok*)
[@@deriving sexp_of]

type type_identifier (* inlined *) = identifier (*tok*)
[@@deriving sexp_of]

type field_identifier (* inlined *) = identifier (*tok*)
[@@deriving sexp_of]

type concatenated_string (* inlined *) = (
    string_literal
  * string_literal list (* one or more *)
)
[@@deriving sexp_of]

type sized_type_specifier (* inlined *) = (
    [
        `Signed of Token.t (* "signed" *)
      | `Unsi of Token.t (* "unsigned" *)
      | `Long of Token.t (* "long" *)
      | `Short of Token.t (* "short" *)
    ]
      list (* one or more *)
  * [ `Id of identifier (*tok*) | `Prim_type of primitive_type (*tok*) ]
      option
)
[@@deriving sexp_of]

type goto_statement (* inlined *) = (
    Token.t (* "goto" *) * identifier (*tok*) * Token.t (* ";" *)
)
[@@deriving sexp_of]

type preproc_parenthesized_expression (* inlined *) = (
    Token.t (* "(" *) * preproc_expression * Token.t (* ")" *)
)
[@@deriving sexp_of]

type preproc_unary_expression (* inlined *) = (
    anon_choice_BANG * preproc_expression
)
[@@deriving sexp_of]

type preproc_include (* inlined *) = (
    pat_ca8830e (*tok*)
  * [
        `Str_lit of string_literal
      | `System_lib_str of system_lib_string (*tok*)
      | `Id of identifier (*tok*)
      | `Prep_call_exp of preproc_call_expression
    ]
  * Token.t (* "\n" *)
)
[@@deriving sexp_of]

type comma_expression (* inlined *) = (
    expression * Token.t (* "," *) * anon_choice_exp_
)
[@@deriving sexp_of]

type conditional_expression (* inlined *) = (
    expression * Token.t (* "?" *) * expression * Token.t (* ":" *)
  * expression
)
[@@deriving sexp_of]

type preproc_ifdef_in_field_declaration_list (* inlined *) = (
    anon_choice_pat_25b90ba
  * identifier (*tok*)
  * field_declaration_list_item list (* zero or more *)
  * anon_choice_prep_else_in_field_decl_list option
  * pat_c46d1b2 (*tok*)
)
[@@deriving sexp_of]

type field_declaration (* inlined *) = (
    declaration_specifiers
  * (
        field_declarator
      * (Token.t (* "," *) * field_declarator) list (* zero or more *)
    )
      option
  * bitfield_clause option
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type assignment_expression (* inlined *) = (
    assignment_left_expression
  * [
        `EQ of Token.t (* "=" *)
      | `STAREQ of Token.t (* "*=" *)
      | `SLASHEQ of Token.t (* "/=" *)
      | `PERCEQ of Token.t (* "%=" *)
      | `PLUSEQ of Token.t (* "+=" *)
      | `DASHEQ of Token.t (* "-=" *)
      | `LTLTEQ of Token.t (* "<<=" *)
      | `GTGTEQ of Token.t (* ">>=" *)
      | `AMPEQ of Token.t (* "&=" *)
      | `HATEQ of Token.t (* "^=" *)
      | `BAREQ of Token.t (* "|=" *)
    ]
  * expression
)
[@@deriving sexp_of]

type initializer_pair (* inlined *) = (
    [ `Subs_desi of subscript_designator | `Field_desi of field_designator ]
      list (* one or more *)
  * Token.t (* "=" *)
  * [ `Exp of expression | `Init_list of initializer_list ]
)
[@@deriving sexp_of]

type cast_expression (* inlined *) = (
    Token.t (* "(" *) * type_descriptor * Token.t (* ")" *) * expression
)
[@@deriving sexp_of]

type preproc_if_in_field_declaration_list (* inlined *) = (
    pat_3df6e71 (*tok*)
  * preproc_expression
  * Token.t (* "\n" *)
  * field_declaration_list_item list (* zero or more *)
  * anon_choice_prep_else_in_field_decl_list option
  * pat_c46d1b2 (*tok*)
)
[@@deriving sexp_of]

type preproc_elif_in_field_declaration_list (* inlined *) = (
    pat_bfeb4bb (*tok*)
  * preproc_expression
  * Token.t (* "\n" *)
  * field_declaration_list_item list (* zero or more *)
  * anon_choice_prep_else_in_field_decl_list option
)
[@@deriving sexp_of]

type unary_expression (* inlined *) = (anon_choice_BANG * expression)
[@@deriving sexp_of]

type macro_type_specifier (* inlined *) = (
    identifier (*tok*) * Token.t (* "(" *) * type_descriptor
  * Token.t (* ")" *)
)
[@@deriving sexp_of]

type parenthesized_declarator (* inlined *) = (
    Token.t (* "(" *) * declarator * Token.t (* ")" *)
)
[@@deriving sexp_of]

type array_field_declarator (* inlined *) = (
    field_declarator
  * Token.t (* "[" *)
  * type_qualifier list (* zero or more *)
  * anon_choice_exp option
  * Token.t (* "]" *)
)
[@@deriving sexp_of]

type union_specifier (* inlined *) = (
    Token.t (* "union" *)
  * ms_declspec_modifier option
  * anon_choice_field_id_opt_field_decl_list
)
[@@deriving sexp_of]

type array_declarator (* inlined *) = (
    declarator
  * Token.t (* "[" *)
  * type_qualifier list (* zero or more *)
  * anon_choice_exp option
  * Token.t (* "]" *)
)
[@@deriving sexp_of]

type enum_specifier (* inlined *) = (
    Token.t (* "enum" *)
  * [
        `Id_opt_enum_list of (identifier (*tok*) * enumerator_list option)
      | `Enum_list of enumerator_list
    ]
)
[@@deriving sexp_of]

type preproc_else_in_field_declaration_list (* inlined *) = (
    pat_56631e5 (*tok*)
  * field_declaration_list_item list (* zero or more *)
)
[@@deriving sexp_of]

type abstract_pointer_declarator (* inlined *) = (
    Token.t (* "*" *)
  * type_qualifier list (* zero or more *)
  * abstract_declarator option
)
[@@deriving sexp_of]

type parameter_declaration (* inlined *) = (
    declaration_specifiers
  * [ `Decl of declarator | `Abst_decl of abstract_declarator ] option
)
[@@deriving sexp_of]

type pointer_declarator (* inlined *) = (
    ms_based_modifier option
  * Token.t (* "*" *)
  * ms_pointer_modifier list (* zero or more *)
  * type_qualifier list (* zero or more *)
  * declarator
)
[@@deriving sexp_of]

type pointer_field_declarator (* inlined *) = (
    ms_based_modifier option
  * Token.t (* "*" *)
  * ms_pointer_modifier list (* zero or more *)
  * type_qualifier list (* zero or more *)
  * field_declarator
)
[@@deriving sexp_of]

type abstract_parenthesized_declarator (* inlined *) = (
    Token.t (* "(" *) * abstract_declarator * Token.t (* ")" *)
)
[@@deriving sexp_of]

type compound_literal_expression (* inlined *) = (
    Token.t (* "(" *) * type_descriptor * Token.t (* ")" *)
  * initializer_list
)
[@@deriving sexp_of]

type function_field_declarator (* inlined *) = (
    field_declarator * parameter_list
)
[@@deriving sexp_of]

type struct_specifier (* inlined *) = (
    Token.t (* "struct" *)
  * ms_declspec_modifier option
  * anon_choice_field_id_opt_field_decl_list
)
[@@deriving sexp_of]

type sizeof_expression (* inlined *) = (
    Token.t (* "sizeof" *)
  * [
        `Exp of expression
      | `LPAR_type_desc_RPAR of (
            Token.t (* "(" *) * type_descriptor * Token.t (* ")" *)
        )
    ]
)
[@@deriving sexp_of]

type abstract_function_declarator (* inlined *) = (
    abstract_declarator option
  * parameter_list
)
[@@deriving sexp_of]

type abstract_array_declarator (* inlined *) = (
    abstract_declarator option
  * Token.t (* "[" *)
  * type_qualifier list (* zero or more *)
  * anon_choice_exp option
  * Token.t (* "]" *)
)
[@@deriving sexp_of]

type function_declarator (* inlined *) = (
    declarator
  * parameter_list
  * attribute_specifier list (* zero or more *)
)
[@@deriving sexp_of]

type parenthesized_field_declarator (* inlined *) = (
    Token.t (* "(" *) * field_declarator * Token.t (* ")" *)
)
[@@deriving sexp_of]

type return_statement (* inlined *) = (
    Token.t (* "return" *)
  * anon_choice_exp_ option
  * Token.t (* ";" *)
)
[@@deriving sexp_of]

type empty_declaration (* inlined *) = (type_specifier * Token.t (* ";" *))
[@@deriving sexp_of]

type init_declarator (* inlined *) = (
    declarator
  * Token.t (* "=" *)
  * [ `Init_list of initializer_list | `Exp of expression ]
)
[@@deriving sexp_of]

type parenthesized_type_declarator (* inlined *) = (
    Token.t (* "(" *) * type_declarator * Token.t (* ")" *)
)
[@@deriving sexp_of]

type pointer_type_declarator (* inlined *) = (
    ms_based_modifier option
  * Token.t (* "*" *)
  * ms_pointer_modifier list (* zero or more *)
  * type_qualifier list (* zero or more *)
  * type_declarator
)
[@@deriving sexp_of]

type array_type_declarator (* inlined *) = (
    type_declarator
  * Token.t (* "[" *)
  * type_qualifier list (* zero or more *)
  * anon_choice_exp option
  * Token.t (* "]" *)
)
[@@deriving sexp_of]

type function_type_declarator (* inlined *) = (
    type_declarator * parameter_list
)
[@@deriving sexp_of]

type labeled_statement (* inlined *) = (
    identifier (*tok*) * Token.t (* ":" *) * statement
)
[@@deriving sexp_of]

type do_statement (* inlined *) = (
    Token.t (* "do" *) * statement * Token.t (* "while" *)
  * parenthesized_expression * Token.t (* ";" *)
)
[@@deriving sexp_of]

type case_statement (* inlined *) = (
    [
        `Case_exp of (Token.t (* "case" *) * expression)
      | `Defa of Token.t (* "default" *)
    ]
  * Token.t (* ":" *)
  * [
        `Choice_labe_stmt of non_case_statement
      | `Decl of declaration
      | `Type_defi of type_definition
    ]
      list (* zero or more *)
)
[@@deriving sexp_of]

type linkage_specification (* inlined *) = (
    Token.t (* "extern" *)
  * string_literal
  * [
        `Func_defi of function_definition
      | `Decl of declaration
      | `Decl_list of declaration_list
    ]
)
[@@deriving sexp_of]

type if_statement (* inlined *) = (
    Token.t (* "if" *)
  * parenthesized_expression
  * statement
  * (Token.t (* "else" *) * statement) option
)
[@@deriving sexp_of]

type while_statement (* inlined *) = (
    Token.t (* "while" *) * parenthesized_expression * statement
)
[@@deriving sexp_of]

type switch_statement (* inlined *) = (
    Token.t (* "switch" *) * parenthesized_expression * compound_statement
)
[@@deriving sexp_of]

type preproc_elif (* inlined *) = (
    pat_bfeb4bb (*tok*)
  * preproc_expression
  * Token.t (* "\n" *)
  * translation_unit
  * anon_choice_prep_else option
)
[@@deriving sexp_of]

type preproc_ifdef (* inlined *) = (
    anon_choice_pat_25b90ba
  * identifier (*tok*)
  * translation_unit
  * anon_choice_prep_else option
  * pat_c46d1b2 (*tok*)
)
[@@deriving sexp_of]

type for_statement (* inlined *) = (
    Token.t (* "for" *)
  * Token.t (* "(" *)
  * [ `Decl of declaration | `Opt_choice_exp_SEMI of expression_statement ]
  * expression option
  * Token.t (* ";" *)
  * anon_choice_exp_ option
  * Token.t (* ")" *)
  * statement
)
[@@deriving sexp_of]

type preproc_if (* inlined *) = (
    pat_3df6e71 (*tok*)
  * preproc_expression
  * Token.t (* "\n" *)
  * translation_unit
  * anon_choice_prep_else option
  * pat_c46d1b2 (*tok*)
)
[@@deriving sexp_of]

type preproc_else (* inlined *) = (pat_56631e5 (*tok*) * translation_unit)
[@@deriving sexp_of]

let dump_tree root =
  sexp_of_translation_unit root
  |> Print_sexp.to_stdout
