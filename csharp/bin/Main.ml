(* Generated by ocaml-tree-sitter. *)

open Tree_sitter_csharp

let () =
  Tree_sitter_run.Main.run
    ~lang:"csharp"
    ~parse_source_file:Parse.parse_source_file
    ~parse_input_tree:Parse.parse_input_tree
    ~dump_tree:CST.dump_tree
