**2021-02-07: This repo was split into one repo per language e.g.
https://github.com/returntocorp/semgrep-javascript**

# ocaml-tree-sitter-lang

C/OCaml parsers for multiple programming languages, generated
by [ocaml-tree-sitter](https://github.com/returntocorp/ocaml-tree-sitter)
from tree-sitter grammars.

Installation
--

Here are some crude instructions for setting up a project using these
parsers:

1. Install the [tree-sitter](https://github.com/tree-sitter/tree-sitter)
   runtime library: Clone the repo, then `make && sudo make install`.
2. Install the runtime libraries provided by
   [ocaml-tree-sitter](https://github.com/returntocorp/ocaml-tree-sitter):
   Clone the repo. Install the OCaml dependencies with
   `opam install --deps-only -y .`, then `make && make install`.
3. Copy the desired source subtrees (e.g. `ruby/`) into to your dune
   project. Adding the whole ocaml-tree-sitter-lang tree from a
   tarball or as a git submodule should work too.
