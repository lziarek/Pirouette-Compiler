val dump_choreo_ast : Choreo.program -> string
(** [dump_ast prog] converts the choreo AST [prog] into a JSON string. *)

val dot_graph : Choreo.program -> string
(* generate_dot_code print the choreo AST [prog] into given output file *)