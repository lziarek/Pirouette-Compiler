type ftv
type local_subst
type local_ctx
type choreo_subst
type choreo_ctx
type global_ctx

val infer_local_expr
  :  local_ctx
  -> ftv Ast_core.Local.M.expr
  -> local_subst * ftv Ast_core.Local.M.typ

val infer_local_pattern
  :  local_ctx
  -> ftv Ast_core.Local.M.pattern
  -> local_subst * ftv Ast_core.Local.M.typ * local_ctx

val infer_choreo_expr
  :  choreo_ctx
  -> global_ctx
  -> ftv Ast_core.Choreo.M.expr
  -> choreo_subst * ftv Ast_core.Choreo.M.typ

(* val infer_choreo_pattern
   :  choreo_ctx
   -> global_ctx
   -> ftv Ast_core.Choreo.M.pattern
   -> choreo_subst * ftv Ast_core.Choreo.M.typ * choreo_ctx *)

val infer_choreo_stmt
  :  choreo_ctx
  -> global_ctx
  -> ftv Ast_core.Choreo.M.stmt
  -> choreo_subst * ftv Ast_core.Choreo.M.typ

val infer_choreo_stmt_block
  :  ftv Ast_core.Choreo.M.stmt_block
  -> ftv Ast_core.Choreo.M.typ
