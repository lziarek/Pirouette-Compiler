module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M

type errmsg = string
type typvar = string
type ftv = (typvar, errmsg) result
type local_subst = (typvar * ftv Local.typ) list
type choreo_subst = (typvar * ftv Choreo.typ) list
type local_ctx = (string * ftv Local.typ) list
type choreo_ctx = (string * ftv Choreo.typ) list
type global_ctx = (string * string * ftv Local.typ) list

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
