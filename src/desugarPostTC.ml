open Ast

let mk_id = Desugar.mk_id
let no_branches_exn = Desugar.no_branches_exn

let no_cases_exn l value =
  SPrimApp(l, "throwNoCasesMatched", [SSrcloc(l, l); value])

class desugar_visitor = object(self)
  inherit default_map_visitor

  method s_cases_else(l, typ, value, branches, els) =
    let name = global_names "cases" in
    let typ_compiled = self # visit_ann typ in
    let val_exp = self#visit_expr value in
    let val_id = SId(l, name) in
    SLetExpr(l, [SLetBind(l, SBind(l, false, name, typ_compiled), val_exp)],
             SCasesElse(l, ABlank, val_id, List.map self#visit_cases_branch branches, self#visit_expr els))

  method s_cases(l, typ, value, branches) =
    let name = global_names "cases" in
    let typ_compiled = self # visit_ann typ in
    let val_exp = self#visit_expr value in
    let val_id = SId(l, name) in
    SLetExpr(l, [SLetBind(l, SBind(l, false, name, typ_compiled), val_exp)],
             SCasesElse(l, ABlank, val_id, List.map self#visit_cases_branch branches,
                        SBlock(l, [no_cases_exn l val_id])))

  method s_check(l, name, body, _) =
    SId(l, SGlobal("nothing"))
end

let desugar_post_tc program compile_env =
  match program with
  | SProgram(l, _provide, provide_types, imports, body) ->
    SProgram(l, _provide, provide_types, imports, (new desugar_visitor)#visit_expr body)
