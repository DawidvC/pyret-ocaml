open JAst

type compiled_code_printer =
  | CCP of JExpr.t
  | CCPString of string

let pyret_to_js_pretty width = function
  | CCP(compiled) ->
    PPrint.pretty_string (JExpr.tosource compiled) width
  | CCPString(string) ->
    failwith "Cannot generate pretty JS from code string"

let pyret_to_js_runnable = function
  | CCP(compiled) ->
    JExpr.to_ugly_source compiled
  | CCPString(compiled) ->
    compiled

let pyret_js_runnable printer = function
  | CCP(compiled) ->
    JExpr.print_ugly_source printer compiled
  | CCPString(compiled) ->
    printer compiled

let make_compiled_pyret program_ast env options =
  let anfed = Anf.anf_program program_ast in
  let visitor = AnfLoopCompiler.splitting_compiler env options in
  CCP(visitor#visit_program anfed)
