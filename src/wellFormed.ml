module A = Ast
module C = CompileStructs
module CE = CompileStructs.CompileError
module U = PyretUtils

module SSet = Set.Make(String)

let errors = ref []
let in_check_block = ref false
let cur_shared = ref []
let param_current_where_everywhere = ref false

let reserved_names = List.fold_left (fun acc x -> SSet.add x acc) SSet.empty [
    "function";
    "break";
    "return";
    "do";
    "yield";
    "throw";
    "continue";
    "while";
    "class";
    "interface";
    "type";
    "generator";
    "alias";
    "extends";
    "implements";
    "module";
    "package";
    "namespace";
    "use";
    "public";
    "private";
    "protected";
    "static";
    "const";
    "enum";
    "sumper";
    "export";
    "new";
    "try";
    "finally";
    "debug";
    "spy";
    "switch";
    "this";
    "match";
    "case";
    "with";
    "__proto__"
  ]

let list_find : 'a. ('a -> bool) -> 'a list -> 'a option = fun pred list ->
  try
    Some(List.find pred list)
  with
  | Not_found -> None

let add_error err =
  errors := err :: !errors;
  ()

let wf_error msg loc =
  add_error (CE.WFErr(msg,loc))

let wf_error2 msg loc1 loc2 =
  add_error (CE.WFErrSplit(msg, [loc1; loc2]))

let duplicate_id id loc1 loc2 =
  add_error (CE.DuplicateId(id, loc1, loc2))

let reserved_name loc id =
  add_error (CE.ReservedName(loc, id))

let wrap_visit_check res_thunk =
  let cur_in_check = !in_check_block in
  in_check_block := true;
  let ret = res_thunk() in
  in_check_block := cur_in_check;
  ret

let ensure_empty_block loc typ block =
  if (not !param_current_where_everywhere) then
    (match block with
     | A.SBlock(_,[]) -> ()
     | _ -> wf_error ("where: blocks only allowed on named function declarations and data, not on"
                      ^ typ) loc)
  else ()

let rec ensure_unique_cases =
  let same_name name = function
    | A.SCasesBranch(_,_,n,_,_)
    | A.SSingletonCasesBranch(_,_,n,_) -> n = name in
  let cases_branch_loc = function
    | A.SCasesBranch(l,_,_,_,_)
    | A.SSingletonCasesBranch(l,_,_,_) -> l in
  function
  | [] -> ()
  | f::rest ->
    (match f with
     | A.SCasesBranch(l,pat_loc,name,args,body) ->
       (match (list_find (same_name name) rest) with 
        | Some(found) -> wf_error2 ("Duplicate case for "^name) (cases_branch_loc found) pat_loc
        | _ -> ())
     | A.SSingletonCasesBranch(l,pat_loc,name,body) ->
       (match (list_find (same_name name) rest) with
        | Some(found) -> wf_error2 ("Duplicate case for "^name) (cases_branch_loc found) pat_loc
        | _ -> ()));
    ensure_unique_cases rest

let same_name name = function
  | A.SBind (_,_,id,_) ->
    match id with
    | A.SName(_,s) -> s = name
    | _ -> false
let same_id id = function
  | A.SBind(_,_,id2,_) -> id = id2
let name_to_string n = PPrint.pretty_string (A.name_tosource n) 80
let bind_loc = function
  | A.SBind(l,_,_,_) -> l
let rec ensure_unique_ids =
  function
  | [] -> ()
  | f::rest ->
    (match f with
     | A.SBind(l,_,id,_) ->
       match id with
       | A.SUnderscore(_) -> ()
       | A.SName(_,name) ->
         (match (list_find (same_name name) rest) with
          | Some(found) ->
            wf_error2 ("Found duplicate id "^(name_to_string id)^" in list of bindings")
              l (bind_loc found)
          | None -> ())
       | _ ->
         match (list_find (same_id id) rest) with
         | Some(found) ->
           wf_error2 ("Found duplicate id "^(name_to_string id)^" in list of bindings")
             l (bind_loc found)
         | None -> ());
    ensure_unique_ids rest

let rec ensure_unique_bindings = function
  | [] -> ()
  | f::rest ->
    (match f with
     | A.SBind(l,shadows,id,_) ->
       if shadows then () else
         (match id with
          | A.SUnderscore(_) -> ()
          | _ -> match (list_find (same_id id) rest) with
            | Some(found) -> duplicate_id (name_to_string id) l (bind_loc found)
            | None -> ()));
    ensure_unique_bindings rest

let rec ensure_unique_fields = function
  | [] -> ()
  | f :: rest ->
    (let field_name (f : A.member) =
       match f with
       | A.SDataField (_,name,_)
       | A.SMutableField (_,name,_,_)
       | A.SMethodField (_,name,_,_,_,_,_,_) -> name in
     let field_loc (f : A.member) =
       match f with
       | A.SDataField(l,_,_)
       | A.SMutableField(l,_,_,_)
       | A.SMethodField(l,_,_,_,_,_,_,_) -> l in
     let first_name = field_name f in
     let matches_first fld = (field_name fld) = first_name in
     match (list_find matches_first rest) with
     | Some(found) -> add_error (CE.DuplicateField(first_name, field_loc f, field_loc found))
     | None -> ());
    ensure_unique_fields rest

let check_underscore_name_gen : (string -> 'a -> bool) -> ('a -> A.loc) -> 'a list -> string -> bool =
  fun has_name get_loc fields kind_of_thing ->
  let underscores = List.filter (has_name "_") fields in
  match underscores with
  | [] -> true
  | f::_ -> wf_error ("Cannot use underscore as a "^kind_of_thing) (get_loc f); false

let check_underscore_name =
  let member_loc = function
    | A.SDataField(l,_,_)
    | A.SMutableField(l,_,_,_)
    | A.SMethodField(l,_,_,_,_,_,_,_) -> l in
  let member_has_name name = function
    | A.SDataField(_,s,_)
    | A.SMutableField(_,s,_,_)
    | A.SMethodField(_,s,_,_,_,_,_,_) -> s = name in
  check_underscore_name_gen member_has_name member_loc

let check_underscore_name_variant =
  let variant_loc (v : A.variant) =
    match v with
    | A.SVariant (l,_,_,_,_)
    | A.SSingletonVariant (l,_,_) -> l in
  let variant_has_name name = function
    | A.SVariant (_,_,s,_,_)
    | A.SSingletonVariant(_,s,_) -> name = s in
  check_underscore_name_gen variant_has_name variant_loc

let check_underscore_name_data =
  let get_loc = function
    | A.SData(l,_,_,_,_,_,_) -> l
    | _ -> failwith "Invalid input to check_underscore_name_data" in
  let has_name name = function
    | A.SData(_,n,_,_,_,_,_) -> name = n
    | _ -> failwith "Invalid input to check_underscore_name_data" in
  check_underscore_name_gen has_name get_loc

let rec ensure_distinct_lines loc = function
  | [] -> ()
  | first::rest ->
    let first_l = A.expr_loc first in
    match loc with
    | A.Srcloc.Builtin(_) -> ensure_distinct_lines first_l rest
    | A.Srcloc.Srcloc(_,_,_,_,end_line1,_,_) ->
      match first_l with
      | A.Srcloc.Builtin(_) -> ensure_distinct_lines loc rest
      | A.Srcloc.Srcloc(_,start_line2,_,_,_,_,_) ->
        (if end_line1 = start_line2 then
           wf_error2 "Found two expressions on the same line" loc first_l);
        ensure_distinct_lines first_l rest

let rec ensure_unique_variant_ids =
  let variant_loc = function
    | A.SVariant (l,_,_,_,_)
    | A.SSingletonVariant (l,_,_) -> l in
  let variant_name = function
    | A.SVariant(_,_,s,_,_)
    | A.SSingletonVariant(_,s,_) -> s in
  let variant_has_name name v = (variant_name v) = name in
  function
  | [] -> ()
  | f::rest ->
    match (list_find (variant_has_name (variant_name f)) rest) with
    | Some(found) ->
      wf_error2 ("Found duplicate id "^(variant_name f)^" in list of bindings")
        (variant_loc f) (variant_loc found)
    | None -> ensure_unique_variant_ids rest

let wf_last_stmt = function
  | A.SLet(l,_,_,_) -> wf_error "Cannot end a block in a let-binding" l
  | A.SVar(l,_,_) -> wf_error "Cannot end a block in a var-binding" l
  | A.SRec(l,_,_) -> wf_error "Cannot end a block in a rec-binding" l
  | A.SFun(l,_,_,_,_,_,_,_) -> wf_error "Cannot end a block in a fun-binding" l
  | A.SData(l,_,_,_,_,_,_) -> wf_error "Cannot end a block with a data definition" l
  | _ -> ()

let fields_to_binds = List.map (function
    | A.SDataField(l,name,_)
    | A.SMutableField(l,name,_,_)
    | A.SMethodField(l,name,_,_,_,_,_,_) ->
      A.SBind(l,false,A.SName(l,name),A.ABlank))

let opname op = String.sub op 2 ((String.length op) - 2)
let rec reachable_ops self l op ast =
  match ast with
  | A.SOp(l2,op2,left2,right2) ->
    (if (op = op2) then
       begin
         let _ = (reachable_ops self l op left2,
                  reachable_ops self l op right2) in ()
       end
     else
       wf_error2 ("Cannot mix binary operators of different types: `"
                  ^(opname op)^"` and `"^(opname op2)
                  ^"`. Use parentheses to disambiguate.") l l2);
    true
  | _ -> self#visit_expr ast

let wf_block_stmts visitor l stmts =
  let is_bind = function
    | A.SVar(_,_,_)
    | A.SLet(_,_,_,_)
    | A.SRec(_,_,_) -> true
    | _ -> false in
  let bind_names = List.map (function
    | A.SVar(_,n,_)
    | A.SLet(_,n,_,_)
    | A.SRec(_,n,_) -> n
    | _ -> failwith "Internal error: is_bind failed") in
  let bind_stmts = bind_names (List.filter is_bind stmts) in
  ensure_unique_bindings (List.rev bind_stmts);
  ensure_distinct_lines A.dummy_loc stmts;
  List.for_all visitor#visit_expr stmts

let wf_examples_body visitor = function
  | A.SBlock(_,stmts) ->
    List.for_all (function
        | A.SCheckTest(l,_,_,_,_) ->
          wf_error ("Found something other than an example. "
                    ^"Example blocks must contain only test statements") l;
          false
        | _ -> true) stmts
  | _ -> failwith "wf_examples_body called with non-block"

let is_underscore = function
  | A.SId(_,name) ->
    (match name with
     | A.SUnderscore(_) -> true
     | _ -> false)
  | _ -> false

class well_formed_visitor = object(self)
  inherit A.default_iter_visitor
  method s_program _ = failwith "Impossible"
  method s_special_import(l,kind,args) =
    match kind with
    | "my-gdrive" ->
      if (List.length args) <> 1 then
        begin
          wf_error "Imports with my-gdrive should have one argument, the name of the file" l;
          false
        end
      else true
    | "shared-gdrive" ->
      if (List.length args) <> 2 then
        begin
          wf_error ("Imports with shared-gdrive should have two arguments, "
                    ^"the name of the file and the file's id, which you can get "
                    ^"from the share URL") l;
          false
        end
      else true
    | "js-http" -> true
    | "gdrive-js" ->
      if (List.length args) <> 2 then
        begin
          wf_error ("Imports with gdrive-js should have two arguments, "
                    ^"the name of the file and the file's id") l;
          false
        end
      else true
    | _ -> true

  method s_data (l,_,_,_,_,_,_) =
    wf_error "Cannot define a data expression except at the top level of a file" l;
    false
  method s_data_expr (l,_,_,_,_,_,_,_) =
    wf_error "Cannot define a data expression except at the top level of a file" l;
    false
  method s_type (l,_,_) =
    wf_error "Cannot define a type alias except at the top level of a file" l;
    false
  method s_newtype (l,_,_) =
    wf_error "Cannot define a newtype except at the top level of a file" l;
    false
  method s_type_let_expr (l,_,_) =
    wf_error "Cannot define newtypes or type aliases except at the top level of a file" l;
    false
  method s_op(l,op,left,right) =
    reachable_ops self l op left && reachable_ops self l op right
  method s_cases_branch(_,pat_loc,name,args,body) =
    if name = "_" then begin
      wf_error "Found a cases branch using _ rather than a constructor name; use 'else' instead"
        pat_loc;
      false
    end else begin
      ensure_unique_ids (List.map (function
          | A.SCasesBind(_,_,b) -> b) args);
      (List.for_all self#visit_cases_bind args) && self#visit_expr body
    end
  method s_singleton_cases_branch(_,pat_loc,name,body) =
    if name = "_" then begin
      wf_error "Found a cases branch using _ rather than a constructor name; use 'else' instead"
        pat_loc;
      false
    end else self#visit_expr body

  method s_var(l,bind,value) =
    (match bind with
     | A.SBind (bind_l,_,id,_) ->
       match id with
       | A.SUnderscore(_) -> add_error (CE.PointlessVar(A.Srcloc.combine (A.Srcloc.at_start l) bind_l))
       | _ -> ());
    self#visit_bind bind && self#visit_expr value

  method s_rec = self#s_var
  method s_var_bind = self#s_var

  method s_block(l,stmts) =
    match stmts with
    | [] -> wf_error "Empty block" l; true
    | _ -> wf_last_stmt (U.last stmts); wf_block_stmts self l stmts

  method s_bind(l,shadows,name,ann) =
    let name_str = name_to_string name
    and name_is_underscore = (match name with
        | A.SUnderscore(_) -> true
        | _ -> false) in
    (if (SSet.mem name_str reserved_names) then
      reserved_name l name_str else ());
    (if shadows && name_is_underscore then
       add_error (CE.PointlessShadow(l)) else ());
    self#visit_name name && self#visit_ann ann

  method s_check_test(l,op,refinement,left,right) =
    let op_name = (PPrint.pretty_string (A.checkop_tosource op) 80) in
    (if (not !in_check_block) then
       wf_error ("Cannot use `" ^ op_name ^ "` outside of a `check` or `where` block") l else ());
    (match refinement with
     | None -> ()
     | Some(refinement) ->
       match op with
       | A.SOpIs -> ()
       | A.SOpIsNot -> ()
       | A.SOpSatisfies
       | A.SOpSatisfiesNot ->
         wf_error ("Cannot use refinement syntax `%(...)` with `" ^ op_name ^ "`. "
                   ^ "Consider changing the predicate instead.") l
       | _ ->
           wf_error ("Cannot use refinement syntax `%(...)` with `" ^ op_name ^ "`.") l);
    self#visit_expr left && self#visit_expr_option right

  method s_method_field(l,name,params,args,ann,doc,body,_check) =
    (if (SSet.mem name reserved_names) then
       reserved_name l name else ());
    self#s_method(l,params,args,ann,doc,body,_check)

  method s_data_field(l,name,value) =
    (if SSet.mem name reserved_names then
       reserved_name l name else ());
    self#visit_expr value

  method s_mutable_field(l,name,ann,value) =
    (if SSet.mem name reserved_names then
       reserved_name l name else ());
    self#visit_ann ann && self#visit_expr value

  method s_method(l,params,args,ann,_,body,_check) =
    (match args with
     | [] -> wf_error "Cannot have a method with zero arguments" l
     | _ -> ());
    ensure_unique_ids args;
    (match _check with
     | None -> ()
     | Some(_check) -> ensure_empty_block l "methods" _check);
    List.for_all self#visit_bind args
    && self#visit_ann ann
    && self#visit_expr body
    && wrap_visit_check (fun () -> self#visit_expr_option _check)

  method s_lam(l,params,args,ann,_,body,_check) =
    ensure_unique_ids args;
    (match _check with
     | None -> ()
     | Some(_check) -> ensure_empty_block l "anonymous functions" _check);
    List.for_all self#visit_name params
    && List.for_all self#visit_bind args
    && self#visit_ann ann
    && self#visit_expr body
    && wrap_visit_check (fun () -> self#visit_expr_option _check)

  method s_fun(l,name,params,args,ann,_,body,_check) =
    (if SSet.mem name reserved_names then
       reserved_name l name else ());
    List.for_all self#visit_name params
    && List.for_all self#visit_bind args
    && self#visit_ann ann
    && self#visit_expr body
    && wrap_visit_check (fun () -> self#visit_expr_option _check)

  method s_obj(_,fields) =
    ensure_unique_fields (List.rev fields);
    let _ = check_underscore_name fields "field name" in
    List.for_all self#visit_member fields

  method s_check(_,name,body,keyword_check) =
    if not keyword_check then begin
      (** This result was being ignored in the Pyret compiler, so we do the same here. *)
      let _ = wrap_visit_check (fun () -> self#visit_expr body) in
      wf_examples_body self body
    end else
      wrap_visit_check (fun () -> self#visit_expr body)

  method s_if(l,branches) =
    (match branches with
     | hd :: [] -> wf_error "Cannot have an `if` with a single branch" l
     | _ -> ());
    List.for_all self#visit_if_branch branches

  method s_cases(_,typ,value,branches) =
    ensure_unique_cases branches;
    self#visit_ann typ
    && self#visit_expr value
    && List.for_all self#visit_cases_branch branches

  method s_cases_else(l,typ,value,branches,_else) =
    self#s_cases(l,typ,value,branches)
    && self#visit_expr _else

  method s_frac(l,num,den) =
    (if den = 0 then
       add_error (CE.ZeroFraction(l,A.SNum(l,BatNum.of_int num))) else ());
    true

  method s_id(l,id) =
    (if SSet.mem (name_to_string id) reserved_names then
       reserved_name l (name_to_string id) else ());
    true

  method s_provide _ = true

end

class top_level_visitor = object(self)
  inherit A.default_iter_visitor
  val wfv = new well_formed_visitor

  method s_program(_,_provide,_provide_types,imports,body) =
    let ok_body = match body with
      | A.SBlock(l2,stmts) -> wf_block_stmts self l2 stmts
      | _ -> self#visit_expr body in
    ok_body
    && self#visit_provide _provide
    && self#visit_provide_types _provide_types
    && List.for_all self#visit_import imports

  method s_type(_,_,ann) = wfv#visit_ann ann

  method s_newtype _ = true

  method s_type_let_expr(_,binds,body) =
    List.for_all self#visit_type_let_bind binds
    && wfv#visit_expr body

  method s_type_bind(_,_,ann) = wfv#visit_ann ann
  method s_newtype_bind _ = true

  method s_variant(_,_,_,binds,with_members) =
    let variant_member_loc = function
      | A.SVariantMember(l,_,_) -> l in
    let is_underscore = function
      | A.SVariantMember(_,_,b) ->
        match b with
        | A.SBind(_,_,name,_) ->
          match name with
          | A.SUnderscore(_) -> true
          | _ -> false in
    let variant_member_bind = function
      | A.SVariantMember(_,_,b) -> b in
    let ids = (fields_to_binds with_members) @ (List.map variant_member_bind binds) in
    ensure_unique_ids ids;
    let underscores = (List.filter is_underscore binds) in
    let underscores_empty = (match underscores with | [] -> true | _ -> false) in
    (if not underscores_empty then
     wf_error "Cannot use underscore as a field name in data variant "
       (variant_member_loc (List.hd underscores)) else ());
    let _ = check_underscore_name with_members "field name" in
    underscores_empty
    && List.for_all wfv#visit_variant_member binds
    && List.for_all wfv#visit_member with_members

  method s_singleton_variant(_,_,with_members) =
    let _ = ensure_unique_ids (fields_to_binds with_members) in
    List.for_all wfv#visit_member with_members

  method s_data(l,name,params,mixins,variants,shares,_check) =
    let _ = ensure_unique_variant_ids variants in
    let _ = check_underscore_name_variant variants "data variant name" in
    let _ = check_underscore_name shares "shared field name" in
    let _ = check_underscore_name_data [A.SData(l,name,params,mixins,variants,shares,_check)]
        "datatype name" in
    let the_cur_shared = !cur_shared in
    cur_shared := (fields_to_binds shares);
    let params_v = List.for_all wfv#visit_name params
    and mixins_v = List.for_all wfv#visit_expr mixins
    and variants_v = List.for_all wfv#visit_variant variants
    and shares_v = List.for_all wfv#visit_member shares in
    cur_shared := the_cur_shared;
    params_v
    && mixins_v
    && variants_v
    && shares_v
    && wrap_visit_check (fun () -> wfv#visit_expr_option _check)

  method s_data_expr(l,name,namet,params,mixins,variants,shared,_check) =
    let variant_loc = function
      | A.SVariant(l,_,_,_,_)
      | A.SSingletonVariant(l,_,_) -> l in
    let _ = ensure_unique_variant_ids variants in
    let underscores = List.filter (function
        | A.SVariant(_,_,n,_,_)
        | A.SSingletonVariant(_,n,_) -> n = "_") variants in
    let underscores_is_empty = (match underscores with | [] -> true | _ -> false) in
    (if underscores_is_empty then
       wf_error "Cannot use underscore as a data variant name " (variant_loc (List.hd variants))
     else ());
    let the_cur_shared = !cur_shared in
    cur_shared := fields_to_binds shared;
    let ret = List.for_all wfv#visit_name params
              && List.for_all wfv#visit_expr mixins
              && List.for_all wfv#visit_variant variants
              && List.for_all wfv#visit_member shared in
    cur_shared := the_cur_shared;
    underscores_is_empty
    && ret
    && wrap_visit_check (fun () -> wfv#visit_expr_option _check)

  method s_import = wfv#s_import
  method s_include = wfv#s_include
  method s_import_types = wfv#s_import_types
  method s_provide = wfv#s_provide
  method s_provide_types = wfv#s_provide_types
  method s_bind = wfv#s_bind
  method s_var_bind = wfv#s_var_bind
  method s_let_bind = wfv#s_let_bind
  method s_let_expr = wfv#s_let_expr
  method s_letrec_bind = wfv#s_letrec_bind
  method s_letrec = wfv#s_letrec
  method s_hint_exp = wfv#s_hint_exp
  method s_instantiate = wfv#s_instantiate
  method s_block = wfv#s_block
  method s_user_block = wfv#s_user_block
  method s_fun = wfv#s_fun
  method s_var = wfv#s_var
  method s_rec = wfv#s_rec
  method s_let = wfv#s_let
  method s_ref = wfv#s_ref
  method s_when = wfv#s_when
  method s_contract = wfv#s_contract
  method s_assign = wfv#s_assign
  method s_if_branch = wfv#s_if_branch
  method s_if_pipe_branch = wfv#s_if_pipe_branch
  method s_if = wfv#s_if
  method s_if_else = wfv#s_if_else
  method s_if_pipe = wfv#s_if_pipe
  method s_if_pipe_else = wfv#s_if_pipe_else
  method s_cases_branch = wfv#s_cases_branch
  method s_singleton_cases_branch = wfv#s_singleton_cases_branch
  method s_cases = wfv#s_cases
  method s_cases_else = wfv#s_cases_else
  method s_op = wfv#s_op
  method s_check_test = wfv#s_check_test
  method s_paren = wfv#s_paren
  method s_lam = wfv#s_lam
  method s_method = wfv#s_method
  method s_extend = wfv#s_extend
  method s_update = wfv#s_update
  method s_obj = wfv#s_obj
  method s_array = wfv#s_array
  method s_construct = wfv#s_construct
  method s_app = wfv#s_app
  method s_prim_app = wfv#s_prim_app
  method s_frac = wfv#s_frac
  method s_id = wfv#s_id
  method s_id_var = wfv#s_id_var
  method s_id_letrec = wfv#s_id_letrec
  method s_dot = wfv#s_dot
  method s_get_bang = wfv#s_get_bang
  method s_bracket = wfv#s_bracket
  method s_for = wfv#s_for
  method s_check = wfv#s_check
  method s_data_field = wfv#s_data_field
  method s_mutable_field = wfv#s_mutable_field
  method s_method_field = wfv#s_method_field
  method s_for_bind = wfv#s_for_bind
  method s_variant_member = wfv#s_variant_member
  method a_arrow = wfv#a_arrow
  method a_method = wfv#a_method
  method a_record = wfv#a_record
  method a_app = wfv#a_app
  method a_pred = wfv#a_pred
  method a_dot = wfv#a_dot
  method a_field = wfv#a_field
end

module CompileResult = C.CompileResult(struct type t = A.program end)

let check_well_formed ast =
  cur_shared := [];
  errors := [];
  in_check_block := false;
  let tlv = new top_level_visitor in
  let ans =
    (if (tlv#visit_program ast) && ((List.length !errors) = 0) then
       CompileResult.Ok(ast)
     else
       CompileResult.Err(!errors)) in
  cur_shared := [];
  errors := [];
  in_check_block := false;
  ans
