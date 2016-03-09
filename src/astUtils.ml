module A = Ast
module N = AstAnf
module SM = Map.Make(String)

type uri = string
type loc = A.loc
type string_dict = string SM.t

type binding_info =
    BPrim of string (** Some "primitive" value supplied by the initial environment *)
  | BDict of binding_info SM.t (** Some module supplied by the initial environment *)
  | BExp of A.expr (** This name is bound to some expression that we can't interpret yet *)
  | BDot of binding_info * string (** A field lookup off some binding that isn't a BDict *)
  | BTyp (** A type *)
  | BImport of A.import_type (** Imported from a module *)
  | BUnknown (** Any unknown value *)

type binding = EBind of loc * bool * binding_info

let ok_last = function
  | A.SLet(_,_,_,_)
  | A.SVar(_,_,_)
  | A.SRec(_,_,_)
  | A.SFun(_,_,_,_,_,_,_,_)
  | A.SData(_,_,_,_,_,_,_)
  | A.SContract(_,_,_)
  | A.SCheck(_,_,_,_)
  | A.SType(_,_,_)
  | A.SNewtype(_,_,_) -> false
  | _ -> true

let print_error = Printf.fprintf stderr "%s"

let checkers l = A.SApp(l,A.SDot(l,A.SId(l,A.SName(l,"builtins")),"current-checker"),[])

let last lst = match (List.fold_left (fun _ x -> Some(x)) None lst) with
  | None -> failwith "Cannot take last of empty list"
  | Some(x) -> x

let append_nothing_if_necessary = function
  | A.SProgram(l1,_provide,_provide_types,imports,body) ->
    match body with
    | A.SBlock(l2,stmts) ->
      (match stmts with
       | [] -> Some(A.SProgram(l1,_provide,_provide_types,imports,
                               A.SBlock(l2,[A.SId(l2,A.SName(l2,"nothing"))])))
       | _ ->
         let last_stmt = last stmts in
         if (ok_last last_stmt) then None else
           Some(A.SProgram(l1,_provide,_provide_types,imports,
                           A.SBlock(l2,stmts @ [A.SId(A.dummy_loc, A.SName(l2,"nothing"))]))))
    | _ -> None

class flatten_single_blocks = object(self)
  inherit A.default_map_visitor

  method s_block(l,stmts) =
    match stmts with
    | f::[] -> self#visit_expr f
    | _ -> A.SBlock(l,List.map self#visit_expr stmts)

end

class merge_nested_blocks = object(self)
  inherit A.default_map_visitor

  method s_block(l,stmts) =
    let merged_stmts = List.fold_left (fun new_stmts s ->
        let visited = (self#visit_expr s) in
        match visited with
        | A.SBlock(l2,stmts2) -> (List.rev stmts2) @ new_stmts
        | _ -> visited::new_stmts) [] stmts in
    A.SBlock(l,List.rev merged_stmts)

end

let rec bind_exp e env =
  match e with
  | A.SDot(l,o,name) ->
    (match (bind_exp o env) with
     | Some(eb) ->
       (match eb with
        | EBind(loc,mut,info) ->
          match info with
          | BDict(dict) ->
            if SM.mem name dict then
              Some(EBind(A.dummy_loc, false, SM.find name dict))
            else Some(EBind(A.dummy_loc,false,BDot(info,name)))
          | _ -> Some(EBind(A.dummy_loc,false,BDot(info,name))))
     | None -> None)
  | A.SId(_,name)
  | A.SIdVar(_,name)
  | A.SIdLetrec(_,name,_)->
    if (SM.mem (A.name_key name) env) then Some(SM.find (A.name_key name) env)
    else None
  | _ -> Some(EBind(A.dummy_loc,false,BExp(e)))

let bind_or_unknown e env =
  match (bind_exp e env) with
  | None -> BUnknown
  | Some(bind) ->
    match bind with
    | EBind(_,_,info) -> info

(*let binding_type_env_from_env env =
  let acc = ref SM.empty in
  List.iter (fun name ->
      SM.add (A.name_key (A.STypeGlobal(name))) (EBind(A.dummy_loc,false,BTyp)))
*)
