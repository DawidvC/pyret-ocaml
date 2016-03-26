type error_display =
    VSequence of error_display list
  | BulletedSequence of error_display list
  | NumberedSequence of error_display list
  | HSequence of error_display list * string
  | Text of string
  | Loc of Ast.loc
  | MaybeStackLoc of int * bool * (Ast.loc -> error_display) * error_display
  | Code of error_display
  | Styled of error_display * string
  | LocDisplay of Ast.loc * string * error_display
  | Optional of error_display

let error lst = VSequence lst

let para lst = HSequence(lst," ")

let para_nospace lst = HSequence(lst, "")

let bulleted lst = BulletedSequence lst

let numbered lst = NumberedSequence lst

let opt lst = Optional (VSequence lst)

let rec display_to_string e stack =
  let help x = display_to_string x stack in
  let not_optional = function
    | Optional(_) -> false
    | _ -> true in
  let join_str = PyretUtils.join_str in
  match e with
  | Text(str) -> str
  | Loc(l) -> Ast.str_of_loc l
  | MaybeStackLoc(n,ufo,cwl,cwol) ->
    help(cwol) (* TODO: Implement stack display *)
  | LocDisplay(l, _, contents) ->
      (match contents with
       | Loc(l2) -> if l2 = l then help contents
         else (help contents) ^ " (at " ^ (Ast.str_of_loc l) ^ ")"
       | _ -> (help contents) ^ " (at " ^ (Ast.str_of_loc l) ^ ")")
  | Code(contents) -> "`"^(help contents)^"`"
  | Styled(contents,style) -> help contents
  | HSequence(contents,sep) ->
    join_str (List.map help (List.filter not_optional contents)) sep
  | VSequence(contents) ->
    join_str (List.map help (List.filter not_optional contents)) "\n"
  | BulletedSequence(contents) ->
    join_str (List.map (fun x -> "* " ^ (help x)) contents) "\n"
  | NumberedSequence(contents) ->
    join_str (List.mapi (fun i x -> (string_of_int i)^" "^(help x)) contents) "\n"
  | Optional(_) -> ""
