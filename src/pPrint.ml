(** Implementation of Pyret Pretty-Printing library *)
open Format

type pprintdoc =
    MtDoc of int * bool
  | Str of string * int * bool
  | Hardline of int * bool
  | Blank of int * int * bool
  | Concat of pprintdoc * pprintdoc * int * bool
  | Nest of int * pprintdoc * int * bool
  | IfFlat of pprintdoc * pprintdoc * int * bool
  | Align of pprintdoc * int * bool
  | AlignSpaces of int * int * bool
  | Group of pprintdoc * int * bool

type item = Item of int * bool * pprintdoc

let rec dbg_printer doc = match doc with
  | MtDoc(_,_) -> "EmptyDoc"
  | Str(s,_,_) -> sprintf "Str(%s)" s
  | Hardline(_,_) -> "CRLF"
  | Blank(n,_,_) -> sprintf "Blank(%d)" n
  | Concat(fst,snd,_,_) -> sprintf "Concat(%s,%s)" (dbg_printer fst) (dbg_printer snd)
  | Nest(indent,d,_,_) -> sprintf "Nest(%d,%s)" indent (dbg_printer d)
  | IfFlat(flat,vert,_,_) -> sprintf "IfFlat(%s,%s)" (dbg_printer flat) (dbg_printer vert)
  | Group(d,_,_) -> sprintf "Group(%s)" (dbg_printer d)
  | Align(d,_,_) -> sprintf "Align(%s)" (dbg_printer d)
  | AlignSpaces(n,_,_) -> sprintf "AlignSpaces(%d)" n

let rec collect_concats (i : int) (m : bool) (it : pprintdoc) (rest : item list) = match it with
  | Concat(fst,snd,_,_) ->
    collect_concats i m  fst (collect_concats i m snd rest)
  | _ -> (Item(i,m,it))::rest

let get_fwhl doc = match doc with
    | MtDoc(f,h) -> (f,h)
    | Str(_,f,h) -> (f,h)
    | Hardline(f,h) -> (f,h)
    | Blank(_,f,h) -> (f,h)
    | Concat(_,_,f,h) -> (f,h)
    | Nest(_,_,f,h) -> (f,h)
    | IfFlat(_,_,f,h) -> (f,h)
    | Align(_,f,h) -> (f,h)
    | AlignSpaces(_,f,h) -> (f,h)
    | Group(_,f,h) -> (f,h)

let rec format (width : int) (doc : pprintdoc) =
  let cur_line = ref []
  and output = ref [] in
  let emit_text (s : string) =
    cur_line := s::!cur_line in
  let emit_spaces (n : int) =
    emit_text (String.make n ' ')
  and emit_newline (i : int) =
    output := !cur_line::!output;
    cur_line := [(String.make i ' ')]
  and gen_output () =
    output := !cur_line::!output;
    let flines_fun = fun acc line -> (List.fold_left (fun acc piece -> piece ^ acc) "" line)::acc in
    List.fold_left flines_fun [] !output in
  let rec process (column : int) (items : item list) =
    match items with
    | [] -> ()
    | (Item(i,m,d))::rest -> begin match d with
        | MtDoc(_,_) -> process column rest
        | Concat(_,_,_,_) -> process column (collect_concats i m d rest)
        | Str(s,flat_width,_) ->
          emit_text s;
          (*Printf.printf "emitting %s\n" s;*)
          process (column + flat_width) rest
        | Blank(n,_,_) ->
          emit_spaces n;
          process (column + n) rest
        | Align(d,_,_) -> process column ((Item(column,m,d))::rest)
        | Nest(n,d,_,_) -> process column ((Item(i + n,m,d))::rest)
        | Hardline(_,_) ->
          if m then failwith "Impossible for Hardline to be flat"
          else
            begin emit_newline i;
              process i rest end
        | IfFlat(flat,vert,_,_) ->
          process column ((Item(i,m,if m then flat else vert))::rest)
        | AlignSpaces(n,_,_) -> if m then (process column rest)
          else begin emit_spaces n; process (column + n) rest end
        | Group(d,flat_width,has_hardline) ->
          if m then (process column ((Item(i,true,d))::rest))
          else if has_hardline then (process column ((Item(i,false,d))::rest))
          else if ((width - column) >= flat_width) then (process column ((Item(i,true,d))::rest))
          else (process column ((Item(i,false,d))::rest))
      end in
  let (flat_width, has_hardline) = get_fwhl doc in
  process 0 [Item(0,false,Group(doc, flat_width, has_hardline))];
  gen_output()

let (+^) left right = match left with
  | MtDoc(_,_) -> right
  | _ -> match right with
    | MtDoc(_,_) -> left
    | _ -> let (left_fw, left_hl) = get_fwhl left in
      let (right_fw, right_hl) = get_fwhl right in
      if left_hl || right_hl then Concat(left,right,0,true)
      else Concat(left,right,left_fw + right_fw, false)

let mt_doc = MtDoc(0,false)
let hardline = Hardline(0,true)
let align d = let (fw,hl) = get_fwhl d in Align(d,fw,hl)
let group d = let (fw,hl) = get_fwhl d in Group(d,fw,hl)
let if_flat flat vert = let (fw,hl) = get_fwhl flat in IfFlat(flat,vert,fw,hl)
let nest n d = let (fw,hl) = get_fwhl d in Nest(n,d,fw,hl)
let concat fst snd = fst +^ snd
let blank n = Blank(n,n,false)
let str s = Str(s,String.length s, false)
let number n = str(string_of_int n)

let lparen = str "("
let rparen = str ")"
let lbrace = str "{"
let rbrace = str "}"
let lbrack = str "["
let rbrack = str "]"
let langle = str "<"
let rangle = str ">"
let comma = str ","
let sbreak n = if_flat (blank n) hardline
let commabreak = comma +^ (sbreak 1)

let flow_map sep f items =
  let foldfun = fun acc item -> match acc with
    | MtDoc(_,_) -> f item
    | _ -> acc +^ (group sep +^ (f item)) in
  List.fold_left foldfun mt_doc items
let flow items = flow_map (sbreak 1) (fun x -> x) items
let vert items = flow_map hardline (fun x -> x) items
let parens d = group (lparen +^ d +^ rparen)
let braces d = group (lbrace +^ d +^ rbrace)
let brackets d = group (lbrack +^ d +^ rbrack)
let str_squote = str "'"
let str_dquote = str "\""
let dquote s = group (str_dquote +^ s +^ str_dquote)
let squote s = group (str_squote +^ s +^ str_squote)

let hang i d = align (nest i d)
let prefix n b x y = group (x +^ (nest n ((sbreak b) +^ y)))
let infix (n : int) (b : int) (op : pprintdoc) (x : pprintdoc) (y : pprintdoc) =
  prefix n b (x +^ (blank b) +^ op) y
let infix_break (n : int) (b : int) (op : pprintdoc) (x : pprintdoc) (y : pprintdoc) =
  prefix n b x (op +^ (blank b) +^ y)
let surround (n : int) (b : int) (pp_open : pprintdoc) (contents : pprintdoc) (pp_close : pprintdoc) =
  match pp_close with
  | MtDoc(_,_) -> group (pp_open +^ (nest n ((sbreak b) +^ contents)))
  | _ -> group (pp_open +^ (nest n ((sbreak b) +^ contents)) +^ (sbreak b) +^ pp_close)
let soft_surround n b pp_open contents pp_close =
  match pp_close with
  | MtDoc(_,_) -> group (pp_open +^ (nest n (group ((sbreak b) +^ contents))))
  | _ -> group (pp_open +^ (nest n (group ((sbreak b) +^ contents))) +^ (group ((sbreak b) +^ pp_close)))

let separate sep docs =
  List.fold_left (fun acc d -> match d with
      | MtDoc(_,_) -> acc
      | _ -> match acc with
        | MtDoc(_,_) -> d
        | _ -> acc +^ sep +^ d) mt_doc docs
let surround_separate n b void pp_open sep pp_close docs = match docs with
  | [] -> void
  | _ -> surround n b pp_open (separate sep docs) pp_close

let label_align_surround label pp_open sep contents pp_close =
  group label +^ (align pp_open +^ (align (separate sep contents))) +^ (group ((sbreak 0) +^ pp_close))

let pretty doc n = format n doc
let pretty_string doc n = List.fold_right (fun x acc -> x ^ "\n" ^ acc) (pretty doc n) ""
