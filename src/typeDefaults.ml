module A = Ast
module G = Gensym
(** Mutable String Dictionary *)
module MSD = PyretUtils.MutableStringDict
module SL = Ast.Srcloc
module SD = PyretUtils.StringDict
module TS = TypeStructs

let t_nothing = TS.t_nothing A.dummy_loc
let t_boolean = TS.t_boolean A.dummy_loc
let t_number = TS.t_number A.dummy_loc
let t_string = TS.t_string A.dummy_loc
let t_array c = TS.t_array c A.dummy_loc
let t_srcloc = TS.t_srcloc A.dummy_loc

module Type = TS.Type
let t_name modulename name = Type.TName(modulename,name,A.dummy_loc)
let t_var name = Type.TVar(name,A.dummy_loc)
let t_arrow params ret = Type.TArrow(params,ret,A.dummy_loc)
let t_top = Type.TTop A.dummy_loc
let t_bot = Type.TBot A.dummy_loc
let t_app func args = Type.TApp(func,args,A.dummy_loc)
let t_record fields = Type.TRecord(fields,A.dummy_loc)
let t_forall params body = Type.TForall(params,body,A.dummy_loc)
let t_data params variants members = Type.TData(params,variants,members,A.dummy_loc)

module Variance = TS.Variance
let constant = Variance.Constant
let invariant = Variance.Invariant
let covariant = Variance.Covariant
let contravariant = Variance.Contravariant

module TypeMember = TS.TypeMember
let t_member name value = TypeMember.TMember(name,value,A.dummy_loc)

module ModuleType = TS.ModuleType
let t_module name provides types aliases = ModuleType.TModule(name,provides,types,aliases)

module TypeVariant = TS.TypeVariant
let t_variant name fields with_members = TypeVariant.TVariant(name,fields,with_members,A.dummy_loc)
let t_singleton_variant name with_members = TypeVariant.TSingletonVariant(name,with_members,A.dummy_loc)

let s_atom s i = A.SAtom(s,i)

let t_number_binop = t_arrow [t_number;t_number] t_number

let t_list = t_name (Some("pyret-builtin://lists")) (A.SGlobal("List"))
let mk_list (a : Type.t) : Type.t =
  t_app t_list [a]

let t_big_array = t_name (Some("pyret-builtin://arrays")) (A.SGlobal("Array"))
let mk_array (typ : Type.t) : Type.t =
  t_app t_big_array [typ]

let t_set = t_name (Some("pyret-builtin://sets")) (A.SGlobal("Set"))
let mk_set (typ : Type.t) : Type.t =
  t_app t_set [typ]

let t_torepr = t_arrow [] t_string
let t_tostring = t_arrow [] t_string

let make_default_aliases () =
  let typify (name, typ) = (A.name_key (A.STypeGlobal(name)), typ) in
  MSD.of_list (List.map typify [
      "Number", t_number;
      "String", t_string;
      "Boolean", t_boolean
    ])

let make_default_typs () =
  let default_typs = MSD.create 20 in
  MSD.add default_typs (A.name_key (A.SGlobal("builtins")))
    (t_record [
        t_member "has-field" (t_arrow [t_record []] t_boolean);
        t_member "current-checker" (t_arrow [] (t_record [
            t_member "run-checks" t_bot;
            t_member "check-is" t_bot;
            t_member "check-is-refinement" t_bot;
            t_member "check-is-not" t_bot;
            t_member "check-is-not-refinement" t_bot;
            t_member "check-is-refinement" t_bot;
            t_member "check-is-not-refinement" t_bot;
            t_member "check-satisfies" t_bot;
            t_member "check-satisfies-not" t_bot;
            t_member "check-raises-str" t_bot;
            t_member "check-raises-not" t_bot;
            t_member "check-raises-other-str" t_bot;
            t_member "check-raises-satisfies" t_bot;
            t_member "check-raises-violates" t_bot
          ]))]);
  (* Need to be fixed to correct type *)
  let add_top str = MSD.add default_typs (A.name_key (A.SGlobal(str))) t_top in
  let add_typ (name,value) = MSD.add default_typs name value in
  let add_glob (name,value) = add_typ ((A.name_key (A.SGlobal(name))),value) in
  List.iter add_top [
    "raw-array-get";
    "raw-array-set";
    "raw-array-of";
    "raw-array-length";
    "raw-array-to-list";
    "raw-array-fold";
    "raw-array";
    "ref-get";
    "ref-set";
    "ref-freeze";
    "equal-always3";
    "equal-now3";
    "identical3";
    "exn-unwrap";
    "test-print";
    "print-error";
    "display-error";
    "brander";
    "run-task";
    "string-split";
    "string-split-all";
    "string-explode";
    "string-index-of";
    "string-to-code-points";
    "string-from-code-points"
  ];
  List.iter add_glob [
    "_empty", (let tva = A.SAtom("A",37) in
               t_forall [t_var tva] (mk_list (t_var tva)));
    "_link", (let tva = A.SAtom("A",37) in
              let tv = t_var tva in
              t_forall [tv; mk_list tv] (mk_list tv));
    "nothing", t_name None (A.STypeGlobal("Nothing"));
    "torepr", t_arrow [t_top] t_string;
    "not", t_arrow [t_boolean] t_boolean;
    "raise", t_arrow [t_top] t_bot;
    "equal-always", t_arrow [t_top; t_top] t_boolean;
    "equal-now", t_arrow [t_top; t_top] t_boolean;
    "identical", t_arrow [t_top; t_top] t_boolean;
    "tostring", t_arrow [t_top] t_string;
    "_times", t_number_binop;
    "_minus", t_number_binop;
    "_divide", t_number_binop;
    "_plus", t_number_binop;
    "is-nothing", t_arrow [t_top] t_boolean;
    "is-number", t_arrow [t_top] t_boolean;
    "is-string", t_arrow [t_top] t_boolean;
    "is-boolean", t_arrow [t_top] t_boolean;
    "is-object", t_arrow [t_top] t_boolean;
    "is-function", t_arrow [t_top] t_boolean;
    "is-raw-array", t_arrow [t_top] t_boolean;
  ];
  List.iter add_typ [
    "isBoolean", t_arrow [t_top] t_boolean;
    "checkWrapBoolean", t_arrow [t_boolean] t_boolean;
    "throwNonBooleanCondition", t_arrow [t_srcloc; t_string; t_top] t_bot;
    "throwNoBranchesMatched", t_arrow [t_srcloc; t_string] t_bot;
    "hasField", t_arrow [t_record []; t_string] t_boolean;
    "makeSrcloc", t_arrow [t_srcloc] t_bot;
  ];
  (* Number Functions *)
  let t_num_to_num = t_arrow [t_number] t_number in
  let nn s = (s, t_num_to_num) in
  List.iter add_glob [
    "num-max", t_number_binop;
    "num-min", t_number_binop;
    "num-equal", t_number_binop;
    "num-within", t_arrow [t_number] (t_arrow [t_number; t_number] t_boolean);
    "num-within-abs", t_arrow [t_number] (t_arrow [t_number; t_number] t_boolean);
    "num-within-rel", t_arrow [t_number] (t_arrow [t_number; t_number] t_boolean);
    (nn "num-abs");
    (nn "num-sin");
    (nn "num-cos");
    (nn "num-tan");
    (nn "num-asin");
    (nn "num-acos");
    (nn "num-atan");
    (nn "num-modulo");
    (nn "num-truncate");
    (nn "num-sqrt");
    (nn "num-sqr");
    (nn "num-ceiling");
    (nn "num-floor");
    (nn "num-round");
    (nn "num-round-even");
    (nn "num-log");
    (nn "num-exp");
    (nn "num-exact");
    (nn "num-to-rational");
    (nn "num-to-roughnum");
    (nn "num-to-fixnum");
    (nn "num-is-integer");
    (nn "num-is-rational");
    (nn "num-is-roughnum");
    (nn "num-is-positive");
    (nn "num-is-negative");
    (nn "num-is-non-positive");
    (nn "num-is-non-negative");
    (nn "num-is-fixnum");
    "num-expt", t_number_binop;
    "num-tostring", t_arrow [t_number] t_string;
    "num-to-string", t_arrow [t_number] t_string;
    "num-to-string-digits", t_arrow [t_number; t_number] t_string;
    "within", t_arrow [t_number] (t_arrow [t_top; t_top] t_boolean);
    "within-abs", t_arrow [t_number] (t_arrow [t_top; t_top] t_boolean);
    "within-rel", t_arrow [t_number] (t_arrow [t_top; t_top] t_boolean);
    "within-now", t_arrow [t_number] (t_arrow [t_top; t_top] t_boolean);
    "within-abs-now", t_arrow [t_number] (t_arrow [t_top; t_top] t_boolean);
    "within-rel-now", t_arrow [t_number] (t_arrow [t_top; t_top] t_boolean);
    (nn "random");
    (nn "num-random");
    (nn "num-random-seed");
  ];
  (* Time Functions *)
  List.iter add_glob [
    "time-now", t_arrow [] t_number
  ];
  (* String Functions *)
  List.iter add_glob [
    "string-gensym", t_arrow [] t_number;
    "string-repeat", t_arrow [t_string; t_number] t_string;
    "string-substring", t_arrow [t_string; t_number; t_number] t_string;
    "string-toupper", t_arrow [t_string] t_string;
    "string-tolower", t_arrow [t_string] t_string;
    "string-append", t_arrow [t_string; t_string] t_string;
    "string-equal", t_arrow [t_string; t_string] t_boolean;
    "string-contains", t_arrow [t_string; t_string] t_boolean;
    "string-to-number", t_arrow [t_string] t_number;
    "string-tonumber", t_arrow [t_string] t_number;
    "string-length", t_arrow [t_string] t_number;
    "string-replace", t_arrow [t_string; t_string; t_string] t_string;
    "string-char-at", t_arrow [t_string; t_number] t_string;
    "string-to-code-point", t_arrow [t_string] t_number;
    "string-from-code-point", t_arrow [t_number] t_string;
  ];
  (* Potpourri *)
  let print_variable = A.SAtom(G.make_name "A", 1) in
  let t_print = t_forall [t_var print_variable]
      (t_arrow [t_var print_variable] (t_var print_variable)) in
  List.iter add_glob [
    "_lessthan", t_number_binop;
    "_lessequal", t_number_binop;
    "_greaterthan", t_number_binop;
    "_greaterequal", t_number_binop;
    "print", t_print;
    "display", t_print
  ];
  default_typs

let make_default_data_exprs () =
  let default_data_exprs = MSD.create 10 in
  MSD.add default_data_exprs
    (A.name_key (A.STypeGlobal("RawArray")))
    (t_data [t_var(A.SAtom("A",10))] [] []);
  let add_with_empty_data s = MSD.add default_data_exprs
      (A.name_key (A.STypeGlobal(s)))
      (t_data [] [] []) in
  List.iter add_with_empty_data [
    "Number"; "String"; "Boolean"; "Nothing"
  ];
  default_data_exprs

let eq_EqualityResult = t_name (Some("pyret-builtin://equality")) (A.SGlobal("EqualityResult"))

let module_const_equality = ModuleType.TModule(
    "pyret-builtin://equality",
    t_record [
      t_member "EqualityResult" (t_arrow [t_top] t_boolean);
      t_member "is-EqualityResult" (t_arrow [t_top] t_boolean);
      t_member "Equal" eq_EqualityResult
    ],
    SD.of_list [
      "EqualityResult", t_data [] [
        t_singleton_variant "Equal" [];
        t_variant "NotEqual" [t_member "reason" t_string] [];
        t_singleton_variant "Unknown" []
      ] []
    ],
    SD.empty
  )

let gen_mk_tva s n = A.SAtom(s,n)
let mk_tva = gen_mk_tva "A"
let with_tva n f = let tva = mk_tva n in t_forall [t_var tva] (f tva)
let gen_mk_tv s n = t_var (gen_mk_tva s n)
let mk_tv n = t_var (mk_tva n)
let with_tv n f = let tv = mk_tv n in t_forall [tv] (f tv)
let gen_with_tvs (s1,n1) (s2,n2) f = let tv1 = gen_mk_tv s1 n1 and tv2 = gen_mk_tv s2 n2 in
  t_forall [tv1; tv2] (f tv1 tv2)
let with_tvs n1 n2 = gen_with_tvs ("A", n1) ("B", n2)

let module_const_arrays = ModuleType.TModule(
    "pyret-builtin://arrays",
    t_record [
      t_member "array" t_top;
      t_member "build-array" (with_tv 2 (fun tv -> t_arrow [t_arrow [t_number] tv; t_number]
                                            (mk_array tv)));
      t_member "array-from-list" (with_tv 3 (fun tv -> t_arrow [mk_list tv] (mk_array tv)));
      t_member "is-array" (with_tv 4 (fun tv -> t_arrow [t_top] t_boolean));
      t_member "array-of" (with_tv 5 (fun tv -> t_arrow [tv; t_number] (mk_array tv)));
      t_member "array-set-now" (with_tv 6 (fun tv -> t_arrow [mk_array tv; t_number; tv] t_nothing));
      t_member "array-get-now" (with_tv 7 (fun tv -> t_arrow [mk_array tv; t_number] tv));
      t_member "array-length" (with_tv 8 (fun tv -> t_arrow [mk_array tv] t_number));
      t_member "array-to-list-now" (with_tv 9 (fun tv -> t_arrow [mk_array tv] (mk_list tv)))
    ],
    (SD.of_list [
        "Array", (with_tv 10 (fun tv ->
            t_data [tv] []
              [
                t_member "get-now" (t_arrow [t_number] tv);
                t_member "set-now" (t_arrow [t_number; tv] t_nothing);
                t_member "to-list-now" (t_arrow [] (mk_list tv));
                t_member "length" (t_arrow [] t_number);
                t_member "_torepr" t_torepr;
                t_member "_tostring" t_tostring
              ]))
      ]),
    (SD.of_list ["Array", t_name None (A.SName(A.dummy_loc, "Array"))])
  )

let gen_constructor mk tva =
  let tv = t_var tva in
  t_record [
    t_member "make" (t_forall [tv] (t_arrow [t_array tv] (mk tv)));
    t_member "make0" (t_forall [tv] (t_arrow [] (mk tv)));
    t_member "make1" (t_forall [tv] (t_arrow [tv] (mk tv)));
    t_member "make2" (t_forall [tv] (t_arrow [tv; tv] (mk tv)));
    t_member "make3" (t_forall [tv] (t_arrow [tv; tv; tv] (mk tv)));
    t_member "make4" (t_forall [tv] (t_arrow [tv; tv; tv; tv] (mk tv)));
    t_member "make5" (t_forall [tv] (t_arrow [tv; tv; tv; tv; tv] (mk tv)));
  ]

let set_constructor = gen_constructor mk_set
let list_constructor = gen_constructor mk_list

let mk_empty_set tva =
  let tv = t_var tva in
  t_forall [tv] (mk_set tv)

let mk_list_to_set tva =
  let tv = t_var tva in
  t_forall [tv] (t_arrow [mk_list tv] (mk_set tv))

let module_const_sets = ModuleType.TModule(
    "pyret-builtin://sets",
    t_record [
      t_member "set" (set_constructor (mk_tva 11));
      t_member "list-set" (set_constructor (mk_tva 12));
      t_member "tree-set" (set_constructor (mk_tva 13));
      t_member "empty-set" (mk_empty_set (mk_tva 14));
      t_member "empty-list-set" (mk_empty_set (mk_tva 15));
      t_member "empty-tree-set" (mk_empty_set (mk_tva 16));
      t_member "list-to-set" (mk_list_to_set (mk_tva 17));
      t_member "list-to-list-set" (mk_list_to_set (mk_tva 18));
      t_member "list-to-tree-set" (mk_list_to_set (mk_tva 19));
    ],
    (let tv = mk_tv 20 in
     let tv_set = mk_set tv in
     let tv_to_tv = t_arrow [tv_set] tv_set in
     SD.of_list [
       "Set", t_data [tv] []
         [
           t_member "length" (t_arrow [] t_number);
           t_member "pick" (t_arrow []
                              (t_app (t_name (Some("pyret-builtin://pick")) (A.SGlobal("Pick")))
                                 [tv; mk_list tv]));
           t_member "_torepr" t_torepr;
           t_member "fold" (let otv = t_var (A.SAtom("B",21)) in
                            t_arrow [t_arrow [tv] otv; otv] otv);
           t_member "member" (t_arrow [tv] t_boolean);
           t_member "add" (t_arrow [tv] tv_set);
           t_member "remove" (t_arrow [tv] tv_set);
           t_member "to-list" (t_arrow [] (mk_list tv));
           t_member "union" tv_to_tv;
           t_member "intersect" tv_to_tv;
           t_member "difference" tv_to_tv;
           t_member "size" (t_arrow [] t_number);
         ]
     ]),
    (SD.of_list ["Set", t_name None (A.SName(A.dummy_loc, "Set"))])
  )

let mk_pred tv = t_arrow [tv] t_boolean

let module_const_lists = ModuleType.TModule(
    "pyret-builtin://lists",
    t_record [
      t_member "List" (t_arrow [t_top] t_boolean);
      t_member "is-List" (t_arrow [t_top] t_boolean);
      t_member "empty" (with_tv 37 mk_list);
      t_member "is-empty" (t_arrow [t_top] t_boolean);
      t_member "link" (with_tv 37 (fun tv -> t_arrow [tv; mk_list tv] (mk_list tv)));
      t_member "is-link" (t_arrow [t_top] t_boolean);
      t_member "range" (t_arrow [t_number; t_number] (mk_list t_number));
      t_member "range-by" (t_arrow [t_number; t_number; t_number] (mk_list t_number));
      t_member "repeat" (with_tv 157 (fun tv -> t_arrow [t_number; tv] (mk_list tv)));
      t_member "filter" (with_tv 160 (fun tv -> t_arrow [mk_pred tv; mk_list tv] (mk_list tv)));
      t_member "partition" (with_tv 165 (fun tv -> t_arrow [mk_pred tv; mk_list tv]
                                            (t_record [t_member "is-true" (mk_list tv);
                                                       t_member "is-false" (mk_list tv)])));
      t_member "find" (with_tv 174 (fun tv -> t_arrow [mk_pred tv; mk_list tv]
                                       (t_app (t_name (Some "pyret-builtin://option")
                                                 (A.SGlobal "Option")) [tv])));
      t_member "split-at" (with_tv 179 (fun tv ->  t_arrow [t_number; mk_list tv]
                                           (t_record [t_member "prefix" (mk_list tv);
                                                      t_member "suffix" (mk_list tv)])));
      t_member "any" (with_tv 189 (fun tv -> t_arrow [mk_pred tv; mk_list tv] t_boolean));
      t_member "all" (with_tv 192 (fun tv -> t_arrow [mk_pred tv; mk_list tv] t_boolean));
      t_member "all2" (with_tvs 199 200 (fun tva tvb -> t_arrow [t_arrow [tva; tvb] t_boolean;
                                                                 mk_list tva; mk_list tvb]
                                            t_boolean));
      t_member "map" (with_tvs 211 212 (fun tva tvb -> t_arrow [t_arrow [tva] tvb; mk_list tva]
                                           (mk_list tvb)));
      t_member "map2" t_top;
      t_member "map3" t_top;
      t_member "map4" t_top;
      t_member "map_n" t_top;
      t_member "map2_n" t_top;
      t_member "map3_n" t_top;
      t_member "map4_n" t_top;
      t_member "each" (with_tv 217 (fun tv -> t_arrow [t_arrow [tv] t_top; mk_list tv]
                                       (t_name None (A.STypeGlobal("Nothing")))));
      t_member "each2" t_top;
      t_member "each3" t_top;
      t_member "each4" t_top;
      t_member "each_n" t_top;
      t_member "each2_n" t_top;
      t_member "each3_n" t_top;
      t_member "each4_n" t_top;
      t_member "fold" (with_tvs 224 225 (fun tva tvb -> t_arrow [t_arrow [tvb; tva] tvb; mk_list tva]
                                            tvb));
      t_member "fold2" t_top;
      t_member "fold3" t_top;
      t_member "fold4" t_top;
      t_member "fold_n" t_top;
      t_member "list" (list_constructor (mk_tva 160));
    ],
    (SD.of_list [
        "List", (with_tv 37 (fun tv ->
            let lotv = mk_list tv in
            t_data [tv] [
              t_singleton_variant "empty" [];
              t_variant "link" [t_member "first" tv; t_member "rest" lotv] []
            ] [
              t_member "join-str" (t_arrow [t_string] t_string);
              t_member "sort" (t_arrow [] lotv);
              t_member "sort-by" (t_arrow [t_arrow [tv;tv] t_boolean; t_arrow [tv;tv] t_boolean] lotv);
              t_member "_tostring" t_tostring;
              t_member "reverse" (t_arrow [] lotv);
              t_member "last" (t_arrow [] tv);
              t_member "append" (t_arrow [lotv] lotv);
              t_member "foldl" (let tvb = t_var (A.SAtom("B", 200)) in
                                t_forall [tvb] (t_arrow [t_arrow [tv; tvb] tvb] tvb));
              t_member "foldr" (let tvb = t_var (A.SAtom("B", 201)) in
                                t_forall [tvb] (t_arrow [t_arrow [tv; tvb] tvb] tvb));
              t_member "member" (t_arrow [tv] t_boolean);
              t_member "filter" t_top;
              t_member "map" (let tvb = t_var (A.SAtom("B",202)) in
                              t_forall [tvb] (t_arrow [t_arrow [tv] tvb] (mk_list tvb)));
              t_member "each" (t_arrow [t_arrow [tv] t_nothing] t_nothing);
              t_member "length" (t_arrow [] t_number);
              t_member "_torepr" t_torepr;
              t_member "_match" t_top;
              t_member "_plus" (t_arrow [lotv] lotv);
              t_member "push" (t_arrow [] lotv);
              t_member "split-at" (t_arrow [tv] (t_record [
                  t_member "prefix" lotv;
                  t_member "suffix" lotv;
                ]));
              t_member "take" (t_arrow [t_number] lotv);
              t_member "drop" (t_arrow [t_number] lotv);
              t_member "get" (t_arrow [t_number] tv);
              t_member "set" (t_arrow [t_number; tv] lotv);
            ]))
      ]),
    (SD.of_list ["List", t_name (Some "pyret-builtin://lists") (A.SName(A.dummy_loc, "List"))])
  )

let t_option param =
  t_app (t_name (Some "pyret-builtin://option") (A.SGlobal("Option")))
    [t_var param]

let t_and_then from_param to_param =
  t_forall
    [t_var to_param]
    (t_arrow [t_arrow [t_var from_param] (t_option to_param)] (t_option to_param))

let module_const_option = ModuleType.TModule(
    "pyret-builtin://option",
    t_record [
      t_member "Option" (t_arrow [t_top] t_boolean);
      t_member "is-Option" (t_arrow [t_top] t_boolean);
      t_member "none" (with_tva 10 t_option);
      t_member "is-none" (t_arrow [t_top] t_boolean);
      t_member "some" (let tv = (t_var (A.SAtom("A90", 10))) in
                       t_forall [tv] (t_arrow [tv] (t_option (A.SAtom("A90",10)))));
      t_member "is-some" (t_arrow [t_top] t_boolean)
    ],
    (SD.of_list [
        "Option", (with_tva 10 (fun tva ->
            let tv = t_var tva in
            t_data
              [tv]
              [
                t_singleton_variant "none" [
                  t_member "_match" t_top;
                  t_member "_torepr" t_torepr;
                  t_member "or-else" (t_arrow [tv] tv);
                  t_member "and-then" (t_and_then tva (A.SAtom("B",10)));
                ];
                t_variant "some"
                  [t_member "value" tv]
                  [
                    t_member "_match" t_top;
                    t_member "_torepr" t_torepr;
                    t_member "or-else" (t_arrow [tv] tv);
                    t_member "and-then" (t_and_then tva (A.SAtom("B",11)));
                  ]
              ]
              [
                t_member "and-then" (t_and_then tva (A.SAtom("B",12)));
                t_member "or-else" (t_arrow [tv] tv);
                t_member "_torepr" t_torepr;
                t_member "_match" t_top;
              ]
          ))
      ]),
    (SD.of_list ["Option", t_name (Some "pyret-builtin://option") (A.SName(A.dummy_loc, "Option"))])
  )

let mk_rte args = t_arrow args (t_name (Some "pyret-builtin://error") (A.SGlobal("RuntimeError")))
let mk_pe args = t_arrow args (t_name (Some "pyret-builtin://error") (A.SGlobal("ParseError")))
let t_type_pred = mk_pred t_top

let module_const_error = ModuleType.TModule(
    "pyret-builtin://error",
    t_record [
      t_member "RuntimeError" t_type_pred;
      t_member "is-RuntimeError" t_type_pred;
      t_member "message-exception" (mk_rte [t_string]);
      t_member "is-message-exception" t_type_pred;
      t_member "no-branches-matched" (mk_rte [t_top; t_string]);
      t_member "is-no-branches-matched" t_type_pred;
      t_member "internal-error" (mk_rte [t_top; t_top]);
      t_member "is-internal-error" t_type_pred;
      t_member "field-not-found" (mk_rte [t_top; t_top; t_string]);
      t_member "is-field-not-found" t_type_pred;
      t_member "lookup-non-object" (mk_rte [t_top; t_top; t_string]);
      t_member "is-lookup-non-object" t_type_pred;
      t_member "extend-non-object" (mk_rte [t_top; t_top]);
      t_member "is-extend-non-object" t_type_pred;
      t_member "non-boolean-condition" (mk_rte [t_top; t_top; t_top]);
      t_member "is-non-boolean-condition" t_type_pred;
      t_member "non-boolean-op" (mk_rte [t_top; t_top; t_top; t_top]);
      t_member "is-non-boolean-op" t_type_pred;
      t_member "generic-type-mismatch" (mk_rte [t_top; t_string]);
      t_member "is-generic-type-mismatch" t_type_pred;
      t_member "outside-numeric-range" (mk_rte [t_top; t_top; t_top]);
      t_member "is-outside-numeric-range" t_type_pred;
      t_member "plus-error" (mk_rte [t_top; t_top]);
      t_member "is-plus-error" t_type_pred;
      t_member "numeric-binop-error" (mk_rte [t_top; t_top; t_top; t_top]);
      t_member "is-numeric-binop-error" t_type_pred;
      t_member "cases-arity-mismatch" (mk_rte [t_top; t_top; t_top]);
      t_member "is-cases-arity-mismatch" t_type_pred;
      t_member "cases-singleton-mismatch" (mk_rte [t_top; t_boolean]);
      t_member "is-cases-singleton-mismatch" t_type_pred;
      t_member "arity-mismatch" (mk_rte [t_top; t_top; t_top]);
      t_member "is-arity-mismatch" t_type_pred;
      t_member "non-function-app" (mk_rte [t_top; t_top]);
      t_member "is-non-function-app" t_type_pred;
      t_member "bad-app" (mk_rte [t_top; t_string; t_string; t_number; t_top]);
      t_member "is-bad-app" t_type_pred;
      t_member "uninitialized-id" (mk_rte [t_top; t_string]);
      t_member "is-uninitialized-id" t_type_pred;
      t_member "module-load-failure" (mk_rte [t_top]);
      t_member "is-module-load-failure" t_type_pred;
      t_member "invalid-array-index" (mk_rte [t_string; t_top; t_number; t_string]);
      t_member "is-invalid-array-index" t_type_pred;
      t_member "user-break" (t_name (Some "pyret-builtin://error") (A.SGlobal("RuntimeError")));
      t_member "is-user-break" t_type_pred;
      t_member "ParseError" t_type_pred;
      t_member "is-ParseError" t_type_pred;
      t_member "parse-error-next-token" (mk_pe [t_top; t_string]);
      t_member "is-parse-error-next-token" t_type_pred;
      t_member "parse-error-eof" (mk_pe [t_top]);
      t_member "is-parse-error-eof" t_type_pred;
      t_member "parse-error-unterminated-string" (mk_pe [t_top]);
      t_member "is-parse-error-unterminated-string" t_type_pred;
      t_member "empty-block" (mk_pe [t_top]);
      t_member "is-empty-block" t_type_pred;
      t_member "bad-block-stmt" (mk_pe [t_top]);
      t_member "is-bad-block-stmt" t_type_pred;
      t_member "bad-check-block-stmt" (mk_pe [t_top]);
      t_member "is-bad-check-block-stmt" t_type_pred;
      t_member "fun-missing-colon" (mk_pe [t_top]);
      t_member "is-fun-missing-colon" t_type_pred;
      t_member "fun-missing-end" (mk_pe [t_top]);
      t_member "is-fun-missing-end" t_type_pred;
      t_member "args-missing-comma" (mk_pe [t_top]);
      t_member "is-args-missing-comma" t_type_pred;
      t_member "app-args-missing-comma" (mk_pe [t_top]);
      t_member "is-app-args-missing-comma" t_type_pred;
      t_member "missing-end" (mk_pe [t_top]);
      t_member "is-missing-end" t_type_pred;
      t_member "missing-comma" (mk_pe [t_top]);
      t_member "is-missing-comma" t_type_pred;
    ],
    (let mk_variant name fields =
       let member_of_field (name, typ) = t_member name typ in
       t_variant name (List.map member_of_field fields) [] in
     SD.of_list [
       "RuntimeError", t_data
         []
         [
           mk_variant "message-exception" ["message", t_string];
           mk_variant "no-branches-matched" ["loc", t_top; "expression", t_string];
           mk_variant "internal-error" ["message", t_top; "info-args", t_top];
           mk_variant "field-not-found" ["loc", t_top; "obj", t_top; "field", t_string];
           mk_variant "lookup-non-object" ["loc", t_top; "non-obj", t_top; "field", t_string];
           mk_variant "extend-non-object" ["loc", t_top; "non-obj", t_top];
           mk_variant "non-boolean-condition" ["loc", t_top; "typ", t_top; "value", t_top];
           mk_variant "non-boolean-op" ["loc", t_top; "position", t_top; "typ", t_top; "value", t_top];
           mk_variant "generic-type-mismatch" ["val", t_top; "typ", t_string];
           mk_variant "outside-numeric-range" ["val", t_top; "low", t_top; "high", t_top];
           mk_variant "plus-error" ["val1", t_top; "val2", t_top];
           mk_variant "numeric-binop-error" ["val1", t_top; "val2", t_top;
                                             "opname", t_top; "methodname", t_top];
           mk_variant "cases-arity-mismatch" ["branch-loc", t_top; "num-args", t_top;
                                              "actual-arity", t_top];
           mk_variant "cases-singleton-mismatch" ["branch-loc", t_top;
                                                  "should-be-singleton", t_boolean];
           mk_variant "arity-mismatch" ["fun-loc", t_top; "expected-arity", t_top; "args", t_top];
           mk_variant "non-function-app" ["loc", t_top; "non-fun-val", t_top];
           mk_variant "bad-app" ["loc", t_top; "fun-name", t_string; "message", t_string;
                                 "arg-position", t_number; "arg-val", t_top];
           mk_variant "uninitialized-id" ["loc", t_top; "name", t_string];
           mk_variant "module-load-failure" ["names", t_top];
           mk_variant "invalid-array-index" ["method-name", t_string; "array", t_top;
                                             "index", t_number; "reason", t_string];
           mk_variant "user-break" [];
         ]
         [
           t_member "_torepr" t_torepr;
           t_member "_tostring" t_tostring;
           t_member "_match" t_top;
         ];
       "ParseError", t_data
         []
         [
           mk_variant "parse-error-next-token" ["loc", t_top; "next-token", t_string];
           mk_variant "parse-error-eof" ["loc", t_top];
           mk_variant "parse-error-unterminated-string" ["loc", t_top];
           mk_variant "empty-block" ["loc", t_top];
           mk_variant "bad-block-stmt" ["loc", t_top];
           mk_variant "bad-check-block-stmt" ["loc", t_top];
           mk_variant "fun-missing-colon" ["loc", t_top];
           mk_variant "fun-missing-end" ["loc", t_top];
           mk_variant "args-missing-comma" ["loc", t_top];
           mk_variant "app-args-missing-comma" ["loc", t_top];
           mk_variant "missing-end" ["loc", t_top];
           mk_variant "missing-comma" ["loc", t_top];
         ]
         [
           t_member "loc" t_top;
           t_member "_tostring" t_tostring;
           t_member "_torepr" t_torepr;
           t_member "_match" t_top;
         ]
     ]),
    (SD.of_list ["Error", t_name None (A.SName(A.dummy_loc, "Error"))])
  )

let t_either a b = t_app (t_name (Some "pyret-builtin://either") (A.SGlobal("Either"))) [a;b]
let module_const_either = ModuleType.TModule(
    "pyret-builtin://either",
    t_record [
      t_member "Either" t_type_pred;
      t_member "is-Either" t_type_pred;
      t_member "left" (gen_with_tvs ("a792", 10) ("b793", 11) (fun tva tvb ->
          t_arrow [tva] (t_either tva tvb)));
      t_member "is-left" t_type_pred;
      t_member "right" (gen_with_tvs ("a794", 10) ("b795", 11) (fun tva tvb ->
          t_arrow [tvb] (t_either tva tvb)));
      t_member "is-right" t_type_pred;
    ],
    (SD.of_list [
        "Either", (gen_with_tvs ("a", 10) ("b", 11) (fun tva tvb ->
            t_data
              [tva;tvb]
              [
                t_variant "left"
                  [t_member "v" tva]
                  [t_member "_match" t_top;
                   t_member "_torepr" t_torepr];
                t_variant "right"
                  [t_member "v" tvb]
                  [t_member "_match" t_top;
                   t_member "_torepr" t_torepr];
              ]
              [
                t_member "v" t_top;
                t_member "_torepr" t_torepr;
                t_member "_match" t_top;
              ]
          ))
      ]),
    (SD.of_list ["Either", t_name (Some "pyret-builtin://either") (A.SName(A.dummy_loc, "Either"))])
  )

let t_s_exp = t_name (Some "pyret-builtin://s-exp-structs") (A.SGlobal("S-Exp"))

let s_exp_struct_mems = [
  t_member "s-list" (t_arrow [mk_list t_s_exp] t_s_exp);
  t_member "s-num" (t_arrow [t_number] t_s_exp);
  t_member "s-str" (t_arrow [t_string] t_s_exp);
  t_member "s-sym" (t_arrow [t_string] t_s_exp);
  t_member "is-s-list" t_type_pred;
  t_member "is-s-num" t_type_pred;
  t_member "is-s-str" t_type_pred;
  t_member "is-s-sym" t_type_pred;
]

let module_const_s_exp = ModuleType.TModule(
    "pyret-builtin://s-exp",
    t_record (s_exp_struct_mems @ [
        t_member "read-s-expr" (t_arrow [t_string] t_s_exp)
      ]),
    (SD.empty),
    (SD.of_list ["S-Exp", t_s_exp])
  )

let module_const_s_exp_structs = ModuleType.TModule(
    "pyret-builtin://s-exp",
    t_record s_exp_struct_mems,
    (SD.of_list [
        "S-Exp", t_data
          []
          [
            t_variant "s-list"
              [t_member "exps" (mk_list t_s_exp)]
              [t_member "_match" t_top;
               t_member "_torepr" t_torepr];
            t_variant "s-num"
              [t_member "n" t_number]
              [t_member "_match" t_top;
               t_member "_torepr" t_torepr];
            t_variant "s-str"
              [t_member "s" t_string]
              [t_member "_match" t_top;
               t_member "_torepr" t_torepr];
            t_variant "s-sym"
              [t_member "s" t_string]
              [t_member "_match" t_top;
               t_member "_torepr" t_torepr];
          ]
          [ t_member "_torepr" t_torepr ]
      ]),
    (SD.empty)
  )

let make_default_modules () =
  let default_modules = MSD.create 10 in
  MSD.add_each default_modules [
    "pyret-builtin://equality", module_const_equality;
    "pyret-builtin://lists", module_const_lists;
    "pyret-builtin://option", module_const_option;
    "pyret-builtin://error", module_const_error;
    "pyret-builtin://either", module_const_either;
    "pyret-builtin://arrays", module_const_arrays;
    "pyret-builtin://sets", module_const_sets;
    "pyret-builtin://s-exp", module_const_s_exp;
    "pyret-builtin://s-exp-structs", module_const_s_exp_structs;
  ];
  default_modules
