open Ppxlib
open Ppxlibextras
open Graph

(*
    pregraph
    graph
*)
let create_edge (edge,node)=
  let pg = make_pregraph in
  let g = make_graph pg edge node in
  g

let ppddump x = () (*stub to hide batteries dump*)


let process_apply (a,c) :string = "apply:" ^ a ^ "|" ^ c
let process_coerce a = "FIXME122"
let process_constraint (a,b) = "(*P31*)process_constraint" ^ a ^ "|" ^b
let process_construct (a,b) = "(*P32*)process_construct" ^a^ "|" ^b
let process_field (a,b) = "(*P33*)process_field" ^ a ^ "|" ^b
let process_for a = "FIXME126"
let process_fun (a,b,c,d) = "(*P35*)process_fun:" ^ a ^ "|" ^b ^ "|" ^c ^ "|" ^d
let process_ifthenelse (a,b,c) = "(*P36*)process_ifthenelse" ^ a ^ b ^c
let process_let (a,b,c) = "let" ^ a ^ "|" ^ b ^ "|" ^ c
let process_letexception (a,b) = "(*P37*)process_letexception" ^ a ^ "|" ^ b
let process_letmodule a = "FIXME131"
let process_match (a,b) = "(*P38*)process_match" ^ a ^ "|" ^ b
let process_newtype a = "FIXME1334"
let process_open a = "FIXME1337"
let process_poly a = "FIXME1338"
let process_record (a, b) = "(*P39*)process_record" ^ a ^ "|" ^  b
let process_send a = "FIXME1340"
let process_sequence  (a,b) = "(*P40*)process_sequences" ^ a ^ "|" ^ b
let process_setfield a = "FIXME1352"
let process_setinstvar a = "FIXME1362"
let process_try a = "FIXME1343"
let process_variant a = "FIXME1355"
let process_while  a = "FIXME1378"

let process_types (x):string = "TOP1" ^ x
let process_array (x):string = "ARRAY" ^ x
let process_extension1 (x):string = "EXT" ^ x
let process_function (x):string = "FUNCTION" ^ x
let process_ident (x):string = "IDENT" ^ x
let process_lazy (x):string = "LAZY" ^ x
let process_letop1 (x):string = "LETOP" ^ x
let process_new (x):string = "NEW" ^ x

let process_object (x):string = "OBJECT" ^ x



let process_override (x):string = "OVERRIDE" ^ x
let process_pack (x):string = "PACK" ^ x     let process_tuple (x):string = "TUPLE" ^ x

let process_constant1 (x):string = "CONSTANT" ^ x
let process_assert (x):string = "ASSERT" ^ x
let process_option ( a):string = "((*P1*)process_option " ^ a ^ ")"



let process_direction_flag ( alist0):string =  "FIXME4"
let process_arg_label ( a:arg_label):string=
  "(*P41*)process_arg_label" ^
  match a with
  | Nolabel -> "NoLabel"
  | Labelled a  -> "label:"^a
  | Optional a -> "label_optional:"^a

let rec process_generic_list_tail name a f :string=
 ( match a with
  | [] -> ""
  | a :: t -> (f a) ^ ";" ^ (process_generic_list_tail name t f ) 
 )

let process_generic_list name a f :string=
  "(" ^ name ^ "[" ^
 ( match a with
  | [] -> ""
  | a :: t -> (f a) ^ ";" ^ (process_generic_list_tail name t f ) 
 ) ^ "] )"

let process_class_structure ( aclass_structure0:class_structure):string=  "FIXME7"
let process_constant ( a:constant):string=
  "(*P42*)process_constant" ^
  match a with
  | Pconst_integer (a,b) -> "int" ^a
  | Pconst_char char  -> "char" ^ String.make 1 char
  | Pconst_string (string,location,stringoption) -> "Pconst_string(\"" ^ string ^ "\")"
  | Pconst_float (string , char_option) -> "float" ^ string


let process_extension ( aextension0:extension):string= "FIXME9"
let process_extension_constructor ( aextension_constructor0:extension_constructor):string= "FIXME10"
let process_label ( alabel0:label):string="(*P43*)process_label" ^ alabel0
let process_letop ( aletop0:letop):string="FIXME12"
let rec process_list ( a ):string=
  "(*P44*)process_list" ^
  match a with
  | [] -> ""
  | a :: t -> a ^ "," ^ (process_list t)

let process_loc ( aloc0):string="FIXME14"

let rec process_id1 a : string =
  match a with
  | Lident string -> string
  | Ldot (longident, string) ->
    (process_id1 (longident)) ^ "." ^ string
  | Lapply (longident,longident2)
    -> (process_id1 (longident))  ^ "."
       ^ (process_id1 (longident2) )

let process_longident_loc ( a :longident_loc):string="ident:" ^ (process_id1 a.txt)
let process_module_expr ( amodule_expr0:module_expr):string="FIXME16"
let process_open_declaration ( aopen_declaration0:open_declaration):string="FIXME17"

let process_types (a,b) = "((*P17*)process_types \"" ^ a ^"\",\"" ^ b ^ "\")"

let process_types_rec_flag__Nonrecursive:string = (process_types ("rec_flag","Nonrecursive") )
let process_types_rec_flag__Recursive:string = (process_types ("rec_flag","Recursive") )

let process_rec_flag__Recursive x :string =match x with
| Recursive  -> process_types_rec_flag__Recursive
| Nonrecursive  -> process_types_rec_flag__Nonrecursive

let process_rec_flag( x : rec_flag):string = process_rec_flag__Recursive x

let process_rec_flag_manual ( x:rec_flag):string="(*P50*)process_rec_flag" ^
                                                 match x with
                                                 | Nonrecursive -> "plain"
                                                 | Recursive -> "rec"







let rec
  process_list1 ( a: (arg_label * expression) list):string=
  "(*P51*)process_list1" ^
  match a with
  | [] -> ""
  | (l,e) :: t -> (process_arg_label l) ^ "|" ^ (process_expression e) ^ "|" ^ (process_list1 t)
and process_expression_option ( a: expression option ):string = "((*P18*)process_expression_option" ^
  (match a with
  | Some x -> (process_expression x)
  | None -> "(null_expression)"
  ) ^")"
and process_string_loc_expression_list x = "(*P52*)process_string_loc_expression_list"
and process_expression_list ( a ):string=
  "(*P53*)process_list" ^
  match a with
  | [] -> "process_expression_list"
  | a :: t -> (process_expression a) ^ "|" ^ (process_expression_list t)
and process_value_binding x =
  "(*P54*)process_value_binding:" ^
  process_pattern(x.pvb_pat) ^ "|" ^
  process_expression(x.pvb_expr)
and process_string_list_option x = "process_string_list_option"
and  process_case (a:case) =
  "(case" ^
  (process_pattern a.pc_lhs ^ "|" ^
  process_expression_option a.pc_guard ^ "|" ^
  process_expression a.pc_rhs) ^
  ")"
  and process_core_type_list x = "process_core_type_list"
and process_string a = "process_string"
and process_closed_flag x = "process_closed_flag"
and process_cases ( a:cases):string=
  "((*P19*)process_cases" ^
 ( match a with
  | [] -> ""
  | a :: t -> (process_case a) ^ "|" ^ (process_cases t)
 ) ^ ")"
and process_row_field_list x= "process_row_field_list"
and process_value_binding_list x = "value_binding_list" ^
    match x with
    | [] -> ""
    | a :: t -> (process_value_binding a) ^ "|" ^ (process_value_binding_list t)
and  print_value_binding_expr (x : expression) : string=
  match x with
  | {
    pexp_desc (* : expression_desc *);
    pexp_loc (* : location  *);
    pexp_loc_stack (* : location_stack *);
    pexp_attributes (* : attributes *); (* [... \[@id1\] \[@id2\]] *)
  } ->
    (* (ppddump ("DBG166:desc", pexp_desc ))); *)
    (* (ppddump ("DBG166:desc", pexp_attributes ))); *)
  "print_value_binding_expr:" ^ "|" ^ (process_expression_desc pexp_desc)
and process_expression_desc ( x:expression_desc):string=
  match x with
  | Pexp_apply (expressionA0,listA1) -> (process_apply ((process_expression expressionA0),(process_list1 listA1)))
  | Pexp_array (listA0) -> (process_array ((process_expression_list listA0)))
  | Pexp_assert (expressionA0) -> (process_assert ((process_expression expressionA0)))
  | Pexp_coerce (expressionA0,optionA1,core_typeA2) -> (process_coerce ((process_expression expressionA0),(process_core_type_option optionA1),(process_core_type core_typeA2)))
  | Pexp_constant (constantA0) -> (process_constant1 ((process_constant constantA0)))
  | Pexp_constraint (expressionA0,core_typeA1) -> (process_constraint ((process_expression expressionA0),(process_core_type core_typeA1)))
  | Pexp_construct (longident_locA0,optionA1) -> (process_construct ((process_longident_loc longident_locA0),(process_expression_option optionA1)))
  | Pexp_extension (extensionA0) -> (process_extension1 ((process_extension extensionA0)))
  | Pexp_field (expressionA0,longident_locA1) -> (process_field ((process_expression expressionA0),(process_longident_loc longident_locA1)))
  | Pexp_for (patternA0,expressionA1,expressionA2,direction_flagA3,expressionA4) -> (process_for ((process_pattern patternA0),(process_expression expressionA1),(process_expression expressionA2),(process_direction_flag direction_flagA3),(process_expression expressionA4)))
  | Pexp_fun (arg_labelA0,optionA1,patternA2,expressionA3) -> (process_fun ((process_arg_label arg_labelA0),(process_expression_option optionA1),(process_pattern patternA2),(process_expression expressionA3)))
  | Pexp_function (casesA0) -> (process_function ((process_cases casesA0)))
  | Pexp_ident (longident_locA0) -> (process_ident ((process_longident_loc longident_locA0)))
  | Pexp_ifthenelse (expressionA0,expressionA1,optionA2) -> (process_ifthenelse ((process_expression expressionA0),(process_expression expressionA1),(process_expression_option optionA2)))
  | Pexp_lazy (expressionA0) -> (process_lazy ((process_expression expressionA0)))
  | Pexp_letexception (extension_constructorA0,expressionA1) -> (process_letexception ((process_extension_constructor extension_constructorA0),(process_expression expressionA1)))
  | Pexp_letmodule (locA0,module_exprA1,expressionA2) -> (process_letmodule ((process_loc locA0),(process_module_expr module_exprA1),(process_expression expressionA2)))
  | Pexp_letop (letopA0) -> (process_letop1 ((process_letop letopA0)))
  | Pexp_let (rec_flagA0,listA1,expressionA2) -> (process_let ((process_rec_flag rec_flagA0),(process_value_binding_list listA1),(process_expression expressionA2)))
  | Pexp_match (expressionA0,casesA1) -> (process_match ((process_expression expressionA0),(process_cases casesA1)))
  | Pexp_new (longident_locA0) -> (process_new ((process_longident_loc longident_locA0)))
  | Pexp_newtype (locA0,expressionA1) -> (process_newtype ((process_loc locA0),(process_expression expressionA1)))
  | Pexp_object (class_structureA0) -> (process_object ((process_class_structure class_structureA0)))
  | Pexp_open (open_declarationA0,expressionA1) -> (process_open ((process_open_declaration open_declarationA0),(process_expression expressionA1)))
  | Pexp_override (listA0) -> (process_override ((process_string_loc_expression_list listA0)))
  | Pexp_pack (module_exprA0) -> (process_pack ((process_module_expr module_exprA0)))
  | Pexp_poly (expressionA0,optionA1) -> (process_poly ((process_expression expressionA0),(process_core_type_option optionA1)))
  | Pexp_record (listA0,optionA1) -> (process_record ((process_long_ident_expression_list listA0),(process_expression_option optionA1)))
  | Pexp_send (expressionA0,locA1) -> (process_send ((process_expression expressionA0),(process_loc locA1)))
  | Pexp_sequence (expressionA0,expressionA1) -> (process_sequence ((process_expression expressionA0),(process_expression expressionA1)))
  | Pexp_setfield (expressionA0,longident_locA1,expressionA2) -> (process_setfield ((process_expression expressionA0),(process_longident_loc longident_locA1),(process_expression expressionA2)))
  | Pexp_setinstvar (locA0,expressionA1) -> (process_setinstvar ((process_loc locA0),(process_expression expressionA1)))
  | Pexp_try (expressionA0,casesA1) -> (process_try ((process_expression expressionA0),(process_cases casesA1)))
  | Pexp_tuple (listA0) -> (process_tuple ((process_expression_list listA0)))
  | Pexp_unreachable -> "Unreachable"
  | Pexp_variant (labelA0,optionA1) ->
    (process_variant ((process_label labelA0),
                      (process_expression_option optionA1)))
  | Pexp_while (expressionA0,expressionA1) -> (process_while ((process_expression expressionA0),(process_expression expressionA1)))
and process_pattern_desc x =
    match x with
    |Ppat_any -> "patterna1"
    |Ppat_var (name) -> "pattern_Ppat_var:" ^ name.txt
    |Ppat_alias (pattern ,loc_string) -> "patterna3"
    |Ppat_constant constant  -> "patterna4"
    |Ppat_interval (constant ,constant2)  -> "patterna5"
    |Ppat_tuple pattern_list   -> "patterna6"
    |Ppat_construct (a, b)  -> "patterna7"
    |Ppat_variant (pattern, labeloptional ) -> "patterna8"
    |Ppat_record (list_longident_locpattern, closed_flag) -> "patterna9"
    |Ppat_array (pattern_) -> "patterna10 "
    |Ppat_or (pattern, pattern2)  -> "patterna12"
    |Ppat_constraint (pattern , core_type) -> "Ppat_constraint:" ^ process_pattern pattern ^ "|" ^ process_core_type core_type
    |Ppat_type longident_loc  -> "patterna14"
    |Ppat_lazy pattern  -> "patterna15"
    |Ppat_unpack (loc_option_string) -> "patterna16"
    |Ppat_exception pattern  -> "patterna17"
    |Ppat_extension extension  -> "patterna19"
    |Ppat_open (longident_loc, pattern) -> "patterna30"
and process_long_ident_expression_list x = "(*P56*)process_long_ident_expression_list"
and process_pattern ( apattern0:pattern):string= "PATTERN" ^ (process_pattern_desc apattern0.ppat_desc)
and process_expression ( x:expression):string= "EXPR:" ^process_expression_desc(x.pexp_desc)
and process_record_kind4 :label_declaration -> string_list -> string = fun x s -> ""
and  process_record_kind2(x :label_declaration)(s:string_list) = ""
and    process_record_kind3 x s = ""
and process_core_type ( x ):string = (my_process_core_type x)
and process_string_loc_list x = "process_string_loc_list"
and process_type_decl_core_type x = process_core_type  x
and stringlister2 (x:string_list) : string =
  match x with
  | [] ->"stringlister"
  | h :: t -> h ^ stringlister2(t)
and process_package_type x = "process_package_type"
and process_generic_type a b c = "(process_generic_type \""
                                 ^ a ^ "\" \""
                                 ^ b ^ "\" \""
                                 ^ (stringlister2 c) ^ "\" \""
                                 ^ "\")" 
and process_core_type_option ( a: core_type option ):string = "((*P20*)process_core_type_option " ^
  (match a with
  | Some x -> (process_core_type x)
  | None -> "nope"
  ) ^ ")"
and
  process_record_kind((x,s):label_declaration *string_list):string =
  match x with
    {
     pld_name(* : string loc *);
     pld_mutable(* : mutable_flag *);
     pld_type(* : core_type *);
     pld_loc(* : Location.t *);
     pld_attributes(* : attributes *);
   } ->
    let pct = (my_process_core_type pld_type) in
    (ppddump ("DBG1:precord_kind:",
                                    pld_name,
                                    "mutable",
                                    pld_mutable,
                                    "type",
              pld_type));
    (*
       this is a field in a record
    *)
    "((*P21*)process_record_kind \"" ^ pld_name.txt ^ "\" (body " ^ pct ^ "))"
and process_object_field_list x = "process_object_field_list"
and
  
  gen_process_core_type_desc (x : core_type_desc * string_list):string =
  match x with
    (ctd, s)->
    match ctd with
    (*emit_constructor_arguments:*)| Ptyp_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
                                   (*emit_constructor_arguments:*)| Ptyp_package((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)package_type0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_package" [((*P4*)process_package_type (*emit_core_type_numbered*)package_type0)])
                                   (*emit_constructor_arguments:*)| Ptyp_poly((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_poly" [((*P4*)process_string_loc_list (*emit_core_type_numbered*)list0);((*P4*)process_core_type (*emit_core_type_numbered*)core_type1)])
                                   (*emit_constructor_arguments:*)| Ptyp_variant((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)closed_flag1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option2) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_variant" [((*P4*)process_row_field_list (*emit_core_type_numbered*)list0);((*P4*)process_closed_flag (*emit_core_type_numbered*)closed_flag1);((*P4*)process_string_list_option (*emit_core_type_numbered*)option2)])
                                   (*emit_constructor_arguments:*)| Ptyp_alias((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_alias" [((*P4*)process_core_type (*emit_core_type_numbered*)core_type0);((*P4*)process_string (*emit_core_type_numbered*)string1)])
                                   (*emit_constructor_arguments:*)| Ptyp_class((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_class" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_core_type_list (*emit_core_type_numbered*)list1)])
                                   (*emit_constructor_arguments:*)| Ptyp_object((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)closed_flag1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_object" [((*P4*)process_object_field_list (*emit_core_type_numbered*)list0);((*P4*)process_closed_flag (*emit_core_type_numbered*)closed_flag1)])
                                   (*emit_constructor_arguments:*)| Ptyp_constr((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_constr" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_core_type_list (*emit_core_type_numbered*)list1)])
                                   (*emit_constructor_arguments:*)| Ptyp_tuple((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_tuple" [
        ((*P4*)process_core_type_list (*emit_core_type_numbered*)list0)])
                                   (*emit_constructor_arguments:*)| Ptyp_arrow((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)arg_label0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type2) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_arrow" [((*P4*)process_arg_label (*emit_core_type_numbered*)arg_label0);((*P4*)process_core_type (*emit_core_type_numbered*)core_type1);((*P4*)process_core_type (*emit_core_type_numbered*)core_type2)])
                                   (*emit_constructor_arguments:*)| Ptyp_var((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_var" [((*P4*)process_string (*emit_core_type_numbered*)string0)])
                                   (*emit_constructor_arguments:*)| Ptyp_any -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_any" [])
                                                                                                                                  
and
  my_process_core_type_desc (x : core_type_desc * string_list):string =
  match x with
    (ctd, s)->
    match ctd with
    | Ptyp_constr (a,b) (* of Longident.t loc * core_type list *)
      ->
      let {txt;loc} = a in
      let id1 = process_id1(txt) in

      let edge = create_edge ("Ptyp_constr", id1) in

      (* let concat = (concatlist (id1, astring_list)) in *)
      (* let newy = [id1] @ astring_list in *)
      let newlist = (my_process_core_type_list (b, s)) in
      Printf.printf "DBG1:Ptyp_constr1 '%s' %s" id1 newlist;
      (* "id" ^ a ^ " id2 " ^ myid  *)
      (ppddump (
         "DBG1:Ptyp_constr:",
         "id",a,
         "types",b,
         "context",s,
         "id1", id1
       ));
      "(Ptyp_constr:\"" ^ id1 ^ "|" ^ "\") ->" ^ newlist
    | Ptyp_tuple a (* of core_type list *)
      ->
      (ppddump ("DBG1:Ptyp_tuple:", a ));
      "Ptyp_tuple" ^ my_process_core_type_list(a,  s )
    (*not in test*)
    | Ptyp_any  -> (ppddump ("DBG1:Ptyp_any:")); "any"
    | Ptyp_var name ->(ppddump ("DBG1:Ptyp_var:"  , name)); "var-name"
  | Ptyp_arrow (arg_label , core_type , core_type2) ->
    (* my_process_core_type((core_type, string_list)); *)
    (* my_process_core_type(core_type2, string_list); *)
    (ppddump ("DBG1:Ptyp_arrow10:" )); "arrow"
  | Ptyp_object (a,b)(* of object_field list * closed_flag *)
    ->
    (ppddump ("DBG1:Ptyp_arrow8:" )); "obj"
  | Ptyp_class (a,b) (* of Longident.t loc * core_type list *)
    ->
    let myid = (process_id1 a.txt) in
    (* my_process_core_type_list(b, y :: myid); *)
    (ppddump ("DBG1:Ptyp_arrow7:" )); "class"
  | Ptyp_alias (a,b) (* of core_type * string loc  *)
    ->
    (* my_process_core_type(a, y); *)
    (ppddump ("DBG1:Ptyp_arrow6:" )); "alias"
  | Ptyp_variant (a,b,c) (* of row_field list * closed_flag * label list option *)
    ->
    (ppddump ("DBG1:Ptyp_arrow5:" ));"(Ptyp_variant)"
  | Ptyp_poly (a,b) (* of string loc list * core_type *)
    ->
    (* my_process_core_type(b, y); *)
    (ppddump ("DBG1:Ptyp_arrow4:" )); "poly"
  | Ptyp_package a(* of package_type  *)
    ->
    (ppddump ("DBG1:Ptyp_arrow3:",a )) ; "typ_package"
  (* | Ptyp_open (a,b) (\* of Longident.t loc * core_type *\)-> *)
  (*   (ppddump ("DBG1:Ptyp_arrow2",a,b )) *)
  | Ptyp_extension a (* of extension   *)    ->
    (ppddump ("DBG1:Ptyp_extension:",a )); "extension"
and
  process_record_kind_list(p,x,s) : string =
  match x with
  | [] -> ""
  | h :: t ->
    (process_record_kind (h ,  s)) ^ "/" ^ (process_record_kind_list (p, t, s))
and
  my_process_core_type(x: core_type ):string=
  match x with
    {
      ptyp_desc(* : core_type_desc *);
      ptyp_loc(* : Location.t *);
      ptyp_loc_stack(* : location_stack *);
      ptyp_attributes(* : attributes; *)
    }->
    let td = (my_process_core_type_desc (ptyp_desc, [])) in
    "ptyp_desc:" ^ td
and my_process_core_type_list(x: core_type_list * string_list):string =
  match x with
  | (a,b) ->
    match a with
    | [] -> "my_process_core_type_list:"
    | h :: t ->
      my_process_core_type  h ^ "," ^ my_process_core_type_list(t,b)


let print_value_binding_list2 (x : value_binding) : string =
  match x with
  | {
    pvb_pat; (* : pattern; *)
    pvb_expr; (* : expression; *)
    pvb_attributes; (* : attributes; *)
    pvb_loc; (* : location; *)
  } ->
    (ppddump ("DBG1:value_binding.pat:", pvb_pat ));
    (ppddump ("DBG1:value_binding.expr:", pvb_expr ));
    (*print_value_binding_expr pvb_expr*)
    (ppddump ("DBG1:value_binding.atrr:", pvb_attributes ));
    (ppddump ("DBG1:value_binding.loc:", pvb_loc ));
    "(pattern_value_binding { pvb_pat=" ^ (process_pattern pvb_pat) ^ "; pvb_expr= " ^ (process_expression pvb_expr) ^ "} )"

let fpp =1

let apply_defer y x = "("^ y ^" \"" ^ x ^ "\" )"

let process_type_decl_string x = "(string \"" ^ x ^ "\" )"

let fp2p =1
let process_type_decl_int x:string = "(int " ^ (string_of_int x) ^ ")"
let process_type_decl_bool x:string = "(bool " ^ (string_of_bool x) ^ ")"
let process_type_decl_position (x:position):string = match x with {
  pos_fname(* string*);pos_lnum(* int*);pos_bol(* int*);pos_cnum(* int*)
} ->(process_type_decl_string pos_fname)^(process_type_decl_int pos_lnum)^(process_type_decl_int pos_bol)^(process_type_decl_int pos_cnum)

let process_type_decl_location (x:location):string = match x with
    {
      loc_start(* position *);loc_end(* position *);
      loc_ghost(* bool*)
    } ->(process_type_decl_position loc_start)^(process_type_decl_position loc_end)^(process_type_decl_bool loc_ghost)


let process_location = process_type_decl_location

let process_type_decl_loc x =
  "(process_type_decl_loc "
  ^ (process_type_decl_string x.txt)
  ^ " "
  ^ (process_type_decl_location x.loc)
  ^ ")"

let process_types_payload__PPat x = "(*P57*)process_types_payload__PPat"

let process_option_expression x = x

let  core_type ( x ):string = (apply_defer "core_type" x)
let process_core_type = process_core_type
let process_type_decl_list_string a = process_generic_list "(*P58*)process_type_decl_list_string" a process_type_decl_string


    


let rec  process_type_declsignature_item_desc (x:signature_item_desc):string = match x with
  | Psig_value value_description0 -> (process_types_signature_item_desc__Psig_value((process_value_description value_description0)))
  (* | Psig_type (rec_flag0,list1) -> (process_types_signature_item_desc__Psig_type((process_rec_flag rec_flag0),(process_list list1))) *)
  (* | Psig_typesubst ( list0) -> (process_types_signature_item_desc__Psig_typesubst((process_list list0))) *)
  (* | Psig_typext ( type_extension0) -> (process_types_signature_item_desc__Psig_typext((process_type_extension type_extension0))) *)
  (* | Psig_exception ( type_exception0) -> (process_types_signature_item_desc__Psig_exception((process_type_exception type_exception0))) *)
  (* | Psig_module ( module_declaration0) -> (process_types_signature_item_desc__Psig_module((process_module_declaration module_declaration0))) *)
  (* | Psig_modsubst ( module_substitution0) -> (process_types_signature_item_desc__Psig_modsubst((process_module_substitution module_substitution0))) *)
  (* | Psig_recmodule ( list0) -> (process_types_signature_item_desc__Psig_recmodule((process_list list0))) *)
  (* | Psig_modtype ( module_type_declaration0) -> (process_types_signature_item_desc__Psig_modtype((process_module_type_declaration module_type_declaration0))) *)
  (* | Psig_modtypesubst ( module_type_declaration0) -> (process_types_signature_item_desc__Psig_modtypesubst((process_module_type_declaration module_type_declaration0))) *)
  (* | Psig_open ( open_description0) -> (process_types_signature_item_desc__Psig_open((process_open_description open_description0))) *)
  (* | Psig_include ( include_description0) -> (process_types_signature_item_desc__Psig_include((process_include_description include_description0))) *)
  (* | Psig_class ( list0) -> (process_types_signature_item_desc__Psig_class((process_list list0))) *)
  (* | Psig_class_type ( list0) -> (process_types_signature_item_desc__Psig_class_type((process_list list0))) *)
  (* | Psig_attribute ( attribute0) -> (process_types_signature_item_desc__Psig_attribute((process_attribute attribute0))) *)
  (* | Psig_extension ( extension0,attributes1) -> (process_types_signature_item_desc__Psig_extension((process_extension extension0),(process_attributes attributes1))) *)
  (* | Psig_exception ( type_exception0) -> (process_types_signature_item_desc__Psig_exception((process_type_exception type_exception0))) *)



and process_types_signature_item_desc__Psig_value((avalue_description):(string)):string = (process_types ("signature_item_desc","Psig_value") ^( avalue_description))
and process_types_payload__PPat((apattern,aoption):(pattern*expression option)):string = (process_types ("payload","PPat") ^(process_pattern apattern)^(process_expression_option aoption))
                                                                             
and process_types_payload__PTyp((acore_type):(core_type)):string = (process_types ("payload","PTyp") ^(process_core_type acore_type))
and process_types_payload__PSig((asignature):(signature)):string = (process_types ("payload","PSig") ^(process_signature asignature))
and proc_list (a,b) = a ^ b
and process_generic_type  p c d  =
  (apply_defer "(*P58*)process_generic_type_" p ^ c ^ "[" ^ (process_type_decl_list_string d)) ^ "]"
and process_type_decl_payload (x: payload):string = 
  match x with
  | PPat(pattern, e) ->
    (process_generic_type
       "payload"
       "PPat"
       [
         (process_pattern pattern);
         (process_expression_option e)         ] 
       )
       
  (* | PTyp(core_type) -> (process_generic_type "payload" "PTyp" (process_core_type core_type)) *)
  (* | PSig(signature) -> (process_generic_type "payload" "PSig" (process_signature signature)) *)
  (* | PStr(structure) -> (process_generic_type "payload" "PStr" (process_structure structure)) *)

and process_type_decl_attributes a = process_generic_list "(*P59*)process_attribute" a process_type_decl_payload

and process_type_decl_value_description (x:value_description):string =
  match x with
    {
      pval_name(* loc->string*);
      pval_type(* core_type*);
      pval_prim(* list->string*);
      pval_attributes(* attributes*);
      pval_loc(* location*)
    } ->
    (process_type_decl_loc pval_name)
    ^(process_type_decl_core_type pval_type)
    ^(process_type_decl_list_string pval_prim)
    (* ^(process_type_decl_attributes pval_attributes) *)
    (* ^(process_type_decl_location pval_loc) *)


and process_value_description x = process_type_decl_value_description x

and process_signature_item (x:signature_item):string = match x with { psig_desc(* signature_item_desc*);psig_loc(* location*)} ->
  "(*P60*)process_signature_item"
  (* ( *)
    (* process_signature_item_desc psig_desc)^(process_type_decl_location psig_loc) *)


and process_signature ( a:signature):string=
  process_generic_list "(*P61*)process_signature" a process_signature_item


let process_types_payload__PSig((asignature0):(signature)):string = (process_types ("payload","PSig") ^(process_signature asignature0))
    
let process_type_declattribute (x:attribute):string =
  match x with {
      attr_name(* loc string *);
      attr_payload(* payload *);
      attr_loc(* location *)
    } ->(process_type_decl_loc attr_name)
        ^(process_type_decl_payload attr_payload)
        ^(process_type_decl_location attr_loc)

let rec process_location_stack (x : location list) : string=
  match x with
  | [] -> ""
  | h :: t ->
    (process_location h)
    ^ "::["
    ^ (process_location_stack t)
    ^ "]"


let process_attributes x =     process_type_decl_attributes x
      
let process_type_decl_pattern (x:pattern):string =
  match x with {
    ppat_desc(* pattern_desc*);
    ppat_loc(* location*);
    ppat_loc_stack(* location_stack*);
    ppat_attributes(* attributes*)
  } ->(
      process_pattern_desc ppat_desc)^(process_location ppat_loc)^(process_location_stack ppat_loc_stack)^"(fixme process_attributes ppat_attributes)"
let process_type_decl_value_binding (x:value_binding):string =
  "(*P62*)process_type_decl_value_binding" ^
  match x with
    {
      pvb_pat (* pattern *)
    ; pvb_expr (* expression *)
    ; pvb_attributes (* attributes *)
    ; pvb_loc (* location *)
    } ->
    (
      process_type_decl_pattern (pvb_pat)
      ^(process_expression pvb_expr)
      ^ "^(fixme process_type_decl_attributes pvb_attributes )"
      ^ "((*P22*)process_type_decl_location pvb_loc)"

    )
      (* process_location_stack *)



let rec print_value_binding_list (x : value_binding list) : string=
  match x with
  | [] -> "print_value_binding_list"
  | h :: t ->
    (process_type_decl_value_binding h)
      ^ (print_value_binding_list2 h)
    ^ ";;" ^(print_value_binding_list t) ^ ";;"


let rec stringlister (x:string_list) : string =
  match x with
  | [] ->"stringlister"
  | h :: t -> h ^ stringlister(t)
and
  process_id2(x:longident *string_list):string =
  match x with
    (a,b) ->
    let sc = stringlister(b) in
    match a with
    | Lident string -> string ^ sc
    | Ldot (longident, string) ->
      (process_id2 (longident,b)) ^ "." ^ string ^ sc
    | Lapply (longident,longident2)
      -> (process_id2 (longident, b))  ^ "."
         ^ (process_id2 (longident2,b) ) ^ sc

let process_id(x:longident_loc* string_list):string =
  match x with
  | (a,b) ->
    match a with
    | {txt;_} ->(process_id2 (txt,b))
(* (({txt2)) ->txt2 *)
    (* (ppddump ("DBG1:process_id:",  txt2)); *)

let splitloc(x:longident_loc * string_list) : string=
  let (a, b) = x in
  match a with
    { txt; loc }  ->
    process_id2 (txt,  b)
let concatlist(a : string * string_list):string_list =
  let (str, string_list) = a in
  let newlist = str :: string_list  in
  newlist



















let rec emit_id1 a : string =
  match a with
  | Lident string -> string
  | Ldot (longident, string) ->
    (emit_id1 (longident)) ^ "." ^ string
  | Lapply (longident,longident2)
    -> (emit_id1 (longident))  ^ "."
       ^ (emit_id1 (longident2) )

let emit_core_type_desc (x : core_type_desc * string_list):string =
  "(*emit_core_type_desc1*)" ^
  match x with
    (ctd, s)->
    match ctd with
    | Ptyp_constr (a,b) (* of Longident.t loc * core_type list *)
      ->
      let {txt;loc} = a in
      let id1 = emit_id1(txt) in
      (* let concat = (concatlist (id1, astring_list)) in *)
      (* let newy = [id1] @ astring_list in *)
      (* let newlist = (my_process_core_type_list (b, s)) in *)
      id1 (* ^ "\"->" ^ newlist *)
    | Ptyp_tuple a (* of core_type list *)
      ->
      "Ptyp_tuple" ^ my_process_core_type_list(a,  s )

    (*not in test*)
    | Ptyp_any  -> (ppddump ("DBG1:Ptyp_any:")); "any"
    | Ptyp_var name ->(ppddump ("DBG1:Ptyp_var:"  , name)); "var-name"
  | Ptyp_arrow (arg_label , core_type , core_type2) ->
    (* my_process_core_type((core_type, string_list)); *)
    (* my_process_core_type(core_type2, string_list); *)
    (ppddump ("DBG1:Ptyp_arrow10:" )); "arrow"

  | Ptyp_object (a,b)(* of object_field list * closed_flag *)
    ->
    (ppddump ("DBG1:Ptyp_arrow8:" )); "obj"
  | Ptyp_class (a,b) (* of Longident.t loc * core_type list *)
    ->
    let myid = (process_id1 a.txt ) in
    (* my_process_core_type_list(b, y :: myid); *)
    (ppddump ("DBG1:Ptyp_arrow7:" )); "class"
  | Ptyp_alias (a,b) (* of core_type * string loc  *)
    ->
    (* my_process_core_type(a, y); *)
    (ppddump ("DBG1:Ptyp_arrow6:" )); "alias"
  | Ptyp_variant (a,b,c) (* of row_field list * closed_flag * label list option *)
    ->
    (ppddump ("DBG1:Ptyp_arrow5:" ));
    ""
  (*is handled elsewhere*)

  | Ptyp_poly (a,b) (* of string loc list * core_type *)
    ->
    (* my_process_core_type(b, y); *)
    (ppddump ("DBG1:Ptyp_arrow4:" )); "poly"
  | Ptyp_package a(* of package_type  *)
    ->
    (ppddump ("DBG1:Ptyp_arrow3:",a )) ; "typ_package"
  (* | Ptyp_open (a,b) (\* of Longident.t loc * core_type *\)-> *)
  (*   (ppddump ("DBG1:Ptyp_arrow2",a,b )) *)
  | Ptyp_extension a (* of extension   *)    ->
    (ppddump ("DBG1:Ptyp_extension:",a )); "extension"


let  emit_core_type(a: core_type * string_list*int):string=
  "(*emit_core_type1*)" ^
  match a with
  | (x,s,n) ->
     match x with
    {
      ptyp_desc(* : core_type_desc *);
      ptyp_loc(* : Location.t *);
      ptyp_loc_stack(* : location_stack *);
      ptyp_attributes(* : attributes; *)
    }->
    let td = (emit_core_type_desc (ptyp_desc,s)) in
    td ^ (string_of_int n)

let  emit_core_type2(a: core_type * string_list*int):string=
  "(*emit_core_type2*)" ^
  match a with
  | (x,s,n) ->
    match x with
      {
        ptyp_desc(* : core_type_desc *);
        ptyp_loc(* : Location.t *);
        ptyp_loc_stack(* : location_stack *);
        ptyp_attributes(* : attributes; *)
      }->
      let td = (emit_core_type_desc (ptyp_desc,s)) in
      td


let rec emit_core_type_list(x: core_type_list * string_list*int):string =
  "(*emit_core_type_list2*)" ^
  match x with
  | (a,b,n) ->
    match a with
    | [] -> ""
    | h :: t ->
      let tt = emit_core_type_list(t,b,n+1)  in
      let h1 = emit_core_type (h,b,n) in
      if tt != "" then
        h1 ^ "," ^ tt
      else
        h1

let  imp_core_type((a,s,n): core_type * string_list*int):string=
  "(*imp_core_type1*)" ^
  let name1 = emit_core_type2(a,s,n) in
  let name = emit_core_type(a,s,n) in
  "((*P16*)process_" ^ name1 ^ " " ^ name  ^ ")"
(* ^"B" ^(string_of_int n) *)


(* calls the function from the constructor*)
let rec imp_core_type_list(x: core_type_list * string_list*int):string =
  match x with
  | (a,b,n) ->
    match a with
    | [] -> ""
    | h :: t ->
      let tt = imp_core_type_list(t,b,n+1)  in
      let one = imp_core_type (h, b,n ) in
      if tt != "" then
        one ^ "," ^ tt
      else
        one

let emit_constructor_arguments(a1:(string*string*constructor_arguments*string_list)):string =
  let (parent,name,x,s) = a1
  in  match x with  | Pcstr_tuple a ->
    let ictl = imp_core_type_list (a,s,0) in
    "(*emit_constructor_arguments*)" ^
    "| " ^ name
    ^ " (emit_core_type_list_special "
    ^ (emit_core_type_list (a,s,0))
    ^ ") -> (process_types_"
    ^ parent
    ^ "__"
    ^ name
    ^ "("
    ^ ictl
    ^ "))"
                                                                                                                                            | other  -> "other"

let  decl_imp_core_type(a: string*string *core_type * string_list*int):string=
  let (parent, parent2, atype, s, n) = a in
  let name = emit_core_type(atype, s, n) in
  let h1 = emit_core_type2(atype, s, n) in
  (print_endline ("DBG12A:decl_imp_core_type:" ^ "let process_" ^ h1 ^ " x : " ^ h1 ^ "= x"));
  "a" ^ name
(* ":" ^ name1  *)
(* ")" *)
(* :string=\""^parent  ^ "__" ^ parent2  ^ "_" ^ name1  ^"\" ^ \"a" ^ name ^ "\"\n" *)
let ff =1
let rec decl_imp_core_type_list(parent,name,a,b,n) =
  "(*decl_imp_core_type_list*)" ^
  match a with
  | [] -> ""
  | h :: t ->
    let h1 = decl_imp_core_type (parent,name, h, b,n) in
    let tt = decl_imp_core_type_list(parent,name,t,b,n+1)  in
    if tt != "" then
      h1 ^ "," ^ tt
    else
      h1

let f=1
let rec decl_imp_core_type_list2((parent,name,a,b,n): string*string*core_type_list * string_list*int):string =
  "(*decl_imp_core_type_list2*)" ^
  match a with
  | [] -> ""
  | h :: t ->
    let h1 = emit_core_type2(h, b, n) in
    let tt = decl_imp_core_type_list2(parent,name,t,b,n+1)  in
    if tt != "" then
      h1 ^ "*" ^ tt
    else
      h1

(*sep with hats ^ string concat *)
let rec decl_imp_core_type_list_hats((parent,name,a,b,n): string*string*core_type_list * string_list*int):string =
  "(*decl_imp_core_type_list_hats*)" ^
  match a with
  | [] -> ""
  | h :: t ->
    let h1 = decl_imp_core_type (parent,name, h, b,n) in
    let quoted = "(core_type2 \"" ^ h1 ^ "\")" in
    let tt = decl_imp_core_type_list_hats(parent,name,t,b,n+1)  in
    if tt != "" then
      quoted ^ "^" ^ tt
    else
      quoted

let decl_emit_constructor_arguments(parent,name,x,s):string =
  match x with
  | Pcstr_tuple a ->
    "let "^ "(*P64*)process_types_" ^ parent ^ "__" ^ name
    ^ "(("    ^  decl_imp_core_type_list (parent,name,a,s,0) ^   "):"
    ^ "("    ^  decl_imp_core_type_list2 (parent,name,a,s,0) ^  ")):string"
    ^ " = (process_types (\"" ^ parent ^ "\",\"" ^ name ^ "\") ^" ^
    (decl_imp_core_type_list_hats (parent,name,a,s,0) ) ^ ")"
  | other  -> "other"

let process_label_declaration  x =
  "Label_decl:" ^  x.pld_name.txt ^ process_core_type x.pld_type

let rec process_label_declaration_list x =
  match x with
  | [] -> ""
  | h :: t ->
    process_label_declaration h ^  ";" ^ (process_label_declaration_list t)

let print_constructor_arguments(a) =
  match a with
  | (x,s) ->
    match x with
    | Pcstr_tuple a ->
      (ppddump ("DBG1:Pcstr_tuple:"  , a));
      "Pcstr_tuple:" ^ (my_process_core_type_list (a,s))

    | Pcstr_record a ->
      (ppddump ("DBG1:Pcstr_record:"  , a));
      "Pcstr_record" ^
      (process_label_declaration_list a)


let rec process_type_variant_constructor_declaration_list(a:string*constructor_declaration list*string_list):string =
  match a with
  | (p,x,s)->
    match x with
    | [] -> "\nDEBUG3A: let process_variant_" ^ p ^ "(x:" ^ p ^")=\nmatch x with\n"
    | h :: t ->
      match h with
      |{
        pcd_name(* : string loc *);
        pcd_vars(* : string loc list *);
        pcd_args(* : constructor_arguments *);
        pcd_res(* : core_type option *);
        pcd_loc(* : Location.t *);
        pcd_attributes(* : attributes *);
      }->
        (print_endline (
            "DBG12C: let process_"
            ^ p ^ "__" ^ pcd_name.txt
            ^ " x :string ="
            ^ "match x with "));
        (* let name = match pcd_name with *)
        (*   | (str,_) -> str *)
        (* (ppddump ( *)
        (*      "DBG1:constructor_declaration:", *)
        (*      pcd_name, *)
        (*      "vars", *)
        (*      pcd_vars, *)
        (*      "args", *)
        (*      pcd_args, *)
        (*      "res", *)
        (*      pcd_res, *)
        (*      "loc", *)
        (*      pcd_loc, *)
        (*      "attrs", *)
        (*      pcd_attributes *)
        (*    )); *)
        let newtext = (emit_constructor_arguments(p,pcd_name.txt, pcd_args, s)) in
        let newtext2 = (decl_emit_constructor_arguments(p,pcd_name.txt, pcd_args, s)) in
        (print_endline ("DBG12B:" ^ newtext2));
        (print_endline ("DBG12C:" ^ newtext));
        let ret =              "(constructor \""^ pcd_name.txt ^ "\" "
                               ^ "{" ^
                               print_constructor_arguments(pcd_args,s)
                               ^ "}" ^
                                ")"
        in
        Printf.printf "DBG1:constructor_declaration_new: %s\n" ret;
        let nextlist = (process_type_variant_constructor_declaration_list(p,t,s)) in
        ret ^nextlist

let bar =1

let process_kind(a) :string=
  match a with
  | (p,x,s)->
    match x with
    (*and type_kind =*)
    | Ptype_abstract  -> (ppddump ("DBG1:Ptype_abstract:"));
      "DBG1:Ptype_abstract"
    | Ptype_variant a ->
      (* (ppddump ("DBG1:Ptype_variant:",  a)); *)
      "(Ptype_variant " ^ (process_type_variant_constructor_declaration_list (p,a,s))       ^ ")"
    (*of constructor_declaration list *)
    | Ptype_record a ->
      process_record_kind_list(p,a,s)
    | Ptype_open -> (ppddump ("DBG1:Ptype_open:")); "Ptype_open"

let print_type_decl(a) =
  match a with
  |(x,s) ->
    match x with
      {
        ptype_name (* : string loc *);
        ptype_params (* : (core_type * (variance * injectivity)) list *);
        ptype_cstrs (*: (core_type * core_type * location) list*) ;
        ptype_kind (*: type_kind*)  ;
        ptype_private (*: private_flag*);
        ptype_manifest (* : core_type option *);
        ptype_attributes (*: attributes*);
        ptype_loc (*: location*)
      } ->
      (* (ppddump ("DBG1:type_decl:", ptype_name)); *)
      (* (ppddump ("DBG1:parameters:", ptype_params)); *)
      (* (ppddump ("DBG1:cstrs:", ptype_cstrs)); *)
      (* (ppddump ("DBG1:kind:",ptype_kind)); *)

      (* (ppddump ("DBG1:private:",  ptype_private, *)
      (*                                 "DBG1:manifest", ptype_manifest, *)
      (*                                 "DBG1:attr", ptype_attributes, *)
      (*                                 "DBG1:loc", ptype_loc *)
      (*                                )); *)
      "print_type_decl:\"" ^  ptype_name.txt ^ "\" = " ^ (process_kind (ptype_name.txt,ptype_kind,s))

type     type_declaration_list = type_declaration list

let rec process_type_decl_list(a:type_declaration_list*string_list):string =
  match a with
  |(x,s)->
    match x with
    | [] -> ""
    | h :: t ->
      (print_type_decl (h,s))
      ^ ";" ^
      (process_type_decl_list (t,s))


(*
   start of generator for record types
      first step to generate record vistior :
   1. function with type
   1.1. single argument x
   2. match x with pattern
   3. action on variablee to print them.

   and type_kind = Parsetree.type_kind =
  | Ptype_record of label_declaration list  (** Invariant: non-empty list *)

           | Ptype_record a ->
            let a = self#list self#label_declaration ctx a in
            ( Ptype_record (Stdlib.fst a),
              self#constr ctx "Ptype_record" [ Stdlib.snd a ] )
*)
let fffff=1

(*the decl list
  list,
  string context,
  separarator
*)


let print_structure_item_desc(a :structure_item_desc*string_list) :string =
  "(print_structure_item_desc " ^
  (
    match a with
    |(x,s)->
      (* (ppddump ("DBG1:structure_item_desc:", x)); *)
      match x with
      | Pstr_value (rec_flag, value_binding_list) ->
        (* (ppddump ("DBG1:Pstr_value:", rec_flag, value_binding_list)); *)
        "(pstr_value "   ^ (process_rec_flag rec_flag) ^ "^" ^ print_value_binding_list(value_binding_list) ^ ")"
      | Pstr_type (rec_flag, type_declaration_list) ->

      (print_endline ("\n(HELPEmitthecode_emit_type_decl_list "^((process_rec_flag rec_flag) ^ Emitthecode.emit_type_decl_list (type_declaration_list,s," "))^")\n"));
      "(emit_pstr_type)"^
      process_type_decl_list((type_declaration_list,s))
    | Pstr_module  module_binding ->
      (* (ppddump ("DBG1:Pstr_module:",module_binding)); *) "module_binding"
    (*open model*)
    | Pstr_open open_description ->(ppddump ("DBG1:Pstr_open", open_description)); "module_open"
    | Pstr_eval (expression,attributes) ->
      (ppddump ("DBG1:Pstr_eval:", expression,attributes));
      "Pstr_eval"
    (*value binding*)
    | Pstr_primitive value_description ->(ppddump ("DBG1:Pstr_primitive:", value_description)) ; "primitive"
    | Pstr_typext  type_extension ->(ppddump ("DBG1:Pstr_typext:", type_extension)); "typeext"
    | Pstr_exception extension_constructor ->(ppddump ("DBG1:Pstr_exception:", extension_constructor)); "exception"
    | Pstr_recmodule  module_binding_list ->(ppddump ("DBG1:Pstr_recmodule:", module_binding_list)) ; "recmodule"
    | Pstr_modtype module_type_declaration ->(ppddump ("DBG1:Pstr_modtype:", module_type_declaration)); "modtype"
    | Pstr_class (class_declarations ) ->(ppddump ("DBG1:Pstr_class:", class_declarations)); "class"
    | Pstr_class_type (class_type_declarations) ->(ppddump ("DBG1:Pstr_class_type:", class_type_declarations)) ; "class_Type"
    | Pstr_include  (include_declaration)->(ppddump ("DBG1:Pstr_include:",include_declaration)); "include"
    | Pstr_attribute (attribute)->(ppddump ("DBG1:Pstr_attribute:", attribute)); "_attribute"
    | Pstr_extension ( extension , attributes)->(ppddump ("DBG1:Pstr_extension:", extension , attributes)) ; "extension"
  )
  ^ ")" (*end of print_desc*)

let print_one_structure_item (x : structure_item) :string =
  "(Gen7.process_structure_item x)"
  (* match x with *)
  (* |{ *)
  (*   pstr_desc; (\*structure_item_desc*\) *)
  (*   _ *)
  (* } -> *)
  (*   "TOPstructure_item_desc:" ^ (print_structure_item_desc (pstr_desc,[])) ^ *)
  (*   "\nTOPstructure_item_desc2:" ^ (Gen.process_types_structure_item_desc (pstr_desc)) *)

let process_all_structure_items proc lst : string =
  let result = List.map proc lst in
  List.iter (fun i -> print_endline i) result;
  ""

let transform x (*ast, bytecodes of the interface *) =
  (ppddump ("DBG13:",x));
  (print_endline ("open Ppxlib"));
  (print_endline 
  (Gen7.process_structure_items x));
  (* let foo = (process_all_structure_items print_one_structure_item x) in *)
    x 

let process_bool x = "bool"

let () = Driver.register_transformation ~impl:transform "simple-ppx"
