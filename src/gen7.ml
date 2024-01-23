open Ppxlib
(*open Ppx*)
(* open Gen4 *)

type string_list = string list

let process_generic_option f a =
  (* (print_endline "process_generic_option"); *)
  match a with
  | Some x -> (f x)
  | None -> ""

let rec stringlister3 (x:string_list) : string =
  match x with
  | [] ->""
  | h :: t ->
    if t != [] then
      let n = stringlister3(t) in
      if h != "" then
        h ^ ";(*L1*)"^ n
      else
        "(*L10*)"^ n
    else
      h
and stringlister2 (x:string_list) : string =
  "[" ^
  ( match x with
    | [] ->""
    | h :: t ->
      if t != [] then
        h ^ ";(*L2*)"^
        stringlister3(t)
      else
        h
  )
  ^"]"
and process_generic_type a b c =
  (* (print_endline ("process_generic_type "  ^ a  ^ " " ^ b));  *)
  "(process_generic_type \""
  ^ a
  ^ "\" \""
  ^ b
  ^ "\" "
  ^ (stringlister2 c)
  ^    ")"
  
let rec
process_generic_list_tail name a f :string=
 ( match a with
  | [] -> ""
  | a :: t ->
    let v1 = (f a) in
    if t != [] then
      v1 ^ ";(*L3*)" ^ (process_generic_list_tail name t f )
    else
      v1
 )

and process_generic_list name a f :string=
  (* (print_endline ("process_list \"" ^ name)) ; *)
  "(" ^ name ^ "[" ^
 ( match a with
  | [] -> ""
  | a :: t ->
    let v1 = (f a) in
    if t != [] then
      v1 ^ ";(*L4*)" ^ (process_generic_list_tail name t f )
    else
      v1
 ) ^ "] )"

let rec  process_structure_items x =
  "let ()=(print_endline " ^
  process_generic_list "process_structure_items" x process_structure_item
  ^ ")"
and process_string_loc_list_pattern_option x = "process_string_loc_list_pattern_option"

and  process_arg_label_expression (a,b)=
  "(process_arg_label_expression "
  ^ process_arg_label a
  ^ " "
  ^  process_expression b
  ^ ")"

and process_longident_loc_core_type (a,b)  =
  "(process_longident_loc_core_type "
  ^ process_longident_loc a
  ^ " "
  ^ process_core_type b
  ^ ")"

and process_generic_list_tail2 (name:string) (a:(longident_loc * core_type) list)  :string=
 ( match a with
  | [] -> ""
  | a :: t ->
    let v1 = (process_longident_loc_core_type  a) in
    if t != [] then
      v1 ^ ";(*L5*)" ^ (process_generic_list_tail2 name t  )
    else
      v1
 )

and process_generic_list2 (name:string) (a:(longident_loc * core_type) list )  :string=
  "(" ^ name ^ "[" ^
 ( match a with
  | [] -> ""
  | a :: t ->
    let v1 = (process_longident_loc_core_type  a) in
    if t != [] then
      v1 ^ ";(*L6*)" ^ (process_generic_list_tail2 name t  )
    else
      v1
 ) ^ "] )"

and process_package_type (x:package_type) =
  (*longident_loc * (longident_loc * core_type) list*)
  match x with (a,b) ->
    process_longident_loc a ^ 
    process_generic_list2 "process_string_loc_list" b

and process_string_list x =
  process_generic_list "process_string_list" x process_string



and process_class_type_fields_list x=
  process_generic_list "process_class_type_fields_list" x process_class_type_field
and process_class_fields_list x=
  process_generic_list "process_class_fields_list" x process_class_field
and process_object_field_list x =
  process_generic_list "process_object_field_list" x process_object_field


and process_string_loc_list x =
  process_generic_list "process_string_loc_list" x process_string_loc

and process_int x : string= "(mint " ^ string_of_int(x) ^ ")"
and process_extension_constructor_list x=
    process_generic_list "process_constructors_list" x process_extension_constructor
and process_location_stack x ="(loc_stack)"
  (* process_generic_list "process_location_stack" x process_location *)
    
and process_row_field_list x =
  process_generic_list "process_row_field_list" x process_row_field

    
    
and process_cases x =
  process_generic_list "process_cases" x process_case

and process_label_declaration_list x =
  process_generic_list "process_label_declaration_list" x process_label_declaration

and process_core_type_list a =
  process_generic_list "process_core_type_list" a process_core_type
and process_expression_list a =
  process_generic_list "process_expression_list" a process_expression
and process_arg_label_expression_list a =
  process_generic_list "process_arg_label_expression_list" a process_arg_label_expression
and process_pattern_list (a:pattern list):string =
  process_generic_list "process_pattern_list" a process_pattern

and process_loc_pattern (a,b) =
 process_longident_loc a ^
 process_pattern b
and process_longident_loc_pattern_list a =
  process_generic_list "process_longident_loc_pattern_list" a process_loc_pattern


and process_loc_expression (a,b) =
 process_longident_loc a ^
 process_expression b
and process_longident_loc_expression_list a =
  process_generic_list "process_longident_loc_expression_list" a process_loc_expression


and process_label_loc_expression x = "\"fixme:process_label_loc_expression\""
and process_label_loc_expression_list a =
  process_generic_list "process_label_loc_expression" a
    process_label_loc_expression

and process_constructor_declaration_list a =
  process_generic_list "process_arg_constructor_declaration" a
    process_constructor_declaration

and process_type_declaration_list a =
  process_generic_list "process_type_declaration_list" a process_type_declaration
and process_with_constraint_list a =
  process_generic_list "process_with_constraint_list" a process_with_constraint
and process_module_declaration_list (a:module_declaration list) =
  process_generic_list "process_module_declaration_list" a process_module_declaration
and process_module_binding_list (a:module_binding list) =
  process_generic_list "process_module_binding_list" a process_module_binding
and process_class_type_declaration(x:class_type_declaration):string = match x with {pci_virt(* virtual_flag*);pci_params(* list*);pci_name(* loc*);pci_expr(* FIXME*);pci_loc(* location*);pci_attributes(* attributes*)} ->(
    (*P2*)process_virtual_flag pci_virt)
    ^ "^" ^ ((*P2*)process_params_list pci_params)
    ^ "^" ^ ((*P2*)process_string_loc pci_name)
and process_class_type_declaration_list a =
  process_generic_list "process_class_type_declaration_list" a process_class_type_declaration

and process_class_declaration (x:class_declaration ) =
  process_class_expr_class_infos x
  (*class_expr class_infos *)
and process_class_declaration_list a =
  process_generic_list "process_class_declaration_list" a process_class_declaration

and process_directive_argument (x:directive_argument):string = match x with {pdira_desc(* directive_argument_desc*);pdira_loc(* location*)} ->((*P2*)process_directive_argument_desc pdira_desc)^ "^" ^ ((*P2*)process_location pdira_loc)


and process_string_option x = "process_string_option"
and process_char_option a = 
    process_generic_option
      process_char  a

and process_attributes x = 
    process_generic_list "process_attribute_list" x process_attribute
and process_directive_argument_option x = "process_directive_argument_option"

and  process_object_field_desc (x:object_field_desc) :string =
  match x with
(*emit_constructor_arguments:*)| Oinherit((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type0) -> ((*P5*)process_generic_type "object_field_desc" "Oinherit" [((*P4*)process_core_type (*emit_core_type_numbered*)core_type0)])
(*emit_constructor_arguments:*)| Otag((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type1) -> ((*P5*)process_generic_type "object_field_desc" "Otag" [((*P4*)process_label_loc (*emit_core_type_numbered*)loc0);((*P4*)process_core_type (*emit_core_type_numbered*)core_type1)])

and process_bool x = "(b " ^ string_of_bool x ^ ")"
and   process_row_field_desc x :string =match x with
(*emit_constructor_arguments:*)| Rinherit((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type0) -> ((*P5*)process_generic_type "row_field_desc" "Rinherit" [((*P4*)process_core_type (*emit_core_type_numbered*)core_type0)])
(*emit_constructor_arguments:*)| Rtag((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)bool1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list2) -> ((*P5*)process_generic_type "row_field_desc" "Rtag" [((*P4*)process_loc (*emit_core_type_numbered*)loc0);((*P4*)process_bool (*emit_core_type_numbered*)bool1);((*P4*)process_core_type_list (*emit_core_type_numbered*)list2)])

and process_pattern_option a =
  match a with
  | Some x -> (process_pattern x)
  | None -> ""

and process_generic_string_option_loc  (a:string option loc):string = "\"FIXMEprocess_generic_string_option_loc\""

and process_string_list_option x =
  process_generic_option
    process_string_list  x
    
and  process_core_type_option x =
  process_generic_option process_core_type  x

and process_manifest_option x = process_generic_option process_core_type  x

and process_char (a:char):string = "(char" ^ String.make 1 a ^ ")"

and process_label_loc (a:label loc) = a.txt

and process_expression_option
    (x:expression option):string = process_generic_option
    process_expression x

and process_module_type_option (x:module_type option):string = process_generic_option process_module_type x
and process_include_description (x:include_description):string = match x with {pincl_mod(* FIXME*);pincl_loc(* location*);pincl_attributes(* attributes*)} ->
  ((*P2*)process_module_type pincl_mod)
  ^ "^" ^ ((*P2*)process_location pincl_loc)^ "^" ^ ((*P2*)process_attributes pincl_attributes)

(* and include_description = module_type include_infos *)
(*
and process_include_description (x:include_description):string = match x with  *)
and process_override_flag x :string =match x with
(*emit_constructor_arguments:*)| Fresh -> ((*P5*)process_generic_type "override_flag" "Fresh" [])
(*emit_constructor_arguments:*)| Override -> ((*P5*)process_generic_type "override_flag" "Override" [])
and process_id1 a : string =
  match a with
  | Lident string -> string
  | Ldot (longident, string) ->
    (process_id1 (longident)) ^ "." ^ string
  | Lapply (longident,longident2)
    -> (process_id1 (longident))  ^ "."
       ^ (process_id1 (longident2) )
and process_value_binding_list (x : value_binding list) : string=
  match x with
  | [] -> "process_value_binding_list"
  | h :: t ->
    let v1 =     (process_value_binding h) in
    if t != [] then
      v1   ^ ";(*L7*)" ^(process_value_binding_list t)
    else
      v1

and process_longident_loc ( a :longident_loc):string="(ident \"" ^ (process_id1 a.txt) ^ "\")"
and process_module_expr_desc x :string =match x with
(*emit_constructor_arguments:*)| Pmod_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "module_expr_desc" "Pmod_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
(*emit_constructor_arguments:*)| Pmod_unpack((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0) -> ((*P5*)process_generic_type "module_expr_desc" "Pmod_unpack" [((*P4*)process_expression (*emit_core_type_numbered*)expression0)])
(*emit_constructor_arguments:*)| Pmod_constraint((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_expr0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type1) -> ((*P5*)process_generic_type "module_expr_desc" "Pmod_constraint" [((*P4*)process_module_expr (*emit_core_type_numbered*)module_expr0);((*P4*)process_module_type (*emit_core_type_numbered*)module_type1)])
(*emit_constructor_arguments:*)| Pmod_apply((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_expr0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_expr1) -> ((*P5*)process_generic_type "module_expr_desc" "Pmod_apply" [((*P4*)process_module_expr (*emit_core_type_numbered*)module_expr0);((*P4*)process_module_expr (*emit_core_type_numbered*)module_expr1)])
(*emit_constructor_arguments:*)| Pmod_functor((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)functor_parameter0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_expr1) -> ((*P5*)process_generic_type "module_expr_desc" "Pmod_functor" [((*P4*)process_functor_parameter (*emit_core_type_numbered*)functor_parameter0);((*P4*)process_module_expr (*emit_core_type_numbered*)module_expr1)])
(*emit_constructor_arguments:*)| Pmod_structure((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)structure0) -> ((*P5*)process_generic_type "module_expr_desc" "Pmod_structure" [((*P4*)process_structure (*emit_core_type_numbered*)structure0)])
(*emit_constructor_arguments:*)| Pmod_ident((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0) -> ((*P5*)process_generic_type "module_expr_desc" "Pmod_ident" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0)])
(* and process_module_expr (x:module_expr):string = match x with {pmod_desc(\* module_expr_desc*\);pmod_loc(\* location*\);pmod_attributes(\* attributes*\)} ->((\*P2*\)process_module_expr_desc pmod_desc)^((\*P2*\)process_location pmod_loc)^((\*P2*\)process_attributes pmod_attributes) *)

(* and open_declaration = module_expr open_infos*)
and process_open_declaration (x:open_declaration):string = match x with {popen_expr(* FIXME*);popen_override(* override_flag*);popen_loc(* location*);popen_attributes(* attributes*)} ->
  ((*P2*)process_module_expr  popen_expr)
  ^ "^" ^ ((*P2*)process_override_flag popen_override)^ "^" ^ ((*P2*)process_location popen_loc)^ "^" ^ ((*P2*)process_attributes popen_attributes)
(*and open_description = longident_loc open_infos*)
and process_open_description (x:open_description):string = match x with {popen_expr(* FIXME*);popen_override(* override_flag*);popen_loc(* location*);popen_attributes(* attributes*)} ->
  ((*P2*)process_longident_loc  popen_expr)
  ^ "^" ^ ((*P2*)process_override_flag popen_override)^ "^" ^ ((*P2*)process_location popen_loc)^ "^" ^ ((*P2*)process_attributes popen_attributes)


and process_include_declaration x =(*= module_expr include_infos*)
  (process_module_expr_include_infos x)
  
    
and process_directive_argument_desc x :string =match x with
(*emit_constructor_arguments:*)| Pdir_bool((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)bool0) -> ((*P5*)process_generic_type "directive_argument_desc" "Pdir_bool" [((*P4*)process_bool (*emit_core_type_numbered*)bool0)])
(*emit_constructor_arguments:*)| Pdir_ident((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident0) -> ((*P5*)process_generic_type "directive_argument_desc" "Pdir_ident" [((*P4*)process_longident (*emit_core_type_numbered*)longident0)])
(*emit_constructor_arguments:*)| Pdir_int((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "directive_argument_desc" "Pdir_int" [((*P4*)process_string (*emit_core_type_numbered*)string0);((*P4*)process_char_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| Pdir_string((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0) -> ((*P5*)process_generic_type "directive_argument_desc" "Pdir_string" [((*P4*)process_string (*emit_core_type_numbered*)string0)])
and process_toplevel_phrase x :string =match x with
(*emit_constructor_arguments:*)| Ptop_dir((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)toplevel_directive0) -> ((*P5*)process_generic_type "toplevel_phrase" "Ptop_dir" [((*P4*)process_toplevel_directive (*emit_core_type_numbered*)toplevel_directive0)])
(*emit_constructor_arguments:*)| Ptop_def((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)structure0) -> ((*P5*)process_generic_type "toplevel_phrase" "Ptop_def" [((*P4*)process_structure (*emit_core_type_numbered*)structure0)])
and process_class_type (x:class_type):string = match x with {pcty_desc(* class_type_desc*);pcty_loc(* location*);pcty_attributes(* attributes*)} ->((*P2*)process_class_type_desc pcty_desc)^ "^" ^ ((*P2*)process_location pcty_loc)^ "^" ^ ((*P2*)process_attributes pcty_attributes)
and process_structure_item_desc x :string =
  (* (print_endline ("process_structure_item_desc")) ; *)
  match x with
(*emit_constructor_arguments:*)| Pstr_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)attributes1) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0);((*P4*)process_attributes (*emit_core_type_numbered*)attributes1)])
(*emit_constructor_arguments:*)| Pstr_attribute((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)attribute0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_attribute" [((*P4*)process_attribute (*emit_core_type_numbered*)attribute0)])
(*emit_constructor_arguments:*)| Pstr_include((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)include_declaration0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_include" [((*P4*)process_include_declaration (*emit_core_type_numbered*)include_declaration0)])
                               (*emit_constructor_arguments:*)| Pstr_class_type((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_class_type"
                                                                                                                                                                               [
                                                                                                                                                                                 ((*P4*)process_class_type_declaration_list (*emit_core_type_numbered*)list0)
                                                                                                                                                                               ])
                               (*emit_constructor_arguments:*)| Pstr_class((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_class" [
    ((*P4*)process_class_declaration_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Pstr_open((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)open_declaration0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_open" [((*P4*)process_open_declaration (*emit_core_type_numbered*)open_declaration0)])
(*emit_constructor_arguments:*)| Pstr_modtype((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type_declaration0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_modtype" [((*P4*)process_module_type_declaration (*emit_core_type_numbered*)module_type_declaration0)])
(*emit_constructor_arguments:*)
                               | Pstr_recmodule(
                                   (*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) ->
                                 (
                                   (*P5*)process_generic_type "structure_item_desc" "Pstr_recmodule" [


                                     ((*P4*)process_module_binding_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Pstr_module((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_binding0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_module" [((*P4*)process_module_binding (*emit_core_type_numbered*)module_binding0)])
(*emit_constructor_arguments:*)| Pstr_exception((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)type_exception0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_exception" [((*P4*)process_type_exception (*emit_core_type_numbered*)type_exception0)])
(*emit_constructor_arguments:*)| Pstr_typext((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)type_extension0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_typext" [((*P4*)process_type_extension (*emit_core_type_numbered*)type_extension0)])
(*emit_constructor_arguments:*)| Pstr_type((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)rec_flag0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_type" [((*P4*)process_rec_flag (*emit_core_type_numbered*)rec_flag0);((*P4*)process_type_declaration_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Pstr_primitive((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)value_description0) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_primitive" [((*P4*)process_value_description (*emit_core_type_numbered*)value_description0)])
(*emit_constructor_arguments:*)| Pstr_value((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)rec_flag0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_value" [((*P4*)process_rec_flag (*emit_core_type_numbered*)rec_flag0);((*P4*)process_value_binding_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Pstr_eval((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)attributes1) -> ((*P5*)process_generic_type "structure_item_desc" "Pstr_eval" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_attributes (*emit_core_type_numbered*)attributes1)])
and process_with_constraint x :string =match x with
(*emit_constructor_arguments:*)| Pwith_modsubst((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc1) -> ((*P5*)process_generic_type "with_constraint" "Pwith_modsubst" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc1)])
(*emit_constructor_arguments:*)| Pwith_typesubst((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)type_declaration1) -> ((*P5*)process_generic_type "with_constraint" "Pwith_typesubst" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_type_declaration (*emit_core_type_numbered*)type_declaration1)])
(*emit_constructor_arguments:*)| Pwith_modtypesubst((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type1) -> ((*P5*)process_generic_type "with_constraint" "Pwith_modtypesubst" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_module_type (*emit_core_type_numbered*)module_type1)])
(*emit_constructor_arguments:*)| Pwith_modtype((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type1) -> ((*P5*)process_generic_type "with_constraint" "Pwith_modtype" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_module_type (*emit_core_type_numbered*)module_type1)])
(*emit_constructor_arguments:*)| Pwith_module((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc1) -> ((*P5*)process_generic_type "with_constraint" "Pwith_module" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc1)])
(*emit_constructor_arguments:*)| Pwith_type((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)type_declaration1) -> ((*P5*)process_generic_type "with_constraint" "Pwith_type" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_type_declaration (*emit_core_type_numbered*)type_declaration1)])
and process_string x = "(string \"" ^ (String.escaped x) ^ "\" )"
                       
and process_loc x = "(loc)"
  (* "(process_loc " *)
  (* ^ (process_string x.txt) *)
  (* ^ "^" *)
  (* ^ (process_location x.loc) *)
  (* ^ ")" *)


and process_string_option_loc x = process_generic_string_option_loc x

and process_module_type_declaration (x:module_type_declaration):string = match x with {pmtd_name(* loc*);pmtd_type(* option*);pmtd_attributes(* attributes*);pmtd_loc(* location*)} ->
  ((*P2*)process_loc pmtd_name)^
  ((*P2*)process_module_type_option pmtd_type)
  ^ "^" ^ ((*P2*)process_attributes pmtd_attributes)
  ^ "^" ^ ((*P2*)process_location pmtd_loc)
and process_signature_item_desc x :string =match x with
(*emit_constructor_arguments:*)| Psig_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)attributes1) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0);((*P4*)process_attributes (*emit_core_type_numbered*)attributes1)])
(*emit_constructor_arguments:*)| Psig_attribute((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)attribute0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_attribute" [((*P4*)process_attribute (*emit_core_type_numbered*)attribute0)])
(*emit_constructor_arguments:*)| Psig_class_type((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_class_type" [((*P4*)process_class_type_declaration_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Psig_class((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_class" [((*P4*)process_class_type_declaration_list (*emit_core_type_numbered*)list0)])
                               (*emit_constructor_arguments:*)| Psig_include((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)include_description0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_include" [
    ((*P4*)process_include_description (*emit_core_type_numbered*)include_description0)
  ])
(*emit_constructor_arguments:*)| Psig_open((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)open_description0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_open" [((*P4*)process_open_description (*emit_core_type_numbered*)open_description0)])
(*emit_constructor_arguments:*)| Psig_modtypesubst((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type_declaration0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_modtypesubst" [((*P4*)process_module_type_declaration (*emit_core_type_numbered*)module_type_declaration0)])
(*emit_constructor_arguments:*)| Psig_modtype((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type_declaration0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_modtype" [((*P4*)process_module_type_declaration (*emit_core_type_numbered*)module_type_declaration0)])
(*emit_constructor_arguments:*)| Psig_recmodule((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_recmodule" [((*P4*)process_module_declaration_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Psig_modsubst((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_substitution0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_modsubst" [((*P4*)process_module_substitution (*emit_core_type_numbered*)module_substitution0)])
(*emit_constructor_arguments:*)| Psig_module((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_declaration0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_module" [((*P4*)process_module_declaration (*emit_core_type_numbered*)module_declaration0)])
(*emit_constructor_arguments:*)| Psig_exception((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)type_exception0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_exception" [((*P4*)process_type_exception (*emit_core_type_numbered*)type_exception0)])
(*emit_constructor_arguments:*)| Psig_typext((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)type_extension0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_typext" [((*P4*)process_type_extension (*emit_core_type_numbered*)type_extension0)])
(*emit_constructor_arguments:*)| Psig_typesubst((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_typesubst" [((*P4*)process_type_declaration_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Psig_type((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)rec_flag0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_type" [((*P4*)process_rec_flag (*emit_core_type_numbered*)rec_flag0);((*P4*)process_type_declaration_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Psig_value((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)value_description0) -> ((*P5*)process_generic_type "signature_item_desc" "Psig_value" [((*P4*)process_value_description (*emit_core_type_numbered*)value_description0)])
and process_functor_parameter x :string =match x with
  (*emit_constructor_arguments:*)
  | Named((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type1) -> ((*P5*)process_generic_type "functor_parameter" "Named" [((*P4*)process_string_option_loc (*emit_core_type_numbered*)loc0);((*P4*)process_module_type (*emit_core_type_numbered*)module_type1)])
  (*emit_constructor_arguments:*)| Unit -> ((*P5*)process_generic_type "functor_parameter" "Unit" [])
and process_label x  = process_string x


  
and process_module_type_desc x :string =match x with
(*emit_constructor_arguments:*)| Pmty_alias((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0) -> ((*P5*)process_generic_type "module_type_desc" "Pmty_alias" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0)])
(*emit_constructor_arguments:*)| Pmty_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "module_type_desc" "Pmty_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
(*emit_constructor_arguments:*)| Pmty_typeof((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_expr0) -> ((*P5*)process_generic_type "module_type_desc" "Pmty_typeof" [((*P4*)process_module_expr (*emit_core_type_numbered*)module_expr0)])
                               (*emit_constructor_arguments:*)| Pmty_with((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) ->
  ((*P5*)process_generic_type "module_type_desc" "Pmty_with" [
    ((*P4*)process_module_type (*emit_core_type_numbered*)module_type0);
    ((*P4*)process_with_constraint_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Pmty_functor((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)functor_parameter0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_type1) -> ((*P5*)process_generic_type "module_type_desc" "Pmty_functor" [((*P4*)process_functor_parameter (*emit_core_type_numbered*)functor_parameter0);((*P4*)process_module_type (*emit_core_type_numbered*)module_type1)])
(*emit_constructor_arguments:*)| Pmty_signature((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)signature0) -> ((*P5*)process_generic_type "module_type_desc" "Pmty_signature" [((*P4*)process_signature (*emit_core_type_numbered*)signature0)])
(*emit_constructor_arguments:*)| Pmty_ident((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0) -> ((*P5*)process_generic_type "module_type_desc" "Pmty_ident" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0)])
and process_str_loc_option x = "\"FIXME:process_str_loc_option\""
and process_class_field_kind x :string =match x with
(*emit_constructor_arguments:*)| Cfk_concrete((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)override_flag0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1) -> ((*P5*)process_generic_type "class_field_kind" "Cfk_concrete" [((*P4*)process_override_flag (*emit_core_type_numbered*)override_flag0);((*P4*)process_expression (*emit_core_type_numbered*)expression1)])
(*emit_constructor_arguments:*)| Cfk_virtual((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type0) -> ((*P5*)process_generic_type "class_field_kind" "Cfk_virtual" [((*P4*)process_core_type (*emit_core_type_numbered*)core_type0)])


and  process_class_expr (x:class_expr):string = match x with {pcl_desc(* class_expr_desc*);pcl_loc(* location*);pcl_attributes(* attributes*)} ->
  ((*P2*)process_class_expr_desc pcl_desc)^ "^" ^ ((*P2*)process_location pcl_loc)^ "^" ^ ((*P2*)process_attributes pcl_attributes)
and process_class_field_desc x :string =match x with
(*emit_constructor_arguments:*)| Pcf_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "class_field_desc" "Pcf_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
(*emit_constructor_arguments:*)| Pcf_attribute((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)attribute0) -> ((*P5*)process_generic_type "class_field_desc" "Pcf_attribute" [((*P4*)process_attribute (*emit_core_type_numbered*)attribute0)])
(*emit_constructor_arguments:*)| Pcf_initializer((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0) -> ((*P5*)process_generic_type "class_field_desc" "Pcf_initializer" [((*P4*)process_expression (*emit_core_type_numbered*)expression0)])
                               (*emit_constructor_arguments:*)| Pcf_constraint((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)(*emit_constructor_arguments_item_tuple_core_type_list*)core_type0,(*emit_constructor_arguments_item_tuple_core_type_list*)core_type1) -> ((*P5*)process_generic_type "class_field_desc" "Pcf_constraint" [
    ((*P4*)process_core_type core_type0);
    ((*P4*)process_core_type core_type1)
  ])
(*emit_constructor_arguments:*)
                               | Pcf_method((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)label_loc,
                                                                                                                         private_flag,
                                                                                                                         class_field_kind0) ->
((*P5*)process_generic_type "class_field_desc" "Pcf_method" [
        (*P4*)(process_label_loc label_loc);
        (process_private_flag private_flag);
        (process_class_field_kind class_field_kind0 )
      ])
(*emit_constructor_arguments:*)| Pcf_val((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc,mutable_flag,class_field_kind0) -> "((*P5*)process_generic_type \"class_field_desc\" \"Pcf_val\" [((*P4*)process_loc,mutable_flag,class_field_kind (*emit_core_type_numbered*)loc,mutable_flag,class_field_kind0)])"
                               (*emit_constructor_arguments:*)| Pcf_inherit((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)override_flag0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_expr1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option2) -> ((*P5*)process_generic_type "class_field_desc" "Pcf_inherit" [((*P4*)process_override_flag (*emit_core_type_numbered*)override_flag0);
                                                                                                                                                                                                                                                                                                                                                                                                                        ((*P4*)process_class_expr (*emit_core_type_numbered*)class_expr1);
                                                                                                                                                                                                                                                                                                                                                                                                                        ((*P4*)process_str_loc_option (*emit_core_type_numbered*)option2)])
and process_class_expr_desc x :string =match x with
(*emit_constructor_arguments:*)| Pcl_open((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)open_description0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_expr1) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_open" [((*P4*)process_open_description (*emit_core_type_numbered*)open_description0);((*P4*)process_class_expr (*emit_core_type_numbered*)class_expr1)])
(*emit_constructor_arguments:*)| Pcl_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
                               (*emit_constructor_arguments:*)
                               | Pcl_constraint((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_expr0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_type1) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_constraint" [((*P4*)process_class_expr (*emit_core_type_numbered*)class_expr0);((*P4*)process_class_type (*emit_core_type_numbered*)class_type1)])
                               | Pcl_let((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)rec_flag0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_expr2) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_let" [((*P4*)process_rec_flag (*emit_core_type_numbered*)rec_flag0);((*P4*)process_value_binding_list (*emit_core_type_numbered*)list1);((*P4*)process_class_expr (*emit_core_type_numbered*)class_expr2)])
                               (*emit_constructor_arguments:*)| Pcl_apply((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_expr0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_apply" [((*P4*)process_class_expr (*emit_core_type_numbered*)class_expr0);
                                                                                                                                                                                                                                                                                                                     ((*P4*)process_arg_label_expression_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Pcl_fun((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)arg_label0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern2,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_expr3) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_fun" [((*P4*)process_arg_label (*emit_core_type_numbered*)arg_label0);((*P4*)process_expression_option (*emit_core_type_numbered*)option1);((*P4*)process_pattern (*emit_core_type_numbered*)pattern2);((*P4*)process_class_expr (*emit_core_type_numbered*)class_expr3)])
(*emit_constructor_arguments:*)| Pcl_structure((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_structure0) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_structure" [((*P4*)process_class_structure (*emit_core_type_numbered*)class_structure0)])
(*emit_constructor_arguments:*)| Pcl_constr((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "class_expr_desc" "Pcl_constr" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_core_type_list (*emit_core_type_numbered*)list1)])
and process_class_type_field_desc x :string =match x with
(*emit_constructor_arguments:*)| Pctf_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "class_type_field_desc" "Pctf_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
(*emit_constructor_arguments:*)| Pctf_attribute((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)attribute0) -> ((*P5*)process_generic_type "class_type_field_desc" "Pctf_attribute" [((*P4*)process_attribute (*emit_core_type_numbered*)attribute0)])
                               (*emit_constructor_arguments:*)| Pctf_constraint((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type,core_type0) -> ((*P5*)process_generic_type "class_type_field_desc" "Pctf_constraint" [
    "((*P4*)process_core_type";
    "core_type (*emit_core_type_numbered*)core_type,core_type0)"
  ])
                               (*emit_constructor_arguments:*)| Pctf_method((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc,private_flag,virtual_flag,core_type0) -> ((*P5*)process_generic_type "class_type_field_desc" "Pctf_method" [
    "(*P4*)process_loc";
    "private_flag,virtual_flag,core_type (*emit_core_type_numbered*)loc,private_flag,virtual_flag,core_type0)"
  ])
                               (*emit_constructor_arguments:*)| Pctf_val((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc,mutable_flag,virtual_flag,core_type0) -> ((*P5*)process_generic_type "class_type_field_desc" "Pctf_val" [
    "((*P4*)process_loc";
    "mutable_flag,virtual_flag,core_type (*emit_core_type_numbered*)loc,mutable_flag,virtual_flag,core_type0)"])
(*emit_constructor_arguments:*)| Pctf_inherit((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_type0) -> ((*P5*)process_generic_type "class_type_field_desc" "Pctf_inherit" [((*P4*)process_class_type (*emit_core_type_numbered*)class_type0)])
and process_class_type_desc (x:class_type_desc) :string =
  match x with
  (*emit_constructor_arguments:*)
  | Pcty_open( open_description0,class_type1)
    ->
    ((*P5*)process_generic_type "class_type_desc" "Pcty_open"
            [
              ((*P4*)process_open_description (*emit_core_type_numbered*)open_description0);
              ((*P4*)process_class_type (*emit_core_type_numbered*)class_type1)])

  (*emit_constructor_arguments:*)
  (* | Pcty_extension((\*emit_constructor_arguments_from_core_type_list*\)(\*emit_core_type_numbered*\)extension0) -> ((\*P5*\)process_generic_type "class_type_desc" "Pcty_extension" [((\*P4*\)process_extension (\*emit_core_type_numbered*\)extension0)]) *)
  (* (\*emit_constructor_arguments:*\) *)
  (* | Pcty_arrow((\*emit_constructor_arguments_from_core_type_list*\)(\*emit_core_type_numbered*\)arg_label0,(\*emit_constructor_arguments_from_core_type_list*\)(\*emit_core_type_numbered*\)core_type1,(\*emit_constructor_arguments_from_core_type_list*\)(\*emit_core_type_numbered*\)class_type2) -> ((\*P5*\)process_generic_type "class_type_desc" "Pcty_arrow" [((\*P4*\)process_arg_label (\*emit_core_type_numbered*\)arg_label0);((\*P4*\)process_core_type (\*emit_core_type_numbered*\)core_type1);((\*P4*\)process_class_type (\*emit_core_type_numbered*\)class_type2)]) *)
(* (\*emit_constructor_arguments:*\)
   | Pcty_signature((\*emit_constructor_arguments_from_core_type_list*\)(\*emit_core_type_numbered*\)class_signature0) -> ((\*P5*\)process_generic_type "class_type_desc" "Pcty_signature" [((\*P4*\)process_class_signature (\*emit_core_type_numbered*\)class_signature0)]) *)
(* (\*emit_constructor_arguments:*\)| Pcty_constr((\*emit_constructor_arguments_from_core_type_list*\)(\*emit_core_type_numbered*\)longident_loc0,(\*emit_constructor_arguments_from_core_type_list*\)(\*emit_core_type_numbered*\)list1) -> ((\*P5*\)process_generic_type "class_type_desc" "Pcty_constr" [((\*P4*\)process_longident_loc (\*emit_core_type_numbered*\)longident_loc0);((\*P4*\)process_core_type_list (\*emit_core_type_numbered*\)list1)]) *)
and process_extension_constructor_kind x :string =match x with
(*emit_constructor_arguments:*)| Pext_rebind((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0) -> ((*P5*)process_generic_type "extension_constructor_kind" "Pext_rebind" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0)])
                               (*emit_constructor_arguments:*)
                               | Pext_decl((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)constructor_arguments1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option2) ->
(
  (*P5*)process_generic_type "extension_constructor_kind" "Pext_decl" [
    ((*P4*)process_string_loc_list (*emit_core_type_numbered*)list0);
    ((*P4*)process_constructor_arguments (*emit_core_type_numbered*)constructor_arguments1);((*P4*)process_core_type_option (*emit_core_type_numbered*)option2)])
and process_constructor_arguments x :string =match x with
  (*emit_constructor_arguments:*)
  | Pcstr_record((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "constructor_arguments" "Pcstr_record" [
    ((*P4*)process_label_declaration_list (*emit_core_type_numbered*)list0)])
  (*emit_constructor_arguments:*)| Pcstr_tuple((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "constructor_arguments"
                                                                                                                                              "Pcstr_tuple" [
                                                                                                                                              ((*P4*)process_core_type_list (*emit_core_type_numbered*)list0)])
and process_type_kind x :string =match x with
(*emit_constructor_arguments:*)| Ptype_open -> ((*P5*)process_generic_type "type_kind" "Ptype_open" [])
(*emit_constructor_arguments:*)| Ptype_record((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "type_kind" "Ptype_record" [((*P4*)process_label_declaration_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Ptype_variant((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "type_kind" "Ptype_variant" [((*P4*)process_constructor_declaration_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Ptype_abstract -> ((*P5*)process_generic_type "type_kind" "Ptype_abstract" [])
and process_expression_desc x :string =match x with
(*emit_constructor_arguments:*)| Pexp_unreachable -> ((*P5*)process_generic_type "expression_desc" "Pexp_unreachable" [])
(*emit_constructor_arguments:*)| Pexp_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
(*emit_constructor_arguments:*)| Pexp_letop((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)letop0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_letop" [((*P4*)process_letop (*emit_core_type_numbered*)letop0)])
(*emit_constructor_arguments:*)| Pexp_open((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)open_declaration0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_open" [((*P4*)process_open_declaration (*emit_core_type_numbered*)open_declaration0);((*P4*)process_expression (*emit_core_type_numbered*)expression1)])
(*emit_constructor_arguments:*)| Pexp_pack((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_expr0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_pack" [((*P4*)process_module_expr (*emit_core_type_numbered*)module_expr0)])
(*emit_constructor_arguments:*)| Pexp_newtype((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_newtype" [((*P4*)process_loc (*emit_core_type_numbered*)loc0);((*P4*)process_expression (*emit_core_type_numbered*)expression1)])
(*emit_constructor_arguments:*)| Pexp_object((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)class_structure0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_object" [((*P4*)process_class_structure (*emit_core_type_numbered*)class_structure0)])
(*emit_constructor_arguments:*)| Pexp_poly((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_poly" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_core_type_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| Pexp_lazy((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_lazy" [((*P4*)process_expression (*emit_core_type_numbered*)expression0)])
(*emit_constructor_arguments:*)| Pexp_assert((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_assert" [((*P4*)process_expression (*emit_core_type_numbered*)expression0)])
(*emit_constructor_arguments:*)| Pexp_letexception((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension_constructor0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_
letexception" [((*P4*)process_extension_constructor (*emit_core_type_numbered*)extension_constructor0);((*P4*)process_expression (*emit_core_type_numbered*)expression1)])
                               (*emit_constructor_arguments:*)| Pexp_letmodule((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)module_expr1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression2) -> ((*P5*)process_generic_type "expression_desc" "Pexp_letmodule" [
    ((*P4*)process_string_option_loc (*emit_core_type_numbered*)loc0);
    ((*P4*)process_module_expr (*emit_core_type_numbered*)module_expr1);((*P4*)process_expression (*emit_core_type_numbered*)expression2)])
(*emit_constructor_arguments:*)| Pexp_override((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_override" [((*P4*)process_label_loc_expression_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Pexp_setinstvar((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_setinstvar" [((*P4*)process_loc (*emit_core_type_numbered*)loc0);((*P4*)process_expression (*emit_core_type_numbered*)expression1)])
(*emit_constructor_arguments:*)| Pexp_new((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_new" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0)])
(*emit_constructor_arguments:*)| Pexp_send((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_send" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_loc (*emit_core_type_numbered*)loc1)])
(*emit_constructor_arguments:*)| Pexp_coerce((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type2) -> ((*P5*)process_generic_type "expression_desc" "Pexp_coerce" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_core_type_option (*emit_core_type_numbered*)option1);((*P4*)process_core_type (*emit_core_type_numbered*)core_type2)])
(*emit_constructor_arguments:*)| Pexp_constraint((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_constraint" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_core_type (*emit_core_type_numbered*)core_type1)])
(*emit_constructor_arguments:*)| Pexp_for((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression2,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)direction_flag3,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression4) -> ((*P5*)process_generic_type "expression_desc" "Pexp_for" [((*P4*)process_pattern (*emit_core_type_numbered*)pattern0);((*P4*)process_expression (*emit_core_type_numbered*)expression1);((*P4*)process_expression (*emit_core_type_numbered*)expression2);((*P4*)process_direction_flag (*emit_core_type_numbered*)direction_flag3);((*P4*)process_expression (*emit_core_type_numbered*)expression4)])
(*emit_constructor_arguments:*)| Pexp_while((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_while" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_expression (*emit_core_type_numbered*)expression1)])
(*emit_constructor_arguments:*)| Pexp_sequence((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_sequence" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_expression (*emit_core_type_numbered*)expression1)])
(*emit_constructor_arguments:*)| Pexp_ifthenelse((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option2) -> ((*P5*)process_generic_type "expression_desc" "Pexp_ifthenelse" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_expression (*emit_core_type_numbered*)expression1);((*P4*)process_expression_option (*emit_core_type_numbered*)option2)])
                               (*emit_constructor_arguments:*)| Pexp_array((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_array" [
    ((*P4*)process_expression_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Pexp_setfield((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression2) -> ((*P5*)process_generic_type "expression_desc" "Pexp_setfield" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc1);((*P4*)process_expression (*emit_core_type_numbered*)expression2)])
(*emit_constructor_arguments:*)| Pexp_field((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_field" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc1)])
(*emit_constructor_arguments:*)| Pexp_record((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_record" [((*P4*)process_longident_loc_expression_list (*emit_core_type_numbered*)list0);((*P4*)process_expression_option (*emit_core_type_numbered*)option1)])
                               (*emit_constructor_arguments:*)| Pexp_variant((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)label0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_variant" [
    ((*P4*)process_label (*emit_core_type_numbered*)label0);
    ((*P4*)process_expression_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| Pexp_construct((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_construct" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_expression_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| Pexp_tuple((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_tuple" [((*P4*)process_expression_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Pexp_try((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)cases1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_try" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_cases (*emit_core_type_numbered*)cases1)])
(*emit_constructor_arguments:*)| Pexp_match((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)cases1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_match" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_cases (*emit_core_type_numbered*)cases1)])
(*emit_constructor_arguments:*)| Pexp_apply((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "expression_desc" "Pexp_apply" [((*P4*)process_expression (*emit_core_type_numbered*)expression0);((*P4*)process_arg_label_expression_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Pexp_fun((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)arg_label0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern2,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression3) -> ((*P5*)process_generic_type "expression_desc" "Pexp_fun" [((*P4*)process_arg_label (*emit_core_type_numbered*)arg_label0);((*P4*)process_expression_option (*emit_core_type_numbered*)option1);((*P4*)process_pattern (*emit_core_type_numbered*)pattern2);((*P4*)process_expression (*emit_core_type_numbered*)expression3)])
(*emit_constructor_arguments:*)| Pexp_function((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)cases0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_function" [((*P4*)process_cases (*emit_core_type_numbered*)cases0)])
(*emit_constructor_arguments:*)| Pexp_let((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)rec_flag0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)expression2) -> ((*P5*)process_generic_type "expression_desc" "Pexp_let" [((*P4*)process_rec_flag (*emit_core_type_numbered*)rec_flag0);((*P4*)process_value_binding_list (*emit_core_type_numbered*)list1);((*P4*)process_expression (*emit_core_type_numbered*)expression2)])
(*emit_constructor_arguments:*)| Pexp_constant((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)constant0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_constant" [((*P4*)process_constant (*emit_core_type_numbered*)constant0)])
(*emit_constructor_arguments:*)| Pexp_ident((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0) -> ((*P5*)process_generic_type "expression_desc" "Pexp_ident" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0)])
and process_pattern_desc x :string =match x with
(*emit_constructor_arguments:*)| Ppat_open((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_open" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_pattern (*emit_core_type_numbered*)pattern1)])
(*emit_constructor_arguments:*)| Ppat_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
(*emit_constructor_arguments:*)| Ppat_exception((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_exception" [((*P4*)process_pattern (*emit_core_type_numbered*)pattern0)])
                               (*emit_constructor_arguments:*)| Ppat_unpack((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_unpack" [
    ((*P4*)process_string_option_loc (*emit_core_type_numbered*)loc0)
  ])
(*emit_constructor_arguments:*)| Ppat_lazy((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_lazy" [((*P4*)process_pattern (*emit_core_type_numbered*)pattern0)])
(*emit_constructor_arguments:*)| Ppat_type((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_type" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0)])
(*emit_constructor_arguments:*)| Ppat_constraint((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_constraint" [((*P4*)process_pattern (*emit_core_type_numbered*)pattern0);((*P4*)process_core_type (*emit_core_type_numbered*)core_type1)])
(*emit_constructor_arguments:*)| Ppat_or((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_or" [((*P4*)process_pattern (*emit_core_type_numbered*)pattern0);((*P4*)process_pattern (*emit_core_type_numbered*)pattern1)])
(*emit_constructor_arguments:*)
| Ppat_array(
    (*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) ->
  ((*P5*)process_generic_type "pattern_desc" "Ppat_array" [
            ((*P4*)process_pattern_list list0)
          ])
(*emit_constructor_arguments:*)| Ppat_record((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)closed_flag1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_record" [((*P4*)process_longident_loc_pattern_list (*emit_core_type_numbered*)list0);((*P4*)process_closed_flag (*emit_core_type_numbered*)closed_flag1)])
(*emit_constructor_arguments:*)| Ppat_variant((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)label0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_variant" [((*P4*)process_label (*emit_core_type_numbered*)label0);((*P4*)process_pattern_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| Ppat_construct((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_construct" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_string_loc_list_pattern_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| Ppat_tuple((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_tuple" [((*P4*)process_pattern_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Ppat_interval((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)constant0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)constant1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_interval" [((*P4*)process_constant (*emit_core_type_numbered*)constant0);((*P4*)process_constant (*emit_core_type_numbered*)constant1)])
(*emit_constructor_arguments:*)| Ppat_constant((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)constant0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_constant" [((*P4*)process_constant (*emit_core_type_numbered*)constant0)])
(*emit_constructor_arguments:*)| Ppat_alias((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc1) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_alias" [((*P4*)process_pattern (*emit_core_type_numbered*)pattern0);((*P4*)process_loc (*emit_core_type_numbered*)loc1)])
(*emit_constructor_arguments:*)| Ppat_var((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)loc0) -> ((*P5*)process_generic_type "pattern_desc" "Ppat_var" [((*P4*)process_loc (*emit_core_type_numbered*)loc0)])
(*emit_constructor_arguments:*)| Ppat_any -> ((*P5*)process_generic_type "pattern_desc" "Ppat_any" [])
and process_core_type_desc x :string =match x with
(*emit_constructor_arguments:*)| Ptyp_extension((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)extension0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_extension" [((*P4*)process_extension (*emit_core_type_numbered*)extension0)])
(*emit_constructor_arguments:*)| Ptyp_package((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)package_type0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_package" [((*P4*)process_package_type (*emit_core_type_numbered*)package_type0)])
(*emit_constructor_arguments:*)| Ptyp_poly((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_poly" [((*P4*)process_string_loc_list (*emit_core_type_numbered*)list0);((*P4*)process_core_type (*emit_core_type_numbered*)core_type1)])
                               (*emit_constructor_arguments:*)| Ptyp_variant((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)closed_flag1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option2) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_variant" [((*P4*)process_row_field_list (*emit_core_type_numbered*)list0);((*P4*)process_closed_flag (*emit_core_type_numbered*)closed_flag1);
                                                                                                                                                                                                                                                                                                                                                                                                                ((*P4*)process_string_list_option (*emit_core_type_numbered*)option2)])
         (*emit_constructor_arguments:*)| Ptyp_alias((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_alias" [((*P4*)process_core_type (*emit_core_type_numbered*)core_type0);((*P4*)process_string (*emit_core_type_numbered*)string1)])
(*emit_constructor_arguments:*)| Ptyp_class((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_class" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_core_type_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Ptyp_object((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)closed_flag1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_object" [((*P4*)process_object_field_list (*emit_core_type_numbered*)list0);((*P4*)process_closed_flag (*emit_core_type_numbered*)closed_flag1)])
(*emit_constructor_arguments:*)| Ptyp_constr((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident_loc0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list1) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_constr" [((*P4*)process_longident_loc (*emit_core_type_numbered*)longident_loc0);((*P4*)process_core_type_list (*emit_core_type_numbered*)list1)])
(*emit_constructor_arguments:*)| Ptyp_tuple((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)list0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_tuple" [((*P4*)process_core_type_list (*emit_core_type_numbered*)list0)])
(*emit_constructor_arguments:*)| Ptyp_arrow((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)arg_label0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type2) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_arrow" [((*P4*)process_arg_label (*emit_core_type_numbered*)arg_label0);((*P4*)process_core_type (*emit_core_type_numbered*)core_type1);((*P4*)process_core_type (*emit_core_type_numbered*)core_type2)])
(*emit_constructor_arguments:*)| Ptyp_var((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0) -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_var" [((*P4*)process_string (*emit_core_type_numbered*)string0)])
(*emit_constructor_arguments:*)| Ptyp_any -> ((*P5*)process_generic_type "core_type_desc" "Ptyp_any" [])
and process_payload x :string =match x with
(*emit_constructor_arguments:*)| PPat((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)pattern0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "payload" "PPat" [((*P4*)process_pattern (*emit_core_type_numbered*)pattern0);((*P4*)process_expression_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| PTyp((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)core_type0) -> ((*P5*)process_generic_type "payload" "PTyp" [((*P4*)process_core_type (*emit_core_type_numbered*)core_type0)])
(*emit_constructor_arguments:*)| PSig((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)signature0) -> ((*P5*)process_generic_type "payload" "PSig" [((*P4*)process_signature (*emit_core_type_numbered*)signature0)])
(*emit_constructor_arguments:*)| PStr((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)structure0) -> ((*P5*)process_generic_type "payload" "PStr" [((*P4*)process_structure (*emit_core_type_numbered*)structure0)])
and process_constant x :string =match x with
(*emit_constructor_arguments:*)| Pconst_float((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "constant" "Pconst_float" [((*P4*)process_string (*emit_core_type_numbered*)string0);((*P4*)process_char_option (*emit_core_type_numbered*)option1)])
(*emit_constructor_arguments:*)| Pconst_string((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)location1,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option2) -> ((*P5*)process_generic_type "constant" "Pconst_string" [((*P4*)process_string (*emit_core_type_numbered*)string0);((*P4*)process_location (*emit_core_type_numbered*)location1);((*P4*)process_string_option (*emit_core_type_numbered*)option2)])
(*emit_constructor_arguments:*)| Pconst_char((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)char0) -> ((*P5*)process_generic_type "constant" "Pconst_char" [((*P4*)process_char (*emit_core_type_numbered*)char0)])
(*emit_constructor_arguments:*)| Pconst_integer((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)option1) -> ((*P5*)process_generic_type "constant" "Pconst_integer" [((*P4*)process_string (*emit_core_type_numbered*)string0);((*P4*)process_char_option (*emit_core_type_numbered*)option1)])
and process_injectivity x :string =match x with
(*emit_constructor_arguments:*)| NoInjectivity -> ((*P5*)process_generic_type "injectivity" "NoInjectivity" [])
(*emit_constructor_arguments:*)| Injective -> ((*P5*)process_generic_type "injectivity" "Injective" [])
and process_variance x :string =match x with
(*emit_constructor_arguments:*)| NoVariance -> ((*P5*)process_generic_type "variance" "NoVariance" [])
(*emit_constructor_arguments:*)| Contravariant -> ((*P5*)process_generic_type "variance" "Contravariant" [])
(*emit_constructor_arguments:*)| Covariant -> ((*P5*)process_generic_type "variance" "Covariant" [])
and process_arg_label x :string =match x with
(*emit_constructor_arguments:*)| Optional((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0) -> ((*P5*)process_generic_type "arg_label" "Optional" [((*P4*)process_string (*emit_core_type_numbered*)string0)])
(*emit_constructor_arguments:*)| Labelled((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0) -> ((*P5*)process_generic_type "arg_label" "Labelled" [((*P4*)process_string (*emit_core_type_numbered*)string0)])
(*emit_constructor_arguments:*)| Nolabel -> ((*P5*)process_generic_type "arg_label" "Nolabel" [])
and process_closed_flag x :string =match x with
(*emit_constructor_arguments:*)| Open -> ((*P5*)process_generic_type "closed_flag" "Open" [])
(*emit_constructor_arguments:*)| Closed -> ((*P5*)process_generic_type "closed_flag" "Closed" [])


and process_virtual_flag x :string =match x with
(*emit_constructor_arguments:*)| Concrete -> ((*P5*)process_generic_type "virtual_flag" "Concrete" [])
(*emit_constructor_arguments:*)| Virtual -> ((*P5*)process_generic_type "virtual_flag" "Virtual" [])
and process_mutable_flag x :string =match x with
(*emit_constructor_arguments:*)| Mutable -> ((*P5*)process_generic_type "mutable_flag" "Mutable" [])
(*emit_constructor_arguments:*)| Immutable -> ((*P5*)process_generic_type "mutable_flag" "Immutable" [])
and process_private_flag x :string =match x with
(*emit_constructor_arguments:*)| Public -> ((*P5*)process_generic_type "private_flag" "Public" [])
(*emit_constructor_arguments:*)| Private -> ((*P5*)process_generic_type "private_flag" "Private" [])
and process_direction_flag x :string =match x with
(*emit_constructor_arguments:*)| Downto -> ((*P5*)process_generic_type "direction_flag" "Downto" [])
(*emit_constructor_arguments:*)| Upto -> ((*P5*)process_generic_type "direction_flag" "Upto" [])
and process_rec_flag x :string =match x with
(*emit_constructor_arguments:*)| Recursive -> ((*P5*)process_generic_type "rec_flag" "Recursive" [])
(*emit_constructor_arguments:*)| Nonrecursive -> ((*P5*)process_generic_type "rec_flag" "Nonrecursive" [])
and process_longident x :string =match x with
(*emit_constructor_arguments:*)| Lapply((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident1) -> ((*P5*)process_generic_type "longident" "Lapply" [((*P4*)process_longident (*emit_core_type_numbered*)longident0);((*P4*)process_longident (*emit_core_type_numbered*)longident1)])
(*emit_constructor_arguments:*)| Ldot((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)longident0,(*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string1) -> ((*P5*)process_generic_type "longident" "Ldot" [((*P4*)process_longident (*emit_core_type_numbered*)longident0);((*P4*)process_string (*emit_core_type_numbered*)string1)])
(*emit_constructor_arguments:*)| Lident((*emit_constructor_arguments_from_core_type_list*)(*emit_core_type_numbered*)string0) -> ((*P5*)process_generic_type "longident" "Lident" [((*P4*)process_string (*emit_core_type_numbered*)string0)])
and process_position (x:position):string ="(pos)"
  (* match x with {pos_fname(\* string*\);pos_lnum(\* int*\);pos_bol(\* int*\);pos_cnum(\* int*\)} -> *)
  (*   "(pos " ^ *)
  (*   ((\*P2*\)process_string pos_fname)^ "^" ^ ((\*P2*\)process_int pos_lnum)^ "^" ^ ((\*P2*\)process_int pos_bol)^ "^" ^ ((\*P2*\)process_int pos_cnum) ^ ")" *)
                                                                                                                                                                                    
and process_location (x:location):string = "(loc2)"
  (* (\* (print_endline ("process_location")) ; *\) *)
  (* "(process_location " ^ *)
  (* match x with {loc_start(\* position*\);loc_end(\* position*\);loc_ghost(\* bool*\)} ->((\*P2*\)process_position loc_start)^ "^" ^ ((\*P2*\)process_position loc_end)^ "^" ^ ((\*P2*\)process_bool loc_ghost) *)
  (*                                                                                                                                                       ^ ")" *)

and process_loc_abstract x b = match x with
    {txt(* FIXME*);loc(* location*)} ->
    ((*P2*)b txt) (* "^" ^ ((\*P2*\)process_location loc) *)
and process_loc_longident x b = process_loc_abstract x process_longident

and process_string_loc (x:string loc):string  =
  match x with {txt(* FIXME*);loc(* location*)} ->
    (process_string(*P2*) txt)
    ^ "^" ^ ((*P2*)process_location loc)
and process_attribute (x:attribute):string = match x with {attr_name(* loc*);attr_payload(* payload*);attr_loc(* location*)}
  ->
  ((*P2*)process_loc attr_name)
  ^ "^" ^ ((*P2*)process_payload attr_payload)^ "^" ^ ((*P2*)process_location attr_loc)
and process_extension (x:extension):string = process_extension x
and process_core_type (x:core_type):string = match x with {ptyp_desc(* core_type_desc*);ptyp_loc(* location*);ptyp_loc_stack(* location_stack*);ptyp_attributes(* attributes*)} ->((*P2*)process_core_type_desc ptyp_desc)^ "^" ^ ((*P2*)process_location ptyp_loc)^ "^" ^ ((*P2*)process_location_stack ptyp_loc_stack)^ "^" ^ ((*P2*)process_attributes ptyp_attributes)
and process_row_field (x:row_field):string = match x with {prf_desc(* row_field_desc*);prf_loc(* location*);prf_attributes(* attributes*)} ->((*P2*)process_row_field_desc prf_desc)^ "^" ^ ((*P2*)process_location prf_loc)^ "^" ^ ((*P2*)process_attributes prf_attributes)

and process_object_field (x:object_field):string = match x with {pof_desc(* object_field_desc*);pof_loc(* location*);pof_attributes(* attributes*)} ->((*P2*)process_object_field_desc pof_desc)^ "^" ^ ((*P2*)process_location pof_loc)^ "^" ^ ((*P2*)process_attributes pof_attributes)

and process_pattern (x:pattern):string = match x with {ppat_desc(* pattern_desc*);ppat_loc(* location*);ppat_loc_stack(* location_stack*);ppat_attributes(* attributes*)} ->((*P2*)process_pattern_desc ppat_desc)^ "^" ^ ((*P2*)process_location ppat_loc)^ "^" ^ ((*P2*)process_location_stack ppat_loc_stack)^ "^" ^ ((*P2*)process_attributes ppat_attributes)

and process_expression (x:expression):string = match x with {pexp_desc(* expression_desc*);pexp_loc(* location*);pexp_loc_stack(* location_stack*);pexp_attributes(* attributes*)} ->((*P2*)process_expression_desc pexp_desc)^ "^" ^ ((*P2*)process_location pexp_loc)^ "^" ^ ((*P2*)process_location_stack pexp_loc_stack)^ "^" ^ ((*P2*)process_attributes pexp_attributes)

and process_case (x:case):string = match x with {pc_lhs(* pattern*);pc_guard(* option*);pc_rhs(* expression*)}
  ->
  ((*P2*)process_pattern pc_lhs)^
                                                                                                                           ((*P2*)process_expression_option pc_guard)
                                                                                                                           ^ "^" ^ ((*P2*)process_expression pc_rhs)

and process_vars_list a =
  process_generic_list "process_var_list" a process_string_loc
and process_binding_op_list a =
  process_generic_list "process_binding_op_list" a process_binding_op
and process_prim_list a =
    process_generic_list "process_prim" a process_string
and process_cstr x  = "\"FIXME:cstr(core_type * core_type * location)\""
and process_cstrs_list a =
  process_generic_list "process_cstrs" a process_cstr
and process_params x  = "\"FIXME:process_params(core_type * (variance * injectivity))\""
and process_params_list a =
  process_generic_list "process_params" a process_params
and process_letop (x:letop):string = match x with {let_(* binding_op*);ands(* list*);body(* expression*)} ->
  (
    (*P2*)process_binding_op let_)
  ^ "^" ^ ((*P2*)process_binding_op_list ands)
  ^ "^" ^ ((*P2*)process_expression body)
and process_binding_op (x:binding_op):string = match x with {pbop_op(* loc*);pbop_pat(* pattern*);pbop_exp(* expression*);pbop_loc(* location*)} ->((*P2*)process_loc pbop_op)^ "^" ^ ((*P2*)process_pattern pbop_pat)^ "^" ^ ((*P2*)process_expression pbop_exp)^ "^" ^ ((*P2*)process_location pbop_loc)
and process_value_description (x:value_description):string = match x with {pval_name(* loc*);pval_type(* core_type*);pval_prim(* list*);pval_attributes(* attributes*);pval_loc(* location*)} ->((*P2*)process_loc pval_name)^ "^" ^ ((*P2*)process_core_type pval_type)^ "^" ^ ((*P2*)process_prim_list pval_prim)^ "^" ^ ((*P2*)process_attributes pval_attributes)^ "^" ^ ((*P2*)process_location pval_loc)
and process_type_declaration (x:type_declaration):string = match x with {ptype_name(* loc*);ptype_params(* list*);ptype_cstrs(* list*);ptype_kind(* type_kind*);ptype_private(* private_flag*);ptype_manifest(* option*);ptype_attributes(* attributes*);ptype_loc(* location*)} ->((*P2*)process_loc ptype_name)^ "^" ^ ((*P2*)process_params_list ptype_params)^ "^" ^ ((*P2*)process_cstrs_list ptype_cstrs)^ "^" ^ ((*P2*)process_type_kind ptype_kind)^ "^" ^ ((*P2*)process_private_flag ptype_private)
                                                                                                                                                                                                                                                                                             ^ "^" ^ ((*P2*)process_manifest_option ptype_manifest)^
                                                                                                                                                                                                                                                                                             ((*P2*)process_attributes ptype_attributes)^
                                                                                                                                                                                                                                                                                             ((*P2*)process_location ptype_loc)

and process_label_declaration (x:label_declaration):string = match x with {pld_name(* loc*);pld_mutable(* mutable_flag*);pld_type(* core_type*);pld_loc(* location*);pld_attributes(* attributes*)} ->((*P2*)process_loc pld_name)^ "^" ^ ((*P2*)process_mutable_flag pld_mutable)^ "^" ^ ((*P2*)process_core_type pld_type)^ "^" ^ ((*P2*)process_location pld_loc)^ "^" ^ ((*P2*)process_attributes pld_attributes)

and process_constructor_declaration (x:constructor_declaration):string = match x with {pcd_name(* loc*);pcd_vars(* list*);pcd_args(* constructor_arguments*);pcd_res(* option*);pcd_loc(* location*);pcd_attributes(* attributes*)} ->(
    (*P2*)process_loc pcd_name)^ "^" ^ ((*P2*)process_vars_list pcd_vars)
    ^ "^" ^ ((*P2*)process_constructor_arguments pcd_args)
    ^ "^" ^ ((*P2*)process_core_type_option pcd_res)
    ^ "^" ^ ((*P2*)process_location pcd_loc)
    ^ "^" ^ ((*P2*)process_attributes pcd_attributes)

and process_type_extension (x:type_extension):string = match x with {ptyext_path(* longident_loc*);ptyext_params(* list*);ptyext_constructors(* list*);ptyext_private(* private_flag*);ptyext_loc(* location*);ptyext_attributes(* attributes*)} ->((*P2*)process_longident_loc ptyext_path)^ "^" ^ ((*P2*)process_params_list ptyext_params)
                                                                                                                                                                                                               ^ "^" ^ ((*P2*)process_extension_constructor_list ptyext_constructors)^ "^" ^ ((*P2*)process_private_flag ptyext_private)^ "^" ^ ((*P2*)process_location ptyext_loc)^ "^" ^ ((*P2*)process_attributes ptyext_attributes)
and process_extension_constructor (x:extension_constructor):string = match x with {pext_name(* loc*);pext_kind(* extension_constructor_kind*);pext_loc(* location*);pext_attributes(* attributes*)} ->((*P2*)process_loc pext_name)^ "^" ^ ((*P2*)process_extension_constructor_kind pext_kind)^ "^" ^ ((*P2*)process_location pext_loc)^ "^" ^ ((*P2*)process_attributes pext_attributes)
and process_type_exception (x:type_exception):string = match x with {ptyexn_constructor(* extension_constructor*);ptyexn_loc(* location*);ptyexn_attributes(* attributes*)} ->((*P2*)process_extension_constructor ptyexn_constructor)^ "^" ^ ((*P2*)process_location ptyexn_loc)^ "^" ^ ((*P2*)process_attributes ptyexn_attributes)


and process_class_signature (x:class_signature):string = match x with {pcsig_self(* core_type*);pcsig_fields(* list*)} ->((*P2*)process_core_type pcsig_self)^ "^" ^ ((*P2*)process_class_type_fields_list pcsig_fields)
and process_class_type_field (x:class_type_field):string = match x with {pctf_desc(* class_type_field_desc*);pctf_loc(* location*);pctf_attributes(* attributes*)} ->((*P2*)process_class_type_field_desc pctf_desc)^ "^" ^ ((*P2*)process_location pctf_loc)^ "^" ^ ((*P2*)process_attributes pctf_attributes)

and process_class_infos x:string = match x with {pci_virt(* virtual_flag*);pci_params(* list*);pci_name(* loc*);pci_expr(* FIXME*);pci_loc(* location*);pci_attributes(* attributes*)} ->((*P2*)process_virtual_flag pci_virt)^ "^" ^ ((*P2*)process_params_list pci_params)^ "^" ^ ((*P2*)process_loc pci_name)
                                                                                                                                                                                                   ^ "^" ^ ((*P2*)"process_FIXME pci_expr")
                                                                                                                                                                                                   ^ "^" ^ ((*P2*)process_location pci_loc)^ "^" ^ ((*P2*)process_attributes pci_attributes)

and process_class_expr_class_infos x:string = match x with {pci_virt(* virtual_flag*);pci_params(* list*);pci_name(* loc*);pci_expr(* FIXME*);pci_loc(* location*);pci_attributes(* attributes*)} ->((*P2*)process_virtual_flag pci_virt)^ "^" ^ ((*P2*)process_params_list pci_params)^ "^" ^ ((*P2*)process_loc pci_name)
                                                                                                                                                                                                   ^(process_class_expr pci_expr)
                                                                                                                                                                                                   ^ "^" ^ ((*P2*)process_location pci_loc)^ "^" ^ ((*P2*)process_attributes pci_attributes)

and process_class_structure (x:class_structure):string = match x with {pcstr_self(* pattern*);pcstr_fields(* list*)} ->((*P2*)process_pattern pcstr_self)
                                                                                    ^ "^" ^ ((*P2*)process_class_fields_list pcstr_fields)

and process_class_field (x:class_field):string = match x with {pcf_desc(* class_field_desc*);pcf_loc(* location*);pcf_attributes(* attributes*)} ->((*P2*)process_class_field_desc pcf_desc)^ "^" ^ ((*P2*)process_location pcf_loc)^ "^" ^ ((*P2*)process_attributes pcf_attributes)

and process_module_type (x:module_type):string = match x with {pmty_desc(* module_type_desc*);pmty_loc(* location*);pmty_attributes(* attributes*)} ->((*P2*)process_module_type_desc pmty_desc)^ "^" ^ ((*P2*)process_location pmty_loc)^ "^" ^ ((*P2*)process_attributes pmty_attributes)


and process_signature ( a:signature):string=
  process_generic_list "(*P61*)process_signature" a process_signature_item

and process_signature_item (x:signature_item):string = match x with {psig_desc(* signature_item_desc*);psig_loc(* location*)} ->((*P2*)process_signature_item_desc psig_desc)^ "^" ^ ((*P2*)process_location psig_loc)

and process_module_declaration (x:module_declaration):string = match x with {pmd_name(* loc*);pmd_type(* module_type*);pmd_attributes(* attributes*);pmd_loc(* location*)} ->
  ((*P2*)process_string_option_loc pmd_name)^
  ((*P2*)process_module_type pmd_type)^ "^" ^ ((*P2*)process_attributes pmd_attributes)^ "^" ^ ((*P2*)process_location pmd_loc)
and process_module_substitution (x:module_substitution):string = match x with {pms_name(* loc*);pms_manifest(* longident_loc*);pms_attributes(* attributes*);pms_loc(* location*)} ->((*P2*)process_loc pms_name)^ "^" ^ ((*P2*)process_longident_loc pms_manifest)^ "^" ^ ((*P2*)process_attributes pms_attributes)^ "^" ^ ((*P2*)process_location pms_loc)

and process_open_infos (x(*:open_infos*)):string = match x with {popen_expr(* FIXME*);popen_override(* override_flag*);popen_loc(* location*);popen_attributes(* attributes*)} ->
  "((*P2*)process_FIXME popen_expr)"
  ^ "^" ^ ((*P2*)process_override_flag popen_override)^ "^" ^ ((*P2*)process_location popen_loc)^ "^" ^ ((*P2*)process_attributes popen_attributes)

and process_module_expr_include_infos (x(*:include_infos*)):string = match x with {pincl_mod(* FIXME*);pincl_loc(* location*);pincl_attributes(* attributes*)} ->
  ((*P2*)process_module_expr pincl_mod)
  ^ "^" ^ ((*P2*)process_location pincl_loc)^ "^" ^ ((*P2*)process_attributes pincl_attributes)

(* and process_include_infos (x(\*:include_infos*\)):string = match x with {pincl_mod(\* FIXME*\);pincl_loc(\* location*\);pincl_attributes(\* attributes*\)} -> *)
(*   ((\*P2*\)process_ pincl_mod) *)
(*   ^ "^" ^ ((\*P2*\)process_location pincl_loc)^ "^" ^ ((\*P2*\)process_attributes pincl_attributes) *)



and process_module_expr (x:module_expr):string = match x with {pmod_desc(* module_expr_desc*);pmod_loc(* location*);pmod_attributes(* attributes*)} ->((*P2*)process_module_expr_desc pmod_desc)^ "^" ^ ((*P2*)process_location pmod_loc)^ "^" ^ ((*P2*)process_attributes pmod_attributes)
and process_structure (x:structure):string =
    process_generic_list "process_structure" x process_structure_item
and process_structure_item (x:structure_item):string =
  (* (print_endline "process_structure_item") ; *)
  match x with {
    pstr_desc;
    pstr_loc
  } ->
    ((*P2*)process_structure_item_desc pstr_desc)^ "^" ^ ((*P2*)process_location pstr_loc) 
and process_value_binding (x:value_binding):string = match x with {pvb_pat(* pattern*);pvb_expr(* expression*);pvb_attributes(* attributes*);pvb_loc(* location*)} ->((*P2*)process_pattern pvb_pat)^ "^" ^ ((*P2*)process_expression pvb_expr)^ "^" ^ ((*P2*)process_attributes pvb_attributes)^ "^" ^ ((*P2*)process_location pvb_loc)

and process_module_binding (x:module_binding):string = match x with {pmb_name(* loc*);pmb_expr(* module_expr*);pmb_attributes(* attributes*);pmb_loc(* location*)} ->((*P2*)process_string_option_loc pmb_name)^ "^" ^ ((*P2*)process_module_expr pmb_expr)^ "^" ^ ((*P2*)process_attributes pmb_attributes)^ "^" ^ ((*P2*)process_location pmb_loc)



and process_toplevel_directive (x:toplevel_directive):string = match x with {pdir_name(* loc*);pdir_arg(* option*);pdir_loc(* location*)} ->
  ((*P2*)process_loc pdir_name)
  ^ "^" ^ ((*P2*)process_directive_argument_option pdir_arg)^ "^" ^ ((*P2*)process_location pdir_loc)

