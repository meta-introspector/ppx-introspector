
let process_var_list x = "process_var_list"
let process_arg_constructor_declaration x = "process_arg_constructor_declaration"
let process_label_declaration_list x = "process_label_declaration_list"
let process_params x = "process_params"
let process_cstrs x = "process_cstrs"
let process_core_type_list x = "process_core_type_list"
let process_type_declaration_list x = "process_type_declaration_list"
let loc = "loc"
let loc2  = "loc"
let loc_stack  = "loc"
let process_generic_type (a:string) (b:string) (c:string list):string = "process_generic_type"
let process_loc a = "process_loc"
let ident a = "ident"

let process_string_loc_list_pattern_option x =
  "process_string_loc_list_pattern_option"

let process_arg_label_expression x y = "process_arg_label_expression"
let process_expression_list x = "process_expression_list"
let process_arg_label_expression_list a = "process_arg_label_expression_list"
let process_location a = "process_loc"
let process_location_stack a = "process_loc"
let attributes = "attributes"
let process_value_binding_list = "process_value_binding_list"
let pos a = "process_loc"
let b a = "process_loc"
let mint a = "process_loc"
let process_string a = "process_string"
let process_attribute_list a = "process_attribute_list"
let process_string_option = "process_attribute_list"
let string a = "process_string"
let process_structure_items x = "process_structure_items" ^ x
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

and process_structure_item x ="process_structure_item"

and process_structure_item_desc x :string = "process_structure_item_desc"


                                                         
let process_structure_items x =
  "let ()=(print_endline " ^
  process_generic_list "process_structure_items" x process_structure_item
  ^ ")"


let () =
  print_endline
    (process_structure_items
       [
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [ ident "Obj.t"; (*L2*) process_core_type_list [] ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list [])
               ];
           ]
         ^ loc2;
         (*L4*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [ ident "__"; (*L2*) process_core_type_list [] ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) ;
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [ ident "__"; (*L2*) process_core_type_list [] ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) ;
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "isofhlevel";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) ;
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_record"
                     [
                       process_label_declaration_list
                         [
                           loc
                           ^ process_generic_type "mutable_flag" "Immutable" []
                           ^ process_generic_type "core_type_desc" "Ptyp_var"
                               [ string "t" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ loc2
                           ^ process_attribute_list [];
                           (*L4*)
                           loc
                           ^ process_generic_type "mutable_flag" "Immutable" []
                           ^ process_generic_type "core_type_desc" "Ptyp_var"
                               [ string "p" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ loc2
                           ^ process_attribute_list [];
                         ];
                     ]
                 ^ process_generic_type "private_flag" "Public" []
                 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_variant"
                     [
                       process_arg_constructor_declaration
                         [
                           loc ^ process_var_list []
                           ^ process_generic_type "constructor_arguments"
                               "Pcstr_tuple"
                               [ process_core_type_list [] ]
                           ^ loc2 ^ process_attribute_list [];
                         ];
                     ]
                 ^ process_generic_type "private_flag" "Public" []
                 ^ (process_attribute_list []) ;
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "x" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_arrow"
                         [
                           process_generic_type "arg_label" "Nolabel" [];
                           (*L2*)
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L1*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "isaprop";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_constr"
                                     [
                                       ident "paths";
                                       (*L2*)
                                       process_core_type_list
                                         [
                                           process_generic_type "core_type_desc"
                                             "Ptyp_var"
                                             [ string "x" ]
                                           ^ loc2 ^ loc_stack
                                           ^ process_attribute_list [];
                                         ];
                                     ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "paths";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "y" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "y" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_constr"
                         [
                           ident "isofhlevel";
                           (*L2*)
                           process_core_type_list
                             [
                               process_generic_type "core_type_desc"
                                 "Ptyp_constr"
                                 [
                                   ident "hfiber";
                                   (*L2*)
                                   process_core_type_list
                                     [
                                       process_generic_type "core_type_desc"
                                         "Ptyp_var"
                                         [ string "x" ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                       (*L4*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_var"
                                         [ string "y" ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                     ];
                                 ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "isofhlevelf";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "y" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [ ident "__"; (*L2*) process_core_type_list [] ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "coq_UU"; (*L2*) process_core_type_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "isaprop";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_constr"
                                     [
                                       ident "__";
                                       (*L2*)
                                       process_core_type_list [];
                                     ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "coq_UU"; (*L2*) process_core_type_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "isofhlevel";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_constr"
                                     [
                                       ident "__";
                                       (*L2*)
                                       process_core_type_list [];
                                     ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [ ident "__"; (*L2*) process_core_type_list [] ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [ ident "__"; (*L2*) process_core_type_list [] ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [ ident "__"; (*L2*) process_core_type_list [] ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "y" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_constr"
                         [
                           ident "hProptoType"; (*L2*) process_core_type_list [];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "t" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_arrow"
                             [
                               process_generic_type "arg_label" "Nolabel" [];
                               (*L2*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "t" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                               (*L1*)
                               process_generic_type "core_type_desc"
                                 "Ptyp_constr"
                                 [
                                   ident "paths";
                                   (*L2*)
                                   process_core_type_list
                                     [
                                       process_generic_type "core_type_desc"
                                         "Ptyp_var"
                                         [ string "t" ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                     ];
                                 ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "y" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_constr"
                         [
                           ident "iscontr";
                           (*L2*)
                           process_core_type_list
                             [
                               process_generic_type "core_type_desc"
                                 "Ptyp_constr"
                                 [
                                   ident "hfiber";
                                   (*L2*)
                                   process_core_type_list
                                     [
                                       process_generic_type "core_type_desc"
                                         "Ptyp_var"
                                         [ string "x" ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                       (*L4*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_var"
                                         [ string "y" ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                     ];
                                 ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_arrow"
                             [
                               process_generic_type "arg_label" "Nolabel" [];
                               (*L2*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "x" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                               (*L1*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "y" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "isweq";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                   (*L4*)
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "y" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "paths";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "a" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "paths";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "b" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "x" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_arrow"
                         [
                           process_generic_type "arg_label" "Nolabel" [];
                           (*L2*)
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L1*)
                           process_generic_type "core_type_desc" "Ptyp_arrow"
                             [
                               process_generic_type "arg_label" "Nolabel" [];
                               (*L2*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "x" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                               (*L1*)
                               process_generic_type "core_type_desc"
                                 "Ptyp_arrow"
                                 [
                                   process_generic_type "arg_label" "Nolabel" [];
                                   (*L2*)
                                   process_generic_type "core_type_desc"
                                     "Ptyp_constr"
                                     [
                                       ident "hProptoType";
                                       (*L2*)
                                       process_core_type_list [];
                                     ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                   (*L1*)
                                   process_generic_type "core_type_desc"
                                     "Ptyp_arrow"
                                     [
                                       process_generic_type "arg_label"
                                         "Nolabel" [];
                                       (*L2*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_constr"
                                         [
                                           ident "hProptoType";
                                           (*L2*)
                                           process_core_type_list [];
                                         ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                       (*L1*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_constr"
                                         [
                                           ident "hProptoType";
                                           (*L2*)
                                           process_core_type_list [];
                                         ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                     ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "x" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_constr"
                         [
                           ident "hProptoType"; (*L2*) process_core_type_list [];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "x" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_arrow"
                         [
                           process_generic_type "arg_label" "Nolabel" [];
                           (*L2*)
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L1*)
                           process_generic_type "core_type_desc" "Ptyp_arrow"
                             [
                               process_generic_type "arg_label" "Nolabel" [];
                               (*L2*)
                               process_generic_type "core_type_desc"
                                 "Ptyp_constr"
                                 [
                                   ident "hProptoType";
                                   (*L2*)
                                   process_core_type_list [];
                                 ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                               (*L1*)
                               process_generic_type "core_type_desc"
                                 "Ptyp_constr"
                                 [
                                   ident "hProptoType";
                                   (*L2*)
                                   process_core_type_list [];
                                 ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "y" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "dirprod";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "istrans";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "isrefl";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "dirprod";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "ispreorder";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "issymm";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "x" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_arrow"
                         [
                           process_generic_type "arg_label" "Nolabel" [];
                           (*L2*)
                           process_generic_type "core_type_desc" "Ptyp_var"
                             [ string "x" ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L1*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [ ident "hProp"; (*L2*) process_core_type_list [] ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "hrel";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "iseqrel";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "dirprod";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_arrow"
                             [
                               process_generic_type "arg_label" "Nolabel" [];
                               (*L2*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "x" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                               (*L1*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "y" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_arrow"
                             [
                               process_generic_type "arg_label" "Nolabel" [];
                               (*L2*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "y" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                               (*L1*)
                               process_generic_type "core_type_desc" "Ptyp_var"
                                 [ string "x" ]
                               ^ loc2 ^ loc_stack ^ process_attribute_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "x" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_constr"
                         [ ident "hProp"; (*L2*) process_core_type_list [] ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "dirprod";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "hProptoType";
                               (*L2*)
                               process_core_type_list [];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "dirprod";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_arrow"
                                     [
                                       process_generic_type "arg_label"
                                         "Nolabel" [];
                                       (*L2*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_var"
                                         [ string "x" ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                       (*L1*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_arrow"
                                         [
                                           process_generic_type "arg_label"
                                             "Nolabel" [];
                                           (*L2*)
                                           process_generic_type "core_type_desc"
                                             "Ptyp_var"
                                             [ string "x" ]
                                           ^ loc2 ^ loc_stack
                                           ^ process_attribute_list [];
                                           (*L1*)
                                           process_generic_type "core_type_desc"
                                             "Ptyp_arrow"
                                             [
                                               process_generic_type "arg_label"
                                                 "Nolabel" [];
                                               (*L2*)
                                               process_generic_type
                                                 "core_type_desc" "Ptyp_constr"
                                                 [
                                                   ident "hProptoType";
                                                   (*L2*)
                                                   process_core_type_list [];
                                                 ]
                                               ^ loc2 ^ loc_stack
                                               ^ process_attribute_list [];
                                               (*L1*)
                                               process_generic_type
                                                 "core_type_desc" "Ptyp_arrow"
                                                 [
                                                   process_generic_type
                                                     "arg_label" "Nolabel" [];
                                                   (*L2*)
                                                   process_generic_type
                                                     "core_type_desc"
                                                     "Ptyp_constr"
                                                     [
                                                       ident "hProptoType";
                                                       (*L2*)
                                                       process_core_type_list [];
                                                     ]
                                                   ^ loc2 ^ loc_stack
                                                   ^ process_attribute_list [];
                                                   (*L1*)
                                                   process_generic_type
                                                     "core_type_desc"
                                                     "Ptyp_constr"
                                                     [
                                                       ident "hProptoType";
                                                       (*L2*)
                                                       process_core_type_list [];
                                                     ]
                                                   ^ loc2 ^ loc_stack
                                                   ^ process_attribute_list [];
                                                 ]
                                               ^ loc2 ^ loc_stack
                                               ^ process_attribute_list [];
                                             ]
                                           ^ loc2 ^ loc_stack
                                           ^ process_attribute_list [];
                                         ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                     ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                   (*L4*)
                                   process_generic_type "core_type_desc"
                                     "Ptyp_arrow"
                                     [
                                       process_generic_type "arg_label"
                                         "Nolabel" [];
                                       (*L2*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_var"
                                         [ string "x" ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                       (*L1*)
                                       process_generic_type "core_type_desc"
                                         "Ptyp_arrow"
                                         [
                                           process_generic_type "arg_label"
                                             "Nolabel" [];
                                           (*L2*)
                                           process_generic_type "core_type_desc"
                                             "Ptyp_var"
                                             [ string "x" ]
                                           ^ loc2 ^ loc_stack
                                           ^ process_attribute_list [];
                                           (*L1*)
                                           process_generic_type "core_type_desc"
                                             "Ptyp_arrow"
                                             [
                                               process_generic_type "arg_label"
                                                 "Nolabel" [];
                                               (*L2*)
                                               process_generic_type
                                                 "core_type_desc" "Ptyp_constr"
                                                 [
                                                   ident "hProptoType";
                                                   (*L2*)
                                                   process_core_type_list [];
                                                 ]
                                               ^ loc2 ^ loc_stack
                                               ^ process_attribute_list [];
                                               (*L1*)
                                               process_generic_type
                                                 "core_type_desc" "Ptyp_arrow"
                                                 [
                                                   process_generic_type
                                                     "arg_label" "Nolabel" [];
                                                   (*L2*)
                                                   process_generic_type
                                                     "core_type_desc"
                                                     "Ptyp_constr"
                                                     [
                                                       ident "hProptoType";
                                                       (*L2*)
                                                       process_core_type_list [];
                                                     ]
                                                   ^ loc2 ^ loc_stack
                                                   ^ process_attribute_list [];
                                                   (*L1*)
                                                   process_generic_type
                                                     "core_type_desc"
                                                     "Ptyp_constr"
                                                     [
                                                       ident "hProptoType";
                                                       (*L2*)
                                                       process_core_type_list [];
                                                     ]
                                                   ^ loc2 ^ loc_stack
                                                   ^ process_attribute_list [];
                                                 ]
                                               ^ loc2 ^ loc_stack
                                               ^ process_attribute_list [];
                                             ]
                                           ^ loc2 ^ loc_stack
                                           ^ process_attribute_list [];
                                         ]
                                       ^ loc2 ^ loc_stack
                                       ^ process_attribute_list [];
                                     ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_constr"
                     [
                       ident "total2";
                       (*L2*)
                       process_core_type_list
                         [
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "hsubtype";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                           (*L4*)
                           process_generic_type "core_type_desc" "Ptyp_constr"
                             [
                               ident "iseqclass";
                               (*L2*)
                               process_core_type_list
                                 [
                                   process_generic_type "core_type_desc"
                                     "Ptyp_var"
                                     [ string "x" ]
                                   ^ loc2 ^ loc_stack
                                   ^ process_attribute_list [];
                                 ];
                             ]
                           ^ loc2 ^ loc_stack ^ process_attribute_list [];
                         ];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_variant"
                     [
                       process_arg_constructor_declaration
                         [
                           loc ^ process_var_list []
                           ^ process_generic_type "constructor_arguments"
                               "Pcstr_tuple"
                               [ process_core_type_list [] ]
                           ^ loc2 ^ process_attribute_list [];
                           (*L4*)
                           loc ^ process_var_list []
                           ^ process_generic_type "constructor_arguments"
                               "Pcstr_tuple"
                               [
                                 process_core_type_list
                                   [
                                     process_generic_type "core_type_desc"
                                       "Ptyp_constr"
                                       [
                                         ident "nat";
                                         (*L2*)
                                         process_core_type_list [];
                                       ]
                                     ^ loc2 ^ loc_stack
                                     ^ process_attribute_list [];
                                   ];
                               ]
                           ^ loc2 ^ process_attribute_list [];
                         ];
                     ]
                 ^ process_generic_type "private_flag" "Public" []
                 
               ]
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                       (*L4*)
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_variant"
                     [
                       process_arg_constructor_declaration
                         [
                           loc ^ process_var_list []
                           ^ process_generic_type "constructor_arguments"
                               "Pcstr_tuple"
                               [
                                 process_core_type_list
                                   [
                                     process_generic_type "core_type_desc"
                                       "Ptyp_var"
                                       [ string "a" ]
                                     ^ loc2 ^ loc_stack
                                     ^ process_attribute_list [];
                                   ];
                               ]
                           ^ loc2 ^ process_attribute_list [];
                           (*L4*)
                           loc ^ process_var_list []
                           ^ process_generic_type "constructor_arguments"
                               "Pcstr_tuple"
                               [
                                 process_core_type_list
                                   [
                                     process_generic_type "core_type_desc"
                                       "Ptyp_var"
                                       [ string "b" ]
                                     ^ loc2 ^ loc_stack
                                     ^ process_attribute_list [];
                                   ];
                               ]
                           ^ loc2 ^ process_attribute_list [];
                         ];
                     ]
                 ^ process_generic_type "private_flag" "Public" []
                 
               ]
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc ^ process_params [] ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_variant"
                     [ process_arg_constructor_declaration [] ]
                 ^ process_generic_type "private_flag" "Public" []
                 
               ]
           ]
         ^ loc2;
         (*L3*)
         process_generic_type "structure_item_desc" "Pstr_type"
           [
             process_generic_type "rec_flag" "Recursive" [];
             (*L2*)
             process_type_declaration_list
               [
                 loc
                 ^ process_params
                     [
                       "FIXME:process_params(core_type * (variance * \
                        injectivity))";
                     ]
                 ^ process_cstrs []
                 ^ process_generic_type "type_kind" "Ptype_abstract" []
                 ^ process_generic_type "private_flag" "Public" []
                 ^ process_generic_type "core_type_desc" "Ptyp_arrow"
                     [
                       process_generic_type "arg_label" "Nolabel" [];
                       (*L2*)
                       process_generic_type "core_type_desc" "Ptyp_var"
                         [ string "x" ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                       (*L1*)
                       process_generic_type "core_type_desc" "Ptyp_constr"
                         [ ident "empty"; (*L2*) process_core_type_list [] ]
                       ^ loc2 ^ loc_stack ^ process_attribute_list [];
                     ]
                 ^ loc2 ^ loc_stack
                 ^ (process_attribute_list []) 
               ];
           ]
         ^ loc2;
       ])
