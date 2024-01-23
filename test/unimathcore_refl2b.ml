let process_cases x = "process_cases"
let process_var_list x = "process_var_list"

let process_arg_constructor_declaration x =
  "process_arg_constructor_declaration"

let process_label_declaration_list x = "process_label_declaration_list"
let process_params x = "process_params"
let process_cstrs x = "process_cstrs"
let process_core_type_list x = "process_core_type_list"
let process_type_declaration_list x = "process_type_declaration_list"
let loc = "loc"
let loc2 = "loc"
let loc_stack = "loc"

let process_generic_type (a : string) (b : string) (c : string list) =
  "process_generic_type"

let process_loc a = "process_loc"

let ident a : string =
  print_endline ("ident " ^ a);

  "ident"

let process_string_loc_list_pattern_option =
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

let rec process_generic_list_tail name a f : string =
  match a with
  | [] -> ""
  | a :: t ->
      let v1 = f a in
      if t != [] then v1 ^ "\n\n;(*L3*)" ^ process_generic_list_tail name t f
      else v1

and process_generic_list name a f : string =
  "(" ^ name ^ "["
  ^ (match a with
    | [] -> ""
    | a :: t ->
        let v1 = f a in
        if t != [] then v1 ^ "\n\n;(*L4*)" ^ process_generic_list_tail name t f
        else v1)
  ^ "] )"

and process_structure_item x = "process_structure_item"
and process_structure_item_desc x : string = "process_structure_item_desc"

let process_structure_items x =
  "let ()=(print_endline "
  ^ process_generic_list "process_structure_items" x process_structure_item
  ^ ")"

let foo1 =
  process_generic_type "structure_item_desc" "Pstr_value"
    [
      process_generic_type "rec_flag" "Nonrecursive" [];
      (*L2*)
      process_generic_type "pattern_desc" "Ppat_var" [ loc ]
      ^ loc2 ^ loc_stack ^ process_attribute_list []
      ^ process_generic_type "expression_desc" "Pexp_fun"
          [
            process_generic_type "arg_label" "Nolabel" [];
            (*L2*)
            (*L10*)
            process_generic_type "pattern_desc" "Ppat_var" [ loc ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
            (*L1*)
            process_generic_type "expression_desc" "Pexp_constant"
              [
                process_generic_type "constant" "Pconst_string"
                  [
                    string "process_arg_constructor_declaration";
                    (*L2*)
                    loc2;
                    (*L1*)
                    process_string_option;
                  ];
              ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
          ]
      ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ process_attribute_list []
      ^ loc2;
    ]
  ^ loc2

let foo2 =
  process_generic_type "structure_item_desc" "Pstr_value"
    [
      process_generic_type "rec_flag" "Nonrecursive" [];
      
(*L2*)
      process_generic_type "pattern_desc" "Ppat_var" [ loc ]
      ^ loc2 ^ loc_stack ^ process_attribute_list []
      ^ process_generic_type "expression_desc" "Pexp_fun"
          [
            process_generic_type "arg_label" "Nolabel" [];
            
(*L2*)
            
(*L10*)
            process_generic_type "pattern_desc" "Ppat_var" [ loc ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
            
(*L1*)
            process_generic_type "expression_desc" "Pexp_constant"
              [
                process_generic_type "constant" "Pconst_string"
                  [
                    string "process_label_declaration_list";
                    
(*L2*)
                    loc2;
                    
(*L1*)
                    process_string_option;
                  ];
              ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
          ]
      ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ process_attribute_list []
      ^ loc2;
    ]
  ^ loc2

let foo1 =
  process_generic_type "structure_item_desc" "Pstr_value"
    [
      process_generic_type "rec_flag" "Nonrecursive" [];
      
(*L2*)
      process_generic_type "pattern_desc" "Ppat_var" [ loc ]
      ^ loc2 ^ loc_stack ^ process_attribute_list []
      ^ process_generic_type "expression_desc" "Pexp_fun"
          [
            process_generic_type "arg_label" "Nolabel" [];
            
(*L2*)
            
(*L10*)
            process_generic_type "pattern_desc" "Ppat_var" [ loc ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
            
(*L1*)
            process_generic_type "expression_desc" "Pexp_constant"
              [
                process_generic_type "constant" "Pconst_string"
                  [
                    string "process_params";
                    
(*L2*)
                    loc2;
                    
(*L1*)
                    process_string_option;
                  ];
              ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
          ]
      ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ process_attribute_list []
      ^ loc2;
    ]
  ^ loc2

let foo1 =
  process_generic_type "structure_item_desc" "Pstr_value"
    [
      process_generic_type "rec_flag" "Nonrecursive" [];
      
(*L2*)
      process_generic_type "pattern_desc" "Ppat_var" [ loc ]
      ^ loc2 ^ loc_stack ^ process_attribute_list []
      ^ process_generic_type "expression_desc" "Pexp_fun"
          [
            process_generic_type "arg_label" "Nolabel" [];
            
(*L2*)
            
(*L10*)
            process_generic_type "pattern_desc" "Ppat_var" [ loc ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
            
(*L1*)
            process_generic_type "expression_desc" "Pexp_constant"
              [
                process_generic_type "constant" "Pconst_string"
                  [
                    string "process_var_list";
                    
(*L2*)
                    loc2;
                    
(*L1*)
                    process_string_option;
                  ];
              ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
          ]
      ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ process_attribute_list []
      ^ loc2;
    ]
  ^ loc2

let foo1 =
  process_generic_type "structure_item_desc" "Pstr_value"
    [
      process_generic_type "rec_flag" "Nonrecursive" [];
      
(*L2*)
      process_generic_type "pattern_desc" "Ppat_var" [ loc ]
      ^ loc2 ^ loc_stack ^ process_attribute_list []
      ^ process_generic_type "expression_desc" "Pexp_fun"
          [
            process_generic_type "arg_label" "Nolabel" [];
            
(*L2*)
            
(*L10*)
            process_generic_type "pattern_desc" "Ppat_var" [ loc ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
            
(*L1*)
            process_generic_type "expression_desc" "Pexp_constant"
              [
                process_generic_type "constant" "Pconst_string"
                  [
                    string "process_arg_constructor_declaration";
                    
(*L2*)
                    loc2;
                    
(*L1*)
                    process_string_option;
                  ];
              ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
          ]
      ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ process_attribute_list []
      ^ loc2;
    ]
  ^ loc2

let foo1 =
  process_generic_type "structure_item_desc" "Pstr_value"
    [
      process_generic_type "rec_flag" "Nonrecursive" [];
      
(*L2*)
      process_generic_type "pattern_desc" "Ppat_var" [ loc ]
      ^ loc2 ^ loc_stack ^ process_attribute_list []
      ^ process_generic_type "expression_desc" "Pexp_fun"
          [
            process_generic_type "arg_label" "Nolabel" [];
            
(*L2*)
            
(*L10*)
            process_generic_type "pattern_desc" "Ppat_var" [ loc ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
            
(*L1*)
            process_generic_type "expression_desc" "Pexp_constant"
              [
                process_generic_type "constant" "Pconst_string"
                  [
                    string "process_label_declaration_list";
                    
(*L2*)
                    loc2;
                    
(*L1*)
                    process_string_option;
                  ];
              ]
            ^ loc2 ^ loc_stack ^ process_attribute_list [];
          ]
      ^ loc2 ^ loc_stack ^ process_attribute_list [] ^ process_attribute_list []
      ^ loc2;
    ]
  ^ loc2


