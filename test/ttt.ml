let ()=(print_endline (
    process_generic_type "structure_item_desc" "Pstr_value"
      [
        (process_generic_type "rec_flag" "Nonrecursive" []) ;
        (*L2*)(process_generic_type "pattern_desc" "Ppat_var" [(loc)])^(loc2)^(loc_stack)^(process_attribute_list[] )^(process_generic_type "expression_desc" "Pexp_fun" [(process_generic_type "arg_label" "Nolabel" []);
                                                                                                                                                                          (*L2*)(*L10*)(process_generic_type "pattern_desc" "Ppat_var" [(loc)])^(loc2)^(loc_stack)^(process_attribute_list[] );
                                                                                                                                                                          (*L1*)(process_generic_type "expression_desc" "Pexp_constant"
                                                                                                                                                                                   [
                                                                                                                                                                                     (process_generic_type "constant" "Pconst_string"
                                                                                                                                                                                        [
                                                                                                                                                                                          (string "process_var_list" );
                                                                                                                                                                                          (*L2*)(loc2);
                                                                                                                                                                                          (*L1*)process_string_option
                                                                                                                                                                                        ])
                                                                                                                                                                                   ])^(loc2)^(loc_stack)^(process_attribute_list[] )
                                                                                                                                                                         ])^(loc2)^(loc_stack)^(process_attribute_list[] )^(process_attribute_list[] )^(loc2)
      ])^(loc2)
  )
  
