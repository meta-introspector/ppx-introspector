open Ppxlib
let process_generic_type a b c = "process_generic_type"
let process_loc a  = "process_loc"
let process_location a  = "process_loc"
let process_location_stack a  = "process_loc"
let attributes  = "attributes"
let process_value_binding_list = "process_value_binding_list"
let pos a  = "process_loc"
let b a  = "process_loc"
let mint a  = "process_loc"
let process_string a  = "process_string"
let string a  = "process_string"  
let foo = (process_generic_type "structure_item_desc" "Pstr_value" [
    (process_generic_type "rec_flag" "Nonrecursive" []);
    (process_generic_type "pattern_desc" "Ppat_var" [
        (process_loc
           (process_string "foo" )^
         (process_location
            (pos
               (string "./test/test3.ml" )
             ^(mint 1)
             ^(mint 0)^(mint 4)
            )
          ^(pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 7))^(b false)
         )
        )
      ]
    )^
    (process_location (pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 4))^(pos (string "./test/test3.ml" )
                                                                                    ^(mint 1)^(mint 0)^(mint 7))^(b false))^(process_location_stack[] )
    ^(attributes)
    ^(process_generic_type "expression_desc" "Pexp_constant" [(process_generic_type "constant" "Pconst_integer" [(string "1" );"FIXME1221"]);])
    ^(process_location (pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 10))^(pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 11))^(b false))^(process_location_stack[] )^(attributes)^(attributes)^(process_location (pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 0))^(pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 11))^(b false));
    process_value_binding_list])^(process_location (pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 0))^(pos (string "./test/test3.ml" )^(mint 1)^(mint 0)^(mint 11))^(b false))
