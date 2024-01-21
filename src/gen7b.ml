open Ppxlib

open Ppx
open Gen4
open Gen7

    
let process_type_decl_position (x:position):string = match x with {pos_fname(* string*);pos_lnum(* int*);pos_bol(* int*);pos_cnum(* int*)} ->((*P2*)process_type_decl_string pos_fname)^((*P2*)process_type_decl_int pos_lnum)^((*P2*)process_type_decl_int pos_bol)^((*P2*)process_type_decl_int pos_cnum) 
 let process_type_decl_location (x:location):string = match x with {loc_start(* position*);loc_end(* position*);loc_ghost(* bool*)} ->((*P2*)process_type_decl_position loc_start)^((*P2*)process_type_decl_position loc_end)^((*P2*)process_type_decl_bool loc_ghost) 
 let process_type_decl_location (x:location):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_position (x:position):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_location_stack (x:location_stack):string = "match x with {(terminal_ptype_abstract)} ->SKIP "

let process_type_decl_loc_abstract x b = match x with {txt(* FIXME*);loc(* location*)} ->((*P2*)b txt)^((*P2*)process_type_decl_location loc)

let process_type_decl_longident (x:longident):string = "match x with {(ptype_variant "
let process_type_decl_loc_longident x b = process_type_decl_loc_abstract x process_type_decl_longident

let process_type_decl_loc x = process_type_decl_loc_abstract x process_type_decl_string


 let process_type_decl_longident_loc (x:longident_loc):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_rec_flag (x:rec_flag):string = "match x with {(ptype_variant "
 let process_type_decl_direction_flag (x:direction_flag):string = "match x with {(ptype_variant "
 let process_type_decl_private_flag (x:private_flag):string = "match x with {(ptype_variant "
 let process_type_decl_mutable_flag (x:mutable_flag):string = "match x with {(ptype_variant "
 let process_type_decl_virtual_flag (x:virtual_flag):string = "match x with {(ptype_variant "
 let process_type_decl_override_flag (x:override_flag):string = "match x with {(ptype_variant "
 let process_type_decl_closed_flag (x:closed_flag):string = "match x with {(ptype_variant "
 let process_type_decl_label (x:label):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_arg_label (x:arg_label):string = "match x with {(ptype_variant "
 let process_type_decl_variance (x:variance):string = "match x with {(ptype_variant "
 let process_type_decl_injectivity (x:injectivity):string = "match x with {(ptype_variant "
 let process_type_decl_constant (x:constant):string = "match x with {(ptype_variant "
let process_type_decl_attribute (x:attribute):string = match x with {attr_name(* loc*);attr_payload(* payload*);attr_loc(* location*)}
  ->
  ((*P2*)process_type_decl_loc attr_name)
  ^((*P2*)process_type_decl_payload attr_payload)^((*P2*)process_type_decl_location attr_loc) 
 let process_type_decl_extension (x:extension):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_attributes (x:attributes):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_payload (x:payload):string = "match x with {(ptype_variant "
 let process_type_decl_core_type (x:core_type):string = match x with {ptyp_desc(* core_type_desc*);ptyp_loc(* location*);ptyp_loc_stack(* location_stack*);ptyp_attributes(* attributes*)} ->((*P2*)process_type_decl_core_type_desc ptyp_desc)^((*P2*)process_type_decl_location ptyp_loc)^((*P2*)process_type_decl_location_stack ptyp_loc_stack)^((*P2*)process_type_decl_attributes ptyp_attributes) 
 let process_type_decl_core_type_desc (x:core_type_desc):string = "match x with {(ptype_variant "
 let process_type_decl_package_type (x:package_type):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_row_field (x:row_field):string = match x with {prf_desc(* row_field_desc*);prf_loc(* location*);prf_attributes(* attributes*)} ->((*P2*)process_type_decl_row_field_desc prf_desc)^((*P2*)process_type_decl_location prf_loc)^((*P2*)process_type_decl_attributes prf_attributes) 
 let process_type_decl_row_field_desc (x:row_field_desc):string = "match x with {(ptype_variant "
 let process_type_decl_object_field (x:object_field):string = match x with {pof_desc(* object_field_desc*);pof_loc(* location*);pof_attributes(* attributes*)} ->((*P2*)process_type_decl_object_field_desc pof_desc)^((*P2*)process_type_decl_location pof_loc)^((*P2*)process_type_decl_attributes pof_attributes) 
 let process_type_decl_object_field_desc (x:object_field_desc):string = "match x with {(ptype_variant "
 let process_type_decl_pattern (x:pattern):string = match x with {ppat_desc(* pattern_desc*);ppat_loc(* location*);ppat_loc_stack(* location_stack*);ppat_attributes(* attributes*)} ->((*P2*)process_type_decl_pattern_desc ppat_desc)^((*P2*)process_type_decl_location ppat_loc)^((*P2*)process_type_decl_location_stack ppat_loc_stack)^((*P2*)process_type_decl_attributes ppat_attributes) 

 let process_type_decl_expression (x:expression):string = match x with {pexp_desc(* expression_desc*);pexp_loc(* location*);pexp_loc_stack(* location_stack*);pexp_attributes(* attributes*)} ->((*P2*)process_type_decl_expression_desc pexp_desc)^((*P2*)process_type_decl_location pexp_loc)^((*P2*)process_type_decl_location_stack pexp_loc_stack)^((*P2*)process_type_decl_attributes pexp_attributes) 
 let process_type_decl_expression_desc (x:expression_desc):string = "match x with {(ptype_variant "
let process_type_decl_case (x:case):string = match x with {pc_lhs(* pattern*);pc_guard(* option*);pc_rhs(* expression*)}
  ->
  ((*P2*)process_type_decl_pattern pc_lhs)^
                                                                                                                           ((*P2*)process_type_decl_expression_option pc_guard)
                                                                                                                           ^((*P2*)process_type_decl_expression pc_rhs) 
let process_string_loc x = "process_string_loc" ^ x.txt
let process_type_decl_vars_list a =
    process_generic_list "process_var_list" a process_string_loc
let process_type_decl_binding_op_list a = 
  process_generic_list "process_binding_op_list" a process_binding_op

let process_type_decl_prim_list a =
    process_generic_list "process_prim" a process_string

let process_cstr x  = "FIXME:cstr(core_type * core_type * location)"
let process_type_decl_cstrs_list a =
  process_generic_list "process_cstrs" a process_cstr
    
let process_params x  = "FIXME:process_params (core_type * (variance * injectivity))"
let process_type_decl_params_list a =
  process_generic_list "process_params" a process_params
      
let process_type_decl_letop (x:letop):string = match x with {let_(* binding_op*);ands(* list*);body(* expression*)} ->
  (
    (*P2*)process_type_decl_binding_op let_)
  ^((*P2*)process_type_decl_binding_op_list ands)
  ^((*P2*)process_type_decl_expression body)

let process_type_decl_type_kind x = process_type_kind x

 let process_type_decl_binding_op (x:binding_op):string = match x with {pbop_op(* loc*);pbop_pat(* pattern*);pbop_exp(* expression*);pbop_loc(* location*)} ->((*P2*)process_type_decl_loc pbop_op)^((*P2*)process_type_decl_pattern pbop_pat)^((*P2*)process_type_decl_expression pbop_exp)^((*P2*)process_type_decl_location pbop_loc) 
 let process_type_decl_value_description (x:value_description):string = match x with {pval_name(* loc*);pval_type(* core_type*);pval_prim(* list*);pval_attributes(* attributes*);pval_loc(* location*)} ->((*P2*)process_type_decl_loc pval_name)^((*P2*)process_type_decl_core_type pval_type)^((*P2*)process_type_decl_prim_list pval_prim)^((*P2*)process_type_decl_attributes pval_attributes)^((*P2*)process_type_decl_location pval_loc) 
 let process_type_decl_type_declaration (x:type_declaration):string = match x with {ptype_name(* loc*);ptype_params(* list*);ptype_cstrs(* list*);ptype_kind(* type_kind*);ptype_private(* private_flag*);ptype_manifest(* option*);ptype_attributes(* attributes*);ptype_loc(* location*)} ->((*P2*)process_type_decl_loc ptype_name)^((*P2*)process_type_decl_params_list ptype_params)^((*P2*)process_type_decl_cstrs_list ptype_cstrs)^((*P2*)process_type_decl_type_kind ptype_kind)^((*P2*)process_type_decl_private_flag ptype_private)^((*P2*)process_type_decl_manifest_option ptype_manifest)^((*P2*)process_type_decl_attributes ptype_attributes)^((*P2*)process_type_decl_location ptype_loc) 
 let process_type_decl_type_kind (x:type_kind):string = "match x with {(ptype_variant "
 let process_type_decl_label_declaration (x:label_declaration):string = match x with {pld_name(* loc*);pld_mutable(* mutable_flag*);pld_type(* core_type*);pld_loc(* location*);pld_attributes(* attributes*)} ->((*P2*)process_type_decl_loc pld_name)^((*P2*)process_type_decl_mutable_flag pld_mutable)^((*P2*)process_type_decl_core_type pld_type)^((*P2*)process_type_decl_location pld_loc)^((*P2*)process_type_decl_attributes pld_attributes) 

let process_type_decl_constructor_arguments (x:constructor_arguments):string = 
  (process_constructor_arguments x)
  
let process_constructor_arguments x : constructor_arguments= x
  
let process_type_decl_constructor_declaration (x:constructor_declaration):string = match x with {pcd_name(* loc*);pcd_vars(* list*);pcd_args(* constructor_arguments*);pcd_res(* option*);pcd_loc(* location*);pcd_attributes(* attributes*)} ->(
    (*P2*)process_type_decl_loc pcd_name)^((*P2*)process_type_decl_vars_list pcd_vars)
    ^((*P2*)process_type_decl_constructor_arguments pcd_args)
    ^((*P2*)process_type_decl_core_type_option pcd_res)
    ^((*P2*)process_type_decl_location pcd_loc)
    ^((*P2*)process_type_decl_attributes pcd_attributes) 
     
let process_type_decl_type_extension (x:type_extension):string = match x with {ptyext_path(* longident_loc*);ptyext_params(* list*);ptyext_constructors(* list*);ptyext_private(* private_flag*);ptyext_loc(* location*);ptyext_attributes(* attributes*)} ->((*P2*)process_type_decl_longident_loc ptyext_path)^((*P2*)process_type_decl_params_list ptyext_params)^((*P2*)process_type_decl_constructors_list ptyext_constructors)^((*P2*)process_type_decl_private_flag ptyext_private)^((*P2*)process_type_decl_location ptyext_loc)^((*P2*)process_type_decl_attributes ptyext_attributes) 
 let process_type_decl_extension_constructor (x:extension_constructor):string = match x with {pext_name(* loc*);pext_kind(* extension_constructor_kind*);pext_loc(* location*);pext_attributes(* attributes*)} ->((*P2*)process_type_decl_loc pext_name)^((*P2*)process_type_decl_extension_constructor_kind pext_kind)^((*P2*)process_type_decl_location pext_loc)^((*P2*)process_type_decl_attributes pext_attributes) 
 let process_type_decl_type_exception (x:type_exception):string = match x with {ptyexn_constructor(* extension_constructor*);ptyexn_loc(* location*);ptyexn_attributes(* attributes*)} ->((*P2*)process_type_decl_extension_constructor ptyexn_constructor)^((*P2*)process_type_decl_location ptyexn_loc)^((*P2*)process_type_decl_attributes ptyexn_attributes) 
 let process_type_decl_extension_constructor_kind (x:extension_constructor_kind):string = "match x with {(ptype_variant "


 let process_type_decl_class_signature (x:class_signature):string = match x with {pcsig_self(* core_type*);pcsig_fields(* list*)} ->((*P2*)process_type_decl_core_type pcsig_self)^((*P2*)process_type_decl_fields_list pcsig_fields) 
 let process_type_decl_class_type_field (x:class_type_field):string = match x with {pctf_desc(* class_type_field_desc*);pctf_loc(* location*);pctf_attributes(* attributes*)} ->((*P2*)process_type_decl_class_type_field_desc pctf_desc)^((*P2*)process_type_decl_location pctf_loc)^((*P2*)process_type_decl_attributes pctf_attributes) 
 let process_type_decl_class_type_field_desc (x:class_type_field_desc):string = "match x with {(ptype_variant "
let process_type_decl_class_infos x:string = match x with {pci_virt(* virtual_flag*);pci_params(* list*);pci_name(* loc*);pci_expr(* FIXME*);pci_loc(* location*);pci_attributes(* attributes*)} ->((*P2*)process_type_decl_virtual_flag pci_virt)^((*P2*)process_type_decl_params_list pci_params)^((*P2*)process_type_decl_loc pci_name)
                                                                                                                                                                                                   ^((*P2*)"process_type_decl_FIXME pci_expr")
                                                                                                                                                                                                   ^((*P2*)process_type_decl_location pci_loc)^((*P2*)process_type_decl_attributes pci_attributes) 
 let process_type_decl_class_description (x:class_description):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_class_type_declaration (x:class_type_declaration):string = "match x with {(terminal_ptype_abstract)} ->SKIP "

 let process_type_decl_class_expr_desc (x:class_expr_desc):string = "match x with {(ptype_variant "
 let process_type_decl_class_structure (x:class_structure):string = match x with {pcstr_self(* pattern*);pcstr_fields(* list*)} ->((*P2*)process_type_decl_pattern pcstr_self)^((*P2*)process_type_decl_fields_list pcstr_fields) 
 let process_type_decl_class_field_desc (x:class_field_desc):string = "match x with {(ptype_variant "
let process_type_decl_class_field (x:class_field):string = match x with {pcf_desc(* class_field_desc*);pcf_loc(* location*);pcf_attributes(* attributes*)} ->((*P2*)process_type_decl_class_field_desc pcf_desc)^((*P2*)process_type_decl_location pcf_loc)^((*P2*)process_type_decl_attributes pcf_attributes) 

 let process_type_decl_class_field_kind (x:class_field_kind):string = "match x with {(ptype_variant "
 let process_type_decl_class_declaration (x:class_declaration):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_module_type (x:module_type):string = match x with {pmty_desc(* module_type_desc*);pmty_loc(* location*);pmty_attributes(* attributes*)} ->((*P2*)process_type_decl_module_type_desc pmty_desc)^((*P2*)process_type_decl_location pmty_loc)^((*P2*)process_type_decl_attributes pmty_attributes) 

 let process_type_decl_functor_parameter (x:functor_parameter):string = "match x with {(ptype_variant "
 let process_type_decl_signature (x:signature):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_signature_item (x:signature_item):string = match x with {psig_desc(* signature_item_desc*);psig_loc(* location*)} ->((*P2*)process_type_decl_signature_item_desc psig_desc)^((*P2*)process_type_decl_location psig_loc) 
 let process_type_decl_signature_item_desc (x:signature_item_desc):string = "match x with {(ptype_variant "
let process_type_decl_module_declaration (x:module_declaration):string = match x with {pmd_name(* loc*);pmd_type(* module_type*);pmd_attributes(* attributes*);pmd_loc(* location*)} ->
  ((*P2*)process_type_decl_string_option_loc pmd_name)^
  ((*P2*)process_type_decl_module_type pmd_type)^((*P2*)process_type_decl_attributes pmd_attributes)^((*P2*)process_type_decl_location pmd_loc) 
 let process_type_decl_module_substitution (x:module_substitution):string = match x with {pms_name(* loc*);pms_manifest(* longident_loc*);pms_attributes(* attributes*);pms_loc(* location*)} ->((*P2*)process_type_decl_loc pms_name)^((*P2*)process_type_decl_longident_loc pms_manifest)^((*P2*)process_type_decl_attributes pms_attributes)^((*P2*)process_type_decl_location pms_loc) 
let process_type_decl_module_type_declaration (x:module_type_declaration):string = match x with {pmtd_name(* loc*);pmtd_type(* option*);pmtd_attributes(* attributes*);pmtd_loc(* location*)} ->(
    (*P2*)process_type_decl_loc pmtd_name)
    ^((*P2*)process_type_decl_module_type_option pmtd_type)
    ^((*P2*)process_type_decl_attributes pmtd_attributes)
    ^((*P2*)process_type_decl_location pmtd_loc) 
let process_type_decl_open_infos (x(*:open_infos*)):string = match x with {popen_expr(* FIXME*);popen_override(* override_flag*);popen_loc(* location*);popen_attributes(* attributes*)} ->
  "((*P2*)process_type_decl_FIXME popen_expr)"
  ^((*P2*)process_type_decl_override_flag popen_override)^((*P2*)process_type_decl_location popen_loc)^((*P2*)process_type_decl_attributes popen_attributes) 
 let process_type_decl_open_description (x:open_description):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_open_declaration (x:open_declaration):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
let process_type_decl_include_infos (x(*:include_infos*)):string = match x with {pincl_mod(* FIXME*);pincl_loc(* location*);pincl_attributes(* attributes*)} ->
  "((*P2*)process_type_decl_FIXME pincl_mod)"
  ^((*P2*)process_type_decl_location pincl_loc)^((*P2*)process_type_decl_attributes pincl_attributes) 
 let process_type_decl_include_description (x:include_description):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_include_declaration (x:include_declaration):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_with_constraint (x:with_constraint):string = "match x with {(ptype_variant "
 let process_type_decl_module_expr (x:module_expr):string = match x with {pmod_desc(* module_expr_desc*);pmod_loc(* location*);pmod_attributes(* attributes*)} ->((*P2*)process_type_decl_module_expr_desc pmod_desc)^((*P2*)process_type_decl_location pmod_loc)^((*P2*)process_type_decl_attributes pmod_attributes) 
 let process_type_decl_module_expr_desc (x:module_expr_desc):string = "match x with {(ptype_variant "
 let process_type_decl_structure (x:structure):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
 let process_type_decl_structure_item (x:structure_item):string = match x with {pstr_desc(* structure_item_desc*);pstr_loc(* location*)} ->((*P2*)process_type_decl_structure_item_desc pstr_desc)^((*P2*)process_type_decl_location pstr_loc) 
 let process_type_decl_structure_item_desc (x:structure_item_desc):string = "match x with {(ptype_variant "
 let process_type_decl_value_binding (x:value_binding):string = match x with {pvb_pat(* pattern*);pvb_expr(* expression*);pvb_attributes(* attributes*);pvb_loc(* location*)} ->((*P2*)process_type_decl_pattern pvb_pat)^((*P2*)process_type_decl_expression pvb_expr)^((*P2*)process_type_decl_attributes pvb_attributes)^((*P2*)process_type_decl_location pvb_loc) 
 let process_type_decl_module_binding (x:module_binding):string = match x with {pmb_name(* loc*);pmb_expr(* module_expr*);pmb_attributes(* attributes*);pmb_loc(* location*)} ->((*P2*)process_type_decl_string_option_loc pmb_name)^((*P2*)process_type_decl_module_expr pmb_expr)^((*P2*)process_type_decl_attributes pmb_attributes)^((*P2*)process_type_decl_location pmb_loc) 
 let process_type_decl_toplevel_phrase (x:toplevel_phrase):string = "match x with {(ptype_variant "
let process_type_decl_toplevel_directive (x:toplevel_directive):string = match x with {pdir_name(* loc*);pdir_arg(* option*);pdir_loc(* location*)} ->
  ((*P2*)process_type_decl_loc pdir_name)
  ^((*P2*)process_type_decl_directive_argument_option pdir_arg)^((*P2*)process_type_decl_location pdir_loc) 
 let process_type_decl_directive_argument (x:directive_argument):string = match x with {pdira_desc(* directive_argument_desc*);pdira_loc(* location*)} ->((*P2*)process_type_decl_directive_argument_desc pdira_desc)^((*P2*)process_type_decl_location pdira_loc) 
 let process_type_decl_directive_argument_desc (x:directive_argument_desc):string = "match x with {(ptype_variant "
 let process_type_decl_cases (x:cases):string = "match x with {(terminal_ptype_abstract)} ->SKIP "
