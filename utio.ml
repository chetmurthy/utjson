open Utypes
open Utparse0
open Utprint

let of_string_exn s = s |> parse_string parse_utype_eoi
let to_string t = print_utype Pprintf.empty_pc t

let struct_item_of_string_exn s = s |> parse_string parse_struct_item_eoi
let struct_item_to_string t = print_struct_item Pprintf.empty_pc t

let sig_item_of_string_exn s = s |> parse_string parse_sig_item_eoi
let sig_item_to_string t = print_sig_item Pprintf.empty_pc t

let structure_of_string_exn s = s |> parse_string parse_structure_eoi
let structure_to_string t = print_structure Pprintf.empty_pc t

let signature_of_string_exn s = s |> parse_string parse_signature_eoi
let signature_to_string t = print_signature Pprintf.empty_pc t

let module_expr_of_string_exn s = s |> parse_string parse_module_expr_eoi
let module_expr_to_string t = print_module_expr Pprintf.empty_pc t

let module_type_of_string_exn s = s |> parse_string parse_module_type_eoi
let module_type_to_string t = print_module_type Pprintf.empty_pc t

module Debug = struct
let printer = show_utype_t
let cmp = equal_utype_t

let struct_item_printer x = "<<"^(show_struct_item_t x)^">>"
let struct_item_cmp = equal_struct_item_t


let module_expr_printer x = "<<"^(show_module_expr_t x)^">>"
let module_expr_cmp = equal_module_expr_t

let module_type_printer x = "<<"^(show_module_type_t x)^">>"
let module_type_cmp = equal_module_type_t

let sig_item_printer x = "<<"^(show_sig_item_t x)^">>"
let sig_item_cmp = equal_sig_item_t

let signature_printer x = "<<"^(show_signature x)^">>"
let signature_cmp = equal_signature

let structure_printer x = "<<"^(show_structure x)^">>"
let structure_cmp = equal_structure
end


module Normal = struct
include Debug
let printer x  = "<<"^(to_string x)^">>"
let struct_item_printer x = "<<"^(struct_item_to_string x)^">>"
let module_expr_printer x = "<<"^(module_expr_to_string x)^">>"
let module_type_printer x = "<<"^(module_type_to_string x)^">>"
let sig_item_printer x = "<<"^(sig_item_to_string x)^">>"
let signature_printer x = "<<"^(signature_to_string x)^">>"
let structure_printer x = "<<"^(structure_to_string x)^">>"
end
