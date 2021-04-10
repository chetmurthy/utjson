
type base_type_t = [%import: Utypes.base_type_t]
[@@deriving show { with_path = false },eq]

module Bound = struct
  type 'a t = [%import: 'a Utypes.Bound.t]
  [@@deriving show { with_path = false },eq]
end

type size_constraint_t = [%import: Utypes.size_constraint_t]
[@@deriving show { with_path = false },eq]

type range_constraint_t = [%import: Utypes.range_constraint_t]
[@@deriving show { with_path = false },eq]

[%%import: Utypes.atomic_utype_t]
[@@deriving show { with_path = false },eq]

[%%import: Utypes.struct_item_t]
[@@deriving show { with_path = false },eq]

type token = [%import: Utypes.token]
