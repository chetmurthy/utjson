open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Utypes ;


value plist_with sep f sh pc l =
  let l = List.map (fun s -> (s, sep)) l in
  pprintf pc "%p" (Prtools.plist f sh) l
;

value pr_utype = Eprinter.make "utype";
value print_utype = Eprinter.apply pr_utype;
value pr_base_type = Eprinter.make "base_type";
value print_base_type = Eprinter.apply pr_base_type;
value pr_atomic = Eprinter.make "atomic";
value print_atomic = Eprinter.apply pr_atomic;

value qstring pc s =
  pprintf pc "\"%s\"" s
;

value print_size_constraint pc (lo,hi) =
  let open Bound in 
  pprintf pc "%s%s,%s%s"
    (if lo.inclusive then "[" else "(")
    (string_of_int lo.it)
    (match hi.it with [ None -> "max" | Some n -> string_of_int n ])
    (if hi.inclusive then "]" else ")")
;

value print_range_constraint pc (lo,hi) =
  let open Bound in 
  pprintf pc "%s%s,%s%s"
    (if lo.inclusive then "[" else "(")
    (match lo.it with [ None -> "min" | Some n -> string_of_float n ])
    (match hi.it with [ None -> "max" | Some n -> string_of_float n ])
    (if hi.inclusive then "]" else ")")
;

value print_id pc id = pprintf pc "%s" id ;

EXTEND_PRINTER
  pr_utype:
    [ "||"
      [ Or x y -> pprintf pc "%p || %p" next x curr y ]
    | "&&"
      [ And x y -> pprintf pc "%p && %p" next x curr y ]
    | "not"
      [ Not x -> pprintf pc "not %p" next x ]
    | "simple"
      [ Simple x -> pprintf pc "%p" print_base_type x
      | Atomic l -> pprintf pc "[@[@;%p@;]@]" (plist_with "" print_atomic 0) l
      | Ref [] id -> pprintf pc "%p" print_id id
      | Ref l id -> pprintf pc "%p.%p" (plist_with "." print_id 0) l print_id id
      | x -> pprintf pc "(%p)" print_utype x
      ]
    ] ;
  pr_base_type:
    [ [ JNull -> pprintf pc "null"
      | JString -> pprintf pc "string"
      | JBool -> pprintf pc "bool"
      | JNumber -> pprintf pc "number"
      | JArray -> pprintf pc "array"
      | JObject -> pprintf pc "object"
    ] ] ;
  pr_atomic:
    [ [ Field s t -> pprintf pc "%p: %p;" qstring s print_utype t
      | FieldRE re t -> pprintf pc "%s : %p;" re print_utype t
      | FieldRequired l -> pprintf pc "required %p;" (plist_with ", " qstring 0) l
      | ArrayOf t -> pprintf pc "of %p;" print_utype t
      | ArrayTuple l -> pprintf pc "%p;" (plist_with " * " print_utype 0) l
      | ArrayUnique -> pprintf pc "unique;"
      | ArrayIndex n t -> pprintf pc "%d: %p" n print_utype t
      | Size sc -> pprintf pc "size %p;" print_size_constraint sc
      | StringRE re -> pprintf pc "%s" re
      | NumberBound rc -> pprintf pc "bounds %p;" print_range_constraint rc
      | Sealed True -> pprintf pc "sealed;"
      | Sealed False -> pprintf pc "unsealed;"
      | OrElse t -> pprintf pc "orelse %p;" print_utype t
    ] ] ;

END;