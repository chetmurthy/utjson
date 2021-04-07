open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Utypes ;


value pr_utype = Eprinter.make "utype";
value print_utype = Eprinter.apply pr_utype;
value pr_base_type = Eprinter.make "base_type";
value print_base_type = Eprinter.apply pr_base_type;

EXTEND_PRINTER
  pr_utype:
    [ "||"
      [ Or x y -> pprintf pc "%p || %p" curr x next y ]
    | "&&"
      [ And x y -> pprintf pc "%p && %p" curr x next y ]
    | "not"
      [ Not x -> pprintf pc "not %p" curr x ]
    | "simple"
      [ Simple x -> pprintf pc "%p" print_base_type x
      | x -> pprintf pc "(%p)" print_utype x ]
    ] ;
  pr_base_type:
    [ [ JNull -> "null"
      | JString -> "string"
      | JBool -> "bool"
      | JNumber -> "number"
      | JArray -> "array"
      | JObject -> "object"
    ] ] ;

END;
