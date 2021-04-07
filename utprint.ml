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
      [ Or x y -> pprintf pc "%p || %p" next x curr y ]
    | "&&"
      [ And x y -> pprintf pc "%p && %p" next x curr y ]
    | "not"
      [ Not x -> pprintf pc "not %p" next x ]
    | "simple"
      [ Simple x -> pprintf pc "%p" print_base_type x
      | x -> pprintf pc "(%p)" print_utype x ]
    ] ;
  pr_base_type:
    [ [ JNull -> pprintf pc "null"
      | JString -> pprintf pc "string"
      | JBool -> pprintf pc "bool"
      | JNumber -> pprintf pc "number"
      | JArray -> pprintf pc "array"
      | JObject -> pprintf pc "object"
    ] ] ;

END;
