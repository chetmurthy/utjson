open OUnit2
open OUnitTest
open Pa_ppx_testutils

open Utypes
open Utparse0
open Utprint
open Utconv

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;

let warning s = Fmt.(pf stderr "%s\n%!" s)

let matches ~pattern text =
  match Str.search_forward (Str.regexp (Str.quote pattern)) text 0 with
    _ -> true
  | exception Not_found -> false

let assert_raises_exn_pattern ?msg pattern f =
  Testutil.assert_raises_exn_pred ?msg
    (function
        Failure msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Stdlib.Stream.Error msg) when matches ~pattern msg -> true
      | Stdlib.Stream.Error msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Failure msg) when matches ~pattern msg -> true
      | Invalid_argument msg when matches ~pattern msg -> true
      | _ -> false
    )
    f

let of_string_exn s = s |> parse_string parse_utype_eoi
let to_string t = print_utype Pprintf.empty_pc t

let struct_item_of_string_exn s = s |> parse_string parse_struct_item_eoi
let struct_item_to_string t = print_struct_item Pprintf.empty_pc t

let sig_item_of_string_exn s = s |> parse_string parse_sig_item_eoi
let sig_item_to_string t = print_sig_item Pprintf.empty_pc t

let module_expr_of_string_exn s = s |> parse_string parse_module_expr_eoi
let module_expr_to_string t = print_module_expr Pprintf.empty_pc t

let module_type_of_string_exn s = s |> parse_string parse_module_type_eoi
let module_type_to_string t = print_module_type Pprintf.empty_pc t

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

let success (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer ~cmp expect (of_string_exn arg)

let fail (exnmsg, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_raises_exn_pattern ~msg exnmsg (fun () -> of_string_exn arg)

let success_struct_item (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:struct_item_printer ~cmp:struct_item_cmp
    expect (struct_item_of_string_exn arg)

let success_module_expr (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:module_expr_printer ~cmp:module_expr_cmp
    expect (module_expr_of_string_exn arg)

let success_module_type (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:module_type_printer ~cmp:module_type_cmp
    expect (module_type_of_string_exn arg)

let success_sig_item (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:sig_item_printer ~cmp:sig_item_cmp
    expect (sig_item_of_string_exn arg)


let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let parsing = "parsing" >::: [
    "utype" >:: (fun ctxt -> List.iter success [
        (Simple JString, "string")
      ; ((Ref (["M"], "t")), "M.t")
      ; ((Atomic
            [(Enum
                [`List ([`Int (1); `Int (2)]); `Assoc ([("a", `Int (2))]);
                 `Bool (true)])
            ]),
         {|[ enum [1,2], {"a":2}, true ; ]|})
      ; ((Atomic [(Field ("a", (Simple JObject)))]),
         {|[ "a": object ]|})
      ; ((Atomic [(Field ("a", (Simple JObject)))]),
         {|[ "a": object ; ]|})
      ; ((Atomic [(Field ("a", (Simple JObject))); (Field ("b", (Simple JObject)))]),
         {|[ "a": object ; "b" : object ]|})
      ; ((Atomic [(Field ("a", (Simple JObject))); (Field ("b", (Simple JObject)))]),
         {|[ "a": object ; "b" : object ;]|})
      ; ((And ((Simple JObject),
               (Atomic
                  [(Field ("productid", (Ref ([], "integer"))));
                   (Field ("productName", (Simple JString)));
                   (Field ("price",
                           (And ((Simple JNumber),
                                 (Atomic
                                    [(NumberBound
                                        ({ it = (Some 0.); exclusive = true },
                                         { it = None; exclusive = false }))
                                    ])
                                ))
                          ));
                   (Field ("tags",
                           (And ((Simple JArray),
                                 (Atomic
                                    [(ArrayOf (Simple JString));
                                     (Size
                                        ({ it = 1; exclusive = false },
                                         { it = None; exclusive = true }));
                                     ArrayUnique
                                    ])
                                ))
                          ));
                   (Field ("dimensions",
                           (And ((Simple JObject),
                                 (Atomic
                                    [(Field ("length", (Simple JNumber)));
                                     (Field ("width", (Simple JNumber)));
                                     (Field ("height", (Simple JNumber)));
                                     (FieldRequired ["length"; "width"; "height"])])
                                ))
                          ));
                   (FieldRequired ["productid"; "productName"; "price"; "tags"])])
              )), {|
         object && [ "productid" : integer ;
                     "productName" : string ;
                     "price" : (number && [ bounds (0, max] ; ]) ;
                     "tags" : (array && [ of string ; size [1,max) ; unique ; ]) ;
                     "dimensions" : (object && [ "length" : number ; "width" : number ; "height" : number ;
                                                 required "length", "width", "height" ; ]) ;
                     required "productid", "productName", "price", "tags" ;
                   ]
|})
      ]
      )
  ; "utype-fail" >:: (fun ctxt -> List.iter fail [
        ("[atomic_utype] expected after '[' (in [utype])",
         {|[  ]|})
      ]
      )
  ; "struct_item" >:: (fun ctxt -> List.iter success_struct_item [
        (StTypes(false,[("x",Simple JString)]), "type x = string ;")
      ; ((StTypes (false, [("x", (Simple JString)); ("y", (Simple JNumber))])),
         "type x = string and y = number ;")
      ; ((StTypes (true, [("x", (Simple JString)); ("y", (Simple JNumber))])),
         "type rec x = string and y = number ;")
      ; ((StTypes (false, [("x", (Simple JString)); ("y", (Simple JNumber))])),
         "type nonrec x = string and y = number ;")
      ; ((StTypes (false,
                 [("integer", (And ((Simple JNumber), (Atomic [(MultipleOf 1.)]))))])),
         "type integer = number && [ multipleOf 1.0 ; ] ;")
      ; ((StOpen ["M"; "N"]),
         "open M.N;")
      ; ((StInclude ["M"; "N"]),
         "include M.N;")
      ; ((StModuleBinding ("M",
                           (MeStruct [(StTypes (false, [("t", (Simple JObject))]))]))),
         "module M = struct type t = object ; end;")
      ; ((StModuleType ("MTY", (MtSig [(SiType "t")]))),
         "module type MTY = sig t ; end;")
      ; ((StLocal (
          [(StImport ("https://example.com/geographical-location.schema.json",
                    "GeoLoc"))
          ],
          [(StTypes (false,
                   [("product",
                     (And ((Simple JObject),
                           (Atomic
                              [(Field ("productid", (Ref ([], "integer"))));
                               (Field ("productName", (Simple JString)));
                               (Field ("price",
                                       (And ((Simple JNumber),
                                             (Atomic
                                                [(NumberBound
                                                    ({ it = (Some 0.); exclusive = true },
                                                     { it = None; exclusive = false }))
                                                ])
                                            ))
                                      ));
                               (Field ("tags",
                                       (And ((Simple JArray),
                                             (Atomic
                                                [(ArrayOf (Simple JString));
                                                 (Size
                                                    ({ it = 1; exclusive = false },
                                                     { it = None; exclusive = true }));
                                                 ArrayUnique])
                                            ))
                                      ));
                               (Field ("dimensions",
                                       (And ((Simple JObject),
                                             (Atomic
                                                [(Field ("length", (Simple JNumber)));
                                                 (Field ("width", (Simple JNumber)));
                                                 (Field ("height", (Simple JNumber)));
                                                 (FieldRequired ["length"; "width"; "height"])])
                                            ))
                                      ));
                               (FieldRequired
                                  ["productid"; "productName"; "price"; "tags"]);
                               (Field ("warehouseLocation", (Ref (["GeoLoc"], "latlong"))))
                              ])
                          )))
                   ]
                  ))
          ]
        )),
         {|
local
import "https://example.com/geographical-location.schema.json" as GeoLoc ;
in
type product = object && [ "productid" : integer ;
                     "productName" : string ;
                     "price" : (number && [ bounds (0, max] ; ]) ;
                     "tags" : (array && [ of string ; size [1,max) ; unique ; ]) ;
                     "dimensions" : (object && [ "length" : number ; "width" : number ; "height" : number ;
                                                 required "length", "width", "height" ; ]) ;
                     required "productid", "productName", "price", "tags" ;
                     "warehouseLocation" : GeoLoc.latlong ;
                   ] ;
end ;
|})
      ]

      )
  ; "module_type" >:: (fun ctxt -> List.iter success_module_type [
      (MtPath ["M";"N"],
       "M.N")
    ; ((MtSig []),
       "sig end")
    ; ((MtFunctorType (("M", (MtSig [(SiType "u")])), (MtSig [(SiType "t")]))),
       "functor (M: sig u; end) -> sig t ; end")
    ; ((MtSig
          [(SiType "t"); (SiModuleBinding ("M", (MtPath ["MTY"])));
           (SiModuleType ("MTY2", (MtSig [(SiType "u")])))]),
       "sig t; module M : MTY ; module type MTY2 = sig u ; end ; end")
      ]

      )
  ; "module_expr" >:: (fun ctxt -> List.iter success_module_expr [
      (MePath ["M";"N"],
       "M.N")
    ; ((MeFunctorApp ((MePath ["M"]), (MePath ["N"]))),
       "M(N)")
    ; ((MeFunctorApp ((MePath ["M"]),
                      (MeStruct [(StTypes (false, [("t", (Simple JObject))]))]))),
       "M(struct type t = object ; end)")
    ; ((MeFunctor (("M1", (MtSig [])),
                   (MeFunctor (("M2", (MtSig [])),
                               (MeStruct [(StTypes (false, [("t", (Simple JObject))]))])))
                  )),
       "functor (M1: sig end)(M2:sig end) -> struct type t = object ; end")
      ]

      )
  ; "sig_item" >:: (fun ctxt -> List.iter success_sig_item [
      ((SiInclude ["M"; "N"]),
       "include M.N;")
    ; ((SiModuleBinding ("M", (MtPath ["MTY"]))),
       "module M : MTY;")
    ; ((SiModuleType ("MTY", (MtSig [(SiType "t")]))),
       "module type MTY = sig t ; end;")
    ]
      )
  ]


let success (expect, arg) =
  let msg = Fmt.(str "printing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:(fun x -> "<<"^x^">>") expect (to_string (of_string_exn arg))

let success_struct_item (expect, arg) =
  let msg = Fmt.(str "printing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:(fun x -> "<<"^x^">>") expect (struct_item_to_string (struct_item_of_string_exn arg))

let printing = "printing" >::: [
    "utype" >:: (fun ctxt -> List.iter success [
        ("string", "string")
      ; ("M.t", "M.t")
      ; ({|object && [
    "productid": integer;
    "productName": string;
    "price": number && [ bounds (0.,max]; ];
    "tags": array && [
        of string;
        size [1,max);
        unique;
];
    "dimensions": object && [
        "length": number;
        "width": number;
        "height": number;
        required "length",  "width",  "height";
];
    required "productid",  "productName",  "price",  "tags";
]|}, {|
         object && [ "productid" : integer ;
                     "productName" : string ;
                     "price" : number && [ bounds (0, max] ; ] ;
                     "tags" : array && [ of string ; size [1,max) ; unique ; ] ;
                     "dimensions" : object && [ "length" : number ; "width" : number ; "height" : number ;
                                                 required "length", "width", "height" ; ] ;
                     required "productid", "productName", "price", "tags" ;
                   ]
|})
      ]
      )
  ; "struct_item" >:: (fun ctxt -> List.iter success_struct_item [
        ("type nonrec x = string;", "type x = string ;")
      ; ({|type nonrec x = string
and y = number;|},
         {|type nonrec x = string and y = number;|})
      ; ({|type rec x = string
and y = number;|},
                  "type rec x = string and y = number ;")
      ; ({|type nonrec x = string
and y = number;|},
         "type nonrec x = string and y = number ;")
      ; ({|local import "https://example.com/geographical-location.schema.json" as GeoLoc; in type nonrec product = object && [
    "productid": integer;
    "productName": string;
    "price": number && [ bounds (0.,max]; ];
    "tags": array && [
        of string;
        size [1,max);
        unique;
];
    "dimensions": object && [
        "length": number;
        "width": number;
        "height": number;
        required "length",  "width",  "height";
];
    required "productid",  "productName",  "price",  "tags";
    "warehouseLocation": GeoLoc.latlong;
]; end;|},
         {|
local
import "https://example.com/geographical-location.schema.json" as GeoLoc ;
in
type product = object && [ "productid" : integer ;
                     "productName" : string ;
                     "price" : (number && [ bounds (0, max] ; ]) ;
                     "tags" : (array && [ of string ; size [1,max) ; unique ; ]) ;
                     "dimensions" : (object && [ "length" : number ; "width" : number ; "height" : number ;
                                                 required "length", "width", "height" ; ]) ;
                     required "productid", "productName", "price", "tags" ;
                     "warehouseLocation" : GeoLoc.latlong ;
                   ] ;
end ;
|})
      ]
      )
  ]

let item_of_string_exn s = s |> parse_string parse_struct_item_eoi
let item_to_string t = print_struct_item Pprintf.empty_pc t
let item_printer x = "<<"^(show_struct_item_t x)^">>"
let item_cmp = equal_struct_item_t

let success (expect, f) =
  assert_equal ~msg:f ~printer:item_printer ~cmp:item_cmp
    (item_of_string_exn expect)
    (load_file f)

let successf (expectf, f) =
  assert_equal ~msg:f ~printer:item_printer ~cmp:item_cmp
    (load_file expectf)
    (load_file f)

let convert = "convert" >::: [
    "convert" >:: (fun ctxt -> List.iter successf [
        ("schema-overrides/product-schema.utj",
         "schema-overrides/product-schema.json")
      ; ("schema-overrides/ansible-inventory.utj",
         "schemastore/src/schemas/json/ansible-inventory.json")
      ; ("schema-overrides/apibuilder.utj",
         "schema-overrides/apibuilder.json")
      ; ("schema-overrides/apple-app-site-association.utj",
         "schemastore/src/schemas/json/apple-app-site-association.json")
      ; ("schema-overrides/appsettings.utj",
         "schema-overrides/appsettings.json")
      ; ("schema-overrides/appsscript.utj",
         "schemastore/src/schemas/json/appsscript.json")
      ; ("schema-overrides/appveyor.utj",
         "schemastore/src/schemas/json/appveyor.json")
      ; ("schema-overrides/asmdef.utj",
         "schemastore/src/schemas/json/asmdef.json")
      ; ("schema-overrides/avro-avsc.utj",
         "schemastore/src/schemas/json/avro-avsc.json")
      ; ("schema-overrides/azure-iot-edgeagent-deployment-1.0.utj",
         "schemastore/src/schemas/json/azure-iot-edgeagent-deployment-1.0.json")
      ]
      )
  ]


let tests = "all" >::: [
    simple
  ; parsing
  ; printing
  ; convert
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
