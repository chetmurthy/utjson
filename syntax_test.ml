open OUnit2
open OUnitTest
open Pa_ppx_testutils
open Uttestutil

open Utypes
open Utparse0
open Utprint
open Utio
open Utio.Debug
open Utconv
open Utmigrate
open Uttypecheck

let normalize_structure stl =
  let dt = make_dt () in
  let old_migrate_struct_item_t = dt.migrate_struct_item_t in
  let new_migrate_struct_item_t dt = function
      StTypes(true, l) -> StTypes(true, List.stable_sort Stdlib.compare l)
    | st -> old_migrate_struct_item_t dt st in
  let dt = { dt with migrate_struct_item_t = new_migrate_struct_item_t } in
  dt.migrate_structure dt stl

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
  assert_equal ~msg ~printer:signature_printer ~cmp:signature_cmp
    expect (sig_item_of_string_exn arg)


let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let parsing = "parsing" >::: [
    "utype" >:: (fun ctxt -> List.iter success [
        (Simple JString, "string")
      ; ((Ref (Some (REL (ID.of_string "M")), ID.of_string "t")), "M.t")
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
                  [(Field ("productid", (Ref (None, ID.of_string "integer"))));
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
        (StTypes(false,[(ID.of_string "x",Simple JString)]), "type x = string ;")
      ; ((StTypes (false, [(ID.of_string "x", (Simple JString)); (ID.of_string "y", (Simple JNumber))])),
         "type x = string and y = number ;")
      ; ((StTypes (true, [(ID.of_string "x", (Simple JString)); (ID.of_string "y", (Simple JNumber))])),
         "type rec x = string and y = number ;")
      ; ((StTypes (false, [(ID.of_string "x", (Simple JString)); (ID.of_string "y", (Simple JNumber))])),
         "type nonrec x = string and y = number ;")
      ; ((StTypes (false,
                 [(ID.of_string "integer", (And ((Simple JNumber), (Atomic [(MultipleOf 1.)]))))])),
         "type integer = number && [ multipleOf 1.0 ; ] ;")
      ; ((StOpen (DEREF (REL (ID.of_string "M"), (ID.of_string "N")), None)),
         "open M.N;")
      ; ((StInclude (DEREF (REL (ID.of_string "M"), (ID.of_string "N")), None)),
         "include M.N;")
      ; ((StModuleBinding (ID.of_string "M",
                           (MeStruct [(StTypes (false, [(ID.of_string "t", (Simple JObject))]))]))),
         "module M = struct type t = object ; end;")
      ; ((StModuleType ((ID.of_string "MTY"), (MtSig [(SiType (ID.of_string "t"))]))),
         "module type MTY = sig type t ; end;")
      ; ((StLocal (
          [(StImport ("https://example.com/geographical-location.schema.json",
                    (ID.of_string "GeoLoc")))
          ],
          [(StTypes (false,
                   [(ID.of_string "product",
                     (And ((Simple JObject),
                           (Atomic
                              [(Field ("productid", (Ref (None, ID.of_string "integer"))));
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
                               (Field ("warehouseLocation", (Ref (Some (REL (ID.of_string "GeoLoc")), ID.of_string "latlong"))))
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
      (MtPath (Some (REL (ID.of_string "M")), ID.of_string "N"),
       "M.N")
    ; ((MtSig []),
       "sig end")
    ; ((MtFunctorType (((ID.of_string "M"), (MtSig [(SiType (ID.of_string "u"))])), (MtSig [(SiType (ID.of_string "t"))]))),
       "functor (M: sig type u; end) -> sig type t ; end")
    ; ((MtSig
          [(SiType (ID.of_string "t")); (SiModuleBinding ((ID.of_string "M"), (MtPath (None, (ID.of_string "MTY")))));
           (SiModuleType ((ID.of_string "MTY2"), (MtSig [(SiType (ID.of_string "u"))])))]),
       "sig type t; module M : MTY ; module type MTY2 = sig type u ; end ; end")
      ]

      )
  ; "module_expr" >:: (fun ctxt -> List.iter success_module_expr [
      (MePath (DEREF (REL (ID.of_string "M"), (ID.of_string "N"))),
       "M.N")
    ; ((MeFunctorApp ((MePath (REL (ID.of_string "M"))), (MePath (REL (ID.of_string "N"))))),
       "M(N)")
    ; ((MeFunctorApp ((MePath (REL (ID.of_string "M"))),
                      (MeStruct [(StTypes (false, [(ID.of_string "t", (Simple JObject))]))]))),
       "M(struct type t = object ; end)")
    ; ((MeFunctor (((ID.of_string "M1"), (MtSig [])),
                   (MeFunctor (((ID.of_string "M2"), (MtSig [])),
                               (MeStruct [(StTypes (false, [(ID.of_string "t", (Simple JObject))]))])))
                  )),
       "functor (M1: sig end)(M2:sig end) -> struct type t = object ; end")
      ]

      )
  ; "sig_item" >:: (fun ctxt -> List.iter success_sig_item [
      ([SiInclude (DEREF (REL (ID.of_string "M"), (ID.of_string "N")))],
       "include M.N;")
    ; ([SiModuleBinding ((ID.of_string "M"), (MtPath (None, (ID.of_string "MTY"))))],
       "module M : MTY;")
    ; ([SiModuleType ((ID.of_string "MTY"), (MtSig [(SiType (ID.of_string "t"))]))],
       "module type MTY = sig type t ; end;")
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

let structure_cmp a b = structure_cmp (normalize_structure a) (normalize_structure b)

let success (expect, f) =
  assert_equal ~msg:f ~printer:structure_printer ~cmp:structure_cmp
    (structure_of_string_exn expect)
    (convert_file (CC.mk()) f)

let successf (expectf, f) =
  let filepath = ["schema-golden/schema-overrides"] in
  assert_equal ~msg:f ~printer:structure_printer ~cmp:structure_cmp
    (load_file expectf)
    (convert_file (CC.mk ~filepath ()) f)

let successf_test (a,b) =
  (a^" || "^b) >:: (fun ctxt ->
      successf (a,b)
    )

let convert = "convert" >::: (List.map successf_test [
    ("schema-golden/schema-overrides/geographical-location.schema.utj",
     "schema-overrides/geographical-location.schema.json")
  ; ("schema-golden/schema-overrides/product-schema.utj",
     "schema-overrides/product-schema.json")
  ; ("schema-golden/schema-overrides/ansible-inventory.utj",
     "schemastore/src/schemas/json/ansible-inventory.json")
  ; ("schema-golden/schema-overrides/apibuilder.utj",
     "schema-overrides/apibuilder.json")
  ; ("schema-golden/schema-overrides/apple-app-site-association.utj",
     "schemastore/src/schemas/json/apple-app-site-association.json")
  ; ("schema-golden/schema-overrides/appsettings.utj",
     "schema-overrides/appsettings.json")
  ; ("schema-golden/schema-overrides/appsscript.utj",
     "schemastore/src/schemas/json/appsscript.json")
  ; ("schema-golden/schema-overrides/appveyor.utj",
     "schemastore/src/schemas/json/appveyor.json")
  ; ("schema-golden/schema-overrides/asmdef.utj",
     "schemastore/src/schemas/json/asmdef.json")
  ; ("schema-golden/schema-overrides/avro-avsc.utj",
     "schemastore/src/schemas/json/avro-avsc.json")
  ; ("schema-golden/schema-overrides/azure-iot-edgeagent-deployment-1.0.utj",
     "schemastore/src/schemas/json/azure-iot-edgeagent-deployment-1.0.json")
  ]
  )

let tests = "all" >::: [
    simple
  ; parsing
  ; printing
  ; convert
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
