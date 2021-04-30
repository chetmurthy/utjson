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
open Utmigrate.Self
open Uttypecheck

let normalize_structure stl =
  let dt = make_dt () in
  let old_migrate_struct_item_t = dt.migrate_struct_item_t in
  let new_migrate_struct_item_t dt = function
      StTypes(loc, true, l) -> StTypes(loc, true, List.stable_sort Stdlib.compare l)
    | st -> old_migrate_struct_item_t dt st in
  let dt = { dt with migrate_struct_item_t = new_migrate_struct_item_t } in
  dt.migrate_structure dt stl

let success (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer ~cmp:Reloc.(wrap_cmp cmp utype_t) expect (of_string_exn arg)

let fail (exnmsg, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_raises_exn_pattern ~msg exnmsg (fun () -> of_string_exn arg)

let success_struct_item (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:struct_item_printer ~cmp:Reloc.(wrap_cmp struct_item_cmp struct_item_t)
    expect (struct_item_of_string_exn arg)

let success_module_expr (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:module_expr_printer ~cmp:Reloc.(wrap_cmp module_expr_cmp module_expr_t)
    expect (module_expr_of_string_exn arg)

let success_module_type (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:module_type_printer ~cmp:Reloc.(wrap_cmp module_type_cmp module_type_t)
    expect (module_type_of_string_exn arg)

let success_sig_item (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:signature_printer ~cmp:Reloc.(wrap_cmp signature_cmp signature)
    expect (sig_item_of_string_exn arg)


let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let parsing = "parsing" >::: [
    "utype" >:: (fun ctxt -> List.iter success [
        (Simple (Ploc.dummy, JString), "string")
      ; ((Ref(Ploc.dummy, (Some (REL (ID.of_string "M")), ID.of_string "t"))), "M.t")
      ; ((Atomic
            (Ploc.dummy, [(Enum
                (Ploc.dummy, [`List ([`Float (1.); `Float (2.)]); `Assoc ([("a", `Float (2.))]);
                 `Bool (true)]))
            ])),
         {|[ enum [1,2], {"a":2}, true ; ]|})
      ; ((Atomic (Ploc.dummy, [(Field (Ploc.dummy, "a", (Simple (Ploc.dummy, JObject))))])),
         {|[ "a": object ]|})
      ; ((Atomic (Ploc.dummy, [(Field (Ploc.dummy, "a", (Simple (Ploc.dummy, JObject))));FieldRequired (Ploc.dummy, ["a"])])),
         {|[ required "a": object ]|})
      ; ((Atomic (Ploc.dummy, [(Field (Ploc.dummy, "a", (Simple (Ploc.dummy, JObject))))])),
         {|[ "a": object ; ]|})
      ; ((Atomic (Ploc.dummy, [(Field (Ploc.dummy, "a", (Simple (Ploc.dummy, JObject)))); (Field (Ploc.dummy, "b", (Simple (Ploc.dummy, JObject))))])),
         {|[ "a": object ; "b" : object ]|})
      ; ((Atomic (Ploc.dummy, [(Field (Ploc.dummy, "a", (Simple (Ploc.dummy, JObject)))); (Field (Ploc.dummy, "b", (Simple (Ploc.dummy, JObject))))])),
         {|[ "a": object ; "b" : object ;]|})
      ; ((And (Ploc.dummy, (Simple (Ploc.dummy, JObject)),
               (Atomic
                  (Ploc.dummy, [(Field (Ploc.dummy, "productid", (Ref(Ploc.dummy, (None, ID.of_string "integer")))));
                   (Field (Ploc.dummy, "productName", (Simple (Ploc.dummy, JString))));
                   (Field (Ploc.dummy, "price",
                           (And (Ploc.dummy, (Simple (Ploc.dummy, JNumber)),
                                 (Atomic
                                    (Ploc.dummy, [(NumberBound
                                        (Ploc.dummy, ({ it = (Some 0.); exclusive = true },
                                         { it = None; exclusive = false })))
                                    ]))
                                ))
                          ));
                   (Field (Ploc.dummy, "tags",
                           (And (Ploc.dummy, (Simple (Ploc.dummy, JArray)),
                                 (Atomic
                                    (Ploc.dummy, [(ArrayOf (Ploc.dummy, Simple (Ploc.dummy, JString)));
                                     (Size
                                        (Ploc.dummy, ({ it = 1; exclusive = false },
                                         { it = None; exclusive = true })));
                                     ArrayUnique Ploc.dummy
                                    ]))
                                ))
                          ));
                   (Field (Ploc.dummy, "dimensions",
                           (And (Ploc.dummy, (Simple (Ploc.dummy, JObject)),
                                 (Atomic
                                    (Ploc.dummy, [(Field (Ploc.dummy, "length", (Simple (Ploc.dummy, JNumber))));
                                     (Field (Ploc.dummy, "width", (Simple (Ploc.dummy, JNumber))));
                                     (Field (Ploc.dummy, "height", (Simple (Ploc.dummy, JNumber))));
                                     (FieldRequired (Ploc.dummy, ["length"; "width"; "height"]))]))
                                ))
                          ));
                   (FieldRequired (Ploc.dummy, ["productid"; "productName"; "price"; "tags"]))]))
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
  ; "sealed-utype" >:: (fun ctxt -> List.iter success [
        ((Seal (Ploc.dummy, (Simple (Ploc.dummy, JObject)), [], UtFalse Ploc.dummy)), "seal object")
      ; ((Seal (Ploc.dummy, (Simple (Ploc.dummy, JObject)), [("^[^\\+#$\\s\\.]+$", (Simple (Ploc.dummy, JString)))], UtFalse Ploc.dummy)),
         "seal object with /^[^\+#$\s\.]+$/ : string")
      ]
      )
  ; "utype-fail" >:: (fun ctxt -> List.iter fail [
        ("[atomic_utype] expected after '[' (in [utype])",
         {|[  ]|})
      ]
      )
  ; "struct_item" >:: (fun ctxt -> List.iter success_struct_item [
      (StTypes(Ploc.dummy, false,[(ID.of_string "x",None, Simple (Ploc.dummy, JString))]), "type x = string ;")
    ; ((StTypes (Ploc.dummy, false, [(ID.of_string "x",None, (Simple (Ploc.dummy, JString))); (ID.of_string "y",None, (Simple (Ploc.dummy, JNumber)))])),
       "type x = string and y = number ;")
    ; ((StTypes (Ploc.dummy, true, [(ID.of_string "x",None, (Simple (Ploc.dummy, JString))); (ID.of_string "y",None, (Simple (Ploc.dummy, JNumber)))])),
       "type rec x = string and y = number ;")
    ; ((StTypes (Ploc.dummy, false, [(ID.of_string "x",None, (Simple (Ploc.dummy, JString))); (ID.of_string "y",None, (Simple (Ploc.dummy, JNumber)))])),
       "type nonrec x = string and y = number ;")
    ; ((StTypes (Ploc.dummy, false,
                 [(ID.of_string "integer",None, (And (Ploc.dummy, (Simple (Ploc.dummy, JNumber)), (Atomic (Ploc.dummy, [(MultipleOf (Ploc.dummy, 1.))])))))])),
       "type integer = number && [ multipleOf 1.0 ; ] ;")
    ; ((StOpen (Ploc.dummy, DEREF (REL (ID.of_string "M"), (ID.of_string "N")), None)),
       "open M.N;")
    ; ((StInclude (Ploc.dummy, DEREF (REL (ID.of_string "M"), (ID.of_string "N")), None)),
       "include M.N;")
    ; ((StModuleBinding (Ploc.dummy, ID.of_string "M",
                         (MeStruct (Ploc.dummy, [(StTypes (Ploc.dummy, false, [(ID.of_string "t",None, (Simple (Ploc.dummy, JObject)))]))])))),
       "module M = struct type t = object ; end;")
    ; ((StModuleType (Ploc.dummy, (ID.of_string "MTY"), (MtSig (Ploc.dummy, [(SiType (Ploc.dummy, ID.of_string "t",AN.mk false))])))),
       "module type MTY = sig type [] t ; end;")
    ; ((StLocal (Ploc.dummy, 
        [(StImport (Ploc.dummy, "https://example.com/geographical-location.schema.json",
                    (ID.of_string "GeoLoc")))
        ],
        [(StTypes (Ploc.dummy, false,
                   [(ID.of_string "product",None,
                     (And (Ploc.dummy, (Simple (Ploc.dummy, JObject)),
                           (Atomic
                              (Ploc.dummy, [(Field (Ploc.dummy, "productid", (Ref(Ploc.dummy, (None, ID.of_string "integer")))));
                               (Field (Ploc.dummy, "productName", (Simple (Ploc.dummy, JString))));
                               (Field (Ploc.dummy, "price",
                                       (And (Ploc.dummy, (Simple (Ploc.dummy, JNumber)),
                                             (Atomic
                                                (Ploc.dummy, [(NumberBound
                                                    (Ploc.dummy, ({ it = (Some 0.); exclusive = true },
                                                     { it = None; exclusive = false })))
                                                ]))
                                            ))
                                      ));
                               (Field (Ploc.dummy, "tags",
                                       (And (Ploc.dummy, (Simple (Ploc.dummy, JArray)),
                                             (Atomic
                                                (Ploc.dummy, [(ArrayOf (Ploc.dummy, Simple (Ploc.dummy, JString)));
                                                 (Size
                                                    (Ploc.dummy, ({ it = 1; exclusive = false },
                                                     { it = None; exclusive = true })));
                                                 ArrayUnique Ploc.dummy]))
                                            ))
                                      ));
                               (Field (Ploc.dummy, "dimensions",
                                       (And (Ploc.dummy, (Simple (Ploc.dummy, JObject)),
                                             (Atomic
                                                (Ploc.dummy, [(Field (Ploc.dummy, "length", (Simple (Ploc.dummy, JNumber))));
                                                 (Field (Ploc.dummy, "width", (Simple (Ploc.dummy, JNumber))));
                                                 (Field (Ploc.dummy, "height", (Simple (Ploc.dummy, JNumber))));
                                                 (FieldRequired (Ploc.dummy, ["length"; "width"; "height"]))]))
                                            ))
                                      ));
                               (FieldRequired
                                  (Ploc.dummy, ["productid"; "productName"; "price"; "tags"]));
                               (Field (Ploc.dummy, "warehouseLocation", (Ref(Ploc.dummy, (Some (REL (ID.of_string "GeoLoc")), ID.of_string "latlong")))))
                              ]))
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
  ; "sealed-struct_item" >:: (fun ctxt -> List.iter success_struct_item [
      (StTypes(Ploc.dummy, false,[(ID.of_string "x",Some (AN.mk true), Simple (Ploc.dummy, JString))]),
       "type [sealed] x = string ;")
    ; (StTypes(Ploc.dummy, true,[(ID.of_string "x",Some (AN.mk true), Simple (Ploc.dummy, JString));(ID.of_string "y",Some (AN.mk false), Simple (Ploc.dummy, JNumber))]),
       "type rec [sealed] x = string and [] y = number ;")
    ]

      )
  ; "module_type" >:: (fun ctxt -> List.iter success_module_type [
      (MtPath (Ploc.dummy, (Some (REL (ID.of_string "M")), ID.of_string "N")),
       "M.N")
    ; ((MtSig (Ploc.dummy, [])),
       "sig end")
    ; ((MtFunctorType (Ploc.dummy, ((ID.of_string "M"), (MtSig (Ploc.dummy, [(SiType (Ploc.dummy, ID.of_string "u",AN.mk false))]))), (MtSig (Ploc.dummy, [(SiType (Ploc.dummy, ID.of_string "t",AN.mk false))])))),
       "functor (M: sig type [] u; end) -> sig type [] t ; end")
    ; ((MtSig
          (Ploc.dummy, [(SiType (Ploc.dummy, ID.of_string "t",AN.mk true)); (SiModuleBinding (Ploc.dummy, (ID.of_string "M"), (MtPath (Ploc.dummy, (None, (ID.of_string "MTY"))))));
           (SiModuleType (Ploc.dummy, (ID.of_string "MTY2"), (MtSig (Ploc.dummy, [(SiType (Ploc.dummy, ID.of_string "u",AN.mk ~base_types:[JObject] false))]))))])),
       "sig type [sealed] t; module M : MTY ; module type MTY2 = sig type [object] u ; end ; end")
      ]

      )
  ; "module_expr" >:: (fun ctxt -> List.iter success_module_expr [
      (MePath (Ploc.dummy, DEREF (REL (ID.of_string "M"), (ID.of_string "N"))),
       "M.N")
    ; ((MeFunctorApp (Ploc.dummy, (MePath (Ploc.dummy, REL (ID.of_string "M"))), (MePath (Ploc.dummy, REL (ID.of_string "N"))))),
       "M(N)")
    ; ((MeFunctorApp (Ploc.dummy, (MePath (Ploc.dummy, REL (ID.of_string "M"))),
                      (MeStruct (Ploc.dummy, [(StTypes (Ploc.dummy, false, [(ID.of_string "t",None, (Simple (Ploc.dummy, JObject)))]))])))),
       "M(struct type t = object ; end)")
    ; ((MeFunctor (Ploc.dummy, ((ID.of_string "M1"), (MtSig (Ploc.dummy, []))),
                   (MeFunctor (Ploc.dummy, ((ID.of_string "M2"), (MtSig (Ploc.dummy, []))),
                               (MeStruct (Ploc.dummy, [(StTypes (Ploc.dummy, false, [(ID.of_string "t",None, (Simple (Ploc.dummy, JObject)))]))]))))
                  )),
       "functor (M1: sig end)(M2:sig end) -> struct type t = object ; end")
      ]

      )
  ; "sig_item" >:: (fun ctxt -> List.iter success_sig_item [
      ([SiInclude (Ploc.dummy, DEREF (REL (ID.of_string "M"), (ID.of_string "N")))],
       "include M.N;")
    ; ([SiModuleBinding (Ploc.dummy, (ID.of_string "M"), (MtPath (Ploc.dummy, (None, (ID.of_string "MTY")))))],
       "module M : MTY;")
    ; ([SiModuleType (Ploc.dummy, (ID.of_string "MTY"), (MtSig (Ploc.dummy, [(SiType (Ploc.dummy, ID.of_string "t",AN.mk false))])))],
       "module type MTY = sig type [] t ; end;")
    ]
      )
  ; "sealed-sig_item" >:: (fun ctxt -> List.iter success_sig_item [
      ([(SiType (Ploc.dummy, { prefix = "x"; index = -1 }, AN.mk false))],
       "type [] x;")
    ; ([(SiType (Ploc.dummy, { prefix = "x"; index = -1 }, AN.mk true))],
       "type [sealed] x;")
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
    "price": number && [ bounds (0.0,max]; ];
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
    "price": number && [ bounds (0.0,max]; ];
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

let structure_printer x = Normal.structure_printer (normalize_structure x)
let structure_cmp a b = Reloc.(wrap_cmp structure_cmp structure (normalize_structure a) (normalize_structure b))

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
