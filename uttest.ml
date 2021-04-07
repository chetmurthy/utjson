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
let item_of_string_exn s = s |> parse_string parse_utype_structure_item_eoi
let item_to_string t = print_utype_structure_item Pprintf.empty_pc t

let printer = show_utype_t
let cmp = equal_utype_t
let item_printer x = "<<"^(show_struct_item_t x)^">>"
let item_cmp = equal_struct_item_t

let success (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer ~cmp expect (of_string_exn arg)

let success_item (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:item_printer ~cmp:item_cmp
    expect (item_of_string_exn arg)

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
      ; ((And ((Simple JObject),
               (Atomic
                  [(Field ("productid", (Ref ([], "integer"))));
                   (Field ("productName", (Simple JString)));
                   (Field ("price",
                           (And ((Simple JNumber),
                                 (Atomic
                                    [(NumberBound
                                        ({ it = (Some 0.); inclusive = false },
                                         { it = None; inclusive = true }))
                                    ])
                                ))
                          ));
                   (Field ("tags",
                           (And ((Simple JArray),
                                 (Atomic
                                    [(ArrayOf (Simple JString));
                                     (Size
                                        ({ it = 1; inclusive = true },
                                         { it = None; inclusive = false }));
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
  ; "item" >:: (fun ctxt -> List.iter success_item [
        (Decls(false,[("x",Simple JString)]), "type x = string ;")
      ; ((Decls (false, [("x", (Simple JString)); ("y", (Simple JNumber))])),
         "type x = string and y = number ;")
      ; ((Decls (true, [("x", (Simple JString)); ("y", (Simple JNumber))])),
         "type rec x = string and y = number ;")
      ; ((Decls (false, [("x", (Simple JString)); ("y", (Simple JNumber))])),
         "type nonrec x = string and y = number ;")
      ; ((Decls (false,
                 [("integer", (And ((Simple JNumber), (Atomic [(MultipleOf 1.)]))))])),
         "type integer = number && [ multipleOf 1.0 ; ] ;")
      ; ((Local (
          [(Import ("https://example.com/geographical-location.schema.json",
                    "GeoLoc"))
          ],
          [(Decls (false,
                   [("product",
                     (And ((Simple JObject),
                           (Atomic
                              [(Field ("productid", (Ref ([], "integer"))));
                               (Field ("productName", (Simple JString)));
                               (Field ("price",
                                       (And ((Simple JNumber),
                                             (Atomic
                                                [(NumberBound
                                                    ({ it = (Some 0.); inclusive = false },
                                                     { it = None; inclusive = true }))
                                                ])
                                            ))
                                      ));
                               (Field ("tags",
                                       (And ((Simple JArray),
                                             (Atomic
                                                [(ArrayOf (Simple JString));
                                                 (Size
                                                    ({ it = 1; inclusive = true },
                                                     { it = None; inclusive = false }));
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
  ]


let success (expect, arg) =
  let msg = Fmt.(str "printing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:(fun x -> "<<"^x^">>") expect (to_string (of_string_exn arg))

let success_item (expect, arg) =
  let msg = Fmt.(str "printing test for code << %s >>" arg) in
  assert_equal ~msg ~printer:(fun x -> "<<"^x^">>") expect (item_to_string (item_of_string_exn arg))

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
  ; "item" >:: (fun ctxt -> List.iter success_item [
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
]; end|},
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

let item_of_string_exn s = s |> parse_string parse_utype_structure_item_eoi
let item_to_string t = print_utype_structure_item Pprintf.empty_pc t
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
        ("json-schema-samples/product-schema-MODIFIED.utj",
         "json-schema-samples/product-schema-MODIFIED.json")
      ; ("json-schema-samples/ansible-inventory-MODIFIED.utj",
         "json-schema-samples/ansible-inventory-MODIFIED.json")
      ; ("json-schema-samples/apibuilder-MODIFIED.utj",
         "json-schema-samples/apibuilder-MODIFIED.json")
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
