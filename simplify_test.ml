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
open Uttypecheck
open Utextract
open Utsimplify

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let coalesce_atomics = "coalesce_atomics" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
type nonrec t = object && [
    "productId": Predefined.integer;
    "productName": string;
    "price": number && [ bounds (0.0,max]; ];
    "tags": array && [
        unique;
        of string;
        size [1,max];
];
    "dimensions": object && [
        "length": number;
        "width": number;
        "height": number;
        required "length",  "width",  "height";
];
    "warehouseLocation": M0.t;
    required "productId",  "productName",  "price";
];
|} |> structure_of_string_exn )
        ({|
  type nonrec t = object && [
    "productId": Predefined.integer;
    "productName": string;
    "price": number && [ bounds (0.,max] ];
    "tags": array && [ of string ] && [ unique ] && array && [ size [1,max] ];
    "dimensions": object && [
        "length": number;
        "width": number;
        "height": number;
     ] && [ required "length", "width", "height" ];
    "warehouseLocation": M0.t;
  ] && [ required "productId", "productName", "price" ] ;
|} |> structure_of_string_exn |> full_simplify)
      )
  ; "array-of" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
type nonrec t = array && [
    of [
        "name": string;
        "include_vars": string;
        required "include_vars";
];
];
|} |> structure_of_string_exn )
        ({|
type t = array && [
      of [
          "name": string;
          "include_vars": string;
] && [ required "include_vars"; ]
] ;
|} |> structure_of_string_exn |> full_simplify)
      )

  ]



let tests = "all" >::: [
    simple
  ; coalesce_atomics
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
