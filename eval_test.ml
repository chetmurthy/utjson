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
open Uteval

let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let expand_import = "expand-import" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
local module Predefined = struct
  type nonrec integer = number && [ multipleOf 1.000000; ];
    type nonrec scalar = boolean || number || string;
    type nonrec json = null || scalar || array || object;
  end : sig integer; scalar; json; end; in  end;
|} |> structure_of_string_exn )
        ({|
local import "lib/predefined.utj" as Predefined; in
end ;
|} |> structure_of_string_exn |> S2Typecheck.exec)
      )
  ]


let elim_local = "elim_local" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
local  in
 module LOCAL0 = struct
   type nonrec t1 = object;
   type nonrec t2 = object;
  end : sig t1; t2; end;
  include LOCAL0 : sig t1; t2; end;
  type nonrec t3 = t1 && [ "a": t2; ];
 end;
|} |> structure_of_string_exn )
        ({|
local type t1 = object ;
type t2 = object ;
in
  type t3 = t1 && [ "a": t2 ] ;
end ;
|} |> structure_of_string_exn |> S1ElimLocal.exec |> S2Typecheck.exec)
      )
  ]

let elim_include = "elim_include" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
local  in
  module LOCAL0 = struct
    type nonrec t1 = object;
    type nonrec t2 = object;
  end : sig t1; t2; end;
  local  in
    type nonrec t1 = LOCAL0.t1;
    type nonrec t2 = LOCAL0.t2;
   end;
  type nonrec t3 = t1 && [ "a": t2; ];
end;
|} |> structure_of_string_exn )
        ({|
local type t1 = object ;
type t2 = object ;
in
  type t3 = t1 && [ "a": t2 ] ;
end ;
|} |> structure_of_string_exn |> S1ElimLocal.exec |> S2Typecheck.exec |> S3ElimInclude.exec)
      )
  ]

let elim_empty_local = "elim_empty_local" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module LOCAL0 = struct type nonrec t1 = object; type nonrec t2 = object;
  end : sig t1; t2; end;
  type nonrec t1 = LOCAL0.t1; type nonrec t2 = LOCAL0.t2;
  type nonrec t3 = t1 && [ "a": t2; ];
|} |> structure_of_string_exn )
        ({|
local type t1 = object ;
type t2 = object ;
in
  type t3 = t1 && [ "a": t2 ] ;
end ;
|} |> structure_of_string_exn
 |> S1ElimLocal.exec
 |> S2Typecheck.exec
 |> S3ElimInclude.exec
 |> S4ElimEmptyLocal.exec
)
      )
  ]


let tests = "all" >::: [
    simple
  ; expand_import
  ; elim_local
  ; elim_include
  ; elim_empty_local
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
