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

let typecheck = "type_check" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
local module Predefined = struct
  type nonrec integer = number && [ multipleOf 1.000000; ];
  type nonrec scalar = boolean || number || string;
  type nonrec json = null || scalar || array || object;
  type nonrec positive_number = number && [ bounds (0.,max]; ];
  end : sig type integer, json, positive_number, scalar; end; in  end;
|} |> structure_of_string_exn )
        ({|
local import "lib/predefined.utj" as Predefined; in
end ;
|} |> structure_of_string_exn |> S4Typecheck.exec)
      )
  ; "open" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module M = struct
  type nonrec t = object;
end : sig type t; end;
type nonrec t = array;
open M : sig type t; end;
type nonrec u = t;
|} |> structure_of_string_exn )
        ({|
module M = struct type t = object ; end ;
type t = array ;
open M ;
type u = t ;
|} |> structure_of_string_exn |> S4Typecheck.exec)
      )
  ; "cast-cast" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module M = struct
  type nonrec t = object;
  type nonrec u = array;
end : sig type t, u; end : sig type t; end : sig type t; end;
|} |> structure_of_string_exn )
        ({|
module M = (struct
  type t = object ;
  type u = array ;
end : sig type t, u ; end ) : sig type t; end;
|} |> structure_of_string_exn |> S4Typecheck.exec)
      )
  ]


let s1_elim_import = "step-1-elim_import" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module Predefined = struct
  type nonrec integer = number && [ multipleOf 1.000000; ];
  type nonrec scalar = boolean || number || string;
  type nonrec json = null || scalar || array || object;
  type nonrec positive_number = number && [ bounds (0.,max] ];
end ;
|} |> structure_of_string_exn )
        ({|
import "lib/predefined.utj" as Predefined;
|} |> structure_of_string_exn |> S1ElimImport.exec)
      )
  ]

let s2_elim_local = "step-2-elim_local" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
local  in
  module LOCAL0 = struct
    type nonrec t1 = object;
    type nonrec t2 = object;
  end;
  include LOCAL0;
  type nonrec t3 = t1 && [ "a": t2; ];
 end;
|} |> structure_of_string_exn)
        ({|
local type t1 = object ;
type t2 = object ;
in
  type t3 = t1 && [ "a": t2 ] ;
end ;
|} |> structure_of_string_exn |> S2ElimLocal.exec)
      )
  ]

let s3_name_functor_app_subterms = "step-3-name_functor_app_subterms" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
local  in
  module NAMED0 = struct type nonrec t = object; end;
  module NAMED1 = functor (M:sig  end) -> struct  end;
  module M = F(NAMED0)(NAMED1);
end;
|} |> structure_of_string_exn )
        ({|
module M = F(struct type t = object ; end)(functor(M:sig end) -> struct end) ;
|} |> structure_of_string_exn |> S3NameFunctorAppSubterms.exec)
      )
  ]

let s5_doit x =
  x
  |> structure_of_string_exn
  |> S1ElimImport.exec
  |> S2ElimLocal.exec
  |> ElimEmptyLocal.exec
  |> S3NameFunctorAppSubterms.exec
  |> S4Typecheck.exec
  |> S5ElimInclude.exec
       
let s5_elim_include = "step-5-elim_include" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
  module LOCAL0 = struct
    type nonrec t1 = object;
    type nonrec t2 = object;
  end : sig type t1, t2; end;
  local  in
    type nonrec t1 = LOCAL0.t1;
    type nonrec t2 = LOCAL0.t2;
   end;
  type nonrec t3 = t1 && [ "a": t2; ];
|} |> structure_of_string_exn )
        ({|
local type t1 = object ;
type t2 = object ;
in
  type t3 = t1 && [ "a": t2 ] ;
end ;
|} |> s5_doit
        )
      )
  ]

let s6_doit x =
  x
  |> structure_of_string_exn
  |> S1ElimImport.exec
  |> S2ElimLocal.exec
  |> ElimEmptyLocal.exec
  |> S3NameFunctorAppSubterms.exec
  |> S4Typecheck.exec
  |> S5ElimInclude.exec
  |> ElimEmptyLocal.exec

let s6_elim_empty_local = "step-6-elim_empty_local" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module LOCAL0 = struct type nonrec t1 = object; type nonrec t2 = object;
  end : sig type t1, t2; end;
  type nonrec t1 = LOCAL0.t1; type nonrec t2 = LOCAL0.t2;
  type nonrec t3 = t1 && [ "a": t2; ];
|} |> structure_of_string_exn )
        ({|
local type t1 = object ;
type t2 = object ;
in
  type t3 = t1 && [ "a": t2 ] ;
end ;
|} |> s6_doit)
      )
  ]

let s7_doit x =
  x
  |> structure_of_string_exn
  |> S1ElimImport.exec
  |> S2ElimLocal.exec
  |> ElimEmptyLocal.exec
  |> S3NameFunctorAppSubterms.exec
  |> S4Typecheck.exec
  |> S5ElimInclude.exec
  |> ElimEmptyLocal.exec
  |> S7RenameOverridden.exec
  |> S4Typecheck.exec
  |> ElimCastCast.exec

let s7_rename_overridden = "step-7-rename-overridden" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module M = struct
  type nonrec t0 = object;
  type nonrec u0 = t0;
  type nonrec t = array;
  type nonrec u = t;
end : sig type t; type u; end;
|} |> structure_of_string_exn )
        ({|
module M = struct
  type nonrec t = object;
  type nonrec u = t;
  type nonrec t = array;
  type nonrec u = t;
end ;
|} |> s7_doit)
      )
  ]

let s8_doit x =
  x
  |> structure_of_string_exn
  |> S1ElimImport.exec
  |> S2ElimLocal.exec
  |> ElimEmptyLocal.exec
  |> S3NameFunctorAppSubterms.exec
  |> S4Typecheck.exec
  |> S5ElimInclude.exec
  |> ElimEmptyLocal.exec
  |> S7RenameOverridden.exec
  |> S4Typecheck.exec
  |> ElimCastCast.exec
  |> S8Absolute.exec

let s8_absolute = "step-8-absolute" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module M = struct
  type nonrec t0 = object;
  type nonrec u0 = .M.t0;
  type nonrec t = array;
  type nonrec u = .M.t;
end : sig type t; type u; end;
|} |> structure_of_string_exn )
        ({|
module M = struct
type nonrec t = object;
type nonrec u = t;
type nonrec t = array;
type nonrec u = t;
end ;
|} |> s8_doit
)
      )
  ; "modules-1" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module M = struct
  module N = struct
    type nonrec t = object;
    type nonrec u = .M.N.t && [ "b": array; ];
  end : sig type t, u; end;
  module P = .M.N : sig type t, u; end;
end : sig
  module N : sig type t, u; end;
  module P : sig type t, u; end;
end;
|} |> structure_of_string_exn )
        ({|
module M = struct
  module N = struct
    type t = object ;
    type u = t && [ "b": array ] ;
  end ;
  module P = N ;
end ;
|} |> s8_doit
)
      )
  ; "functor-1" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module M = struct
  module F = functor (M:sig type t; end) -> struct
    type nonrec t = M.t && [ "a": object; ];
  end;
  module NAMED0 = struct
    type nonrec t = object;
  end : sig type t; end;
  module N = .M.F(.M.NAMED0) : sig type t; end;
  type nonrec u = N.t;
end : sig
  type u;
  module F : functor (M:sig type t; end) -> sig type t; end;
  module N : sig type t; end;
  module NAMED0 : sig type t; end;
end;
|} |> structure_of_string_exn )
        ({|
module M = struct
  module F = functor(M : sig type t ; end) -> struct
    type t = M.t && [ "a": object ] ;   
  end ;
  module N = F(struct type t = object ; end) ;
  type u = N.t ;
end ;
|} |> s8_doit
)
      )
  ]

let s9_doit x =
  x
  |> structure_of_string_exn
  |> S1ElimImport.exec
  |> S2ElimLocal.exec
  |> ElimEmptyLocal.exec
  |> S3NameFunctorAppSubterms.exec
  |> S4Typecheck.exec
  |> S5ElimInclude.exec
  |> ElimEmptyLocal.exec
  |> S7RenameOverridden.exec
  |> S8Absolute.exec
  |> ElimCastCast.exec

let s9_elim_cast_cast = "step-9-elim-cast-cast" >::: [
    "simple" >:: (fun ctxt -> 
        assert_equal ~printer:Normal.structure_printer ~cmp:structure_cmp
        ({|
module M = struct
  type nonrec t = object;
  type nonrec u = array;
end : sig type t; end;
|} |> structure_of_string_exn )
        ({|
module M = (struct
  type t = object ;
  type u = array ;
end : sig type t, u ; end ) : sig type t; end;
|} |> s9_doit)
      )
  ]

let tests = "all" >::: [
    simple
  ; typecheck
  ; s1_elim_import
  ; s2_elim_local
  ; s3_name_functor_app_subterms
  ; s5_elim_include
  ; s6_elim_empty_local
  ; s7_rename_overridden
  ; s8_absolute
  ; s9_elim_cast_cast
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
