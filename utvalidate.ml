open Asttools
open Pa_ppx_utils.Std
open Ututil
open Utypes
open Utio
open Utmigrate
open Uttypecheck

module REC = struct
  let cache = ref []
  let get s =
    match List.assoc s !cache with
      v -> v
    | exception Not_found ->
      let re = Pcre.regexp s in
      Stack.push cache (s,re) ;
      re
end
module RECache = REC

let lookup_tid loc tdl (mpopt, id as tid) =
  begin match tdl |> List.find_map (fun (_, tid', _, ut) -> if tid=tid' then Some ut else None) with
      Some t -> t
    | None ->
      let ut = Ref (loc, (mpopt, id)) in
      Fmt.(raise_failwithf loc "Utvalidate.lookup_tid: reference %s not found" (Normal.printer ut))
  end

module Ctxt = struct

module As = struct
type t = {
  validated_keys : string list
; sealed : bool
; patterns: (string * utype_t) list
; orelse : utype_t option
}
let mk () = { validated_keys = [] ; sealed = false ; patterns = [] ; orelse = None }
end

module As2 = struct
type t = {
  validated_keys : string list
}
let mk () = { validated_keys = [] }
end

module Ar = struct
type t = {
  tuple : int option ;
  orelse : utype_t option ;
  sealed : bool ;
}
let mk () = { tuple = None ; orelse = None ; sealed = false }
end

module Ar2 = struct
type t = {
  tuple : int option
}
let mk () = { tuple = None }
end

type t =
    Assoc of As.t
  | Assoc2 of As2.t
  | Array of Ar.t
  | Array2 of Ar2.t
  | Other

let start_assoc () = Assoc (As.mk())
let start_assoc2 () = Assoc2 (As2.mk())
let start_array () = Array (Ar.mk())
let start_array2 () = Array2 (Ar2.mk())
let start_other = Other

let add_field ctxt fname = match ctxt with
    Assoc ({ As.validated_keys ; _} as a) -> Ok (Assoc { a with As.validated_keys = fname::validated_keys })
  | Assoc2 ({ As2.validated_keys ; _} as a) -> Ok (Assoc2 As2.{ As2.validated_keys = fname::validated_keys })
  | _ -> Fmt.(failwithf "Ctxt.add_field(%a): internal error: wrong kind of context" Dump.string fname)

let add_pattern ctxt (re, ut) = match ctxt with
    Assoc ({ patterns } as a) -> Ok (Assoc { a with patterns = (re,ut)::patterns })
  | _ -> Fmt.(failwithf "Ctxt.add_pattern(%a,%s): internal error: wrong kind of context"
                Dump.string re (Normal.printer ut))

let set_sealed ctxt  = match ctxt with
    Assoc ({ sealed=_ ; orelse = None } as a) -> Ok (Assoc { a with sealed = true })
  | Assoc _ -> 
    Fmt.(failwithf "ERROR: setting Sealed when Orelse already set%!")

  | Array (Ar.{ orelse=None; _ } as a) -> Ok (Array { a with sealed = true })
  | Array Ar.{ orelse=Some _; _ } ->
    Fmt.(failwithf "ERROR: setting Sealed when Orelse already set%!")

  | _ -> Fmt.(failwithf "Ctxt.set_sealed: internal error: wrong kind of context")

let set_tuple ctxt n  = match ctxt with
    Array a -> Ok (Array { a with tuple = Some n })
  | Array2 a -> Ok (Array2 Ar2.{ tuple = Some n })

  | _ -> Fmt.(failwithf "Ctxt.set_sealed: internal error: wrong kind of context")

let set_orelse ctxt ut  = match ctxt with
    Assoc ({ sealed=false ; orelse = None } as a) -> Ok (Assoc { a with orelse = Some ut })
  | Assoc _ -> 
    Fmt.(failwithf "ERROR: setting Orelse when Sealed already set%!")

  | Array ({ sealed=false; _ } as a) -> Ok (Array { a with orelse = Some ut })
  | Array { sealed=true; _ } ->
    Fmt.(failwithf "ERROR: setting Orelse when Sealed already set%!")

  | _ -> Fmt.(failwithf "Ctxt.set_orelse: internal error: wrong kind of context")
end

let float_multipleOf f n =
  let (fracpart, intpart) = Float.modf (f /. n) in
  fracpart = 0.0

let int_multipleOf f n =
  let (fracpart, intpart) = Float.modf n in
  if fracpart = 0.0 then
    let n = int_of_float n in
    0 = f mod n
  else float_multipleOf (float_of_int f) n

let inrange n (lo, hi) =
  let open Bound in
  (match lo with
     {it=lo; exclusive=true} -> lo < n
   | {it=lo; exclusive=false} -> lo <= n) &&
  (match hi with
     {it=None} -> true
   | {it=Some hi; exclusive=true} -> n < hi
   | {it=Some hi; exclusive=false} -> n <= hi)

let float_numberBounds f (lo, hi) =
  let open Bound in
  (match lo with
     {it=None} -> true
   | {it=Some lo; exclusive=true} -> lo < f
   | {it=Some lo; exclusive=false} -> lo <= f) &&
  (match hi with
     {it=None} -> true
   | {it=Some hi; exclusive=true} -> f < hi
   | {it=Some hi; exclusive=false} -> f <= hi)

let check_format s fmts =
  match fmts with
    "regex" -> begin
      let slen = String.length s in
      if slen >= 2 && String.get s 0 = '/' && String.get s (slen -1) = '/' then
        match Pcre.regexp s with
          _ -> true
        | exception Pcre.Error _ -> false
      else true
    end
  | "ipv4" ->
    s |> Ipaddr.V4.of_string |> Rresult.R.is_ok
  | "ipv6" ->
    s |> Ipaddr.V6.of_string |> Rresult.R.is_ok
  | "email" ->
    s |> Emile.of_string |> Rresult.R.is_ok
  | ("iri"|"uri"|"uri-reference"|"iri-reference") -> begin match Uri.(s |> of_string) with
        _ -> true
      | exception _ -> false
    end
  | "date-time" -> s |> Utlexing.RFC3339.datetime
  | "date" -> s |> Utlexing.RFC3339.date
  | "time" -> s |> Utlexing.RFC3339.time

  | _ -> Fmt.(failwithf "Utvalidate.check_format: unhandled format %a for string %a"
                Dump.string fmts
                Dump.string s
             )

let check_media_type s fmts =
  match fmts with
    "application/json" -> begin match s |> Yojson.Basic.stream_from_string |> list_of_stream with
        _ -> true
      | exception Yojson.Json_error _ -> false
    end

  | _ -> Fmt.(failwithf "Utvalidate.check_media_type: unhandled media type %a" Dump.string fmts)

let lifted_forall f l =
  let rec frec = function
      [] -> Ok ()
    | h::t -> begin match f h with
          Ok () -> frec t
        | Error l -> Error l
      end
  in frec l

let tdl = ref ([] : top_bindings)

let rec utype1 path (j : Yojson.Basic.t) (ctxt : Ctxt.t) t =
  utype path (j : Yojson.Basic.t) (ctxt : Ctxt.t) t

and utype path (j : Yojson.Basic.t) (ctxt : Ctxt.t) t = match t with
      UtTrue _ -> Ok ctxt
    | UtFalse _ -> Error [path, t]
    | Simple (_, bt) -> if base_type j bt then Ok ctxt else Error [path ,t]
    | And (_, ut1,ut2) -> begin match utype path j ctxt ut1 with
          Ok ctxt -> utype path j ctxt ut2
        | Error _ as rv -> rv
      end
    | Or (_, ut1,ut2) -> begin match utype path j ctxt ut1 with
          Error _ -> utype path j ctxt ut2
        | Ok _ as rv -> rv
      end
    | Xor (_, ut1,ut2) -> begin match (utype1 path j ctxt ut1, utype1 path j ctxt ut2) with
          (Ok ctxt, Error _) -> Ok ctxt
        | (Error _, Ok ctxt) -> Ok ctxt
        | (Error e1, Error e2) -> Error (e1@e2)
        | (Ok _, Ok _) -> Error [path, t]
      end
    | Impl (_, ut1,ut2) -> begin match utype path j ctxt ut1 with
          Ok ctxt -> utype path j ctxt ut2
        | Error _ -> Ok ctxt
      end
    | Not (_, ut) -> begin match utype path j ctxt ut with
          Error _ -> Ok ctxt
        | Ok _ -> Error [path,t]
      end
    | Atomic (_, l) -> atomic_type_list path j ctxt l
    | Ref (loc, (mpopt, id)) ->
      utype path j ctxt (lookup_tid loc !tdl (mpopt, id))
    | Seal(loc, ut, patterns, orelse) -> enter_sealed_type loc path j ctxt (ut, patterns, orelse)

  and base_type j bt = match (bt, j) with
      ((JNull, `Null)
      | (JString, `String _)
      | (JBool, `Bool _)
      | (JNumber, (`Int _|`Float _))
      | (JArray, `List _)
      | (JObject, `Assoc _)) -> true
      | _ -> false
        
  and enter_sealed_type loc path j ctxt (ut, patterns, orelse) =
    match j with
      `Assoc l -> begin
        match (utype path j (Ctxt.start_assoc2 ()) ut, patterns, orelse) with
          (Error l, _, _) -> Error l
        | (Ok (Ctxt.Assoc2 { validated_keys }),
           [], UtTrue _)  ->
          Ok ctxt

        | (Ok (Ctxt.Assoc2 a), patterns, orelse) ->
          finish_assoc2 loc path l ctxt (a,patterns,orelse)
        | (Ok _, _, _) -> assert false
      end

    | `List l when patterns = [] -> begin
        match (utype path j (Ctxt.start_array2 ()) ut, orelse) with
          (Error l, _) -> Error l
        | (Ok (Ctxt.Array2 a), _) -> finish_array2 loc path l ctxt (a, orelse)
        | (Ok _, _) -> assert false
          
      end

    | `List l -> Error [path, ut]

    | _ -> begin match utype path j Ctxt.start_other ut with
          Error l -> Error l
        | Ok _ -> Ok ctxt
      end

  and enter_utype loc path j ut =
    match j with
      `Assoc l -> begin
        match utype path j (Ctxt.start_assoc ()) ut with
          Error l -> Error l
        | Ok (Ctxt.Assoc { validated_keys ; sealed=false ; patterns = [] ; orelse=None }) ->
          Ok ()

        | Ok (Ctxt.Assoc a) ->
          finish_assoc loc path l a
        | Ok _ -> assert false
      end

    | `List l -> begin
        match utype path j (Ctxt.start_array ()) ut with
          Error l -> Error l
        | Ok (Ctxt.Array a) -> finish_array loc path l a 
        | Ok _ -> assert false
          
      end

    | _ -> begin match utype path j Ctxt.start_other ut with
          Error l -> Error l
        | Ok _ -> Ok ()
      end

  and finish_array loc path l = function
      { tuple = None } -> Ok ()
    | { tuple = Some n } when n = List.length l -> Ok ()
    | { tuple = Some n ; sealed=false ; orelse = Some ut } ->
      let l = nthtail l n in
      l |> List.mapi (fun i x -> (i+n, x))
      |> lifted_forall (fun (i,j) ->
          enter_utype loc ((string_of_int i)::path) j ut)

    | { tuple = Some n ; sealed=false ; orelse = None } ->
      Ok ()
    | { tuple = Some n ; sealed=true ; orelse = None } ->
      Error [path, UtFalse loc]

  and finish_array2 loc path l ctxt = function
      ({ tuple = None }, _) -> Ok ctxt
    | ({ tuple = Some n }, _) when n = List.length l -> Ok ctxt
    | ({ tuple = Some n }, ut) ->
      let l = nthtail l n in
      l |> List.mapi (fun i x -> (i+n, x))
      |> lifted_forall (fun (i,j) ->
          enter_utype loc ((string_of_int i)::path) j ut)
      |> (function Ok () -> Ok ctxt | Error l -> Error l)

  and finish_assoc loc path l = function
      { validated_keys ; sealed ; patterns ; orelse } ->
      assert ((orelse = None) || not sealed) ;
      l |> lifted_forall (fun (k, j) ->
          if List.mem k validated_keys then Ok()
          else match patterns |> List.find_map (fun (restr, ut) ->
              if Pcre.pmatch ~rex:(REC.get restr) k then Some ut else None) with
            Some ut -> enter_utype loc (k::path) j ut
          | None ->
            match orelse with
              None -> if not sealed then Ok() else Error [path, UtFalse loc]
            | Some ut -> enter_utype loc (k::path) j ut
        )

  and finish_assoc2 loc path l ctxt = function
      ({ validated_keys }, patterns, orelse) ->
      l |> lifted_forall (fun (k, j) ->
          if List.mem k validated_keys then Ok ()
          else match patterns |> List.find_map (fun (restr, ut) ->
              if Pcre.pmatch ~rex:(REC.get restr) k then Some ut else None) with
            Some ut -> enter_utype loc (k::path) j ut
          | None -> enter_utype loc (k::path) j orelse
        )
      |> (function Ok () -> Ok ctxt | Error l -> Error l)

  and atomic_type_list path j ctxt l =
    List.fold_left (fun ctxt t ->
        match ctxt with Error l -> Error l | Ok ctxt ->
          atomic_type path j ctxt t
      ) (Ok ctxt) l

  and atomic_type path j ctxt t = match (j, t) with
      (`Assoc l, Field (_, fname, ut)) -> begin match List.assoc fname l with
          v -> begin match enter_utype (loc_of_atomic_utype t) (fname::path) v ut with
              Ok () ->  Ctxt.add_field ctxt fname
            | Error l -> Error l
          end
        | exception Not_found -> Ok ctxt
      end

    | (`Assoc l, FieldRequired (loc, fl)) ->
      if fl |> List.for_all (fun fname -> List.mem_assoc fname l) then
        Ok ctxt
      else Error[path,Atomic(loc, [t])]

    | (`Assoc l, PropertyNames (loc, ut)) ->
      begin match l |> lifted_forall (fun (k,_) -> enter_utype loc (k::path) (`String k) ut) with
          Ok () -> Ok ctxt
        | Error l -> Error [path,Atomic(loc, [t])]
      end

    | (`List l, ArrayOf (loc, ut)) ->
      begin match l
                  |> List.mapi (fun i x -> (i,x))
                  |> lifted_forall (fun (i,j) -> enter_utype loc ((string_of_int i)::path) j ut) with
        Ok () -> Ok ctxt
      | Error l -> Error l
      end

  | (`List l, ArrayTuple (loc, utl)) ->
    let utlen = List.length utl in
    if List.length l < utlen then
      Error [path, Atomic(loc, [t])]
    else
      let l =
      if List.length l > utlen then
        firstn utlen l
      else l in
      let pairs = List.map2 (fun a b -> (a,b)) l utl in
      let numbered_pairs = List.mapi (fun i x -> (i,x)) pairs in
      begin match numbered_pairs |> lifted_forall (fun (i, (j, ut)) ->
          enter_utype loc ((string_of_int i)::path) j ut) with
        Ok () ->
        Ctxt.set_tuple ctxt utlen
      | Error l -> Error l
      end 

  | (_, ArrayTuple (loc, utl)) -> Error [path, Atomic(loc,[t])]

   | (`List l, ArrayUnique loc) ->
      if List.length (List.sort_uniq Stdlib.compare l) = List.length l then
        Ok ctxt else Error [path,Atomic(loc, [t])]

(*
  | ArrayIndex of int * utype_t
*)
    | (`List l, Size (loc, range)) ->
      if inrange (List.length l) range then Ok ctxt else Error[path,Atomic(loc, [t])]

    | (`String s, Size(loc, range)) ->
      if inrange (String.length s) range then Ok ctxt else Error[path,Atomic(loc, [t])]

    | (`Assoc l, Size (loc, range)) ->
      if inrange (List.length l) range then Ok ctxt else Error[path,Atomic(loc, [t])]

    | (`String s, StringRE (loc, restr)) ->
      if Pcre.pmatch ~rex:(REC.get restr) s then Ok ctxt else Error[path,Atomic(loc, [t])]

  | (`Int f, NumberBound (loc, range)) ->
    if float_numberBounds (float_of_int f) range then Ok ctxt else Error[path,Atomic(loc, [t])]

  | (`Float f, NumberBound (loc, range)) ->
    if float_numberBounds f range then Ok ctxt else Error[path,Atomic(loc, [t])]

  | (`Int f, MultipleOf (loc, n)) ->
    if int_multipleOf f n then Ok ctxt else Error [path, Atomic(loc, [t])]

  | (`Float f, MultipleOf (loc, n)) ->
    if float_multipleOf f n then Ok ctxt else Error [path, Atomic(loc, [t])]

  | (j, Enum (loc, jl)) ->
    if List.mem (canon_json j) jl then Ok ctxt else Error[path,Atomic(loc, [t])]

  | (_, Default _) -> Ok ctxt

  | (`String s, Format (loc, fmt)) ->
    if s = "" || check_format s fmt then Ok ctxt else Error [path, Atomic(loc, [t])]

  | (`String s, ContentMediaType (loc, fmt)) ->
    if check_media_type s fmt then Ok ctxt else Error [path, Atomic(loc, [t])]

(*
  | ContentEncoding of string
*)

  | (_, _) ->
    let loc = loc_of_atomic_utype t in
    Error [path, Atomic(loc,[t])]

  | (_, t) ->
    let loc = loc_of_atomic_utype t in
    Fmt.(raise_failwithf loc "atomic_type: unhandled %s at %s" (Normal.printer (Atomic (loc,[t])))
           (String.concat "/" (List.rev path))
        )

let validate new_tdl j ut =
  tdl := new_tdl ;
  match enter_utype (loc_of_utype ut) [] j ut with
    Ok () -> true
  | Error l ->
    let pp1 pps (path, ut) = Fmt.(pf pps "%s: %s" (String.concat "/" (List.rev path)) (Normal.printer ut)) in
    Fmt.(failwithf "validate: errors@.  %a@." (list ~sep:(const string "\n") pp1) l)
