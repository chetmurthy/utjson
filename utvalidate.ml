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

let lookup_tid tdl (mpopt, id as tid) =
  begin match List.assoc tid tdl with
      t -> t
    | exception Not_found ->
      let ut = Ref (mpopt, id) in
      Fmt.(failwithf "Utvalidate.lookup_tid: reference %s not found" (Normal.printer ut))
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
module Ar = struct
type t = {
  tuple : int option ;
  orelse : utype_t option ;
  sealed : bool ;
}
let mk () = { tuple = None ; orelse = None ; sealed = false }
end

type t =
    Assoc of As.t
  | Array of Ar.t
  | Other

let start_assoc () = Assoc (As.mk())
let start_array () = Array (Ar.mk())
let start_other = Other

let add_field ctxt fname = match ctxt with
    Assoc ({ validated_keys ; _} as a) -> Ok (Assoc { a with validated_keys = fname::validated_keys })
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
  | "uri" -> begin match Uri.(s |> of_string |> scheme) with
        Some _ -> true
      | None -> false
    end
  | "date-time" -> s |> Ptime.of_rfc3339 |> Rresult.R.is_ok

  | _ -> Fmt.(failwithf "Utvalidate.check_format: unhandled format %a" Dump.string fmts)

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

let tdl = ref []

  let rec utype path (j : Yojson.Basic.t) (ctxt : Ctxt.t) t = match t with
      UtTrue -> Ok ctxt
    | UtFalse -> Error [path, t]
    | Simple bt -> if base_type j bt then Ok ctxt else Error [path ,t]
    | And (ut1,ut2) -> begin match utype path j ctxt ut1 with
          Ok ctxt -> utype path j ctxt ut2
        | Error _ as rv -> rv
      end
    | Or (ut1,ut2) -> begin match utype path j ctxt ut1 with
          Error _ -> utype path j ctxt ut2
        | Ok _ as rv -> rv
      end
    | Xor (ut1,ut2) -> begin match (utype path j ctxt ut1, utype path j ctxt ut2) with
          (Ok ctxt, Error _) -> Ok ctxt
        | (Error _, Ok ctxt) -> Ok ctxt
        | (Error e1, Error e2) -> Error (e1@e2)
        | (Ok _, Ok _) -> Error [path, t]
      end
    | Impl (ut1,ut2) -> begin match utype path j ctxt ut1 with
          Ok ctxt -> utype path j ctxt ut2
        | Error _ -> Ok ctxt
      end
    | Not ut -> begin match utype path j ctxt ut with
          Error _ -> Ok ctxt
        | Ok _ -> Error [path,t]
      end
    | Atomic l -> atomic_type_list path j ctxt l
    | Ref (mpopt, id) ->
      utype path j ctxt (lookup_tid !tdl (mpopt, id))

  and base_type j bt = match (bt, j) with
      ((JNull, `Null)
      | (JString, `String _)
      | (JBool, `Bool _)
      | (JNumber, (`Int _|`Float _))
      | (JArray, `List _)
      | (JObject, `Assoc _)) -> true
      | _ -> false
        
  and enter_utype path j ut =
    match j with
      `Assoc l -> begin
        match utype path j (Ctxt.start_assoc ()) ut with
          Error l -> Error l
        | Ok (Ctxt.Assoc { validated_keys ; sealed=false ; patterns = [] ; orelse=None }) ->
          Ok ()

        | Ok (Ctxt.Assoc a) ->
          finish_assoc path l a
        | Ok _ -> assert false
      end

    | `List l -> begin
        match utype path j (Ctxt.start_array ()) ut with
          Error l -> Error l
        | Ok (Ctxt.Array a) -> finish_array path l a 
        | Ok _ -> assert false
          
      end

    | _ -> begin match utype path j Ctxt.start_other ut with
          Error l -> Error l
        | Ok _ -> Ok ()
      end

  and finish_array path l = function
      { tuple = None } -> Ok ()
    | { tuple = Some n } when n = List.length l -> Ok ()
    | { tuple = Some n ; sealed=false ; orelse = Some ut } ->
      let l = nthtail l n in
      l |> List.mapi (fun i x -> (i+n, x))
      |> lifted_forall (fun (i,j) ->
          enter_utype ((string_of_int i)::path) j ut)

    | { tuple = Some n ; sealed=false ; orelse = None } ->
      Ok ()
    | { tuple = Some n ; sealed=true ; orelse = None } ->
      Error [path, UtFalse]

  and finish_assoc path l = function
      { validated_keys ; sealed ; patterns ; orelse } ->
      assert ((orelse = None) || not sealed) ;
      l |> lifted_forall (fun (k, j) ->
          if List.mem k validated_keys then Ok()
          else match patterns |> List.find_map (fun (restr, ut) ->
              if Pcre.pmatch ~rex:(REC.get restr) k then Some ut else None) with
            Some ut -> enter_utype (k::path) j ut
          | None ->
            match orelse with
              None -> if not sealed then Ok() else Error [path, UtFalse]
            | Some ut -> enter_utype (k::path) j ut
        )



  and atomic_type_list path j ctxt l =
    List.fold_left (fun ctxt t ->
        match ctxt with Error l -> Error l | Ok ctxt ->
          atomic_type path j ctxt t
      ) (Ok ctxt) l

  and atomic_type path j ctxt t = match (j, t) with
      (`Assoc l, Field (fname, ut)) -> begin match List.assoc fname l with
          v -> begin match enter_utype (fname::path) v ut with
              Ok () ->  Ctxt.add_field ctxt fname
            | Error l -> Error l
          end
        | exception Not_found -> Ok ctxt
      end
    | (`Assoc l, FieldRequired fl) ->
      if fl |> List.for_all (fun fname -> List.mem_assoc fname l) then
        Ok ctxt
      else Error[path,Atomic[t]]

    | (`Assoc _, FieldRE(re, ut)) ->
      Ctxt.add_pattern ctxt (re, ut)

    | (`Assoc _, Sealed) ->
      Ctxt.set_sealed ctxt

    | (`List _, Sealed) ->
      Ctxt.set_sealed ctxt

    | (`Assoc _, OrElse ut) -> Ctxt.set_orelse ctxt ut

    | (`Assoc l, PropertyNames ut) ->
      begin match l |> lifted_forall (fun (k,_) -> enter_utype (k::path) (`String k) ut) with
          Ok () -> Ok ctxt
        | Error l -> Error [path,Atomic[t]]
      end

    | (`List l, ArrayOf ut) ->
      begin match l
                  |> List.mapi (fun i x -> (i,x))
                  |> lifted_forall (fun (i,j) -> enter_utype ((string_of_int i)::path) j ut) with
        Ok () -> Ok ctxt
      | Error l -> Error l
      end

  | (`List l, ArrayTuple utl) ->
    let utlen = List.length utl in
    if List.length l < utlen then
      Error [path, Atomic[t]]
    else
      let l =
      if List.length l > utlen then
        firstn utlen l
      else l in
      let pairs = List.map2 (fun a b -> (a,b)) l utl in
      let numbered_pairs = List.mapi (fun i x -> (i,x)) pairs in
      begin match numbered_pairs |> lifted_forall (fun (i, (j, ut)) ->
          enter_utype ((string_of_int i)::path) j ut) with
        Ok () ->
        Ctxt.set_tuple ctxt utlen
      | Error l -> Error l
      end

    | (`List l, ArrayUnique) ->
      if List.length (List.sort_uniq Stdlib.compare l) = List.length l then
        Ok ctxt else Error [path,Atomic[t]]

(*
  | ArrayIndex of int * utype_t
*)
    | (`List l, Size range) ->
      if inrange (List.length l) range then Ok ctxt else Error[path,Atomic[t]]

    | (`String s, Size range) ->
      if inrange (String.length s) range then Ok ctxt else Error[path,Atomic[t]]

    | (`Assoc l, Size range) ->
      if inrange (List.length l) range then Ok ctxt else Error[path,Atomic[t]]

    | (`String s, StringRE restr) ->
      if Pcre.pmatch ~rex:(REC.get restr) s then Ok ctxt else Error[path,Atomic[t]]

  | (`Int f, NumberBound range) ->
    if float_numberBounds (float_of_int f) range then Ok ctxt else Error[path,Atomic[t]]

  | (`Float f, NumberBound range) ->
    if float_numberBounds f range then Ok ctxt else Error[path,Atomic[t]]

  | (`Int f, MultipleOf n) ->
    if int_multipleOf f n then Ok ctxt else Error [path, Atomic[t]]

  | (`Float f, MultipleOf n) ->
    if float_multipleOf f n then Ok ctxt else Error [path, Atomic[t]]

  | (j, Enum jl) ->
    if List.mem (canon_json j) jl then Ok ctxt else Error[path,Atomic[t]]

  | (_, Default _) -> Ok ctxt

  | (`String s, Format fmt) ->
    if s = "" || check_format s fmt then Ok ctxt else Error [path, Atomic[t]]

  | (`String s, ContentMediaType fmt) ->
    if check_media_type s fmt then Ok ctxt else Error [path, Atomic[t]]

(*
  | ContentEncoding of string
*)

    | _ -> Fmt.(failwithf "atomic_type: unhandled %a at %s" pp_atomic_utype_t t
                  (String.concat "/" (List.rev path))
               )

let validate new_tdl j ut =
  tdl := new_tdl ;
  match enter_utype [] j ut with
    Ok () -> true
  | Error l ->
    let pp1 pps (path, ut) = Fmt.(pf pps "%s: %s" (String.concat "/" (List.rev path)) (Normal.printer ut)) in
    Fmt.(failwithf "validate: errors@.  %a@." (list ~sep:(const string "\n") pp1) l)
