open Asttools
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
type assoc_t = {
  validated_keys : string list
; sealed : bool
; patterns: (string * utype_t) list
; orelse : utype_t option
}

type t =
    Assoc of assoc_t
  | Other

let start_assoc () = Assoc { validated_keys = [] ; sealed = false ; patterns = [] ; orelse = None }
let start_other = Other

let add_field ctxt fname = match ctxt with
    Assoc ({ validated_keys ; _} as a) -> Some (Assoc { a with validated_keys = fname::validated_keys })
  | _ -> Fmt.(failwithf "Ctxt.add_field(%a): internal error: wrong kind of context" Dump.string fname)

let add_pattern ctxt (re, ut) = match ctxt with
    Assoc ({ patterns ; sealed=false ; orelse=None } as a) -> Some (Assoc { a with patterns = (re,ut)::patterns })
  | Assoc _ ->
    Fmt.(pf stderr "WARNING: setting FieldRE when Orelse/Sealed already set") ;
    None
  | _ -> Fmt.(failwithf "Ctxt.add_pattern(%a,%s): internal error: wrong kind of context"
                Dump.string re (Normal.printer ut))

let set_sealed ctxt  = match ctxt with
    Assoc ({ sealed=_ ; orelse = None ; patterns = [] } as a) -> Some (Assoc { a with sealed = true })
  | Assoc _ -> 
    Fmt.(pf stderr "WARNING: setting Sealed when Orelse/FieldRE already set") ;
    None
  | _ -> Fmt.(failwithf "Ctxt.set_sealed: internal error: wrong kind of context")

let set_orelse ctxt ut  = match ctxt with
    Assoc ({ sealed=false ; orelse = None ; patterns = [] } as a) -> Some (Assoc { a with orelse = Some ut })
  | Assoc _ -> 
    Fmt.(pf stderr "WARNING: setting Orelse when Sealed/FieldRE already set") ;
    None
  | _ -> Fmt.(failwithf "Ctxt.set_sealed: internal error: wrong kind of context")
end

let float_multipleOf ctxt f n =
  let (fracpart, intpart) = Float.modf (f /. n) in
  if fracpart = 0.0 then Some ctxt else None

let int_multipleOf ctxt f n =
  let (fracpart, intpart) = Float.modf n in
  if fracpart = 0.0 then
    let n = int_of_float n in
    if 0 = f mod n then Some ctxt else None
  else float_multipleOf ctxt (float_of_int f) n

let inrange ctxt n (lo, hi) =
  let open Bound in
  if (match lo with
        {it=lo; exclusive=true} -> lo < n
      | {it=lo; exclusive=false} -> lo <= n) &&
     (match hi with
        {it=None} -> true
      | {it=Some hi; exclusive=true} -> n < hi
      | {it=Some hi; exclusive=false} -> n <= hi) then
    Some ctxt
  else None

let float_numberBounds ctxt f (lo, hi) =
  let open Bound in
  if (match lo with
     {it=None} -> true
   | {it=Some lo; exclusive=true} -> lo < f
   | {it=Some lo; exclusive=false} -> lo <= f) &&
  (match hi with
     {it=None} -> true
   | {it=Some hi; exclusive=true} -> f < hi
   | {it=Some hi; exclusive=false} -> f <= hi) then
    Some ctxt
  else None

let tdl = ref []

  let rec utype (j : Yojson.Basic.t) (ctxt : Ctxt.t) t = match t with
      UtTrue -> Some ctxt
    | UtFalse -> None
    | Simple bt -> if base_type j bt then Some ctxt else None
    | And (ut1,ut2) -> begin match utype j ctxt ut1 with
          Some ctxt -> utype j ctxt ut2
        | None -> None
      end
    | Or (ut1,ut2) -> begin match utype j ctxt ut1 with
          None -> utype j ctxt ut2
        | Some _ as rv -> rv
      end
    | Xor (ut1,ut2) -> begin match (utype j ctxt ut1, utype j ctxt ut2) with
          (Some ctxt, None) -> Some ctxt
        | (None, Some ctxt) -> Some ctxt
        | _ -> None
      end
    | Impl (ut1,ut2) -> begin match utype j ctxt ut1 with
          Some ctxt -> utype j ctxt ut2
        | None -> Some ctxt
      end
    | Not ut -> begin match utype j ctxt ut with
          None -> Some ctxt
        | Some _ -> None
      end
    | Atomic l -> atomic_type_list j ctxt l
    | Ref (mpopt, id) ->
      utype j ctxt (lookup_tid !tdl (mpopt, id))

  and base_type j bt = match (bt, j) with
      ((JNull, `Null)
      | (JString, `String _)
      | (JBool, `Bool _)
      | (JNumber, (`Int _|`Float _))
      | (JArray, `List _)
      | (JObject, `Assoc _)) -> true
      | _ -> false
        
  and enter_utype j ut =
    match j with
      `Assoc l -> begin
        match utype j (Ctxt.start_assoc ()) ut with
          None -> false
        | Some (Ctxt.Assoc { validated_keys ; sealed=false ; patterns = [] ; orelse=None }) ->
          true

        | Some (Ctxt.Assoc { validated_keys ; sealed=true ; patterns = [] ; orelse=None }) ->
          l |> List.for_all (fun (k, _) -> List.mem k validated_keys)

        | Some (Ctxt.Assoc ({ validated_keys ; sealed=false } as a)) ->
          finish_assoc l a
        | Some _ -> assert false
      end
    | _ -> begin match utype j Ctxt.start_other ut with
          None -> false
        | Some _ -> true
      end

  and finish_assoc l = function
      { validated_keys ; sealed=false ; patterns ; orelse } ->
      l |> List.for_all (fun (k, j) ->
          if List.mem k validated_keys then true
          else match patterns |> List.find_map (fun (restr, ut) ->
              if Pcre.pmatch ~rex:(REC.get restr) k then Some ut else None) with
            Some ut -> enter_utype j ut
          | None ->
            match orelse with
              None -> true
            | Some ut -> enter_utype j ut
        )



  and atomic_type_list j ctxt l =
    List.fold_left (fun ctxt t ->
        match ctxt with None -> None | Some ctxt ->
          atomic_type j ctxt t
      ) (Some ctxt) l

  and atomic_type j ctxt t = match (j, t) with
      (`Assoc l, Field (fname, ut)) -> begin match List.assoc fname l with
          v ->
          if enter_utype v ut then
            Ctxt.add_field ctxt fname
          else None
        | exception Not_found -> Some ctxt
      end
    | (`Assoc l, FieldRequired fl) ->
      if fl |> List.for_all (fun fname -> List.mem_assoc fname l) then
        Some ctxt
      else None

    | (`Assoc _, FieldRE(re, ut)) ->
      Ctxt.add_pattern ctxt (re, ut)

    | (`Assoc _, Sealed) ->
      Ctxt.set_sealed ctxt

    | (`Assoc _, OrElse ut) -> Ctxt.set_orelse ctxt ut

    | (`Assoc l, PropertyNames ut) ->
      if l |> List.for_all (fun (k,_) -> enter_utype (`String k) ut) then
        Some ctxt
      else None

    | (`List l, ArrayOf ut) ->
      if l |> List.for_all (fun j -> enter_utype j ut) then
        Some ctxt
      else None

(*
  | ArrayTuple of utype_t list
*)
    | (`List l, ArrayUnique) ->
      if List.length (List.sort_uniq Stdlib.compare l) = List.length l then
        Some ctxt else None

(*
  | ArrayIndex of int * utype_t
*)
    | (`List l, Size range) ->
      inrange ctxt (List.length l) range
(*
  | StringRE of string
*)
  | (`Int f, NumberBound range) ->
    float_numberBounds ctxt (float_of_int f) range

  | (`Float f, NumberBound range) ->
    float_numberBounds ctxt f range

  | (`Int f, MultipleOf n) ->
    int_multipleOf ctxt f n

  | (`Float f, MultipleOf n) ->
    float_multipleOf ctxt f n

  | (j, Enum jl) ->
    if List.mem j jl then Some ctxt else None

(*
  | Default of Yojson.Basic.t
  | Format of string
  | ContentMediaType of string
  | ContentEncoding of string
*)

    | _ -> Fmt.(failwithf "atomic_type: unandled %a" pp_atomic_utype_t t)

let validate new_tdl j tid =
  tdl := new_tdl ;
  enter_utype j (lookup_tid !tdl tid)
