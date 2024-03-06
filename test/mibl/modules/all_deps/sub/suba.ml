(* suba/foo.ml *)

open Subsig

(* open AString *)

(* let M = Astring.foo *)


let json_string = {|
  {"number" : 42,
   "string" : "yes",
   "list": ["for", "sure", 42]}|}
(* val json_string : string *)

let json = Yojson.Safe.from_string json_string
(* val json : Yojson.Safe.t *)

let () = Format.printf "Parsed to %a" Yojson.Safe.pp json
