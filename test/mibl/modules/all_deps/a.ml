open B                          (* module *)
open C1                         (* struct *)
open C2                         (* struct *)
open D1                         (* sig *)
open D2                         (* sig *)
open Suba                       (* module *)

let test_dump_uchar () =
 let str u = Format.asprintf "%a" Fmt.Dump.uchar u in
 assert (str Uchar.min = "U+0000");
 assert (str Uchar.(succ min) = "U+0001");
 assert (str Uchar.(of_int 0xFFFF) = "U+FFFF");
 assert (str Uchar.(succ (of_int 0xFFFF)) = "U+10000");
 assert (str Uchar.(pred max) = "U+10FFFE");
 assert (str Uchar.max = "U+10FFFF");
 ()


let n = 10000

let () =
  while true do
    let t0 = Unix.gettimeofday () in
    for _i = 1 to n do
      ignore @@ Fmt.str "Hello %a" Fmt.string "world"
    done;
    let t1 = Unix.gettimeofday () in
    Printf.printf "Formatted %.0f messages/second\n%!" (float n /. (t1 -. t0))
  done

let () =
  UnixLabels.foo()

let () =
  Fmt_tty.setup()
