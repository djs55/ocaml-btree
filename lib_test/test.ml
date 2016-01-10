(*
 * Copyright (c) 2015 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Lwt
open OUnit

let expect_ok msg = function
  | `Error _ -> failwith msg
  | `Ok x -> x

let expect_ok_msg = function
  | `Error (`Msg m) -> failwith m
  | `Error _ -> failwith "unexpected error"
  | `Ok x -> x

let heap_format () =
  let t =
    Ramdisk.connect ~name:"heap"
    >>= fun x ->
    let from = expect_ok "Ramdisk.connect" x in
    let module H = Heap.Make(Ramdisk) in
    H.format ~block:from ()
    >>= fun x ->
    let () = expect_ok_msg x in
    Lwt.return () in
  Lwt_main.run t

let heap_connect () =
  let t =
    Ramdisk.connect ~name:"heap"
    >>= fun x ->
    let from = expect_ok "Ramdisk.connect" x in
    let module H = Heap.Make(Ramdisk) in
    H.format ~block:from ()
    >>= fun x ->
    let () = expect_ok_msg x in
    H.connect ~block:from ()
    >>= fun h ->
    let _ = expect_ok_msg h in
    Lwt.return () in
  Lwt_main.run t

let heap_allocate_deallocate () =
  let t =
    Ramdisk.connect ~name:"heap"
    >>= fun x ->
    let from = expect_ok "Ramdisk.connect" x in
    let module H = Heap.Make(Ramdisk) in
    H.format ~block:from ()
    >>= fun x ->
    let () = expect_ok_msg x in
    H.connect ~block:from ()
    >>= fun h ->
    let h = expect_ok_msg h in
    H.root ~heap:h ()
    >>= fun root ->
    let root = expect_ok_msg root in
    H.Bytes.allocate ~parent:root ~index:0 ~length:1L ()
    >>= fun block ->
    let block = expect_ok_msg block in
    H.Bytes.deallocate ~t:block ()
    >>= fun x ->
    let () = expect_ok_msg x in
    Lwt.return () in
  Lwt_main.run t

let heap_write_read () =
  let t =
    Ramdisk.connect ~name:"heap"
    >>= fun x ->
    let from = expect_ok "Ramdisk.connect" x in
    let module H = Heap.Make(Ramdisk) in
    H.format ~block:from ()
    >>= fun x ->
    let () = expect_ok_msg x in
    H.connect ~block:from ()
    >>= fun h ->
    let h = expect_ok_msg h in
    (* Allocate 2 blocks *)
    H.root ~heap:h ()
    >>= fun root ->
    let root = expect_ok_msg root in
    H.Bytes.allocate ~parent:root ~index:0 ~length:1L ()
    >>= fun block1 ->
    let bytes1 = expect_ok_msg block1 in
    H.Bytes.allocate ~parent:root ~index:2 ~length:1L ()
    >>= fun block2 ->
    let bytes2 = expect_ok_msg block2 in
    (* Fill block1 with random data *)
    Random.self_init();
    Mirage_block.random (module H.Bytes) bytes1
    >>= fun x ->
    let () = expect_ok_msg x in
    (* block1 should be different to block2 *)
    Mirage_block.compare (module H.Bytes) bytes1 (module H.Bytes) bytes2
    >>= fun x ->
    let result = expect_ok_msg x in
    if result == 0 then failwith "blocks erroneously compared the same";
    (* Copy block1 to block2 *)
    Mirage_block.copy (module H.Bytes) bytes1 (module H.Bytes) bytes2
    >>= fun x ->
    let () = expect_ok_msg x in
    (* block1 should be the same as block2 *)
    Mirage_block.compare (module H.Bytes) bytes1 (module H.Bytes) bytes2
    >>= fun x ->
    let result = expect_ok_msg x in
    assert_equal ~printer:string_of_int 0 result;
    (* Deallocate in the "easy" order to not use the GC codepath *)
    H.Bytes.deallocate ~t:bytes2 ()
    >>= fun x ->
    let () = expect_ok_msg x in
    H.Bytes.deallocate ~t:bytes1 ()
    >>= fun x ->
    let () = expect_ok_msg x in
    Lwt.return () in
  Lwt_main.run t

let tree_create () =
  let t =
    Ramdisk.connect ~name:"heap"
    >>= fun x ->
    let block = expect_ok "Ramdisk.connect" x in
    let module T = Btree.Make(Ramdisk)(IntElement) in
    T.create ~block ~d:2 ()
    >>= fun t ->
    let _ = expect_ok "T.create" t in
    Lwt.return () in
  Lwt_main.run t

let tests = [
  "heap_format" >:: heap_format;
  "heap connect" >:: heap_connect;
  "heap allocate-deallocate" >:: heap_allocate_deallocate;
  "heap write then read back" >:: heap_write_read;
  "tree create" >:: tree_create;
]

let _ =
  let suite = "main" >::: tests in
  OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
