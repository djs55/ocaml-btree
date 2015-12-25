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

let tests = [
  "heap_format" >:: heap_format;
  "heap connect" >:: heap_connect;
]

let _ =
  let suite = "main" >::: tests in
  OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
