(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
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

let alloc = Heap.alloc

module Make(B: V1_LWT.BLOCK)(E: Btree_s.ELEMENT) = struct
  type element = E.t

  module Heap = Heap.Make(B)

  type t = {
    heap: Heap.heap;
    d: int; (* minimum number of keys in a node *)
  }
  type block = B.t

  let header_index = 0 (* in root block *)

  let magic = "MIRAGEBTREE\174\067\003\088\230"

  cstruct tree_hdr {
      uint8_t magic[16];
      uint16_t d;
    } as little_endian

  let connect block =
    let open Lwt.Infix in
    B.get_info block
    >>= fun info ->
    let open Error.Infix in
    Heap.connect ~block ()
    >>= fun heap ->
    Heap.root ~heap ()
    >>= fun root ->
    Heap.Refs.get root
    >>= fun refs ->
    match (if Array.length refs <= header_index then None else refs.(header_index)) with
    | None ->
      Lwt.return (`Error (`Msg "Failed to read b-tree description block"))
    | Some ref ->
      begin
        Heap.lookup ~heap ~ref ()
        >>= function
        | Heap.Refs _ ->
          Lwt.return (`Error (`Msg "b-tree description block has the wrong type"))
        | Heap.Bytes bytes ->
          let buf = alloc info.B.sector_size in
          let open Error.FromBlock in
          Heap.Bytes.read bytes 0L [ buf ]
          >>= fun () ->
          let magic' = Cstruct.to_string (get_tree_hdr_magic buf) in
          let d = get_tree_hdr_d buf in
          if magic <> magic'
          then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected b-tree description magic, expected '%s' but read '%s'" magic magic')))
          else Lwt.return (`Ok { heap; d })
      end

  let create ~block ~d () =
    let open Lwt.Infix in
    B.get_info block
    >>= fun info ->
    let open Error.Infix in
    Heap.format ~block ()
    >>= fun () ->
    Heap.connect ~block ()
    >>= fun heap ->
    Heap.root ~heap ()
    >>= fun root ->
    Heap.Bytes.allocate ~parent:root ~index:header_index ~length:(Int64.of_int sizeof_tree_hdr) ()
    >>= fun bytes ->
    let buf = alloc info.B.sector_size in
    set_tree_hdr_magic magic 0 buf;
    set_tree_hdr_d buf d;
    let open Error.FromBlock in
    Heap.Bytes.write bytes 0L [ buf ]
    >>= fun () ->
    Lwt.return (`Ok { heap; d })

  let insert _ _ = failwith "insert"
  let delete _ _ = failwith "delete"
end
