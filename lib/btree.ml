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
    root: Heap.reference; (* the root block *)
  }
  type block = B.t

  let header_index = 0 (* in root block *)

  let root_node_index = 1 (* in root block *)

  let magic = "MIRAGEBTREE\174\067\003\088\230"

  cstruct tree_hdr {
      uint8_t magic[16];
      uint16_t d;
    } as little_endian

  module Node = struct

    let magic = "NODE\161\218\173\152\079\151\194\065\090\038\040\183"

    cstruct node_hdr {
        uint8_t magic[16];
      } as little_endian

    type t = unit

    let read ~heap ~ref () =
      let open Error.Infix in
      Heap.lookup ~heap ~ref ()
      >>= fun b ->
      let open Lwt.Infix in
      Heap.Block.get_info b
      >>= fun info ->
      let open Error.FromBlock in
      let buf = alloc info.Heap.Block.sector_size in
      Heap.Block.read b 0L [ buf ]
      >>= fun () ->
      let magic' = Cstruct.to_string (get_node_hdr_magic buf) in
      if magic <> magic'
      then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected block header magic, expected '%s' but read '%s'" magic magic')))
      else begin
        Lwt.return (`Ok ())
      end

    let write ~heap ~ref ~t () =
      let open Error.Infix in
      Heap.lookup ~heap ~ref ()
      >>= fun b ->
      let open Lwt.Infix in
      Heap.Block.get_info b
      >>= fun info ->
      let open Error.FromBlock in
      let buf = alloc info.Heap.Block.sector_size in
      set_node_hdr_magic magic 0 buf;
      Heap.Block.write b 0L [ buf ]
      >>= fun () ->
      Lwt.return (`Ok ())

    let allocate ~heap ~d ~parent ~index () =
      let open Error.Infix in
      Heap.Block.allocate ~parent ~index ~nrefs:(2 * d) ~nbytes:(Int64.of_int (2 * d * E.size)) ()
      >>= fun block ->
      let ref = Heap.Block.ref block in
      let t = () in
      write ~heap ~ref ~t ()
      >>= fun () ->
      Lwt.return (`Ok ref)
  end

  let connect block =
    let open Lwt.Infix in
    B.get_info block
    >>= fun info ->
    let open Error.Infix in
    Heap.connect ~block ()
    >>= fun heap ->
    Heap.root ~heap ()
    >>= fun root ->
    Heap.Block.get root
    >>= fun refs ->
    match
      (if Array.length refs <= header_index then None else refs.(header_index)),
      (if Array.length refs <= root_node_index then None else refs.(root_node_index))
    with
    | Some hdr, Some root ->
      begin
        Heap.lookup ~heap ~ref:hdr ()
        >>= fun block ->
        let buf = alloc info.B.sector_size in
        let open Error.FromBlock in
        Heap.Block.read block 0L [ buf ]
        >>= fun () ->
        let magic' = Cstruct.to_string (get_tree_hdr_magic buf) in
        let d = get_tree_hdr_d buf in
        if magic <> magic'
        then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected b-tree description magic, expected '%s' but read '%s'" magic magic')))
        else Lwt.return (`Ok { heap; d; root })
      end
    | _, _ ->
      Lwt.return (`Error (`Msg "Failed to read b-tree description block"))

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
    Heap.Block.allocate ~parent:root ~index:header_index ~nbytes:(Int64.of_int sizeof_tree_hdr) ~nrefs:0 ()
    >>= fun bytes ->
    let buf = alloc info.B.sector_size in
    set_tree_hdr_magic magic 0 buf;
    set_tree_hdr_d buf d;
    let open Error.FromBlock in
    Heap.Block.write bytes 0L [ buf ]
    >>= fun () ->
    let open Error.Infix in
    Node.allocate ~heap ~d ~parent:root ~index:root_node_index ()
    >>= fun root ->
    Lwt.return (`Ok { heap; d; root })

  let insert _ _ = failwith "insert"
  let delete _ _ = failwith "delete"
end
