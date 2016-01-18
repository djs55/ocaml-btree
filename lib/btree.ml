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
let roundup = Heap.roundup

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
        uint16_t n; (* current size of the array *)
      } as little_endian

    let sizeof_node t = sizeof_node_hdr + (2 * t.d * E.size)

    type node = {
      t: t;
      ref: Heap.reference;
      block: Heap.Block.t;
      keys: E.t array;
    }

    let read ~t ~ref () =
      let open Error.Infix in
      Heap.lookup ~heap:t.heap ~ref ()
      >>= fun b ->
      let open Lwt.Infix in
      Heap.Block.get_info b
      >>= fun info ->
      let open Error.FromBlock in
      let buf = alloc (roundup info.Heap.Block.sector_size (sizeof_node_hdr + (2 * t.d * E.size))) in
      Heap.Block.read b 0L [ buf ]
      >>= fun () ->
      let open Error.Infix in
      let magic' = Cstruct.to_string (get_node_hdr_magic buf) in
      if magic <> magic'
      then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected block header magic, expected '%s' but read '%s'" magic magic')))
      else begin
        let n = get_node_hdr_n buf in
        let buf = Cstruct.shift buf sizeof_node_hdr in
        let rec loop acc rest remaining =
          let open Error.Infix in
          if remaining = 0
          then return (List.rev acc)
          else begin
            Lwt.return (E.unmarshal rest)
            >>= fun (e, rest) ->
            loop (e :: acc) rest (remaining - 1)
          end in
        loop [] buf n
        >>= fun keys ->
        let keys = Array.of_list keys in
        let block = b in
        Lwt.return (`Ok { t; ref; block; keys })
      end

    let write_internal ~heap ~d ~ref ~keys () =
      let open Error.Infix in
      Heap.lookup ~heap ~ref ()
      >>= fun b ->
      let open Lwt.Infix in
      Heap.Block.get_info b
      >>= fun info ->
      let buf = alloc(roundup info.Heap.Block.sector_size (sizeof_node_hdr + (2 * d * E.size))) in
      set_node_hdr_magic magic 0 buf;
      set_node_hdr_n buf (Array.length keys);
      let open Error.Infix in
      let rec loop rest = function
        | [] -> return rest
        | e :: es ->
          Lwt.return (E.marshal e rest)
          >>= fun rest ->
          loop rest es in
      loop (Cstruct.shift buf sizeof_node_hdr) (Array.to_list keys)
      >>= fun _padding ->
      let open Error.FromBlock in
      Heap.Block.write b 0L [ buf ]
      >>= fun () ->
      return ()

    let write ~node () = write_internal ~heap:node.t.heap ~d:node.t.d ~ref:node.ref ~keys:node.keys

    let allocate ~heap ~d ~parent ~index () =
      let open Error.Infix in
      Heap.Block.allocate ~parent ~index ~nrefs:(2 * d + 1) ~nbytes:(Int64.of_int (sizeof_node_hdr + (2 * d * E.size))) ()
      >>= fun block ->
      let ref = Heap.Block.ref block in
      write_internal ~heap ~d ~ref ~keys:[||] ()
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

  let search t element node =
    (* Find the least element greater than or equal to the one to be inserted *)
    let least_greater, _ = Array.fold_left (fun (result, idx) e ->
      match E.compare node.Node.keys.(idx) element with
      | `LessThan -> result, idx + 1
      | `Equal
      | `GreaterThan -> min result idx, idx + 1
    ) (max_int, 0) node.Node.keys in
    if least_greater = max_int
    then `After (* this element would logically be after all existing elements *)
    else if E.compare node.Node.keys.(least_greater) element = `Equal
    then `Here least_greater (* this exact element found *)
    else `Before least_greater (* element would be just before this one *)

  let insert t element =
    let open Error.Infix in
    Node.read ~t ~ref:t.root ()
    >>= fun node ->
    match search t element node with
    | `After ->
      failwith "unimplemented: insert into empty node"
    | `Here _ ->
      failwith "unimplemented: replace existing mapping"
    | `Before _ ->
      (* If idx is a valid reference then follow it *)
      if true then failwith "unimplemented: recurse";
      if Array.length node.Node.keys = 2 * t.d then begin
        failwith "unimplemented: split node"
      end else begin
        failwith "unimplemented: insert into existing node"
      end

  let mem t element =
    let open Error.Infix in
    let rec aux ref =
      Node.read ~t ~ref ()
      >>= fun node ->
      match search t element node with
      | `After ->
        if Array.length node.Node.keys = 0
        then Lwt.return (`Ok false)
        else begin
          (* Look up reference len *)
          Heap.Block.get node.Node.block
          >>= fun refs ->
          begin match refs.(Array.length node.Node.keys) with
          | Some rf ->
            aux rf
          | None ->
            Lwt.return (`Ok false)
          end
        end
      | `Here _ ->
        Lwt.return (`Ok true)
      | `Before idx ->
        (* Look up reference idx *)
        Heap.Block.get node.Node.block
        >>= fun refs ->
        begin match refs.(idx) with
        | Some rf ->
          aux rf
        | None ->
          Lwt.return (`Ok false)
        end in
    aux t.root

  let delete t element =
    let open Error.Infix in
    Node.read ~t ~ref:t.root ()
    >>= fun _ ->
    failwith "delete"
end
