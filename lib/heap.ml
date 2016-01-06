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
open Sexplib.Std
open Lwt.Infix

let roundup multiple size = (size + multiple - 1) / multiple * multiple

let alloc size =
  let npages = (roundup 4096 size) / 4096 in
  let pages = Io_page.get npages in
  Cstruct.(sub (Io_page.to_cstruct pages) 0 size)

module Root_block(B: V1_LWT.BLOCK) = struct
  (* The first 16 bytes will contain a magic string *)
  let magic = "MIRAGEFS\192\157\086\025\215\044\040\236"

  (* The first sector will contain a "root block" *)
  cstruct hdr {
      uint8_t magic[16];
      uint32_t version;
      uint64_t root;
      uint64_t high_water_mark;
      uint64_t free_list;
    } as little_endian

  type t = {
    magic: string;
    version: int32;
    root: int64; (* reference to the root reference block *)
    high_water_mark: int64; (* sectors >= the high_water_mark have not been written to *)
    free_list: int64; (* reference to the first block on the free list *)
  } with sexp

  let create () = { magic; version = 0l; root = 0L; high_water_mark = 1L; free_list = 0L }

  let read ~block =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    let open Error.FromBlock in
    B.read block 0L [ sector ]
    >>= fun () ->
    let magic' = Cstruct.to_string (get_hdr_magic sector) in
    let version = get_hdr_version sector in
    let root = get_hdr_root sector in
    let high_water_mark = get_hdr_high_water_mark sector in
    let free_list = get_hdr_free_list sector in
    if magic <> magic'
    then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected header magic, expected '%s' but read '%s'" magic magic')))
    else begin
      if version <> 0l
      then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected header version, expected '%ld' but read '%ld'" 0l version)))
      else Lwt.return (`Ok { magic; version; root; high_water_mark; free_list })
    end
  let write ~block t =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    set_hdr_magic magic 0 sector;
    set_hdr_version sector t.version;
    set_hdr_root sector t.root;
    set_hdr_high_water_mark sector t.high_water_mark;
    set_hdr_free_list sector t.free_list;
    let open Error.FromBlock in
    B.write block 0L [ sector ]
    >>= fun () ->
    Lwt.return (`Ok ())
end

module Allocated_block(B: V1_LWT.BLOCK) = struct
  let magic = "MIRAGEBLOCK\089\060\224\199\110"

  cstruct hdr {
      uint8_t magic[16];
      uint32_t version;
      uint64_t length;
      uint8_t deleted;
      uint8_t tag;
    } as little_endian


  type tag = [
    | `Bytes (* block contains raw data *)
    | `Refs (* block contains an array of references to blocks *)
  ] with sexp

  let tag_of_int = function
    | 1 -> `Ok `Bytes
    | 2 -> `Ok `Refs
    | n -> `Error (`Msg (Printf.sprintf "Unknown tag: %d" n))

  let int_of_tag = function
    | `Bytes -> 1
    | `Refs -> 2

  type t = {
    magic: string;
    version: int32;
    length: int64;
    deleted: bool;
    tag: tag;
  } with sexp

  let create ~length ~tag = { magic; version = 0l; length; deleted = false; tag }

  let read ~block ~offset =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    let open Error.FromBlock in
    B.read block offset [ sector ]
    >>= fun () ->
    let magic = Cstruct.to_string (get_hdr_magic sector) in
    let version = get_hdr_version sector in
    let length = get_hdr_length sector in
    let deleted = get_hdr_deleted sector = 1 in
    let tag = get_hdr_tag sector in
    let open Error.Infix in
    Lwt.return (tag_of_int tag)
    >>= fun tag ->
    Lwt.return (`Ok { magic; version; length; deleted; tag })

  let write ~block ~offset t =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    set_hdr_magic t.magic 0 sector;
    set_hdr_version sector t.version;
    set_hdr_length sector t.length;
    set_hdr_deleted sector (if t.deleted then 1 else 2);
    set_hdr_tag sector (int_of_tag t.tag);
    let open Error.FromBlock in
    B.write block 0L [ sector ]
    >>= fun () ->
    Lwt.return (`Ok ())
end

(* Blocks are free if either
   - they are >= the high_water_mark
   - they are reachable via the free_list
*)


module Make(Underlying: V1_LWT.BLOCK) = struct
  module Root_block = Root_block(Underlying)
  module Allocated_block = Allocated_block(Underlying)

  type heap = {
    underlying: Underlying.t;
    info: Underlying.info;
    mutable root_block: Root_block.t;
  }

  let allocate ~heap ~length ~tag () =
    (* if there are blocks above the high-water mark, grab those.
       otherwise trigger a compaction *)
    let sectors_required = (Int64.to_int length + heap.info.sector_size - 1) / heap.info.sector_size + 1 in

    if heap.root_block.Root_block.high_water_mark > heap.info.Underlying.size_sectors then begin
      failwith "unimplemented: garbage collection"
    end else begin
      let offset = heap.root_block.Root_block.high_water_mark in
      (* bump the high_water_mark *)
      heap.root_block <- { heap.root_block with Root_block.high_water_mark = Int64.(add heap.root_block.Root_block.high_water_mark (of_int sectors_required)) };
      let open Error.Infix in
      Root_block.write ~block:heap.underlying heap.root_block
      >>= fun () ->
      (* write an allocation header *)
      let h = Allocated_block.create ~length ~tag in
      Allocated_block.write ~block:heap.underlying ~offset h
      >>= fun () ->
      Lwt.return (`Ok (offset, h))
    end

    let deallocate ~heap ~offset ~h () =
      (* Mark the block as deleted to help detect use-after-free bugs *)
      let h = { h with Allocated_block.deleted = true } in
      let open Error.Infix in
      Allocated_block.write ~block:heap.underlying ~offset h
      >>= fun () ->
      (* If this block is the highest allocated block, then decrease the low
         water mark *)
      let sector_size = heap.info.sector_size in
      let length_sectors = (Int64.to_int h.Allocated_block.length + sector_size - 1) / sector_size + 1 in
      if Int64.(add offset (of_int length_sectors)) = heap.root_block.Root_block.high_water_mark then begin
        heap.root_block <- { heap.root_block with Root_block.high_water_mark = offset };
        Root_block.write ~block:heap.underlying heap.root_block
      end else begin
        (* Add the blocks to the free list *)
        failwith "unimplemented: deallocate"
      end

  type reference = int64

  module Refs = struct
    type t = {
      heap: heap;
      offset: int64;
      h: Allocated_block.t;
      length: int;
    }

    let ref t = t.offset

    let malloc t =
      let sector_size = Int64.of_int t.heap.info.Underlying.sector_size in
      let size_sectors = Int64.(div (pred (add t.h.Allocated_block.length sector_size)) sector_size) in
      let size_bytes = Int64.mul size_sectors sector_size in
      alloc (Int64.to_int size_bytes)

    let connect ~heap ~offset ~h =
      let sector = alloc heap.info.Underlying.sector_size in
      let open Error.FromBlock in
      Underlying.read heap.underlying (Int64.add offset 1L) [ sector ]
      >>= fun () ->
      let length = Int64.to_int (Cstruct.LE.get_uint64 sector 0) in
      Lwt.return (`Ok { heap; offset; h; length })

    let get t =
      let buf = malloc t in
      let open Error.FromBlock in
      Underlying.read t.heap.underlying (Int64.(add t.offset 1L)) [ buf ]
      >>= fun () ->
      let results = Array.create t.length None in
      for i = 0 to t.length - 1 do
        match Cstruct.LE.get_uint64 buf ((i + 1) * 8) with
        | 0L -> ()
        | n -> results.(i) <- Some n
      done;
      Lwt.return (`Ok results)

    let set t rfs =
      let buf = malloc t in
      Cstruct.LE.set_uint64 buf 0 (Int64.of_int t.length);
      for i = 0 to t.length - 1 do
        Cstruct.LE.set_uint64 buf ((i + 1) * 8) (match rfs.(i) with None -> 0L | Some x -> x)
      done;
      let open Error.FromBlock in
      Underlying.write t.heap.underlying (Int64.(add t.offset 1L)) [ buf ]
      >>= fun () ->
      Lwt.return (`Ok ())

    let allocate_internal ~heap ~length () =
      let open Error.Infix in
      (* Each reference is a 64-bit integer. The first integer is the length *)
      let length_bytes = Int64.(mul 8L (of_int (length + 1))) in
      allocate ~heap ~length:length_bytes ~tag:`Refs ()
      >>= fun (offset, h) ->
      (* FIXME: a crash at this point will leave the block unreferenced *)
      let size = roundup heap.info.Underlying.sector_size (Int64.to_int length_bytes) in
      let data = alloc size in
      Cstruct.memset data 0;
      Cstruct.LE.set_uint64 data 0 (Int64.of_int length);
      let open Error.FromBlock in
      Underlying.write heap.underlying (Int64.(add offset 1L)) [ data ]
      >>= fun () ->
      Lwt.return (`Ok { heap; offset; h; length })

    let allocate ~parent ~index ~length () =
      let open Error.Infix in
      allocate_internal ~heap:parent.heap ~length ()
      >>= fun block ->
      (* Write the reference in the parent block *)
      get parent
      >>= fun current_refs ->
      current_refs.(index) <- Some block.offset;
      set parent current_refs
      >>= fun () ->
      (* Now the block is fully referenced *)
      Lwt.return (`Ok block)

    let deallocate ~t () =
      deallocate ~heap:t.heap ~offset:t.offset ~h:t.h ()
  end

  module Bytes = struct
    (* This could evolve into something like a device-mapper device, when we
       start supporting non-contiguous blocks *)

    type id = unit
    type 'a io = 'a Lwt.t
    type error = Mirage_block.Error.error
    type page_aligned_buffer = Cstruct.t
    type info = {
      read_write: bool;
      sector_size: int;
      size_sectors: int64;
    }
    type t = {
      heap: heap;
      offset: int64;
      h: Allocated_block.t;
      info: info;
      mutable connected: bool;
    }

    let ref t = t.offset

    let get_info t = Lwt.return t.info
    let disconnect t =
      t.connected <- false;
      Lwt.return ()

    let check_bounds t ofs bufs =
      let length = List.fold_left (fun acc x -> acc + (Cstruct.len x)) 0 bufs in
      let length_sectors = (length + t.info.sector_size - 1) / t.info.sector_size in
      if Int64.(add ofs (of_int length_sectors)) > t.info.size_sectors
      then Lwt.return (`Error (`Unknown (Printf.sprintf "I/O out of bounds: ofs=%Ld length_sectors=%d size_sectors=%Ld" ofs length_sectors t.info.size_sectors)))
      else Lwt.return (`Ok ())

    let read t ofs bufs =
      let open Error.Infix in
      check_bounds t ofs bufs
      >>= fun () ->
      (* Data follows the header sector *)
      Underlying.read t.heap.underlying (Int64.(add (add ofs t.offset) 1L)) bufs

    let write t ofs bufs =
      let open Error.Infix in
      check_bounds t ofs bufs
      >>= fun () ->
      (* Data follows the header sector *)
      Underlying.write t.heap.underlying (Int64.(add (add ofs t.offset) 1L)) bufs

    let connect ~heap ~offset ~h =
      Underlying.get_info heap.underlying
      >>= fun info ->
      let size_sectors = Int64.(div (pred (add h.Allocated_block.length (of_int info.Underlying.sector_size))) (of_int info.Underlying.sector_size)) in
      let info = {
        read_write = info.Underlying.read_write;
        sector_size = info.Underlying.sector_size;
        size_sectors = size_sectors;
      } in
      Lwt.return {
        heap;
        offset;
        h;
        info;
        connected = true;
      }

    let create heap offset h = connect ~heap ~offset ~h

    let allocate ~parent ~index ~length () =
      let open Error.Infix in
      allocate ~heap:parent.Refs.heap ~length ~tag:`Bytes ()
      >>= fun (offset, h) ->
      (* FIXME: at this point if we crash the block will be lost *)
      Refs.get parent
      >>= fun parent_refs ->
      parent_refs.(index) <- Some offset;
      Refs.set parent parent_refs
      >>= fun () ->
      (* return the allocated BLOCK *)
      let open Lwt.Infix in
      create parent.Refs.heap offset h
      >>= fun bytes ->
      Lwt.return (`Ok bytes)

    let deallocate ~t () =
      deallocate ~heap:t.heap ~offset:t.offset ~h:t.h ()
  end

  type block =
    | Bytes of Bytes.t
    | Refs of Refs.t

  let lookup ~heap ~ref () =
    let open Error.Infix in
    Allocated_block.read ~block:heap.underlying ~offset:ref
    >>= fun h ->
    match h.Allocated_block.tag with
    | `Bytes ->
      let open Lwt.Infix in
      Bytes.connect ~heap ~offset:ref ~h
      >>= fun x ->
      Lwt.return (`Ok (Bytes x))
    | `Refs ->
      Refs.connect ~heap ~offset:ref ~h
      >>= fun x ->
      Lwt.return (`Ok (Refs x))

  let connect ~block () =
    Underlying.get_info block
    >>= fun info ->
    let open Error.Infix in
    (* read the root block *)
    Root_block.read ~block
    >>= fun root_block ->
    Lwt.return (`Ok { underlying = block; info; root_block })

  let format ~block () =
    (* write a fresh root block *)
    let open Error.Infix in
    let root = Root_block.create () in
    Root_block.write ~block root
    >>= fun () ->
    connect ~block ()
    >>= fun heap ->
    Refs.allocate_internal ~heap ~length:32 ()
    >>= fun b ->
    let rf = Refs.ref b in
    let root = { root with Root_block.root = rf } in
    Root_block.write ~block root

  let root ~heap () =
    let ref = heap.root_block.Root_block.root in
    let open Error.Infix in
    lookup ~heap ~ref ()
    >>= function
    | Bytes _ -> assert false
    | Refs x -> Lwt.return (`Ok x)

end
