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

type 'a error = [ `Ok of 'a | `Error of [ `Msg of string ] ]

module BlockError = struct
  let (>>=) m f = m >>= function
    | `Error e -> Lwt.return (`Error (`Msg (Mirage_block.Error.string_of_error e)))
    | `Ok x -> f x
end

module Error = struct
  let (>>=) m f = m >>= function
    | `Error e -> Lwt.return (`Error e)
    | `Ok x -> f x
end

let alloc size =
  let npages = (size + 4095) / 4096 in
  let pages = Io_page.get npages in
  Cstruct.(sub (Io_page.to_cstruct pages) 0 size)

module Root_block(B: V1_LWT.BLOCK) = struct
  (* The first 16 bytes will contain a magic string *)
  let magic = "MIRAGEFS\192\157\086\025\215\044\040\236"

  (* The first sector will contain a "root block" *)
  cstruct hdr {
    uint8_t magic[16];
    uint32_t version;
    uint64_t high_water_mark;
    uint64_t free_list;
  } as little_endian

  type t = {
    magic: string;
    version: int32;
    high_water_mark: int64; (* sectors >= the high_water_mark have not been written to *)
    free_list: int64; (* reference to the first block on the free list *)
  } with sexp

  let create () = { magic; version = 0l; high_water_mark = 0L; free_list = 0L }

  let read ~block =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    let open BlockError in
    B.read block 0L [ sector ]
    >>= fun () ->
    let magic' = Cstruct.to_string (get_hdr_magic sector) in
    let version = get_hdr_version sector in
    let high_water_mark = get_hdr_high_water_mark sector in
    let free_list = get_hdr_free_list sector in
    if magic <> magic'
    then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected header magic, expected '%s' but read '%s'" magic magic')))
    else begin
      if version <> 0l
      then Lwt.return (`Error (`Msg (Printf.sprintf "Unexpected header version, expected '%ld' but read '%ld'" 0l version)))
      else Lwt.return (`Ok { magic; version; high_water_mark; free_list })
    end
  let write ~block t =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    set_hdr_magic magic 0 sector;
    set_hdr_version sector t.version;
    set_hdr_high_water_mark sector t.high_water_mark;
    set_hdr_free_list sector t.free_list;
    let open BlockError in
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
  } as little_endian

  type t = {
    magic: string;
    version: int32;
    length: int64;
  } with sexp

  let create length = { magic; version = 0l; length }

  let read ~block ~offset =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    let open BlockError in
    B.read block offset [ sector ]
    >>= fun () ->
    let magic = Cstruct.to_string (get_hdr_magic sector) in
    let version = get_hdr_version sector in
    let length = get_hdr_length sector in
    Lwt.return (`Ok { magic; version; length })

  let write ~block ~offset t =
    B.get_info block
    >>= fun info ->
    let sector = alloc info.B.sector_size in
    set_hdr_magic t.magic 0 sector;
    set_hdr_version sector t.version;
    set_hdr_length sector t.length;
    let open BlockError in
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

  type t = {
    underlying: Underlying.t;
    info: Underlying.info;
    mutable root_block: Root_block.t;
  }

  module Block = struct
    type t = unit
    type id = unit
    type 'a io = 'a Lwt.t
    type error = Mirage_block.Error.error
    type page_aligned_buffer = Cstruct.t
    type info = {
      read_write: bool;
      sector_size: int;
      size_sectors: int64;
    }
    let get_info _ = failwith "get_info"
    let disconnect _ = failwith "disconnect"
    let read _ _ _ = failwith "read"
    let write _ _ _ = failwith "write"

    let create () = failwith "Block.create"
  end


  let format ~block () =
    (* write a fresh root block *)
    Root_block.(write ~block (create ()))

  let connect ~block () =
    Underlying.get_info block
    >>= fun info ->
    let open Error in
    (* read the root block *)
    Root_block.read ~block
    >>= fun root_block ->
    Lwt.return (`Ok { underlying = block; info; root_block })

  let allocate ~t ~length () =
    (* if there are blocks above the high-water mark, grab those.
       otherwise trigger a compaction *)
    let sectors_required = (Int64.to_int length + t.info.Underlying.sector_size - 1) / t.info.Underlying.sector_size + 1 in

    if t.root_block.Root_block.high_water_mark > t.info.Underlying.size_sectors then begin
      failwith "unimplemented: garbage collection"
    end else begin
      let offset = t.root_block.Root_block.high_water_mark in
      (* bump the high_water_mark *)
      t.root_block <- { t.root_block with Root_block.high_water_mark = Int64.(add t.root_block.Root_block.high_water_mark (of_int sectors_required)) };
      let open Error in
      Root_block.write ~block:t.underlying t.root_block
      >>= fun () ->
      (* write an allocation header *)
      let h = Allocated_block.create length in
      Allocated_block.write ~block:t.underlying ~offset h
      >>= fun () ->
      (* return the allocated BLOCK *)
      Lwt.return (Block.create ())
    end

  let deallocate ~block () =
    (* Add the blocks to the free list *)
    failwith "unimplemented: deallocate"

end
