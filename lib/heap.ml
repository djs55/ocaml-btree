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
open Lwt.Infix

(* The first 16 bytes will contain a magic string *)
let magic = "MIRAGEFS\192\157\086\025\215\044\040\236"

(* The first sector will contain a "root block" *)
cstruct root_block {
  uint8_t magic[16];
  uint32_t version;
  uint64_t high_water_mark;
  uint64_t free_list;
} as little_endian

(* Blocks are free if either
   - they are >= the high_water_mark
   - they are reachable via the free_list
*)

module Make(Underlying: V1_LWT.BLOCK) = struct

  type t = {
    underlying: Underlying.t;
    high_water_mark: int64; (* sectors >= the high_water_mark have not been written to *)
    free_list: int64; (* reference to the first block on the free list *)
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
  end

  let format ~block () =
    (* write a fresh root block *)
    failwith "unimplemented: format"

  let connect ~block () =
    (* read the root block *)
    failwith "unimplemented: connect"

  let allocate ~t ~length () =
    (* if there are blocks above the high-water mark, grab those.
       otherwise trigger a compaction *)
    failwith "unimplemented: allocate"

  let deallocate ~block () =
    (* Add the blocks to the free list *)
    failwith "unimplemented: deallocate"

end
