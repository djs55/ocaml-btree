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
open Error

module type COMPARABLE = sig
  type t

  val compare: t -> t -> [ `LessThan | `Equal | `GreaterThan ]
end

module type SERIALISABLE = sig
  type t

  val size: int

  val marshal: t -> Cstruct.t -> Cstruct.t error
  val unmarshal: Cstruct.t -> (t * Cstruct.t) error
end

(** Data stored within the B-tree nodes *)
module type ELEMENT = sig
  type t

  include COMPARABLE with type t := t
  include SERIALISABLE with type t := t
end

(** A space of blocks in which the b-tree nodes are stored *)
module type HEAP = sig
  type t
  type 'a io

  val block_size: t -> int io

  type block = int64

  val allocate: unit -> block io
end

(** A b-tree *)
module type TREE = sig
  type t
  type element
  type block

  val create: block:block -> d:int -> unit -> t error Lwt.t
  val connect: block -> t error Lwt.t

  val insert: t -> element -> unit error Lwt.t
  val delete: t -> element -> unit error Lwt.t
  val mem: t -> element -> bool error Lwt.t
end
