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

(** A heap over a raw block device.

    This module allows clients to allocate and free blocks.
*)
open Error


module Make(Underlying: V1_LWT.BLOCK): sig

  type heap
  (** A heap containing blocks *)

  type reference
  (** A reference to a block, stored inside a Ref block *)

  module Refs: sig
    type t

    val allocate: parent:t -> index:int -> length:int -> unit -> t error Lwt.t
    (** Allocate a child reference block and set a reference to it from
        [t] at array index [index]. The created block will be of length
        [length] references. *)

    val deallocate: t:t -> unit -> unit error Lwt.t
    (** Mark a reference block as unused so that it may be garbage collected. *)

    val ref: t -> reference
    (** Return a reference to the block *)

    val get: t -> reference option array error Lwt.t
    (** Read the array of references stored in the block *)

    val set: t -> reference option array -> unit error Lwt.t
    (** Update the array of references stored in the block *)
  end
  (** An array of optional references to other blocks *)

  module Bytes: sig
    type t

    val allocate: parent:Refs.t -> index:int -> length:int64 -> unit -> t error Lwt.t
    (** Allocate a block of length [length] and return it so it may be
        updated.
        FIXME: add this to a transaction somehow
    *)

    val deallocate: t:t -> unit -> unit error Lwt.t
    (** Deallocate a block by adding it to the free list.
        FIXME: add this to a transaction somehow
    *)

    val ref: t -> reference
    (** Return a reference to the block *)

    include V1_LWT.BLOCK with type t := t

  end
  (** Raw data on the underlying device *)

  type block =
    | Bytes of Bytes.t
    | Refs of Refs.t
    (** An allocated block *)

  val lookup: heap:heap -> ref:reference -> unit -> block error Lwt.t
  (** [lookup ref] dereferences the [ref] and reads the block from disk *)

  val format: block:Underlying.t -> unit -> unit error Lwt.t
  (** [format block] initialises the underlying block device. Some data will
      be lost, but the device won't be securely erased. *)

  val connect: block:Underlying.t -> unit -> heap error Lwt.t
  (** [connect block] connects to the Heap stored on [block] *)

  val root: heap:heap -> unit -> Refs.t error Lwt.t
  (** [root heap ()] returns the root reference block *)

end
