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

type 'a error = [ `Ok of 'a | `Error of [ `Msg of string ] ]

module Make(Underlying: V1_LWT.BLOCK): sig

  type t
  (** A heap containing blocks *)

  module Block: V1_LWT.BLOCK
  (** An allocated (i.e. non-free) block on the underying device *)

  val format: block:Underlying.t -> unit -> unit error Lwt.t
  (** [format block] initialises the underlying block device. Some data will
      be lost, but the device won't be securely erased. *)

  val connect: block:Underlying.t -> unit -> t error Lwt.t
  (** [connect block] connects to the Heap stored on [block] *)

  val allocate: t:t -> length:int64 -> unit -> Block.t error Lwt.t
  (** Allocate a block of length [length] and return it so it may be
      updated.
      FIXME: add this to a transaction somehow
  *)

  val deallocate: block:Block.t -> unit -> unit error Lwt.t
  (** Deallocate a block by adding it to the free list.
      FIXME: add this to a transaction somehow
  *)

end
