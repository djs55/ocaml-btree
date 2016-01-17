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

type t = int64

let compare (a: t) (b: t) = match compare a b with
  | 0 -> `Equal
  | -1 -> `LessThan
  | 1 -> `GreaterThan
  | _ -> assert false

let size = 8

let marshal t buf =
  let buf' = Cstruct.len buf in
  if size > buf'
  then `Error (`Msg (Printf.sprintf "Cannot marshal value of length %d into buffer of length %d" size buf'))
  else begin
    Cstruct.LE.set_uint64 buf 0 t;
    `Ok (Cstruct.shift buf size)
  end

let unmarshal buf =
  let buf' = Cstruct.len buf in
  if size > buf'
  then `Error (`Msg (Printf.sprintf "Cannot unmarshal value of length %d from buffer of length %d" size buf'))
  else `Ok (Cstruct.LE.get_uint64 buf 0, Cstruct.shift buf size)
