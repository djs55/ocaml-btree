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

type t = string

let compare (a: t) (b: t) = compare a b

let size t = String.length t

let marshal t buf =
  let t' = size t in
  let buf' = Cstruct.len buf in
  if t' > buf'
  then `Error (`Msg (Printf.sprintf "Cannot marshal string of length %d into buffer of length %d" t' buf'))
  else begin
    Cstruct.blit_from_string t 0 buf 0 t';
    `Ok (Cstruct.shift buf t')
  end

let unmarshal buf =
  `Error (`Msg "unmarshal unimplemented")
