(*
Copyright (C) 2003 Guillaume Marceau <gmarceau at cs dot brown dot edu>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)

(* 
   This is the basis for writing customized printf-like 
   functions.
   Grabed from the caml gurus at Iria via the usenet
*)

external format_int: string -> int -> string = "format_int"
external format_int32: string -> int32 -> string = "int32_format"
external format_nativeint: string -> nativeint -> string = "nativeint_format"
external format_int64: string -> int64 -> string = "int64_format"
external format_float: string -> float -> string = "format_float"

let bad_format fmt pos =
  invalid_arg
    ("printf: bad format " ^ String.sub fmt pos (String.length fmt - pos))

let format_string format s =
  let rec parse_format neg i =
    if i >= String.length format then (0, neg) else
    match String.unsafe_get format i with
    | '1'..'9' ->
        (int_of_string (String.sub format i (String.length format - i - 1)),
         neg)
    | '-' ->
        parse_format true (succ i)
    | _ ->
        parse_format neg (succ i) in
  let (p, neg) =
    try parse_format false 1 with Failure _ -> bad_format format 0 in
  if String.length s < p then begin
    let res = String.make p ' ' in
    if neg 
    then String.blit s 0 res 0 (String.length s)
    else String.blit s 0 res (p - String.length s) (String.length s);
    res
  end else
    s

let extract_format fmt start stop widths =
  match widths with
  | [] -> String.sub fmt start (stop - start + 1)
  | _  ->
      let b = Buffer.create (stop - start + 10) in
      let rec fill_format i w =
        if i > stop then Buffer.contents b else
          match (String.unsafe_get fmt i, w) with
            ('*', h::t) ->
              Buffer.add_string b (string_of_int h); fill_format (succ i) t
          | ('*', []) ->
              bad_format fmt start (* should not happen *)
          | (c, _) ->
              Buffer.add_char b c; fill_format (succ i) w
      in fill_format start (List.rev widths)

let scan_format fmt pos cont_s cont_a cont_t =
  let rec scan_flags widths i =
    match String.unsafe_get fmt i with
    | '*' ->
        Obj.magic(fun w -> scan_flags (w :: widths) (succ i))
    | '0'..'9' | '.' | '#' | '-' | ' ' | '+' -> scan_flags widths (succ i)
    | _ -> scan_conv widths i
  and scan_conv widths i =
    match String.unsafe_get fmt i with
    | '%' ->
        cont_s "%" (succ i)
    | 's' | 'S' as conv ->
        Obj.magic (fun (s: string) ->
          let s = if conv = 's' then s else "\"" ^ String.escaped s ^ "\"" in
          if i = succ pos (* optimize for common case %s *)
          then cont_s s (succ i)
          else cont_s (format_string (extract_format fmt pos i widths) s)
                      (succ i))
    | 'c' | 'C' as conv ->
        Obj.magic (fun (c: char) ->
          if conv = 'c'
          then cont_s (String.make 1 c) (succ i)
          else cont_s ("'" ^ Char.escaped c ^ "'") (succ i))
    | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
        Obj.magic(fun (n: int) ->
          cont_s (format_int (extract_format fmt pos i widths) n) (succ i))
    | 'f' | 'e' | 'E' | 'g' | 'G' ->
        Obj.magic(fun (f: float) ->
          cont_s (format_float (extract_format fmt pos i widths) f) (succ i))
    | 'b' ->
        Obj.magic(fun (b: bool) ->
          cont_s (string_of_bool b) (succ i))
    | 'a' ->
        Obj.magic (fun printer arg ->
          cont_a printer arg (succ i))
    | 't' ->
        Obj.magic (fun printer ->
          cont_t printer (succ i))
    | 'l' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: int32) ->
              cont_s (format_int32 (extract_format fmt pos (succ i) widths) n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | 'n' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: nativeint) ->
              cont_s (format_nativeint
                         (extract_format fmt pos (succ i) widths)
                         n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | 'L' ->
        begin match String.unsafe_get fmt (succ i) with
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun (n: int64) ->
              cont_s (format_int64 (extract_format fmt pos (succ i) widths) n)
                     (i + 2))
        | _ ->
            bad_format fmt pos
        end
    | _ ->
        bad_format fmt pos
  in scan_flags [] (pos + 1)


let kprintf kont fmt =
  let fmt = (Obj.magic fmt : string) in
  let len = String.length fmt in
  let dest = Buffer.create (len + 16) in
  let rec doprn i =
    if i >= len then begin
      let res = Buffer.contents dest in
      Buffer.clear dest;  (* just in case kprintf is partially applied *)
      Obj.magic (kont res)
    end else
    match String.unsafe_get fmt i with
    | '%' -> scan_format fmt i cont_s cont_a cont_t
    |  c  -> Buffer.add_char dest c; doprn (succ i)
  and cont_s s i =
    Buffer.add_string dest s; doprn i
  and cont_a printer arg i =
    Buffer.add_string dest (printer () arg); doprn i
  and cont_t printer i =
    Buffer.add_string dest (printer ()); doprn i
  in doprn 0

(* 
   A customized print which flushes it output
   after each call (not unlike C's original printf)
*)
     
let printf fmt_etc =
  let k result = 
    print_string result;
    flush stdout; ""
  in
  kprintf k fmt_etc
    
let fprintf chn fmt_etc =
  let k result = 
    output_string chn result;
    flush chn; ""
  in
  kprintf k fmt_etc
