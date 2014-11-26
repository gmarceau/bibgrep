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
module P = LocalPrintf

type query =
    BaseQuery of string option * string
  | AndQuery of query list
  | OrQuery of query list
  | NotQuery of query
      
let errorPosition = ref 0

let printQuery query =
  let ind i = String.make i ' ' in
  let rec loop node i = 
    match node with
        BaseQuery(Some(field), word) ->
          P.printf "%s%s : %s\n" (ind i) field word
      | BaseQuery(None, word) ->
          P.printf "%s <any> %s\n" (ind i) word
      | AndQuery(queries) ->
          P.printf "%sand:\n" (ind i);
          List.iter
            (fun item -> loop item (i+4))
            queries
      | OrQuery(queries) ->
          P.printf "%sor:\n" (ind i);
          List.iter
            (fun item -> loop item (i+4))
            queries
      | NotQuery(subQuery) ->
          P.printf "%snot:\n" (ind i);
          loop subQuery (i+4)
  in loop query 2
       
