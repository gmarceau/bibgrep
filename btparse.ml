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

exception Parse_error

type c_file
type ast
type status = [`ok | `error | `eof]

type file = 
    { filename : string;
      fileHandle : c_file;
      mutable line : int;
      mutable seek : int
    }
      

external initialize : unit -> unit = "ml_bt_initialize"
external cleanup : unit -> unit = "ml_bt_cleanup"
external free_ast : ast -> unit = "ml_bt_free_ast"

external c_open_file : string -> c_file = "ml_open_file" 
external c_close_file : c_file -> unit = "ml_close_file"
external c_parse_entry : c_file -> string -> (ast * int) = "ml_bt_parse_entry"
external c_parse_string_entry : string -> (ast * int) = "ml_bt_parse_entry_s"
external parse_string_done : unit -> unit = "ml_bt_parse_string_done"
external c_first_field : ast -> (ast * string * int) = "ml_bt_first_field"
external c_next_field : ast -> ast -> (ast * string * int) = "ml_bt_next_field"
external get_text : ast -> string = "ml_bt_get_text"
external c_split_list : string -> string -> string -> int -> string -> string array = "ml_bt_split_list"
external c_split_name : string -> string -> int -> int -> string array array = "ml_bt_split_name"
external current_line : unit -> int = "ml_current_line"
external current_seek : c_file -> int = "ml_current_seek"
external c_seek_to : c_file -> int -> int = "ml_seek_to"
external purify_string : string -> string = "ml_bt_purify_string"

let open_file filename = 
  try 
    { filename = filename;
      fileHandle = c_open_file filename;
      line = 0;
      seek = 0;
    }
  with Failure(_) -> raise (Sys_error("Could not open file `" ^ filename ^"'"))

let close_file file = 
  try c_close_file file.fileHandle
  with Failure(_) -> raise (Sys_error("Error when closing file `" ^ file.filename ^ "'"))

let parse_entry file =

  let (entry, statusCode) = c_parse_entry file.fileHandle file.filename in
  file.line <- current_line ();
  file.seek <- current_seek file.fileHandle;

  match statusCode with
      0 -> (Some(entry), false)
    | 1 -> (Some(entry), true)
    | 2 -> raise End_of_file
    | 3 -> (None, true)
    | _ -> assert false

let parse_string_entry string =
  let (entry, statusCode) = c_parse_string_entry string in
  match statusCode with
      0 -> (Some(entry), false)
    | 1 -> (Some(entry), true)
    | 2 -> raise End_of_file
    | 3 -> (None, true)
    | _ -> assert false


let first_field entry = 
  let (ast, fieldName, statusCode) = c_first_field entry in
  match statusCode with
    | 0 -> None
    | 1 -> Some (ast, fieldName)
    | _ -> assert false


let next_field entry ast = 
  let (ast, fieldName, statusCode) = c_next_field entry ast in
  match statusCode with
    | 0 -> None
    | 1 -> Some (ast, fieldName)
    | _ -> assert false


let split_list v str delim description =
  try c_split_list str delim v.filename v.line description
  with Failure("parse error") -> [||]

let split_name v str name_num =
  if str = "" then ([||], [||], [||], [||])
  else 
    try (match c_split_name str v.filename v.line name_num with
             [| first; von; last; jr |] -> (first, von, last, jr)
           | _ -> assert false)
    with Failure("parse error") -> ([||],[||],[||],[||])

let current_entry_line file = file.line
let current_entry_seek file = file.seek

let seek_to file offset =
  if not ((c_seek_to file.fileHandle offset) = 0) then
    raise (Sys_error(Printf.sprintf "Error seeking along file `%s'" file.filename))

let get_fields entryAst =
  let rec loop field acc = match field with
      None -> acc
    | Some(fieldAst, fieldName) -> 
        let text = get_text fieldAst in
        loop (next_field entryAst fieldAst) ((fieldName, text) :: acc)
  in loop (first_field entryAst) []
  
