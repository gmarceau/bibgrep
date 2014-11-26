exception Parse_error

type file
type ast
(* type status = [ `ok | `error | `eof ] *)

(* raises `failwith' upon trouble opening the file *)

val initialize : unit -> unit
val cleanup : unit -> unit 
val free_ast : ast -> unit 

val open_file : string -> file 
val close_file : file -> unit

(* the parse_ functions throws End_of_file when done.
   The boolean is true if there was an error, the ast comes back
   as None if no text could be recovered from the error
*)
val parse_entry : file -> ast option * bool
val parse_string_entry : string -> ast option * bool
val parse_string_done : unit -> unit
val free_ast : ast -> unit

val first_field : ast -> (ast * string) option
val next_field : ast -> ast -> (ast * string) option

val get_fields : ast -> (string * string) list (* return pairs of field name and it content as a list *)
val get_text : ast -> string
val split_list : file -> string -> string -> string -> string array


(* Returns 4 array corresponding to the tokens of part of the name :
   the first name, 'von', the last name, 'jr', respectively
*)
val split_name : file -> string -> int -> (string array * string array * string array * string array)

val current_entry_line : file -> int
val current_entry_seek : file -> int

val seek_to : file -> int -> unit

val purify_string : string -> string
