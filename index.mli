
exception Bad_file of string

type coordinates =
    {
      filename : string;
      line : int;
      seekOffset : int;
      entryId : int;
      length : int;
      mutable fullText : string option
    }

type srcStatistics

type t

val indexFormatVersionNumber : float

val create : unit -> t

(* Strings passed to the getHits functions should 
   have been tokenized by Index.tokenize. Otherwise
   they will hardly have the right form
*)
val getAllHits : t -> string -> Ptset.t (* index, word *)
val getFieldHits : t -> string -> string -> Ptset.t (* index, fieldname, word *)
val getFileHits : t -> string -> string -> Ptset.t (* index, filename, word *)
val getFilesHits : t -> string list -> string -> Ptset.t (* index, filenames, word *)
val getFileFieldHits : t -> string -> string -> string -> Ptset.t (* index, filename, fieldname, word *)
val getFilesFieldHits : t -> string list -> string -> string -> Ptset.t (* index, filenames, fieldname, word *)
val getNegation : t -> string list -> Ptset.t -> Ptset.t (* index, filenames, indesirables *)

val getEntry : t -> int -> coordinates

(* tokenize a string a in a way that is consistant 
   with the way the index was created. 

   It also changes the case 
*)
val tokenize : string -> string list 
val tokenizeThisRegexp : string -> string list (* this version of tokenize does not throw away regexp characters *)
val isBadWord : string -> bool

val addText : t -> coordinates -> string -> string -> unit
val addAuthorField : t -> coordinates -> Btparse.file -> string -> unit

(* throws Sys_error is the file is not found : *)
val addFile : t -> string -> unit
val removeFile : t -> string -> unit
val size : t -> int

val getFiles : t -> string list
val isFileIndexOutOfDate : t -> string -> bool

val isChanged : t -> bool
val clearChanged : t -> unit

val save : t -> string -> unit
val load : string -> bool -> t


(*
val pack :
    t ->
    int * int * bool * (int, coordinates) LocalHashtbl.t * (string, srcStatistics) LocalHashtbl.t *
    (string, (string, (string, int array) LocalHashtbl.t) LocalHashtbl.t) LocalHashtbl.t

val unpack : 
    int * int * bool * (int, coordinates) LocalHashtbl.t * (string, srcStatistics) LocalHashtbl.t *
    (string, (string, (string, int array) LocalHashtbl.t) LocalHashtbl.t) LocalHashtbl.t
    -> t

*)
