

type resultSet = 
    {
      isIrrelevant : bool; (* generated from bad words *)
      negated : bool; (* when this field is true, the result should be interpreted
                        as `everything *not* it .hits' *)
      hits : Ptset.t;
    }

val executeQuery : Index.t -> string list -> Query.query -> Index.coordinates list * string list
val loadTextWithFileCache : (string, in_channel) Hashtbl.t -> Index.coordinates -> unit
val loadText : Index.coordinates -> unit

(* This is much faster than calling `loadText' repetitively, 
   since it avoids open and closing the same files over and over *)
val loadTexts : Index.coordinates list -> unit
