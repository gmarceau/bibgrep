
type query =
    BaseQuery of string option * string (* fieldname, keyword string, untokenized *)
  | AndQuery of query list
  | OrQuery of query list
  | NotQuery of query

val errorPosition : int ref
val printQuery : query -> unit

