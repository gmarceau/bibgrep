type token =
  | OR
  | OPEN_BRACE
  | OPEN_SQRBRACE
  | OPEN_PAREN
  | CLOSE_BRACE
  | CLOSE_SQRBRACE
  | CLOSE_PAREN
  | WORD of (string)
  | FIELDWORD of (string)
  | EOL
  | NEG
  | JUNK of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Query.query
