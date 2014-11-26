{
  open QueryPar
  module P = LocalPrintf

}

let wordChar =      [^ '{' '}' '[' ']' '(' ')' ' ' '\t' '\n' '"' '/']
let fieldChar =     [^ '{' '}' '[' ']' '(' ')' ' ' '\t' '\n' '"' '/' ':']
let initFieldChar = [^ '{' '}' '[' ']' '(' ')' ' ' '\t' '\n' '"' '/' ':' '-' '!' ]
let whiteChar = [' ' '\t' '\n']

rule token = parse
   "//" { OR }
  | '{' { OPEN_BRACE }
  | '}' { CLOSE_BRACE }
  | '[' { OPEN_SQRBRACE }
  | ']' { CLOSE_SQRBRACE }
  | '(' { OPEN_PAREN }
  | ')' { CLOSE_PAREN }
  | '-'|'!' { NEG }
  | whiteChar+ { token lexbuf }
  | '"' { WORD(tokenString lexbuf) }
  | eof { EOL }
  | (initFieldChar fieldChar*)? ':' wordChar+ { FIELDWORD((Lexing.lexeme lexbuf)) }
  | (initFieldChar fieldChar*)? ':' '"'
      {
        let head = Lexing.lexeme lexbuf in
        let rest = tokenString lexbuf in
        FIELDWORD((String.sub head 0 ((String.length head) - 1)) ^ rest)
      }
  | initFieldChar fieldChar* { WORD(Lexing.lexeme lexbuf) }
  | _ { JUNK(Lexing.lexeme lexbuf) }
      
and tokenString = parse
    [^ '"']* '"' 
    {
      let text = Lexing.lexeme lexbuf in
      String.sub text 0 ((String.length text) - 1)
    }

{
  
  let tokenToString tok =
    match tok with
        OR -> "or "
      | OPEN_PAREN -> "( "
      | CLOSE_PAREN -> ") "
      | OPEN_BRACE -> "{ "
      | CLOSE_BRACE -> "} "
      | OPEN_SQRBRACE -> "[ "
      | CLOSE_SQRBRACE -> "] "
      | WORD(s) -> Printf.sprintf "%s " s
      | FIELDWORD(s) -> Printf.sprintf "%s " s
      | NEG -> "- "
      | JUNK(s) -> Printf.sprintf "JUNK(%s) " s
      | EOL -> ""
          
          
  let tokenization str =
    let nowDone = ref false in
    let lexbuf = Lexing.from_string str in
    let tokens = ref [] in
    while not !nowDone do
      let tok = token lexbuf in
      (match !tokens, tok with
           (JUNK(s) :: tl), JUNK(r) -> 
             tokens := JUNK(s ^ r) :: tl
         | _, EOL -> 
             nowDone := true
         | _ -> 
             tokens := tok :: !tokens);
    done;
    List.rev !tokens
      
  let tokenizationToString tokens =
    List.fold_left
      (fun acc tok -> acc ^ (tokenToString tok))
      ""
      tokens
      
  let test () =
    print_string (tokenizationToString (tokenization "author:lee // author:\"loo red\""));
    print_string (tokenizationToString (tokenization "-( -author:lee // author:\"loo red\" ) title:")) 
      
}
