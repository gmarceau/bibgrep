%{
  module P = LocalPrintf
  module S = String
  open Query
                 
  let lastField = ref "" 

  let parse_error _ =
    errorPosition := Parsing.symbol_start ()

  exception No_previous_field
%}
%token OR
%token OPEN_BRACE
%token OPEN_SQRBRACE
%token OPEN_PAREN
%token CLOSE_BRACE
%token CLOSE_SQRBRACE
%token CLOSE_PAREN
%token <string> WORD
%token <string> FIELDWORD
%token EOL
%token NEG
%token <string> JUNK  
%start main
%type <Query.query> main

%left OR
%%
  
  
  
main:
  orExpr EOL { $1 }
  ;


orExpr:
  andExpr { $1 }
  | orExpr OR andExpr 
      { 
        match $1 with
            OrQuery(q) -> OrQuery($3 :: q)
          | _ -> OrQuery([$1; $3]) 
      }


andExpr:
  non_empty_expr_list 
  { 
    match $1 with
        [h] -> h
      | _ -> AndQuery($1)
  }
  
non_empty_expr_list:
  expr_list expr { $2 :: $1 }

expr_list:
    /* empty */ { [] }
  | expr_list expr { $2 :: $1 }
 

expr:
  WORD { BaseQuery(None, $1) }
  
  | FIELDWORD 
      {
        let colon = S.index $1 ':' in
        if colon = 0 then 
          if !lastField = "" then raise No_previous_field
          else BaseQuery(Some(!lastField), (S.sub $1 1 ((S.length $1) -1)))
        else
          let fieldText = S.sub $1 0 colon in
          let queryText = S.sub $1 (colon+1) ((S.length $1) - colon - 1) in
          lastField := fieldText;
          BaseQuery(Some(fieldText), queryText)
      }
  | OPEN_BRACE orExpr CLOSE_BRACE { $2 }
  | OPEN_SQRBRACE orExpr CLOSE_SQRBRACE { $2 }
  | OPEN_PAREN orExpr CLOSE_PAREN { $2 }
  | NEG expr { NotQuery($2) }
;


  
  
%%
  
  
