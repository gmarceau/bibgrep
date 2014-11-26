#include <btparse.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdio.h>

extern int zzline;


CAMLprim value ml_open_file(value filename) 
{
  CAMLparam1(filename);
  FILE *p = fopen(String_val(filename), "r");
  if (!p) failwith("cannot open file");
  CAMLreturn((value)p);
}

CAMLprim void ml_close_file(value file) 
{
  CAMLparam1(file);
  if (fclose((FILE*)file)) failwith("error closing file");
  CAMLreturn0;
}

CAMLprim void ml_bt_initialize(void) 
{
  CAMLparam0();
  bt_initialize();
  CAMLreturn0;
}

CAMLprim void ml_bt_cleanup(void) 
{
  CAMLparam0();
  bt_cleanup();
  CAMLreturn0;
}

CAMLprim void ml_bt_free_ast(value ast)
{
  CAMLparam1(ast);
  if ((AST*)ast) bt_free_ast((AST*)ast);
  CAMLreturn0;
}

CAMLprim value ml_bt_parse_entry(value file, value filename)
{
  CAMLparam2(file, filename);
  CAMLlocal2(ast, result);
  boolean status;
  
  ast = (value)bt_parse_entry((FILE*)file, String_val(filename), 0, &status);

  if (ast && status)
    // all ok
    status = 0;
  else if (ast && !status)
    // recoverable parse error
    status = 1;
  else if (!ast && status)
    // Reached the eof
    status = 2;
  else if (!ast && !status)
    // irrecoverable parse error, will have to skip some text
    status = 3;


  result = alloc(2, 0);
  if (ast) Store_field(result, 0, ast);
  Store_field(result, 1, Val_int(status));
   
  CAMLreturn(result);
}

CAMLprim value ml_bt_parse_entry_s(value str)
{
  CAMLparam1(str);
  CAMLlocal2(ast, result);
  boolean status;
  
  ast = (value)bt_parse_entry_s((char*)str, NULL, 1, 0, &status);

  if (ast && status)
    // all ok
    status = 0;
  else if (ast && !status)
    // recoverable parse error
    status = 1;
  else if (!ast && status)
    // Reached the eof
    status = 2;
  else if (!ast && !status)
    // irrecoverable parse error, will have to skip some text
    status = 3;


  result = alloc(2, 0);
  if (ast) Store_field(result, 0, ast);
  Store_field(result, 1, Val_int(status));
   
  CAMLreturn(result);
}

CAMLprim void ml_bt_parse_string_done(void)
{
  CAMLparam0();
  bt_parse_entry_s(NULL, NULL, 1, 0, NULL);
  CAMLreturn0;
}

CAMLprim value ml_bt_first_field(value entry)
{
  CAMLparam1(entry);
  CAMLlocal3(ast, valueFieldName, result);
  char *fieldName;

  ast = (value)bt_next_field((AST*)entry, NULL, &fieldName);

  result = alloc(3, 0);

  if (ast)
  {
    valueFieldName = alloc_string(strlen(fieldName));
    strcpy(String_val(valueFieldName), fieldName);
    Store_field(result, 0, ast);
    Store_field(result, 1, valueFieldName);
  }
  Store_field(result, 2, Val_int(ast != 0));
  
  CAMLreturn(result);
}

CAMLprim value ml_bt_next_field(value entry, value prev)
{
  CAMLparam2(entry, prev);
  CAMLlocal3(ast, valueFieldName, result);
  char *fieldName;

  ast = (value)bt_next_field((AST*)entry, (AST*)prev, &fieldName);

  result = alloc(3, 0);

  if (ast)
  {
    valueFieldName = alloc_string(strlen(fieldName));
    strcpy(String_val(valueFieldName), fieldName);
    Store_field(result, 0, ast);
    Store_field(result, 1, valueFieldName);
  }

  Store_field(result, 2, Val_int(ast != 0));

  CAMLreturn(result);
}

CAMLprim value ml_bt_get_text(value ast) 
{
  CAMLparam1(ast);
  CAMLlocal1(resultValue);

  char *result = bt_get_text((AST*)ast);
  if (!result) failwith("Invalid ast node");
  resultValue = alloc_string(strlen(result));
  strcpy(String_val(resultValue), result);
  CAMLreturn(resultValue);
}

CAMLprim value ml_bt_split_list(value str, value delim, value filename, 
                                value line, value description)
{
  CAMLparam5(str, delim, filename, line, description);
  CAMLlocal2(resultValue, tempStr);
  int i;
  bt_stringlist *result = bt_split_list(String_val(str), 
                                        String_val(delim),
                                        String_val(filename),
                                        Int_val(line),
                                        String_val(description));

  if (!result) failwith("parse error");

  resultValue = alloc(result->num_items, 0);
  for(i = 0; i<result->num_items; i++)
  {
    if (result->items[i])
    {
      tempStr = alloc_string(strlen(result->items[i]));
      strcpy(String_val(tempStr), result->items[i]);
    }
    else
    {
      tempStr = alloc_string(0);
      strcpy(String_val(tempStr), "");
    }

    Store_field(resultValue, i, tempStr);
  }

  bt_free_list(result);

  CAMLreturn(resultValue);
}

CAMLprim ml_bt_split_name(value nameStr, value filename, value line, value name_num)
{
  CAMLparam4(nameStr, filename, line, name_num);
  CAMLlocal3(resultValue, tempStr, tempArray);
  bt_namepart PART_FLAGS[] = { BTN_FIRST, BTN_VON, BTN_LAST, BTN_JR };
  int part, i;
  bt_name *name = bt_split_name(String_val(nameStr),
                                String_val(filename),
                                Int_val(line),
                                Int_val(name_num));

  if (!name) failwith("parse error");

  resultValue = alloc(4, 0);
  for(part = 0; part<4; part++)
  {
    char **tokens;
    int num_tokens;
    
    tokens = name->parts[PART_FLAGS[part]];
    num_tokens = name->part_len[PART_FLAGS[part]];
    
    tempArray = alloc(num_tokens, 0);
    for (i = 0; i < num_tokens; i++)
    {
      if (tokens[i])
      {
        tempStr = alloc_string(strlen(tokens[i]));
        strcpy(String_val(tempStr), tokens[i]);
        Store_field(tempArray, i, tempStr);
      }
      else
      {
        tempStr = alloc_string(0);
        strcpy(String_val(tempStr), "");
        Store_field(tempArray, i, tempStr);
      }
    }

    Store_field(resultValue, part, tempArray);
  }
  bt_free_name(name);

  CAMLreturn(resultValue);
}


CAMLprim value ml_current_line(void) 
{
  CAMLparam0();
  CAMLreturn(Val_int(zzline > 0 ? zzline - 1 : 0));
}

CAMLprim value ml_current_seek(value fileHandle) 
{
  CAMLparam1(fileHandle);
  int seek;

  /* minus two to remove the parse two character lookahead : */
  seek = ftell((FILE*)fileHandle)-2;

  CAMLreturn(Val_int(seek > 0 ? seek : 0));
}

CAMLprim value ml_seek_to(value fileHandle, value offset)
{
  CAMLparam1(fileHandle);

  CAMLreturn(Val_int(fseek((FILE*)fileHandle, Int_val(offset), SEEK_SET)));
}

CAMLprim value ml_bt_purify_string(value str)
{
  CAMLparam1(str);
  CAMLlocal1(rtn);
  rtn = alloc_string(strlen(String_val(str)));
  strcpy(String_val(rtn), String_val(str));
  bt_purify_string(String_val(rtn), 0);

  CAMLreturn(rtn);
}
