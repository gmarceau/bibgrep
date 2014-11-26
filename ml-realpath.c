#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>

CAMLprim value ml_realpath(value filename) 
{
  CAMLparam1(filename);
  CAMLlocal1(resultValue);
  char temp[PATH_MAX + PATH_MAX];
  char *result;
  char resolved_path[PATH_MAX];
  resolved_path[0] = 0;

  if (String_val(filename)[0] == '~')
  {
    strcpy(temp, getenv("HOME"));
    strcat(temp, String_val(filename)+1);
  }
  else 
    strcpy(temp, String_val(filename));


  result = realpath(String_val(temp), resolved_path);

  if (!result && errno != ENOENT) 
    failwith("could not find real path");

  resultValue = alloc_string(strlen(resolved_path));
  strcpy(String_val(resultValue), resolved_path);
  CAMLreturn(resultValue);
}
