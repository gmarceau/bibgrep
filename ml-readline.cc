
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <ctype.h>

static int completionsInitialized = 0;

CAMLprim value ml_readline(value prompt);

CAMLprim value ml_readline(value prompt)
{
  CAMLparam1(prompt);
  CAMLlocal1(result);
  char *line;
  char *p;
  
  line = readline(String_val(prompt));
  
  if (!line) failwith("readline eof");
  
  for(p = line; *p; p++)
    if (!isspace(*p))
    {
      add_history(line);
      break;
    }
  
  result = alloc_string(strlen(line));
  strcpy(String_val(result), line);
  free(line);
  
  CAMLreturn(result);
}

static int commandCnt;
static char** commands;

char *completion_generator(char *text, int state)
{
  static int list_index;
  int len;
  char *name;
  int i;

  if (!state)
  {
    list_index = 0;
    len = strlen(text);
  }
  
  while(list_index < commandCnt)
  {
    name = commands[list_index++];
    if (strncmp(name, text, len) == 0)
      return dupstr(name);
  }
  return NULL;
}


char **completer(const char *text, int start, int end) 
{
  char**matches;

  matches = NULL;
  if (start == 0)
    matches = rl_completion_matches(text, completion_generator);

  return matches;
}

CAMLprim value ml_readline_with_command_completions(value prompt, value commandNames)
{
  CAMLparam2(prompt, commandNames);
  int i;

  if (!completionsInitialized)
  {
    rl_attempted_completion_function = completer;
    completionsInitialized = 1;
    commands = NULL;
  }

  if (commands) 
  {
    for(i = 0; i<commandCnt; i++)
      free(commands[i]);
    free(commands);
  }

  commandCnt = Wosize_val(commandNames);
  commands = malloc(commandCnt);

  for(i = 0; i<commandCnt; i++)
  {
    commands[i] = malloc(strlen(String_val(Field(commandNames, i))));
    strcpy(commands[i], String_val(Field(commandNames, i)));
  }


  CAMLreturn(ml_readline(prompt));
}


