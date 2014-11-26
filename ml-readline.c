
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <ctype.h>

extern char *xmalloc ();
static int completionsInitialized = 0;
static int readlineNameInitialized = 0;
static char *ml_readline_name;

char *dupstr(char *s)
{
  char *r;

  r = xmalloc(strlen(s) + 1);
  strcpy (r, s);
  return (r);
}

CAMLprim value ml_readline(value programName, value prompt)
{
  CAMLparam2(programName, prompt);
  CAMLlocal1(result);
  char *line;
  char *p;
  
  /* Allow conditional parsing of the ~/.inputrc file. */

  if (readlineNameInitialized && ml_readline_name)
    free(ml_readline_name);

  ml_readline_name = xmalloc(strlen(String_val(programName)));
  strcpy(ml_readline_name, String_val(programName));
  rl_readline_name = rl_readline_name;
  readlineNameInitialized = 1;


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

static int ml_command_cnt;
static char** ml_commands;

char *completion_generator(const char *text, int state)
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
  
  while(list_index < ml_command_cnt)
  {
    name = ml_commands[list_index++];

    if (strncmp(name, text, len) == 0)
    {
      return dupstr(name);
    }
  }
  return NULL;
}


char **completer(const char *text, int start, int end) 
{
  char**matches;
  int i;

  matches = NULL;
  if (start == 0)
    matches = rl_completion_matches(text, completion_generator);

  return matches;
}

CAMLprim value ml_readline_with_command_completions(value programName, value prompt, value commandNames)
{
  CAMLparam3(programName, prompt, commandNames);
  int i;

  if (!completionsInitialized)
  {
    rl_attempted_completion_function = completer;
    completionsInitialized = 1;
    ml_commands = NULL;
  }

  if (ml_commands) 
  {
    for(i = 0; i<ml_command_cnt; i++)
      free(ml_commands[i]);
    free(ml_commands);
  }

  ml_command_cnt = Wosize_val(commandNames);
  ml_commands = (char**)xmalloc(ml_command_cnt * sizeof(char*));

  for(i = 0; i<ml_command_cnt; i++)
  {
    ml_commands[i] = malloc(strlen(String_val(Field(commandNames, i))));
    strcpy(ml_commands[i], String_val(Field(commandNames, i)));
  }


  CAMLreturn(ml_readline(programName, prompt));
}


