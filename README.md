bibgrep
=======

Bibgrep indexes and searches BibTex files for entries matching a given
query. Its usage is similar to the command `grep` and the queries
uses a Google-like syntax.


Bibgrep is written mostly in Ocaml, with small wrappers around C
libraries. It also uses one Emacs-lisp module and one line of Ruby. A
small Awk script was also used to generate the list of frequent words
in English BibTex files, and counting the Makefile, it bring the total
number of programming languages involved to 6. 


The code is structured into five components:

* Parsing BibTex files : `btparse.ml ml-btparse.c`

  These files are only a Ocaml-to-C interface to the 'btparse'
  library, which does the all the grunt work.  It is fairly resilient
  to errors in BibTex files, although a number of bugs had to be fixed.

  Btparse was written by Greg Ward, the at McGill university. The file
  btparse.ps.gz documents its history and its scope. Quickly said, it
  tries to duplicate `bibtex`'s behavior as closely as possibly,
  and comes quite close to doing so.

  The fixed bugs are : When parsing multiple files, the error recovery
  failed notice the jump to a new file and reset the parser. There was
  a missing return at the bottom of the international letter detection
  switch case. The text clean up function was too aggressive. Finally,
  many warning messages for benign syntax errors were turned off.

* Indexing BibTex files : `index.ml ptset.ml`

  `Ptset.ml` is an implementation of Patricia trees writen by
  Jean-Christophe Filliatre at the Univesite Paris Sud. Patricia trees
  are a data structure over integers which support $O(log(n))$ set
  union, set intersection and set difference. Like bucket sorters,
  they achieve their theorem defying performance by taking advantage
  of the inherent structure of integers. In short, Patricia tree are
  Trie trees which select according to the bits of the binary
  representation of the numbers.

  `index.ml` is fairly strait forward.  It first assigns a unique
  id number the each BibTex entry. Then, for each indexed file, for
  each field and for each word, it keeps a Patricia tree of the entries
  containing that word. Theses tree are kept in hashtable for snappy
  $O(1)$ lookups. `index.ml` also implements matching a word or a
  regular expression against the trees.

  The index does not keep the full text of the entries. Rather it keep
  the original position the of entry as a seek offset into the
  file. This help keeping the size of the index low. Since the index
  is aggressively kept up to date (see `main.ml`), we are mostly
  guaranteed to be able to read the entries back strait from the disk,
  once it will be time to display them.

* Parsing the query : `queryLex.mll queryPar.mly query.ml`

  These implement parsing of the query language. They are a
  standard-issue lexer/parser pair. Notice that the lexer does not
  have a token for colons. Rather, the parser breaks the field
  specification from the keywords manually. It makes the grammar
  resulting much simpler.

* Executing boolean queries : `engine.ml `

  This code traverses queries an interprets them using the fast set
  operations of Patricia trees stored in the index. It does so while
  keeping track of which keywords were ignored for being too frequent
  in the English language to warrant being used as a search key.

  The code which loads the entries' text from their seek offset is
  also here.
  
* Interacting with the user : `main.ml readline.ml
                                   ml-readline.c bibgrep.el `

  `readline.ml` and `ml-readline.c` act as a thin Ocaml-to-C
  wrapper for the true and trusted Unix readline library. Readline is
  used to display the interactive prompt. It provides generous key
  bindings to edit commands, file name completions, and user
  customization via the user's `.inputrc` file.

  `bibgrep.el` is the nifty emacs interface. It is only 64 lines
  of code long!
  
  The meat of the interaction code is in `main.el`. 

  The rather large `executeQuery` glues together the query lexer,
  the query parser, the BibTex parser and the engine, to form the
  hearth of bibgrep. It contains the sorting comparator, with its many
  tie breakers cases. When sorting, executeQuery reparses the BibTex
  entries right after they were read back from the disk, and isolate
  the content of the sort field. Finally, `executeQuery` is also
  responsible for generating the one-line summaries, if they were
  requested.

  The `interactivePrompt` function does just that. It present a
  prompt, calls the readline library, interprets the commands, and
  then loops until the user decide otherwise.

  Finally, the `main` function does all the odds and ends. It parses the
  command line, loads, updates and saves the index (as necessary).
  
  
Coding conventions used
=======

* Containers all have a plural variable name. Non containers all
  have a single variable name.

* `file` indicates a handle, whereas `filename` indicates a string.

* Few abbreviation are used. The most significant of them is `{\tt
        rtn}` (return value). Variable named `rtn` hold a value
        which will eventually be returned by the current function
        (after, maybe, some clean up).

* Lines are indented at 120 characters to make optimal usage of
  that big monitor you have.

* Predicates are pure-functional (they do not assign to any
  `ref` variable, nor do they assign to any `mutable` fields), and
  return a boolean. They are named with the prefix `is` or `has`

* Variables whose purpose is better understood from the context are
  given the same name as their type.

* Variable name with a '2' numeral indicate a hashmap.  The word
  around the '2' describes the domain and the image of the map. A
  hashmap to another hashmap will have multiple `2`'s in its name.
  They associate to the right. For examples :
  `name2relatives2relationship`
  should parses as `Name to (Relatives to
  Relationship)`.  That is, it is many hashmaps from the name of a
  relatives to the relationships, stored as values inside a hashmap
  whose keys are person's names.
  
  
* Variables of type `option` are prefixed with `may`

* Function whose name begins with `may` silently leave invalid
  arguments untouched.

How to make
-------

This code is coded in an ancient version of OCaml. It will
almost certainly *not* compile out-of-the-box.

You will need to fetch and compile the btparse library here :

  http://starship.python.net/~gward/btOOL/

Before compiling btparse, you should apply the patch file
btparse-0.33.diff. It fixes a number of bugs in btparse you
could do without. Once that's done, modify the Makefile's
BTPARSE_DIR variable to point to the location of your btparse
directory.

Then, run 'make' and everything should follow
through. Running 'make' without parameters is equivalent to
running 'make native', which gives you a fast, optimized,
stand alone build of bibgrep. 'make bytecode' on the other
hand, generates a smaller, slower binary which depends on
libbtparse.so. You will have to make sure libbtparse.so so
somewhere where dynamicly linked libraries are looked
for. Fidling with the LD_LIBRARY_PATH environment variable
might be required.


