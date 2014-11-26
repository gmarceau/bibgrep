(require 'easymenu)

;;;###autoload
(defgroup bibgrep nil
  "an indexing and searching of BibTex files"
  :group 'tex)

;;;###autoload
(defcustom bibgrep-program "bibgrep"
  "Path to the bibgrep program"
  :type '(file :must-match t)
  :group 'bibgrep)

;;;###autoload
(defcustom bibgrep-target-files 'ask
  "The names of BibTex files which are searched by default"
  :type '(choice 
          (const :tag "Ask in minibuffer" ask)
          (const :tag "All indexed files" all-indexed-files)
          (repeat file))
  :group 'bibgrep)

;;;###autoload
(defcustom bibgrep-index-file "~/.bibgrep.idx"
  "The name of the index file to use"
  :type '(choice (const :tag "Do not use indexing" nil)
                 file)
  :group 'bibgrep)

(defvar bibgrep-history)

(let ((map
       (easy-menu-create-menu 
        "Bibgrep menu."
        '(["Search" bibgrep]
          "---"
          ["Configure" (customize-group 'bibgrep)]
          ["Help" (describe-function 'bibgrep)]))))
  (define-key global-map [menu-bar tools bibgrep]
    (cons "Bibgrep" map)))






;;;###autoload
(defun bibgrep (target-files query)
"bibgrep will create an index for each BibTex file it touches, and save
the result in \"~/.bibgrep.idx\" (by defaults) to speedup future
queries to the same files. Bibgrep watches the modification date and the
size of the\n original BibTex file and will update (and delete) its index
whenever needed. Bibgrep searches within the BibTex files mentioned on the
command line and only theses files, even when using an index files.


The query language is an extension of the best-known query 
language. It uses the KEY COLON WORD convention used at google.com
(everybody's favorite search engine).

Enter a number of keywords and bibgrep will return entries that
include all the search terms (automatic \"and\" queries). For example,
to search for McGill's effort in robotic soccer, query:

     robocup mcgill

On the command line, you would type :\"# bibgrep 'robocup mcgill' file.bib\"

You can also exclude a word by adding a minus sign (\"-\") in front
of the term to avoid. For example, to search for green trees,
rather than algorithmic ones, query:

     tree -binary -search

To restrict a word to a specific field, write the field name, a colon,
then the search term (all without spaces). For example, to search for
George Bush's contributions to Science Magazine, query:

     author:George author:Bush title:Science

If the field name is ommited, it defaults to the previous field name
explicitly mentioned. The example above can be rewriten as:

     author:George :Bush title:Science

It is also possible to acheive the same effect using Quotes
(althought this will change if searching per sentence is ever
implemented) :

     author:\"George Bush\" title:Science

The \"author\" field can also be searched by first or last name. Search
using field \"firstname:\" and \"lastname:\" respectively. The seldom
used BibTex field \"von:\" and \"jr:\" are also supported.

Disjunctions (\"or\" queries) are also supported. Alternatives should be
separated with a double forward slash (\"//\"), and either \"()\" \"{}\" or
\"[]\" can be used for grouping, as long as they match pairwise. Negation of
disjunctions also works as expected. For example, to find delicious meal
accompagnements, avoiding the usual suspects from France, query:

     (wine // champagne)  -(france // bordeaux)

It is possible use a exclamation mark (\"!\") instead of a dash. This is a
useful for queries which begin with a negation, otherwise they would be
mistaken for a unknown option. Note that some shells (bash) will insist on
having a space between the \"!\" and the following word.

     ! author:Knuth   ! author:Dykstra

Shell-style regular expressions are supported, both as search word and as
field name. \"*\" matches a serie of characters, and \"?\" matches a single
character or nothing. They are most useful to abbreviate field names and
to search for the singular and plurial of a word at once. To match titles
with either \"computer\" or \"computers\" (and some other unlikely things),
query:

     t*:computer?

Search terms are always case insentives."

  (interactive
   (list
    (cond ((eq bibgrep-target-files 'ask)
           (list (read-file-name "Target file: " nil nil t)))
          ((eq bibgrep-target-files 'all-indexed-files)
           '("--all"))
          (t bibgrep-target-files))
    (read-from-minibuffer "Query (bibgrep syntax and regexps): " nil nil nil 'bibgrep-history)))

  (let ((target-files-str "")
        (files target-files))
    (while files
      (setq target-files-str (concat target-files-str (car files)))
      (setq files (cdr files)))

    (let ((command-line 
           (format "%s --summaries %s '%s' %s"
                   bibgrep-program
                   (cond ((not bibgrep-index-file) "--no-index")
                         ((equal bibgrep-index-file "~/.bibgrep.idx") "")
                         (t (concat "--index " bibgrep-index-file)))
                   query
                   target-files-str)))
         
      (compile-internal command-line "No more matches" "bibgrep"))))

(provide 'bibgrep)