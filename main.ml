(*
Copyright (C) 2003 Guillaume Marceau <gmarceau at cs dot brown dot edu>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)
module P = LocalPrintf
module A = Array
module H = LocalHashtbl
open Index
  
(* -- Utilities ------------------------------------ *)

module VP = struct
  let verbose = ref false

(* The VP module : a convolution brought to you
   by ocaml's massive printf-format hack :

   printf-like functions must bear the `printf' name, 
   otherwise the compiler will not type according the
   the format string 
*)
  let printf fmt_etc =
    P.kprintf 
      (fun result -> if !verbose then (prerr_string result; flush stderr))
      fmt_etc
end
  
let getFullText entry =
  match entry with
      { fullText = Some(text) } -> text
    | _ -> assert false

(* handle two-digits dates rather arbitrarly : *)
let mayResolveTwoDigitYear year =
  if year < 30 then year + 2000
  else if year < 100 then year + 1900
  else year

let isAccessible filename permissions =
  try
    Unix.access (Realpath.realpath filename) permissions;
    true
  with Unix.Unix_error(_,_,_) -> false
    
let isFileExists filename = isAccessible filename [ Unix.F_OK ] 
let isFileReadable filename = isAccessible filename [ Unix.R_OK ]
let isFileWritable filename = isAccessible filename [ Unix.W_OK ]

let forgetStaleFiles index = 
  List.iter 
    (fun bibFilename ->
       if Index.isFileIndexOutOfDate index bibFilename then
         (VP.printf "`%s' has changed, clearing the index...\n" bibFilename;
          Index.removeFile index bibFilename))
    (Index.getFiles index)

let doForgetFiles index filenames =
  List.iter
    (fun filename ->
       if not (List.mem filename (Index.getFiles index)) then
         P.printf "remove:`%s' was already not in the index.\n" filename
       else
         (VP.printf "Removing `%s' from the index...\n" filename;
          Index.removeFile index filename))
    filenames

(* Throws Sys_error exceptions *)
let doAddFiles index filenames =
  List.iter
    (fun filename ->
       if not (List.mem filename (Index.getFiles index)) then
         (if not (isFileReadable filename) then
            P.fprintf stderr "Cannot read target file `%s'\n" filename
          else
            (VP.printf "Indexing `%s'...\n" filename;
             Index.addFile index filename)))
    filenames
    
let doSaveIndex index filename =
  if not (Index.isChanged index) then
    VP.printf "Index did not change, no need to save.\n"
  else if (Index.size index) = 0 && isFileExists filename then
    (P.printf "Index is now empty, deleting `%s'...\n" filename;
     Unix.handle_unix_error Unix.unlink filename)
  else
    (VP.printf "Saving index `%s'...\n" filename;
     (* it doesn't make sence to save index for stdin : *)
     if List.mem "-" (Index.getFiles index) then
       Index.removeFile index "-";
     Index.save index filename);
  Index.clearChanged index


(* -- Data and options --------------------------------*)

let versionString = "0.50"
let defaultIndexFilename = "~/.bibgrep.idx"
let searchAll = ref false
let summarized = ref false

(* Double negation variable names are a big no-no. 
   Except when you are trying to keep consistent with 
   the interface's sensible defaults *)
let noSaveIndex = ref false
let noLoadIndex = ref false
let noUpdateStaleFiles = ref false
let forgetFiles = ref false
let listIndexedFiles = ref false
let indexFilename = ref defaultIndexFilename
let gatheringTheRestCommandLine = ref false
let mayQuery = ref None
let targetFilenames = ref []
let maySortField = ref None
let interactive = ref false

let hasOption opt = match opt with
    Some(_) -> true
  | None -> false
      
(* GatherRest implements the "-- query" rest option : *)
let beginGather () =
  (match !mayQuery with
       (* The first filename was optimistically
          taken to be the query. Put it back amoung the
          files, the query is at the end of the options :
       *)
       Some(q) -> targetFilenames := q :: !targetFilenames;
     | None -> ());
  mayQuery := None;
  gatheringTheRestCommandLine := true
  

let gatherRest s = 
  if not !gatheringTheRestCommandLine then beginGather ();
  (match !mayQuery with
       Some(q) -> mayQuery := Some(q ^ " " ^ s)
     | None -> mayQuery := Some(s))

  
let usageDoc =
  ("\n    bibgrep - an indexing and searching tool for BibTex files.\n" ^
   "\n" ^
   "bibgrep searches the named input BibTex files for entries matching\n" ^
   "a given query. Its usage is similar to the command `grep'.\n" ^
   "\n" ^
   "Use --morehelp for the details\n")

let usageSyntaxDoc =
  ((Printf.sprintf "usage: %s [OPTION] ... QUERY [FILE]...\n" (Filename.basename Sys.argv.(0))) ^
   (Printf.sprintf "       %s --interactive [FILE]...\n" (Filename.basename Sys.argv.(0))) ^
   (Printf.sprintf "       %s --forget [FILE]...\n\n" (Filename.basename Sys.argv.(0))))

   
let longUsageDoc =
  (usageDoc ^
   "\n" ^
   "bibgrep will create an index for each BibTex file it touches, and save\n" ^
   "the result in \"~/.bibgrep.idx\" (by defaults) to speedup future\n" ^
   "queries to the same files. Bibgrep watches the modification date and the\n" ^
   "size of the\n original BibTex file and will update (and delete) its index\n" ^
   "whenever needed. Bibgrep searches within the BibTex files mentioned on the\n" ^
   "command line and only theses files, even when using an index files.\n" ^
   "\n" ^
   "\n" ^
   "The query language is an extension of the best-known query \n" ^
   "language. It uses the KEY COLON WORD convention used at google.com\n" ^
   "(everybody's favorite search engine).\n" ^
   "\n" ^
   "Enter a number of keywords and bibgrep will return entries that\n" ^
   "include all the search terms (automatic \"and\" queries). For example,\n" ^
   "to search for McGill's effort in robotic soccer, query:\n" ^
   "\n" ^
   "     robocup mcgill\n" ^
   "\n" ^ 
   "On the command line, you would type :\"# bibgrep 'robocup mcgill' file.bib\"\n" ^
   "\n" ^
   "You can also exclude a word by adding a minus sign (\"-\") in front\n" ^
   "of the term to avoid. For example, to search for green trees,\n" ^
   "rather than algorithmic ones, query:\n" ^
   "\n" ^
   "     tree -binary -search\n" ^
   "\n" ^
   "To restrict a word to a specific field, write the field name, a colon,\n" ^
   "then the search term (all without spaces). For example, to search for\n" ^
   "George Bush's contributions to Science Magazine, query:\n" ^
   "\n" ^
   "     author:George author:Bush title:Science\n" ^
   "\n" ^
   "If the field name is omitted, it defaults to the previous field name\n" ^
   "explicitly mentioned. The example above can be rewritten as:\n" ^
   "\n" ^
   "     author:George :Bush title:Science\n" ^
   "\n" ^
   "It is also possible to achieve the same effect using Quotes\n" ^
   "(although this will change if searching per sentence is ever\n" ^
   "implemented) :\n" ^
   "\n" ^
   "     author:\"George Bush\" title:Science\n" ^
   "\n" ^
   "The \"author\" field can also be searched by first or last name. Search\n" ^
   "using field \"firstname:\" and \"lastname:\" respectively.\n" ^
   "\n" ^
   "Disjunctions (\"or\" queries) are also supported. Alternatives should be\n" ^
   "separated with a double forward slash (\"//\"), and either \"()\" \"{}\" or\n" ^
   "\"[]\" can be used for grouping, as long as they match pairwise. Negation of\n" ^
   "disjunctions also works as expected. For example, to find delicious meal\n" ^
   "accompagnements, avoiding the usual suspects from France, query:\n" ^
   "\n" ^
   "     (wine // champagne)  -(france // bordeaux)\n" ^
   "\n" ^
   "It is possible use a exclamation mark (\"!\") instead of a dash. This is a\n" ^
   "useful for queries which begin with a negation, otherwise they would be\n" ^
   "mistaken for a unknown option. Note that some shells (bash) will insist on\n" ^
   "having a space between the \"!\" and the following word.\n" ^
   "\n" ^
   "     ! author:Knuth   ! author:Dykstra\n" ^
   "\n" ^
   "Shell-style regular expressions are supported, both as search word and as\n" ^
   "field name. \"*\" matches a sequence of characters, and \"?\" matches a single\n" ^
   "character or nothing. They are most useful to abbreviate field names and\n" ^
   "to search for the singular and plural of a word at once. To match titles\n" ^
   "with either \"computer\" or \"computers\" (and some other unlikely things),\n" ^
   "query:\n" ^
   "\n" ^
   "     t*:computer?\n" ^
   "\n" ^
   "Search terms are always case insensitive.\n" ^
   "\n" ^
   "\n" ^
   "The list of available options follow :\n")

let interactiveUsageDoc = 
  ("Commands are :\n" ^
   "  find <query>      searches for BibTex entries. Type \"morehelp\"\n" ^
   "  add <file> ...     add BibTex file(s) to the list of files to search\n" ^
   "  remove <file> ...  remove BibTex file(s) from the list of files to search\n" ^
   "  targets            list the file targeted for querying\n" ^
   "\n" ^
   "  index <file> ...   add file(s) to the index\n" ^
   "  forget <file> ...  remove file(s) from the index\n" ^
   "  list               list the indexed files\n" ^
   "  savenow            save the index to disk now\n" ^
   "\n" ^
   "  exit               leave bibgrep\n" ^
   "  quit               leave bibgrep\n" ^
   "\n" ^
   "The following commands changes options :\n" ^
   "  sort [ off | <field name> ]   sets the sort key\n" ^
   "  summaries [ on | off ]        toggle one-line summaries\n" ^
   "  update [ on | off ]           turns off automatic synchronization of the index\n" ^
   "  save [ on | off ]             disable saving the index when quiting\n" ^
   "  verbose [ on | off ]          become chatty about the search process (on stderr)\n" ^
   "\n" ^
   "Typing the name of an option without the argument shows its\n" ^
   "current status\n" ^
   "\n" ^
   "\n" ^
   "  help               prints this message\n" ^
   "  morehelp           the whole scoop on bibgrep's purpose and the query syntax\n" ^
   "\n")

let interactiveCommands = 
  [| "find"; "add"; "remove"; "targets"; "index"; "forget";
     "list"; "savenow"; "help"; "morehelp"; "exit"; "quit";
     "sort"; "summaries"; "update"; "save"; "verbose"; "cd"; "pwd" |]


let rec argumentList =
  [("--all", Arg.Set searchAll, "\twork with all the already indexed BibTex files");
   ("--index", Arg.String (fun s -> indexFilename := s), "\tspecify an index file to use instead of the defaults (~/.bibgrep.idx)");
   ("--interactive", Arg.Set interactive, "\tpresent a prompt for interactive querying");
   ("--summaries", Arg.Set summarized, "\tprint one line summaries, in a format compatible with emacs' \"M-x compile\" command");

   ("--sort", Arg.String (fun s -> maySortField := Some(s)), "\tset the sorting field");
   ("--list", Arg.Set listIndexedFiles, "\tlist the files indexed in the index file");
   ("--forget", Arg.Set forgetFiles, "\tremove the files given on the command line from the index");
   ("--no-save", Arg.Set noSaveIndex, "\tdo not save the modifications done to the index");
   ("--no-load", Arg.Set noLoadIndex, "\tcreate an index anew, potentialy clobbering the existing file");
   ("--no-update", Arg.Set noUpdateStaleFiles, "\tdo not update the index, even if some BibTex files have changed");
   ("--verbose", Arg.Set VP.verbose, "\tcomment the progress of the search (on stderr)");
   ("--", Arg.Rest gatherRest, "     \tuse the rest of the command line as a query (rather than at its usual place)");
   
   ("--no-index", Arg.Unit (fun () -> noSaveIndex := true; noLoadIndex := true), "\tshort for `--no-load --no-save'");
   ("-i", Arg.Set interactive, "\tshort for --interactive");
   ("-sm", Arg.Set summarized, "\tshort for --summaries");
   ("-st", Arg.String (fun s -> maySortField := Some(s)), "\tshort for --sort");
   ("-l", Arg.Set listIndexedFiles, "\tshort for --list");
   ("-r", Arg.Set forgetFiles, "\tshort for --forget");
   ("-v", Arg.Set VP.verbose, "\tshort for --verbose");
   ("--morehelp", Arg.Unit (fun () -> Arg.usage argumentList longUsageDoc), "\tprints the whole scoop on the query syntax.");
  ]
  


(* -- Function which uses the state of the options ------------- *)

    
(* Throws Sys_error exceptions *)
let executeQuery index filenames queryString maySortField summarized =
  (* The lexer cannot fail since it has a catch-all junk entry ... *)
  let lexbuf = Lexing.from_string queryString in

  try
    (* ... but the parser certainly can fail : *)
    let query = QueryPar.main QueryLex.token lexbuf in
    if !VP.verbose then
      (P.printf "Query parsed as :\n";
       Query.printQuery query;
       print_newline ());
    
    let (hits, ignoredWords) = Engine.executeQuery index filenames query in

    (* Got the results, now display them : *)
    let hitCnt = List.length hits in
    if hitCnt > 0 then
      (VP.printf "Loading the text of the entries...\n";
       Engine.loadTexts hits);
    
    if hits = [] then
      () (* noting to do *)
    else if not summarized && not (hasOption maySortField) then
      (* Printing entries verbatim is easy : *)
      (VP.printf "BibTex entries follow...\n";
       List.iter 
         (fun e -> P.printf "%s\n" (getFullText e))
         hits)
    else
      ((* Sorting and summarizing require some parsing first : *)
        VP.printf "Reparsing entries...\n";
        let entryId2summary = H.create hitCnt in
        let entryId2sortKey = H.create hitCnt in
        List.iter
          (fun e ->
             (let text = getFullText e in
              try
                (match Btparse.parse_string_entry text with
                     (Some(ast), _) ->
                       let fields = Btparse.get_fields ast in
                       
                       if summarized then
                         (let titleText = 
                            (try snd (List.find
                                        (fun (fieldName, _) -> fieldName = "title")
                                        fields)
                             with Not_found -> "")
                          in
                          let authorText =
                            (try snd (List.find
                                        (fun (fieldName, _) -> fieldName = "author")
                                        fields)
                             with Not_found -> "")
                          in
                          let summary = 
                            (Btparse.purify_string titleText)
                            ^ 
                            (if authorText = "" then "" else " -- by " ^ (Btparse.purify_string authorText))
                          in
                          H.replace entryId2summary e.entryId summary);
                       
                       (match maySortField with
                            Some(sortField) ->
                              let sortText =
                                (try snd (List.find
                                            (fun (fieldName, _) -> fieldName = sortField)
                                            fields)
                                 with Not_found -> "")
                              in
                              let purifiedLowerCaseSortText = Btparse.purify_string (String.lowercase sortText) in
                              if sortText <> "" then
                                H.replace entryId2sortKey e.entryId purifiedLowerCaseSortText
                          | None -> ())
                   | _ -> raise End_of_file)
                
              with End_of_file -> 
                P.fprintf stderr "Error parsing entry in file `%s', at line %i.%s\n"
                e.filename
                e.line
                (if !noUpdateStaleFiles then " Index may be out of date\n" else ""));
             
             Btparse.parse_string_done ())
        
          hits;
        
        (* Parsing is annoying, but now things the can go wrong are a thing of the past 
           It is not time to sort :
        *)
        let sortedHits = 
          if not (hasOption maySortField) && summarized then
            (List.sort 
               (fun { entryId = id1 } { entryId = id2 } ->
                  let e1 = Index.getEntry index id1 in
                  let e2 = Index.getEntry index id2 in
                  if e1.filename <> e2.filename then
                    compare e1.filename e2.filename
                  else compare e1.line e2.line)
               hits)
          else
          (match maySortField with
               None -> hits
             | Some(sortField) ->
                 VP.printf "Sorting...\n";
                 let sortByYear = (sortField = "year") in
                 List.sort
                   (fun { entryId = id1 } { entryId = id2 } ->

                      let mem1 = H.mem entryId2sortKey id1 in
                      let mem2 = H.mem entryId2sortKey id2 in
                      (* Missing sort key should cluster together at the bottom : *)
                      if not (mem1 && mem2) then 
                        compare mem1 mem2
                      else
                        ((* Normal entries sort according to the sort text : *)
                          let text1 = H.find entryId2sortKey id1 in
                          let text2 = H.find entryId2sortKey id2 in
                          if sortByYear then
                            (* Sorting by year require an extra string->int conversion *)
                            let number1 = 
                              mayResolveTwoDigitYear
                                (try int_of_string text1
                                 with Failure(_) -> 0 (* bottom clustering again *))
                            in
                            let number2 = 
                              mayResolveTwoDigitYear
                                (try int_of_string text2
                                 with Failure(_) -> 0 (* and again *))
                            in
                            if number1 = number2 then 
                              (* Try to break ties with the id number *)
                              compare id1 id2
                            else compare number1 number2
                          else
                            (* This is the standard case *)
                            if text1 = text2 then 
                              (* Try to break ties with the id number *)
                              compare id1 id2
                            else compare text1 text2))

                   hits)
        in
        (* Finaly, we get to the printing : *)
        VP.printf "BibTex entries follow...\n";
        (if summarized then
           (* Summarized printing *)
           List.iter
             (fun entry -> 
                if H.mem entryId2summary entry.entryId then
                  P.printf "%s:%i: %s\n" entry.filename entry.line 
                    (H.find entryId2summary entry.entryId)
                else P.printf "%s:%i: <no title>\n" entry.filename entry.line)
             sortedHits
         else
           (* Block quote printing *)
           List.iter
             (fun entry -> P.printf "%s\n" (getFullText entry))
             sortedHits));

    (* Print a convinient result summary : *)
    if ignoredWords <> [] then
      (let ignoredCnt = List.length ignoredWords in
       P.fprintf stderr 
         "The following word%s %s very common and %s not included in your search: " (* ah... google *)
         (if ignoredCnt > 1 then "s" else "")
         (if ignoredCnt > 1 then "are" else "is")
         (if ignoredCnt > 1 then "were" else "was");
       List.iter
         (fun w -> P.fprintf stderr "%s " w)
         ignoredWords;
       P.fprintf stderr "\n");
    
      
    if hitCnt = 0 then P.fprintf stderr "No hits.\n"
    else Printf.fprintf stderr "Found %i hit%s.\n" hitCnt (if hitCnt > 1 then "s" else "");
      

  with Parsing.Parse_error -> 

    (* The query is malformed. Let try to give some indication
       why this could be the case : *)
    let tokens = QueryLex.tokenization queryString in
    if (List.exists 
          (fun tok -> match tok with QueryPar.JUNK(_) -> true | _ -> false)
          tokens)
    then
      P.fprintf stderr 
        "Junk characters in query string: %s\n"
        (QueryLex.tokenizationToString tokens)
    else
      (P.fprintf stderr "Malformed query: %s\n" queryString;
       P.fprintf stderr "         %shere --^\n" (String.make !Query.errorPosition ' '))
        

let rec interactivePrompt index indexFilename filenames maySortField summarized noUpdateStaleFiles noSaveIndex =

  let loop () = interactivePrompt
                  index indexFilename
                  filenames maySortField
                  summarized noUpdateStaleFiles noSaveIndex
  in

  let optionError opt =
    P.printf "`%s' need only one argument, either \"on\" or \"off\"\n" opt;
  in

  (try
     let line = Readline.readline_with_command_completion Sys.argv.(0) "bibgrep > " interactiveCommands in
     let tokens = Str.split (Str.regexp "[ \t\n]+") line in

     (match tokens with 
          [] -> loop ()

            (* ---- Help commands : *)
        | "help" :: "find" :: tl ->
            P.printf "%s\n" longUsageDoc;
            loop ()
        | "help" :: tl ->
            P.printf "%s\n" interactiveUsageDoc; 
            loop ()
        | "morehelp" :: tl ->
            P.printf "%s\n" longUsageDoc;
            loop ()
            (* ---- Searching commands : *)
        | [ "cd" ] ->
            Sys.chdir (Sys.getenv "HOME");
            loop ()
        | "cd" :: destination :: tl ->
            (try Sys.chdir destination 
             with Sys_error(msg) ->
               P.fprintf stderr "cd: %s\n" msg);
            loop ()
        | "pwd" :: tl ->
            P.fprintf stderr "%s\n" (Sys.getcwd ());
            loop ()
        | [ "find" ] ->
            P.printf "Enter a query string as well (or type \"help\" or \"morehelp\").\n";
        | "find" :: tl ->
            if filenames = [] then
              P.printf "There are no target files to search in. Use \"add <file> ...\" to select some\n"
            else
              ((* Automatic refreshing of the index, if needed,
                  right before lauching a query :
               *)
                try
                  if not noUpdateStaleFiles then
                    forgetStaleFiles index;

                  doAddFiles index filenames;
                  if not noSaveIndex then
                    doSaveIndex index indexFilename;

                  let queryString = List.fold_left (fun acc s -> acc ^ " " ^ s) "" (List.tl tokens) in
                  executeQuery index filenames queryString maySortField summarized
                with Sys_error(msg) ->
                  P.fprintf stderr "%s\n" msg);
            loop ();
        | "add" :: tl ->
            (try
               doAddFiles index tl;
               if not noSaveIndex then
                 doSaveIndex index indexFilename;
               let newFilenames = List.filter (fun f -> not (List.mem f filenames)) tl in
               interactivePrompt index indexFilename (filenames @ newFilenames) maySortField summarized noUpdateStaleFiles noSaveIndex
             with Sys_error(msg) -> 
               P.fprintf stderr "%s\n" msg;
               loop ())
        | "remove" :: tl ->
            doForgetFiles index tl;
            let filenamesLeft = List.filter (fun f -> not (List.mem f tl)) filenames in
            interactivePrompt index indexFilename filenamesLeft maySortField summarized noUpdateStaleFiles noSaveIndex
        | "targets" :: tl ->
            if tl <> [] then P.fprintf stderr "(ignoring junk after the command).\n";
            List.iter (fun f -> P.printf "%s\n" f) filenames;
            loop ()


            (* ---- Index commands : *)
        | "index" :: tl -> 
            (try doAddFiles index tl;
               if not noSaveIndex then
                 doSaveIndex index indexFilename;
             with Sys_error(msg) -> P.fprintf stderr "%s\n" msg);
            loop ();
        | "forget" :: tl ->
            doForgetFiles index tl;
            loop ()
        | "list" :: tl ->
            if tl <> [] then P.fprintf stderr "(ignoring junk after the command).\n";
            List.iter (fun f -> P.printf "%s\n" f) (Index.getFiles index);
            loop ()
        | "savenow" :: tl ->
            doSaveIndex index indexFilename;
            P.printf "Index saved\n";
            loop ()


            (* ---- Option commands : *)
        | [ "sort" ] ->
            (match maySortField with
                 Some(sortField) -> P.printf "Sorting by : %s\n" sortField
               | None -> P.printf "Sorting is : off\n");
            loop ()
        | [ "sort"; "off" ] ->
            P.printf "Sorting disabled.\n";
            interactivePrompt index indexFilename filenames None summarized noUpdateStaleFiles noSaveIndex
        | [ "sort"; fieldname ] ->
            P.printf "Sort field is now `%s'.\n" fieldname;
            interactivePrompt index indexFilename filenames (Some(fieldname)) summarized noUpdateStaleFiles noSaveIndex
        | "sort" :: tl -> 
            P.printf "Sort needs only one argument, either a field name or \"off\"\n";
            loop ()
        | [ "summaries" ] ->
            P.printf "Summaries are : %s\n" (if summarized then "on" else "off");
            loop ()
        | [ "summaries"; "on" ] ->
            P.printf "Summaries enabled.\n";
            interactivePrompt index indexFilename filenames maySortField true noUpdateStaleFiles noSaveIndex
        | [ "summaries"; "off" ] ->
            P.printf "Summaries disabled.\n";
            interactivePrompt index indexFilename filenames maySortField false noUpdateStaleFiles noSaveIndex
        | "summaries" :: tl -> 
            optionError (List.hd tokens);
            loop ()
        | [ "update" ] ->
            P.printf "Automatic index updating is : %s\n" (if noUpdateStaleFiles then "off" else "on");
            loop ()
        | [ "update"; "on" ] ->
            P.printf "Updating automaticaly. Your index will stay up to date.\n";
            interactivePrompt index indexFilename filenames maySortField summarized false noSaveIndex
        | [ "update"; "off" ] ->
            P.printf "Automatic update disabled (why?).\n";
            interactivePrompt index indexFilename filenames maySortField summarized true noSaveIndex
        | "update" :: tl -> 
            optionError (List.hd tokens);
            loop ()
        | [ "save" ] ->
            P.printf "Saving of the index is : %s\n" (if noSaveIndex then "on" else "off");
            loop ()
        | [ "save"; "on" ] ->
            P.printf "Will save the index when exiting.\n";
            interactivePrompt index indexFilename filenames maySortField summarized noUpdateStaleFiles false
        | [ "save"; "off" ] ->
            P.printf "Will *not* save the index when exiting.\n";
            interactivePrompt index indexFilename filenames maySortField summarized noUpdateStaleFiles true
        | "save" :: tl -> 
            optionError (List.hd tokens);
            loop ()
        | [ "verbose" ] ->
            P.printf "Verbosity is : %s\n" (if !VP.verbose then "on" else "off");
            loop ()
        | [ "verbose"; "on" ] ->
            P.printf "Verbosity set to: great details\n";
            VP.verbose := true;
            loop ()
        | [ "verbose"; "off" ] ->
            P.printf "now silent.\n";
            VP.verbose := false;
            loop ()
        | "verbose" :: tl -> 
            optionError (List.hd tokens);
            loop ()

        | "show" :: "gpl" :: tl ->
            (try
               let f = open_in (Filename.concat (Filename.dirname Sys.argv.(0)) "gpl.txt") in
               let rec loop () =
                 P.printf "%s\n" (input_line f);
                 loop ()
               in loop ()
             with End_of_file -> ()
               | Sys_error _ -> P.printf "%s"
                   ("Could not open the file 'gpl.txt'. You can read a copy of the gpl as it applies" ^
                    "to this program at http://www.gnu.org/licenses/gpl.txt\n"));
            loop ()
                   
        (* ---- Quit commands : *)
        | "exit" :: tl -> ()
        | "quit" :: tl -> ()
        | _ ->
            P.printf "Command not reconized. Try \"help\"\n";
            loop ())
   with Failure("readline eof") ->
     (* Readline did not find any line to read, exit *)
     P.printf "\n");
  
  if not noSaveIndex then
    doSaveIndex index indexFilename
      

let main () =
  Btparse.initialize ();
  
  
  if A.length Sys.argv = 1 then
    (P.printf "%sTry `%s --help' for more information.\n" usageSyntaxDoc (Filename.basename Sys.argv.(0));
     exit(1));
  
  (* Parse the command line : *)
  Arg.parse
    argumentList
    (fun s ->
       match !mayQuery with
           Some(_) -> targetFilenames := s :: !targetFilenames
         | None -> mayQuery := Some(s))
    usageDoc;

  (* Arg.parse does not handle an empty trailling argument.
     We need to make up for it : *)
  if (A.length Sys.argv > 1 
      && Sys.argv.((A.length Sys.argv) - 1) = "--")
  then
    beginGather ();

  (* put the list back into its original order *)
  targetFilenames := List.rev !targetFilenames;

  (* --forgetFile and --interactive do not take a query, tag it with the filenames *)
  (match (!forgetFiles || !interactive), !mayQuery with
       true, Some(q) -> 
         targetFilenames := q :: !targetFilenames;
         mayQuery := None
     | _ -> ());

  (*
    It is rather important that we deal with canonical paths
    as much as possible. These paths are stored inside 
    the index and are being used to identify BibTex files
    between runs.

    Expanding also deals with the tilde character, which
    ocaml otherwise does not handle.
  *)

  indexFilename := Realpath.realpath !indexFilename;
  targetFilenames := List.map Realpath.realpath !targetFilenames;


  (* Show where we stand *)
  if !VP.verbose then
    (P.printf "Index file is `%s'\n" !indexFilename;
     if !targetFilenames = [] then
       P.printf "(No target files given)\n"
     else
       ((P.printf "Target file%s %s:\n" 
           (if (List.length !targetFilenames) > 1 then "s" else "")
           (if (List.length !targetFilenames) > 1 then "are" else "is"));
        List.iter
          (fun targetFilename -> P.printf "%s\n" targetFilename)
          !targetFilenames;
        P.printf "\n");
     if !searchAll then
       P.printf "plus all the already indexed BibTex files\n");
  

  (* Check for erronous combinaison of flags, and file system errors : *)

  let ok = ref false in

  if !noLoadIndex && !forgetFiles then
    P.printf "Cannot --forgetFiles from an index if we are not loading one.\n"

  else if !listIndexedFiles && !noLoadIndex then
    P.printf "Cannot --list the files in an index if we are not loading one.\n"

  else if !gatheringTheRestCommandLine && !forgetFiles then
    P.printf "Removing files from an index does not need a query string.\n"

  else if (((not !noSaveIndex) || (not !noLoadIndex))
           && not (isFileExists (Filename.dirname !indexFilename)))
  then
    P.printf "Directory `%s' of the index does not exists.\n" (Filename.dirname !indexFilename)

  else if (not !noLoadIndex  (* We are using an index ... *)
           && (isFileExists !indexFilename) (* and the file exists ... *)
           && not (isFileReadable !indexFilename)) (* but we cannot read it *)
  then
    P.printf "Permission denied, cannot read from the index `%s'.\n" !indexFilename

  else if (not !noSaveIndex  (* We are saving an index ... *)
           && (isFileExists !indexFilename) (* and the file exists ... *)
           && not (isFileWritable !indexFilename)) (* but we cannot write it *)
  then
    P.printf "Permission denied, cannot write the index `%s'.\n" !indexFilename

  else if !targetFilenames = [] && not !interactive && not !searchAll && not !listIndexedFiles  then
    (if !forgetFiles then P.printf "Please specify the BibTex file(s) to unindex. Use --help for details.\n"
     else P.printf "Please specify the BibTex file(s) to search. Use --help for details.\n")

  else if not (hasOption !mayQuery) && not !interactive && not !forgetFiles && not !listIndexedFiles then
    P.printf "Please give a query to search for. Use --help for details.\n"

  else ok := true;

  if not !ok then exit(1);

    

  (* Load or create the index *)
  let index = 

    try 
      if !noLoadIndex || not (isFileExists !indexFilename) then 
        Index.create ()
      else 
        (VP.printf "Loading index file `%s'...\n" !indexFilename;
         Index.load !indexFilename true)
    with Index.Bad_file(msg) ->
      (P.fprintf stderr "%s\n" msg;
       P.fprintf stderr "Running without an index file\n";
       noSaveIndex := false;
       Index.create ())
  in

  (* Apply the --all flag : *)
  if !searchAll then
    List.iter
      (fun f -> 
         if not (List.mem f !targetFilenames) then
           targetFilenames := f :: !targetFilenames)
      (Index.getFiles index);
  
  (* Apply the --list flag : *)

  if !listIndexedFiles || !VP.verbose then
    (if (Index.getFiles index) = [] then
       P.printf "listing: index `%s' is empty.\n" !indexFilename
     else
       (P.printf "listing:`%s' currently contains indexes for the following BibTex files :\n" !indexFilename;
        List.iter
          (fun filename -> P.printf "%s\n" filename)
          (Index.getFiles index);
        P.printf "\n"));

  (* Forget stale files : *)
  (if not !noUpdateStaleFiles then 
     forgetStaleFiles index;
   
   if !forgetFiles then
     (* Removing files is a special case, there is no query : *)
     (doForgetFiles index !targetFilenames;

      if not !noSaveIndex then
        doSaveIndex index !indexFilename)
   else if !targetFilenames <> [] then
     (* Add new files *)
     (try
        (doAddFiles index !targetFilenames;
         
         (* All changes done, time to save for the future : *)
         if not !noSaveIndex then
           doSaveIndex index !indexFilename;
         
         (* Ready to lift off : *)
         (match !mayQuery with
              Some(q) -> 
                VP.printf "Now searching...\n";
                executeQuery index !targetFilenames q !maySortField !summarized
            | _ -> ()))
      with Sys_error(msg) -> P.fprintf stderr "%s\n" msg));
  

  if !interactive then
    (P.printf "%s" 
       ("Bibgrep version " ^ versionString ^", Copyright (C) 2003 Guillaume Marceau\n" ^
        "Bibgrep comes with ABSOLUTELY NO WARRANTY; for details\n" ^
        "type `show gpl'.  This is free software, and you are welcome\n" ^
        "to redistribute it under certain conditions\n");
     P.printf "Bibgrep interactive prompt. Type \"help\" for commands.\n";

     interactivePrompt 
       index !indexFilename !targetFilenames 
       !maySortField !summarized !noUpdateStaleFiles !noSaveIndex);
  
  Btparse.cleanup ();
  VP.printf "Done\n"

;;

main ()
