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
module H = LocalHashtbl
module A = Array
open Unix 
             
exception Bad_file of string
  
let regexpChars = "][.*^$+?|"
let tokenizerRegex = Str.regexp_case_fold "[a-z0-9]+"
let tokenizerRegexForRegexps = Str.regexp_case_fold ("[" ^ regexpChars ^ "a-z0-9]+")
                                 
(* Words not to index (snatched from biblook.h) *)
let badWords =
  (let ws = 
     [ "a"; "also"; "among"; "an"; "and"; "are"; "as"; "at"; "by"; 
       "for"; "from"; "have"; "in"; "into"; "is"; 
       "of"; "on"; "or"; "over"; "so"; "than"; "the";
       "to"; "under"; "with"; ]
   in
   let hash = H.create (List.length ws) in
   List.iter 
     (fun w -> H.replace hash w true)
     ws;
   hash)
  
type coordinates =
    {
      filename : string;
      line : int;
      seekOffset : int;
      entryId : int;
      length : int;
      mutable fullText : string option
    }
    
type srcStatistics = 
    {
      firstId : int;
      entryCnt : int;
      byteCnt : int;
      modificationTime : float;
    }
    
type hitsOrOffset = 
    Hits of Ptset.t
  | Offset of int
      
type indexHashtables = (string, (string, (string, hitsOrOffset) H.t) H.t) H.t
    
type t =
    {
      mutable mayFile : in_channel option;
      mutable totalEntryCnt : int;
      mutable firstFreeEntryId : int;
      mutable changed : bool;
      
      filename2field2word2hitsOrOffset : indexHashtables;
      entryId2coordinates : (int, coordinates) H.t;
      filename2statistics : (string, srcStatistics) H.t
    }
    
    
(* Simple utility function which will load the hits from
   disk and cache them for future use *)
let gadOrLoadHits index word2hitsOrOffset word =
  let hitsOrOffset = H.gad word2hitsOrOffset word (fun () -> (Hits(Ptset.empty))) in
  (match hitsOrOffset with
       Hits(ptset) -> ptset
     | Offset(offset) ->
         (match index.mayFile with
              Some(file) -> 
                seek_in file offset;
                let ptset : Ptset.t = Marshal.from_channel file in
                H.replace word2hitsOrOffset word (Hits(ptset));
                ptset
            | None -> assert false))
  
  
  
let indexFormatVersionNumber = 1.1
let magicNumber = int_of_string "0xbebc" (* pronouced "bib - bec", which is close 
                                            enough from BibTex Index in my book *)
let create () = 
  {
    mayFile = None;
    totalEntryCnt = 0;
    firstFreeEntryId = 0;
    changed = false;
    
    filename2field2word2hitsOrOffset = H.create 10;
    entryId2coordinates = H.create 10;
    filename2statistics = H.create 10;
  }
  
let isBadWord word =
  H.mem badWords word 
    
let doTokenize str regexp = 
  let lStr = String.lowercase str in
  let result = ref [] in
  (try
     let i = ref 0 in
     while true do
       i := Str.search_forward regexp lStr !i;
       let token = Str.matched_string lStr in
       result := token :: !result;
       i := !i + (String.length token)
     done
   with Not_found -> ());
  !result
    
    
let tokenize str = doTokenize str tokenizerRegex
let tokenizeThisRegexp regexp = doTokenize regexp tokenizerRegexForRegexps
                                  
                                  
                                  
let getNegation index filenames indesirables =
  List.fold_left
    (fun acc filename ->
       let statistics = H.find index.filename2statistics filename in
       let tmpAcc = ref acc in
       for i = statistics.firstId to statistics.firstId + statistics.entryCnt - 1 do
         if not (Ptset.mem i indesirables) then
           tmpAcc := Ptset.add i !tmpAcc
       done;
       !tmpAcc
    )
    Ptset.empty
    filenames
    
let getEntry index id = H.find index.entryId2coordinates id
                          
                          
let regexpRegexp = Str.regexp ".*[][.*+?|].*" (* a regular expression which matches regular expressions *)
                     
let isRegexp regexpStr = Str.string_match regexpRegexp regexpStr 0

(* Turn a shell-style regular expression into a ml-style regular expression,
   then matches it againts the keys in a hash table *)

let expandRegexp stringHash regexpStr = 
  if not (isRegexp regexpStr) then
    (if H.mem stringHash regexpStr then [ regexpStr ] 
     else [])
  else
    let starExpanded = Str.global_replace (Str.regexp "*") ".*" regexpStr in
    let questionMarkExpended = Str.global_replace (Str.regexp "?") ".?" starExpanded in
    
    let regexp = Str.regexp_case_fold questionMarkExpended in 
    H.fold
      (fun key _ acc ->
         if Str.string_match regexp key 0 then key :: acc
         else acc)
      stringHash
      []
      
let getAllHits index wordRegexp =
  let wordRegexp = String.lowercase wordRegexp in
  (H.fold
     (fun filename field2word2hitsOrOffset acc ->
        (H.fold
           (fun field word2hitsOrOffset acc ->
              List.fold_left
                (fun acc word -> 
                   Ptset.union (gadOrLoadHits index word2hitsOrOffset word) acc)
                acc
                (expandRegexp word2hitsOrOffset wordRegexp))
           field2word2hitsOrOffset
           acc))
     index.filename2field2word2hitsOrOffset
     Ptset.empty)
    
let getFieldHits index fieldRegexp wordRegexp =
  let fieldRegexp = String.lowercase fieldRegexp in
  let wordRegexp = String.lowercase wordRegexp in
  H.fold 
    (fun filename field2word2hitsOrOffset acc ->
       List.fold_left
       (fun acc field ->
          let word2hitsOrOffset = H.find field2word2hitsOrOffset field in
          List.fold_left
            (fun acc word -> Ptset.union (gadOrLoadHits index word2hitsOrOffset word) acc)
            acc
            (expandRegexp word2hitsOrOffset wordRegexp))
       acc
       (expandRegexp field2word2hitsOrOffset fieldRegexp))
    index.filename2field2word2hitsOrOffset
    Ptset.empty
    
let getFileFieldHits index filename fieldRegexp wordRegexp : Ptset.t =
  let fieldRegexp = String.lowercase fieldRegexp in
  let wordRegexp = String.lowercase wordRegexp in
  if H.mem index.filename2field2word2hitsOrOffset filename then
    (let field2word2hitsOrOffset = H.find index.filename2field2word2hitsOrOffset filename in
     List.fold_left
       (fun acc field -> 
          let word2hitsOrOffset = H.find field2word2hitsOrOffset field in
          List.fold_left
            (fun acc word -> Ptset.union (gadOrLoadHits index word2hitsOrOffset word) acc)
            acc
            (expandRegexp word2hitsOrOffset wordRegexp))
       Ptset.empty
       (expandRegexp field2word2hitsOrOffset fieldRegexp))
  else Ptset.empty
    
let getFileHits index filename wordRegexp =
  let wordRegexp = String.lowercase wordRegexp in 
  if H.mem index.filename2field2word2hitsOrOffset filename then
    let field2word2hitsOrOffset = H.find index.filename2field2word2hitsOrOffset filename in
    H.fold
      (fun field word2hitsOrOffset acc -> 
         List.fold_left
         (fun acc word -> Ptset.union (gadOrLoadHits index word2hitsOrOffset word) acc)
         acc
         (expandRegexp word2hitsOrOffset wordRegexp))
      field2word2hitsOrOffset
      Ptset.empty
  else Ptset.empty
    
let getFilesFieldHits index filenames fieldRegexp wordRegexp =
  List.fold_left
    (fun acc filename -> Ptset.union acc (getFileFieldHits index filename fieldRegexp wordRegexp))
    Ptset.empty
    filenames
    
let getFilesHits index filenames wordRegexp =
  List.fold_left
    (fun acc filename -> Ptset.union acc (getFileHits index filename wordRegexp))
    Ptset.empty
    filenames
    
    
let addHit index lowcaseField lowcaseWord coordinates =
  if not (isBadWord lowcaseWord) then
    (let field2word2hitsOrOffset = H.gad index.filename2field2word2hitsOrOffset coordinates.filename  (fun () -> H.create 1) in
     let word2hitsOrOffset = H.gad field2word2hitsOrOffset lowcaseField (fun () -> H.create 1) in
     let hits = gadOrLoadHits index word2hitsOrOffset lowcaseWord in
     H.replace word2hitsOrOffset lowcaseWord (Hits (Ptset.add coordinates.entryId hits)))
    
let addText index coordinates field text =
  let field = String.lowercase field in
  List.iter
    (fun token -> 
       addHit index field token coordinates)
    (tokenize (Btparse.purify_string text))
    (* (tokenize text) *)
    
let addAuthorField index coordinates file text : unit = 
  let parts = Btparse.split_list file text "and" "author" in
  for i = 0 to (A.length parts) - 1 do
    let (first, von, last, jr) = Btparse.split_name file parts.(i) i in
    
    A.iter
      (fun i -> addText index coordinates "firstname" i; addText index coordinates "author" i)
      first;
    
    A.iter
      (fun i -> addText index coordinates "lastname" i; addText index coordinates "author" i)
      last;
    
    
    A.iter (fun i -> addText index coordinates "author" i) von;
    A.iter (fun i -> addText index coordinates "author" i) jr;
  done
  
  
let addEntry index file filename entryAst startLine startSeek = 
  
  let coordinates = { filename = filename;
                      line = startLine;
                      seekOffset = startSeek;
                      entryId = index.firstFreeEntryId;
                      fullText = None;
                      length = (Btparse.current_entry_seek file) - startSeek
                    }
  in
  
  
  (* Read the field *)
  let fields = Btparse.get_fields entryAst in
  List.iter
    (fun (fieldName, text) -> 
       if fieldName = "author" then addAuthorField index coordinates file text
       else addText index coordinates fieldName text)
    fields;
  
  H.replace index.entryId2coordinates coordinates.entryId coordinates;
  index.totalEntryCnt <- index.totalEntryCnt + 1;
  index.firstFreeEntryId <- index.firstFreeEntryId + 1
    
    
(* throws Sys_error is the file is not found *)
let addFile index filename =
  Btparse.initialize ();
  let file = Btparse.open_file filename in
  let prevSize = index.totalEntryCnt in
  let firstId = index.firstFreeEntryId  in
  
  (* Throw away previous index (if any) *)
  if H.mem index.filename2field2word2hitsOrOffset filename then
    H.remove index.filename2field2word2hitsOrOffset filename;
  
  (try 
     while true do
       let line = Btparse.current_entry_line file in
       let seek = Btparse.current_entry_seek file in
       (match Btparse.parse_entry file with
            (Some(entryAst), _) ->
              addEntry index file filename entryAst line seek;
              Btparse.free_ast entryAst
          | _ -> ());
     done
   with End_of_file -> ());
  
  (* Btparse.close_file file; *)
  
  H.replace index.filename2statistics filename
    {
      firstId = firstId;
      entryCnt = index.totalEntryCnt - prevSize;
      byteCnt = (stat filename).st_size;
      modificationTime = (stat filename).st_mtime
    };
  index.changed <- true
    
    
let removeFile index filename = 
  index.totalEntryCnt <- index.totalEntryCnt - (H.find index.filename2statistics filename).entryCnt;
  H.remove index.filename2field2word2hitsOrOffset filename;
  H.remove index.filename2statistics filename;
  index.changed <- true
    
let getFiles index = 
  H.fold
    (fun key v acc -> key :: acc)
    index.filename2field2word2hitsOrOffset
    []
    
let isFileIndexOutOfDate index filename =
  try Unix.access filename [ F_OK ];
    let size = (stat filename).st_size in
    let timeStamp = (stat filename).st_mtime in
    let stats = H.find index.filename2statistics filename in
    (stats.byteCnt <> size)
    || 
    (stats.modificationTime <> timeStamp)
  with Unix_error(_,_,_) -> true
    
let size index = index.totalEntryCnt
                   
let test () =
  let index = create () in
  addFile index "/home/rt/lib/bibtex/geom.bib";
  Btparse.cleanup ();
  P.printf "index weight: %i kb\n" ((String.length (Marshal.to_string index [])) / 1024);
  let a = getFieldHits index "title" "SCIENCE" in
  let b = getFieldHits index "title" "THE" in
  let result = Ptset.inter a b in
  Ptset.iter
    (fun i ->
       let coor = H.find index.entryId2coordinates i in
       P.printf "%s:%i:\n" coor.filename coor.line)
    result;
  P.printf "%i result%s\n" (Ptset.cardinal result) (if Ptset.cardinal result > 1 then "s" else "")
    
let isChanged index = index.changed
let clearChanged index = index.changed <- false
                           
                           
let loadAllAndClose index = 
  (match index.mayFile with
       Some(file) ->
         H.iter
         (fun filename field2word2hitsOrOffset ->
            H.iter
            (fun field word2hitsOrOffset ->
               H.iter
               (fun word hitsOrOffset ->
                  (match hitsOrOffset with
                       Hits(_) -> ()
                     | Offset(offset) ->
                         seek_in file offset;
                         H.replace word2hitsOrOffset word (Hits(Marshal.from_channel file))))
               word2hitsOrOffset)
            field2word2hitsOrOffset)
         index.filename2field2word2hitsOrOffset;
         close_in file;
         index.mayFile <- None
           
     | None -> ()) (* nothing to do, already fully loaded *)
  
let saveToChannel index chn =
  Marshal.to_channel chn magicNumber [ Marshal.No_sharing ];
  Marshal.to_channel chn indexFormatVersionNumber [ Marshal.No_sharing ];
  let offsetTable = { index with 
                        filename2field2word2hitsOrOffset = 
                        (H.copy index.filename2field2word2hitsOrOffset);
                        
                        mayFile = None;
                        changed = false
                    }
  in
  (* Deep copy are replace all entries with offset to a dummy zero : *)
  H.iter
    (fun filename originalField2word2hitsOrOffset ->
       let newField2word2hitsOrOffset = H.copy originalField2word2hitsOrOffset in
       H.replace offsetTable.filename2field2word2hitsOrOffset filename newField2word2hitsOrOffset;
       H.iter
         (fun field originalWord2hitsOrOffset ->
            let newWord2hitsOrOffset = H.copy originalWord2hitsOrOffset in
            H.replace newField2word2hitsOrOffset field newWord2hitsOrOffset;
            H.iter
              (fun word hitsOrOffset -> H.replace newWord2hitsOrOffset word (Offset max_int))
              newWord2hitsOrOffset)
         newField2word2hitsOrOffset)
    offsetTable.filename2field2word2hitsOrOffset;
  
  (* Write the dummied up offset table : *)
  let offsetTableSeek = pos_out chn in
  Marshal.to_channel chn offsetTable [ Marshal.No_sharing ];

  (* Save the pset and record the real offsets *)
  H.iter
    (fun filename field2word2hitsOrOffset ->
       H.iter
       (fun field word2hitsOrOffset ->
          H.iter
          (fun word hitsOrOffset -> 
             (match hitsOrOffset with
                  Hits(pset) -> 
                    let offset = pos_out chn in
                    H.tripleReplace offsetTable.filename2field2word2hitsOrOffset filename field word (Offset(offset));
                    Marshal.to_channel chn pset [ Marshal.No_sharing ];
                | Offset(_) -> assert false)
          )
          word2hitsOrOffset)
       field2word2hitsOrOffset)
    index.filename2field2word2hitsOrOffset;
  
  (* Save the real offsets : *)
  seek_out chn offsetTableSeek;
  Marshal.to_channel chn offsetTable [ Marshal.No_sharing ]

    
    
let save index filename =
  loadAllAndClose index;
  let chn = open_out filename in
  saveToChannel index chn;
  close_out chn
    
let load filename lazily =
  let chn = open_in filename in
  let fileMagicNumber : int = Marshal.from_channel chn in 
  (try 
     if fileMagicNumber <> magicNumber then
       raise (Bad_file("Bad magic number. `%s' is not an bibgrep index\n"));
     
     let thisFileIndexFormatVersionNumber : float = Marshal.from_channel chn in
     if not (thisFileIndexFormatVersionNumber = indexFormatVersionNumber) then
       ((* Ocaml's printf format hack forces use to use multiple 
           statements, or use incredibly long lines. We choose the
           former *)
         raise (Bad_file(
                  ((Printf.sprintf
                      "Cannot load index file `%s', it is of version %f, "
                      filename
                      thisFileIndexFormatVersionNumber) 
                   ^
                   (Printf.sprintf
                      "whereas this version of bibgrep only supports version %f. "
                      indexFormatVersionNumber)
                   ^
                   (Printf.sprintf
                      "Delete the index file, move it away, or use --index to select ")
                   ^
                   (Printf.sprintf
                      "another file.")))));
     
     let savedOffsetTable : t =  Marshal.from_channel chn in
     let result = 
       { savedOffsetTable with
           mayFile = Some(chn);
           changed = false;
       }
     in
     if not lazily then loadAllAndClose result;
     result
   with End_of_file -> raise (Bad_file(Printf.sprintf "Index file `%s' is corrupted" filename)))
    

                    
                    
(*
(* Pack and unpack reduce the size of the index by about one half.
   It makes the index file smaller, but also much slower to load *)

let pack index =
  let hash = H.create 10 in
  H.iter
    (fun filename field2word2hitsOrOffset ->
       H.iter
       (fun field word2hitsOrOffset ->
          H.iter
          (fun word _ ->
             let hits = gadOrLoadHits index word2hitsOrOffset word in
             let arr = Array.make (Ptset.cardinal hits) 0 in
             let i = ref 0 in
             Ptset.iter 
               (fun hit -> arr.(!i) <- hit; incr i)
               hits;
             H.tripleReplace hash filename field word arr)
          word2hitsOrOffset)
       field2word2hitsOrOffset)
    index.filename2field2word2hitsOrOffset;
  (index.totalEntryCnt, index.firstFreeEntryId, index.changed,
   index.entryId2coordinates, index.filename2statistics, hash) 
  
let unpack tuple =
  let (totalEntryCnt, firstFreeEntryId, changed,
       entryId2coordinates, filename2statistics, hash) = tuple 
  in
  let filename2field2word2hitsOrOffset = H.create 10 in
  H.iter
    (fun filename field2word2hitsOrOffset ->
       H.iter
       (fun field word2hitsOrOffset ->
          H.iter 
          (fun word hits ->
             let set = 
               Array.fold_left
                 (fun acc hit -> Ptset.add hit acc)
                 Ptset.empty
                 hits
             in
             H.tripleReplace filename2field2word2hitsOrOffset filename field word (Hits(set)))
          word2hitsOrOffset)
       field2word2hitsOrOffset)
    hash;
  {
    mayFile = None;
    totalEntryCnt = totalEntryCnt;
    firstFreeEntryId = firstFreeEntryId;
    changed = changed;
    filename2field2word2hitsOrOffset = filename2field2word2hitsOrOffset;
    entryId2coordinates = entryId2coordinates;
    filename2statistics = filename2statistics
  }
  
  *)
