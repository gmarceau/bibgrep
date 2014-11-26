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

module I = Index
module P = LocalPrintf
module H = LocalHashtbl
module A = Array
open Query   
open Index
              
type resultSet = 
    {
      isIrrelevant : bool; (* generated from bad words *)
      negated : bool; (* when this field is true, the result should be interpreted
                        as `everything *not* it .hits' *)
      hits : Ptset.t;
    }
    

let executeQuery index filenames query : I.coordinates list * string list  = 

  (* Divide the results into to set: those the are negated,
     and those that are not.

     Remove the irrelevants
     
     The result is a pair of lists : (strait results, negated results)
  *)
  let filterAndPartitionResults results =
    let filtered = List.filter (fun r -> not r.isIrrelevant) results in 
    let (strait, negated) = List.partition (fun r -> not r.negated) filtered in
    ((List.map (fun r -> r.hits) strait),
     (List.map (fun r -> r.hits) negated))
  in  
    
  let ignoredWords = ref [] in
  
  let rec loop q = match q with
      BaseQuery(mayFieldname, keywordString) ->
        let tokens = I.tokenizeThisRegexp keywordString in
        let (badWords, goodWords) = List.partition I.isBadWord tokens in
        
        (* Keep track of the ignored words : *)
        List.iter
          (fun w -> 
             if not (List.mem w !ignoredWords) then
               ignoredWords := w :: !ignoredWords)
          badWords;
        
        
        if goodWords = [] then
          (* Search terms are completly useless *)
          { isIrrelevant = true;
            negated = false; (* dummy *)
            hits = Ptset.empty  (* dummy *)
          }
        else
          let hits = 
            (* Standard, with useful words and all that good stuff *)
            (match mayFieldname with
                 Some(fieldname) ->
                   List.fold_left
                   (fun acc token -> 
                      Ptset.inter acc (I.getFilesFieldHits index filenames fieldname token))
                   (I.getFilesFieldHits index filenames fieldname (List.hd tokens))
                   (List.tl tokens)
                   
               | None ->
                   List.fold_left
                   (fun acc token ->
                      Ptset.inter acc (I.getFilesHits index filenames token)
(*                      Ptset.inter acc (I.getAllHits index token) *)

                   )
                   (I.getFilesHits index filenames (List.hd tokens)) 
(*                   (I.getAllHits index (List.hd tokens)) *)
                   (List.tl tokens))
          in
          { isIrrelevant = false;
            negated = false;
            hits = hits
          }

    | OrQuery(subqueries) ->  
        (* Logicaly, we are doing :
               a or b or (not c) or (not d)
           <=> not ( (not a) and (not b) and c and d)
           <=> not ( (c and d) // a // b )

           where "//" is set difference
        *)
        
        let subresults = List.map loop subqueries in
        let (strait, negated) = filterAndPartitionResults subresults in

        if strait = [] && negated = [] then
          (* nothing relevant *)
          { isIrrelevant = true;
            negated = false;
            hits = Ptset.empty
          }
        else if negated = [] then
          (*
            Here, "not negated" is the whole index, 
            which we want to avoid having to deal with.
            We handle this case separatly :
          *)
          let unioned = List.fold_left Ptset.union Ptset.empty strait in
          { isIrrelevant = false;
            negated = false;
            hits = unioned
          }
        else
          let intered = List.fold_left Ptset.inter (List.hd negated) (List.tl negated) in
          let diffed = List.fold_left Ptset.diff intered strait in
          { isIrrelevant = false;
            negated = true;
            hits = diffed
          }

    | AndQuery(subqueries) -> 
        (* Logicaly, we are doing :
               a and b and (not c) and (not d)
           <=> (a and b) // c // d

           where "//" is set difference
        *)

        let subresults = List.map loop subqueries in
        let (strait, negated) = filterAndPartitionResults subresults in

        if strait = [] && negated = [] then
          (* nothing relevant *)
          { isIrrelevant = true;
            negated = false;
            hits = Ptset.empty
          }
          else
            (let intersected = List.fold_left Ptset.inter (List.hd strait) (List.tl strait) in
             let diffed = List.fold_left Ptset.diff intersected negated in
             { isIrrelevant = false;
               negated = false;
               hits = diffed
             })
             
    | NotQuery(subquery) -> 
        let subresult = loop subquery in
        { isIrrelevant = subresult.isIrrelevant;
          negated = not subresult.negated;
          hits = subresult.hits
        }
  in
  let maybeNegativeResults = loop query in
  let resultSet =
    if maybeNegativeResults.isIrrelevant then 
      Ptset.empty
    else if maybeNegativeResults.negated then
      I.getNegation index filenames maybeNegativeResults.hits
    else maybeNegativeResults.hits
  in
  let resultsList =
    Ptset.fold 
      (fun id acc -> (I.getEntry index id) :: acc)
      resultSet
      []
  in (resultsList, !ignoredWords)
    
let loadTextWithFileCache filename2file entry = 
  let file = H.gad filename2file entry.filename (fun () -> open_in entry.filename) in
  seek_in file entry.seekOffset;
  let buf = String.create entry.length in
  really_input file buf 0 entry.length;
  entry.fullText <- Some(buf)
  
let loadText entry =
  let cache = H.create 1 in
  loadTextWithFileCache cache entry;
  H.iter (fun _ file -> close_in file) cache

(* This is much faster than calling `loadText' repetitively, 
   since it avoids open and closing the same files over and over *)
   
let loadTexts entries =
  let cache = H.create 10 in
  List.iter 
    (fun entry -> loadTextWithFileCache cache entry)
    entries;
  H.iter (fun _ file -> close_in file) cache
    


