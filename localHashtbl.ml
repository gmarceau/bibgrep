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
include Hashtbl
let make = create
             
(* gad: if hash does not have item, add defFn ()*)
let gad h item defFn = 
  try Hashtbl.find h item
  with Not_found -> 
    let rtn = defFn () in
    Hashtbl.replace h item rtn;
    rtn
      
let dblGad h a b fn =
  let aHtbl = gad h a (fun () -> make 5) in
  gad aHtbl b fn
    
let dblFind h a b =
  let aHtbl = gad h a (fun () -> make 5) in
  find aHtbl b
    
let dblReplace h a b v =
  let aHtbl = gad h a (fun () -> make 5) in
  replace aHtbl b v
    
let dblMem h a b =
  let aHtbl = gad h a (fun () -> make 5) in
  mem aHtbl b

let tripleGad h a b c fn =
  let aHtbl = dblGad h a b (fun () -> make 5) in
  gad aHtbl c fn
    
let tripleFind h a b c=
  let aHtbl = dblGad h a b (fun () -> make 5) in
  find aHtbl b
    
    
let tripleReplace h a b c v =
  let aHtbl = dblGad h a b(fun () -> make 5) in
  replace aHtbl c v
      

let forthGad h a b c d fn =
  let aHtbl = tripleGad h a b c (fun () -> make 5) in
  gad aHtbl d fn
