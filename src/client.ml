(******************************************************************************)
(* OCaml-IXP                                                                  *)
(*                                                                            *)
(* Copyright 2007 Oscar Hellström, oscar at oscarh dot net.                   *)
(* All rights reserved                                                        *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided that the following conditions are     *)
(* met:                                                                       *)
(*                                                                            *)
(*     * Redistributions of source code must retain the above copyright       *)
(*       notice, this list of conditions and the following disclaimer.        *)
(*     * Redistributions in binary form must reproduce the above copyright    *)
(*       notice, this list of conditions and the following disclaimer in the  *)
(*       documentation and/or other materials provided with the distribution. *)
(*     * The names of its contributors may not be used to endorse or promote  *)
(*       products derived from this software without specific prior written   *)
(*       permission.                                                          *)
(*                                                                            *)
(*                                                                            *)
(* THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND    *)
(* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE      *)
(* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE *)
(* ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE    *)
(* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL *)
(* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR *)
(* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER *)
(* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT         *)
(* LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY  *)
(* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF     *)
(* SUCH DAMAGE.                                                               *)
(******************************************************************************)

(*
 * IXP Client
 * http://v9fs.sourceforge.net/rfc/
 *)

open Printf

type cmd = Read of string | Write of string | Ls of string

let adrs_exp = Str.regexp "unix!\\(.+\\)"
let wmii_address =
    let adrs = Sys.getenv "WMII_ADDRESS" in
    if Str.string_match adrs_exp adrs 0 then Str.matched_group 1 adrs
    else adrs

let user = Sys.getenv "USER"

let print_file dir =
    let owner = dir.Ixpc.uid in
    let group = dir.Ixpc.gid in
    let name = dir.Ixpc.name in
    let size = dir.Ixpc.length in
    printf "%s\t%s\t%d\t%s\n" owner group size name

let print_dirs dirs =
    try
        print_file (List.hd dirs);
        List.iter print_file (List.tl dirs)
    with _ ->
        ()

let do_read address file =
    let conn = Ixpc.connect address in
    let rootfid = Ixpc.attach conn user "/" in
    Ixpc.fread conn rootfid file 0 4090

let write address file =
    let buff = String.create 4096 in
    let len = (input stdin buff 0 4096) - 1 in
    let data = String.sub buff 0 len in
    let conn = Ixpc.connect address in
    let rootfid = Ixpc.attach conn user "/" in
    let count = Ixpc.fwrite conn rootfid file 0 len data in
    if count != len then
        printf "Warning: Could only write %d bytes" count

let read address file =
    let data = do_read address file in
    print_string data

let ls address dir =
    let data = do_read address dir in
    print_dirs (Ixpc.unpack_files data)

let run address cmd =
    match cmd with
    | Read file -> read address file
    | Write file -> write address file
    | Ls file -> ls address file

let main () =
    let cmd = ref None in
    let ixp_address = ref wmii_address in
    let usage = "usage: " ^ 
        Sys.argv.(0) ^ " [-a <address>] read | write | ls <file>" ^
        "\nread\t- Read from a file\n" ^
        "write\t- Write from a file\n" ^
        "ls\t- Read from a directory\n" ^
        "--help\t- Print this help" in
    let rec parse i =
        let next = match Sys.argv.(i) with
        | "read" -> cmd := Some (Read Sys.argv.(i + 1)); i + 2
        | "write" -> cmd := Some (Write Sys.argv.(i + 1)); i + 2
        | "ls" -> cmd := Some (Ls Sys.argv.(i + 1)); i + 2
        | "-a" -> ixp_address := Sys.argv.(i + 1); i + 2
        | "--help" -> cmd := None; Array.length Sys.argv
        | _ -> cmd := None; Array.length Sys.argv in
        if next < Array.length Sys.argv then parse next in
    if Array.length Sys.argv > 1 then parse 1;
    match !cmd with
    | Some command -> run !ixp_address command
    | None -> print_string usage; print_newline ()

let _ = main ()
