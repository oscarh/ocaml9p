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
        | Create of (string * string) | Remove of string

let adrs_exp = Str.regexp "unix!\\(.+\\)"
let wmii_address =
    let adrs = Sys.getenv "WMII_ADDRESS" in
    if Str.string_match adrs_exp adrs 0 then Str.matched_group 1 adrs
    else adrs

let user = Sys.getenv "USER"

let print_file dir =
    let owner = dir.Fcall.uid in
    let group = dir.Fcall.gid in
    let name = dir.Fcall.name in
    let size = Int64.to_int (dir.Fcall.length) in
    let _ = dir.Fcall.mode in
    printf "%s\t%s\t%d\t%s\n" owner group size name

let print_dirs dirs =
    try
        print_file (List.hd dirs);
        List.iter print_file (List.tl dirs)
    with _ ->
        ()

(* Returns fid * iounit *)
let open_fid address file =
    let conn = Ixpc.connect address in
    let rootfid = Ixpc.attach conn user "/" in
    let _, iounit = Ixpc.walk_open conn rootfid true file Ixpc.oREAD in
    (conn, rootfid, iounit)

let write address file =
    let buff = String.create 4096 in
    let len = (input stdin buff 0 4096) - 1 in
    let data = String.sub buff 0 len in
    let conn = Ixpc.connect address in
    let rootfid = Ixpc.attach conn user "/" in
    let i32len = Int32.of_int len in
    let count = Ixpc.fwrite conn rootfid file Int64.zero i32len data in
    if count != i32len then
        printf "Warning: Could only write %d bytes" (Int32.to_int count)

let read address file =
    let conn, fid, iounit = open_fid address file in
    let rec read offset =
        let max_len = Int32.of_int 4096 in
        let data = Ixpc.read conn fid iounit offset max_len in
        print_string data;
        flush stdout;
        let len = String.length data in
        if len > 0 then
            read (Int64.add offset (Int64.of_int len)) in
    read Int64.zero;
    Ixpc.clunk conn fid

let ls address dir =
    let conn, fid, iounit = open_fid address dir in
    let max_len = Int32.of_int 4096 in
    let data = Ixpc.read conn fid iounit Int64.zero max_len in
    print_dirs (Ixpc.unpack_files data);
    Ixpc.clunk conn fid

let create name dir =
    let conn = Ixpc.connect wmii_address in
    let fid = Ixpc.attach conn user dir in
    let newfid = Ixpc.create conn fid name (Int32.of_int 666) Ixpc.oWRITE in
    Ixpc.clunk conn newfid;
    Ixpc.clunk conn fid

let remove name =
    let conn = Ixpc.connect wmii_address in
    let fid = Ixpc.attach conn user "/" in
    let fid = Ixpc.walk conn fid true name in
    Ixpc.clunk conn fid

let run address cmd =
    match cmd with
    | Read file -> read address file
    | Create (name, dir) -> create name dir
    | Remove name -> remove name
    | Write file -> write address file
    | Ls dir -> ls address dir

let main () =
    let cmd = ref None in
    let ixp_address = ref wmii_address in
    let usage = "usage: " ^ 
        Sys.argv.(0) ^ " [-a <address>] read | write | ls <file>" ^
        "\ncreate <name>   - Create a file in dir with name\n" ^
        "remove          - Create file\n" ^
        "read            - Read from a file\n" ^
        "write           - Write from a file\n" ^
        "ls              - Read from a directory\n" ^
        "--help          - Print this help" in
    let rec parse i =
        if i < (Array.length Sys.argv) - 1 then
            let next = match Sys.argv.(i) with
            | "create" -> if Array.length Sys.argv > i + 2 then
                cmd := Some (Create (Sys.argv.(i + 1), Sys.argv.(i + 2))); i + 3
            | "remove" -> cmd := Some (Remove Sys.argv.(i + 1)); i + 2
            | "read" -> cmd := Some (Read Sys.argv.(i + 1)); i + 2
            | "write" -> cmd := Some (Write Sys.argv.(i + 1)); i + 2
            | "ls" -> cmd := Some (Ls Sys.argv.(i + 1)); i + 2
            | "-a" -> ixp_address := Sys.argv.(i + 1); i + 2
            | _ -> cmd := None; Array.length Sys.argv in
            if next < Array.length Sys.argv then parse next in
    if Array.length Sys.argv > 1 then parse 1;
    match !cmd with
    | Some command -> run !ixp_address command
    | None -> print_string usage; print_newline ()

let _ = main ()
