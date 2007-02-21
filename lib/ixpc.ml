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
 * IXP Library Client interface.
 * http://v9fs.sourceforge.net/rfc/
 *
 * Primarily written to be used with WMII.
 * http://www.suckless.org/wmii
 *)

open Fcall

type t = Unix.file_descr

let msize = ref 8192

exception Socket_error of string
exception IXPError of string
exception Internal_error of string

(* File modes *)
let oREAD = 0x00
let oWRITE = 0x01
let oRDWR = 0x02
let oEXEC = 0x03
let oEXCL = 0x04
let oTRUNC = 0x10
let oREXEC = 0x20
let oRCLOSE = 0x40
let oAPPEND = 0x80

let mDIR  = Int32.shift_left Int32.one 31

let delimiter_exp = Str.regexp "/"

let deserialize obj package =
    try
        obj#deserialize package
    with Fcall.Illegal_package_type 107 ->
        let error = new rError obj#tag "" in
        error#deserialize package;
        raise (IXPError error#message)

let send sockfd data =
    try
        let data_len = String.length data in
        let sent_len = Unix.send sockfd data 0 data_len [] in
        if data_len != sent_len then raise (Socket_error "Sent 0 bytes")
    with Unix.Unix_error (num, "send", _) ->
        raise (Socket_error (Unix.error_message num))

let receive sockfd =
    try
        let buff = String.create !msize in
        let recv = Unix.recv sockfd buff in
        let rlen = recv 0 4 [] in
        if rlen = 0 then raise (Socket_error "Socket closed cleanly");
        let plen = Int32.to_int (Fcall.d_int32 buff 0) in
        let rlen = recv 4 plen [] in
        if rlen = 0 then raise (Socket_error "Socket closed cleanly")
        else String.sub buff 0 plen
    with Unix.Unix_error (num, "recv", _) ->
        raise (Socket_error (Unix.error_message num))
            
let fopen fd fid mode =
    let topen = new tOpen fid mode in
    send fd topen#serialize;
    let ropen = new rOpen topen#tag Int32.zero in
    deserialize ropen (receive fd);
    ropen#iounit

let version fd = 
    let tversion = new tVersion (Int32.of_int !msize) in
    send fd tversion#serialize;
    let rversion = new rVersion Int32.zero in
    deserialize rversion (receive fd);
    msize := Int32.to_int rversion#msize

let walk fd oldfid reuse file =
    let wname = Str.split delimiter_exp file in
    let twalk = new tWalk oldfid reuse wname in
    send fd twalk#serialize;
    let rwalk = new rWalk twalk#tag 0 in
    deserialize rwalk (receive fd);
    twalk#newfid

(* Returns (fid * iounit) *)
let walk_open fd oldfid reuse file mode =
    let newfid = walk fd oldfid reuse file in
    (newfid, fopen fd newfid mode)

let clunk fd fid =
    let tclunk = new tClunk fid in
    send fd tclunk#serialize;
    let rclunk = new rClunk tclunk#tag in
    deserialize rclunk (receive fd)

(* Low level function *)
let read fd fid iounit offset count =
    let tread = new tRead fid offset count in
    send fd tread#serialize;
    let rread = new rRead tread#tag "" in
    deserialize rread (receive fd);
    rread#data

(* Low level function *)
let write fd fid iounit offset count data = 
    let rec write offset count data =
        let i32write_len = if iounit > count then count else iounit in
        let write_len = Int32.to_int i32write_len in
        let i64write_len = Int64.of_int write_len in
        let d = String.sub data 0 write_len in
        let twrite = new tWrite fid offset i32write_len d in
        send fd twrite#serialize;
        let rwrite = new rWrite twrite#tag Int32.zero in
        deserialize rwrite (receive fd);
        if rwrite#count != i32write_len then
            (let swrite_len = string_of_int write_len in
            (let msg = "Failed to write " ^ swrite_len ^ " bytes" in 
            raise (IXPError msg)));
        let i_64_count = Int64.of_int32 count in
        if Int64.add offset i64write_len < i_64_count then
            let new_offset = Int64.add offset i64write_len in
            let new_count = Int32.sub count i32write_len in
            let rest = String.sub data write_len (Int32.to_int new_count) in
            write new_offset new_count rest in
    write offset count data;
    count (* FIXME Should we keep track of how much we have written? *)

let fwrite fd relfid file offset count data =
    let fid, iounit = walk_open fd relfid false file oWRITE in
    let count = write fd fid iounit offset count data in
    clunk fd fid;
    count

let remove fd fid =
    let tremove = new tRemove fid in
    send fd tremove#serialize;
    let rremove = new rRemove tremove#tag in
    deserialize rremove (receive fd)

let create fd fid name perm mode =
    let tcreate = new tCreate fid name perm mode in
    send fd tcreate#serialize;
    let rcreate = new rCreate tcreate#tag Int32.zero in
    deserialize rcreate (receive fd);
    rcreate#iounit

let attach fd user aname = 
    let tattach = new tAttach None user aname in
    send fd tattach#serialize;
    let rattach = new rAttach tattach#tag in
    deserialize rattach (receive fd);
    tattach#fid

let connect address =
    let sockaddr = Unix.ADDR_UNIX address in
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect fd sockaddr;
    version fd;
    fd

let unpack_files data = 
    try
        let rec unpack_files data acc =
            let record = Fcall.d_stat data 0 in
            let stat_len = (Fcall.d_int16 data 0) + 2 in
            if stat_len < (String.length data) then
                (let rest_len = (String.length data) - stat_len in
                let rest = String.sub data stat_len rest_len in
                unpack_files rest (record :: acc))
            else
                List.rev (record :: acc) in
        unpack_files data []
    with _ -> raise (IXPError "invalid package, expected directory read")
