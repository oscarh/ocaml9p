(******************************************************************************)
(* OCaml-IXP                                                                  *)
(*                                                                            *)
(* Copyright 2007 Oscar Hellström oscar at oscarh dot net.                    *)
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

open Fcall
open Printf

type t = Unix.file_descr

let msize = ref 1024

exception Socket_error
exception IXPError

type dir = {
    ktype : int;
    kdev : int;
    q_type : int;
    q_vers : int;
    q_path : int;
    mode : int;
    atime : int;
    mtime : int;
    name : string;
    owner : string;
    group : string;
    muid : string;
}

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

let send sockfd data =
    let data_len = String.length data in
    let sent_len = Unix.send sockfd data 0 data_len [] in
    if data_len != sent_len then raise Socket_error

let receive sockfd =
        let buff = String.create !msize in
        let recv = Unix.recv sockfd buff in
        let rlen = recv 0 4 [] in
        if rlen = 0 then raise Socket_error;
        let plen = Fcall.d_int32 buff 0 in
        let rlen = recv 4 plen [] in
        if rlen = 0 then raise Socket_error
        else String.sub buff 0 plen
            

let rec print_dir data =
    let offset = ref 0 in
    let add_offset i = offset := !offset + i in
    (* De-serialize *)
    let struct_size = Fcall.d_int16 data !offset in
    add_offset 2;
    let ktype = Fcall.d_int16 data !offset in
    add_offset 2;
    let kdev = Fcall.d_int32 data !offset in
    add_offset 4;
    let q_type = Fcall.d_int8 data !offset in
    add_offset 1;
    let q_vers = Fcall.d_int32 data !offset in
    add_offset 4;
    let q_path = Fcall.d_int64 data !offset in
    add_offset 8;
    let mode = Fcall.d_int32 data !offset in
    add_offset 4;
    let atime = Fcall.d_int32 data !offset in
    add_offset 4;
    let mtime = Fcall.d_int32 data !offset in
    add_offset 4;
    let fsize = Fcall.d_int64 data !offset in
    add_offset 8;
    let name = Fcall.d_str data !offset in
    add_offset ((String.length name) + 2);
    let owner = Fcall.d_str data !offset in
    add_offset ((String.length owner) + 2);
    let group = Fcall.d_str data !offset in
    add_offset ((String.length group) + 2);
    let muid = Fcall.d_str data !offset in
    add_offset ((String.length muid) + 2);
    print_string owner;
    print_string "\t";
    print_string group;
    print_string "\t";
    print_int fsize;
    print_string "\t";
    print_string name;
    print_string "\t";
    if !offset < (String.length data) then
        (let rest_len = (String.length data) - !offset in
        let rest = String.sub data !offset rest_len in
        print_newline ();
        print_dir rest)

let read fd fid iounit offset count =
    let rec read buff offset =
        let tread = new tRead fid offset count in
        send fd tread#serialize;
        let rread = new rRead tread#tag "" in
        rread#deserialize (receive fd);
        if rread#count > 0 then
            read (buff ^ rread#data) (offset + rread#count)
        else 
            buff in
    read "" offset

let version fd = 
    let tversion = new tVersion !msize in
    send fd tversion#serialize;
    let rversion = new rVersion 0 in
    rversion#deserialize (receive fd);
    msize := rversion#msize

let attach fd user aname = 
    let tattach = new tAttach None user aname in
    send fd tattach#serialize;
    let rattach = new rAttach tattach#tag in
    rattach#deserialize (receive fd);
    tattach#fid

let fopen fd fid mode =
    let topen = new tOpen fid mode in
    send fd topen#serialize;
    let ropen = new rOpen topen#tag 0 in
    ropen#deserialize (receive fd);
    ropen#iounit

let connect address =
    let sockaddr = Unix.ADDR_UNIX address in
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect fd sockaddr;
    version fd;
    fd
