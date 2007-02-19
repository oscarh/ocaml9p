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

type t = Unix.file_descr

let msize = ref 1024

exception Socket_error
exception IXPError

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
    try 
        let recv = Unix.recv sockfd in
        let buff = String.create !msize in
        let rlen = recv buff 0 4 [] in
        if rlen = 0 then raise Socket_error;
        let plen = Fcall.d_int32 buff 0 in
        let rlen = recv buff 4 plen [] in
        if rlen = 0 then raise Socket_error
        else String.sub buff 0 plen
    with Invalid_argument "Unix.recv" ->
        raise Socket_error

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
    (ignore (new rOpen topen#tag 0))

let connect address =
    let sockaddr = Unix.ADDR_UNIX address in
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect fd sockaddr;
    version fd;
    fd
