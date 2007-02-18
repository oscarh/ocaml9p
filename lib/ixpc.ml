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

let send sockfd data =
    let data_len = String.length data in
    let sent_len = Unix.send sockfd data 0 data_len [] in
    if data_len != sent_len then
        (print_string "FIXME: data_len != sent_len, this should not print...";
        false)
    else true

let receive sockfd =
    let recv = Unix.recv sockfd in
    let buff = String.create !msize in
    let rlen = recv buff 0 4 [] in
    if rlen = 0 then raise Socket_error;
    let rlen = recv buff 4 (Fcall.d_int32 buff 0) [] in
    if rlen = 0 then raise Socket_error
    else String.sub buff 0 rlen

let version fd = 
    let tversion = new tVersion !msize in
    if send fd tversion#serialize then
        let rversion = new rVersion (receive fd) in
        msize := rversion#msize
    else 
        raise Socket_error

let attach fd user aname = 
    let tattach = new tAttach None user aname in
    if send fd tattach#serialize then
        ignore (new rAttach tattach#tag (receive fd))
    else
        raise IXPError
        

let connect address user aname =
    let sockaddr = Unix.ADDR_UNIX address in
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect fd sockaddr;
    version fd;
    attach fd user aname
