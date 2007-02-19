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

(*
 * Implementation of http://v9fs.sourceforge.net/rfc/
 *)

type version = V9P2000

exception Unsupported_version of string
exception Illegal_package_type of int
exception Package_not_complete
exception Wrong_tag of int

(* Message tags *)
let tauth = 255
let notag = 255

let _tag = ref 0
let tag () =
    let t = !_tag in
    _tag := t + 1;
    t

let nofid = String.make 4 (char_of_int 255)
let _fids = ref []
let fid () =
    let rec check fid = 
        if List.mem fid !_fids then check (fid + 1)
        else (_fids := fid :: !_fids; fid) in
    check 0

(* Serialize values *)
let s_xbit_int bytes v =
    let str = String.create bytes in
    for i = 0 to (bytes - 1) do
        str.[i] <- char_of_int ((v lsr (i * 8)) land 255)
    done;
    str

let s_int8 v = String.make 1 (char_of_int v)
let s_int16 v = s_xbit_int 2 v
let s_int32 v = s_xbit_int 4 v
let s_int64 v = s_xbit_int 8 v
let s_str s = (s_int16 (String.length s)) ^ s

(* De-serialize unsigned int values. *)
let d_xbit_int str bytes offset =
    let v = ref 0 in
    for i = 0 to bytes - 1 do
        v := !v + ((int_of_char str.[offset + i]) lsl (i * 4))
    done;
    !v

let d_int8 str offset = int_of_char str.[0 + offset]
let d_int16 str offset = d_xbit_int str 2 offset
let d_int32 str offset = d_xbit_int str 4 offset
let d_int64 str offset = d_xbit_int str 8 offset
let d_str str offset =
    let len = d_int16 str offset in
    String.sub str (offset + 2) len

(* Base class for P9 request transmissions and responses. *)
class virtual fcall =
    object (self)
        method virtual mtype : int
        method virtual serialize : String.t
        method virtual deserialize : String.t -> unit
    end

let concat = String.concat ""

class tVersion msize =
    object
        inherit fcall

        val mtype = 100
        val version = V9P2000 (* No other available (?) *)

        method mtype = mtype

        method serialize =
            let vrsn_str = "9P2000" in
            let data = concat [
                s_int8 mtype;
                s_int16 notag;
                s_int32 msize;
                s_str vrsn_str;
            ] in
            s_int32 ((String.length data) + 4) ^ data

        method deserialize package = () (* FIXME *)
    end

class rVersion msize =
    object
        inherit fcall

        val mtype = 101
        val mutable msize = msize

        method deserialize package =
            let mt = d_int8 package 4 in
            if mt != mtype then raise (Illegal_package_type mt);
            let psize = d_int32 package 0 in
            if (String.length package) < psize then raise Package_not_complete;
            (* Ignore tag. *)
            msize <- d_int32 package 7;
            let vrsn_str = d_str package 11 in
            if not (vrsn_str = "9P2000") then 
                raise (Unsupported_version vrsn_str)

        method serialize = "" (* FIXME *)
        method mtype = mtype
        method msize = msize
    end

class tAttach afid uname aname =
    object
        inherit fcall

        val tag = tag ()
        val mtype = 104
        val fid = fid ()

        method serialize =
            let s_afid = match afid with
            | Some id -> s_int32 id
            | None    -> nofid in
            let data = concat [
                s_int8 mtype;
                s_int16 tag;
                s_int32 fid;
                s_afid;
                s_str uname;
                s_str aname;
            ] in
            s_int32 ((String.length data) + 4) ^ data

        method deserialize package = () (* FIXME *)

        method mtype = mtype
        method tag = tag
        method fid = fid
    end

class rAttach tag =
    object
        inherit fcall

        val mtype = 105
        val quit = 0

        method deserialize package =
            let mt = d_int8 package 4 in
            if mt != mtype then raise (Illegal_package_type mt);
            let psize = d_int32 package 0 in
            if (String.length package) < psize then raise Package_not_complete;
            let rtag = d_int16 package 5 in
            if tag != rtag then raise (Wrong_tag rtag)

        method serialize = "" (* FIXME *)

        method mtype = mtype
    end

class rError package tag message =
    object
        inherit fcall

        val mtype = 107
        val mutable message = message

        method deserialize package =
            let mt = d_int8 package 4 in
            if mt != mtype then raise (Illegal_package_type mt);
            let psize = d_int32 package 0 in
            if (String.length package) < psize then raise Package_not_complete;
            let rtag = d_int16 package 5 in
            if tag != rtag then raise (Wrong_tag rtag);
            message <- Some (d_str package 7)

        method serialize = "" (* FIXME *)
        method mtype = mtype
        method message = message
    end

class tOpen fid mode =
    object
        inherit fcall

        val mtype = 112
        val tag = tag ()

        method serialize =
            let data = concat [
                s_int8 mtype;
                s_int16 tag;
                s_int32 fid;
                s_int8 mode;
            ] in
            (s_int32 ((String.length data) + 4)) ^ data

        method deserialize package = () (* FIXME *)

        method mtype = mtype
        method tag = tag
    end

class rOpen tag iounit =
    object
        inherit fcall

        val mtype = 113
        val mutable iounit = iounit

        method deserialize package =
            let mt = d_int8 package 4 in
            if mt != mtype then raise (Illegal_package_type mt);
            let psize = d_int32 package 0 in
            if (String.length package) < psize then raise Package_not_complete;
            let rtag = d_int16 package 5 in
            if tag != rtag then raise (Wrong_tag rtag);
            iounit <- d_int32 package 20
        
        method serialize = "" (* FIXME *)
        method mtype = mtype
        method iounit = iounit
    end

(* Message types *)
let tauth = String.make 1 (char_of_int 102)
let rauth = String.make 1 (char_of_int 103)

let tflush  = String.make 1 (char_of_int 108)
let rflush  = String.make 1 (char_of_int 109)
let twalk  = String.make 1 (char_of_int 110)
let rwalk  = String.make 1 (char_of_int 111)
let tcreate  = String.make 1 (char_of_int 114)
let rcreate  = String.make 1 (char_of_int 115)
let tread  = String.make 1 (char_of_int 116)
let rread  = String.make 1 (char_of_int 117)
let twrite  = String.make 1 (char_of_int 118)
let rwrite  = String.make 1 (char_of_int 119)
let tclunk  = String.make 1 (char_of_int 120)
let rclunk  = String.make 1 (char_of_int 121)
let tremove  = String.make 1 (char_of_int 122)
let rremove  = String.make 1 (char_of_int 123)
let tstat  = String.make 1 (char_of_int 124)
let rstat  = String.make 1 (char_of_int 125)
let twstat  = String.make 1 (char_of_int 126)
let rwstat  = String.make 1 (char_of_int 127)
