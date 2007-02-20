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
 * IXP Library serialization / de-serialization interface
 * http://v9fs.sourceforge.net/rfc/
 *
 * Object Oriented interface to the IXP packages.
 * Primarily meant to be used through the Ixpc module.
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
let new_tag () =
    let t = !_tag in
    _tag := t + 1;
    t

let nofid = String.make 4 (char_of_int 255)
let _fids = ref []
let new_fid () =
    let rec check fid = 
        if List.mem fid !_fids then check (fid + 1)
        else (_fids := fid :: !_fids; fid) in
    check 0
let reuse_fid fid =
    let rec remove l f prev = 
        match l with
        | hd :: tl -> 
            if hd = f then 
                (List.rev prev) @ tl 
            else 
                remove tl f (hd :: prev)
        | [] -> List.rev prev in
    _fids := remove !_fids fid []

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
let d_intx str bytes offset =
    let v = ref 0 in
    for i = 0 to bytes - 1 do
        v := !v + ((int_of_char str.[offset + i]) lsl (i * 8))
    done;
    !v

let d_int8 str offset = int_of_char str.[0 + offset]
let d_int16 str offset = d_intx str 2 offset
let d_int32 str offset = d_intx str 4 offset
let d_int64 str offset = d_intx str 8 offset
let d_str str offset =
    let len = d_int16 str offset in
    if len = 0 then ""
    else String.sub str (offset + 2) len

(* Base class for P9 request transmissions and responses. *)
class virtual fcall =
    object (self)
        val mutable mtype = 0
        val mutable tag = 0

        method mtype = mtype
        method virtual serialize : String.t
        method virtual deserialize : String.t -> unit

        method check package =
            let mt = d_int8 package 4 in
            if mt != mtype then raise (Illegal_package_type mt);
            let psize = d_int32 package 0 in
            if (String.length package) < psize then raise Package_not_complete;
            let rtag = d_int16 package 5 in
            if tag != rtag then raise (Wrong_tag rtag);
    end

let concat = String.concat ""

class tVersion msize =
    object
        inherit fcall

        val version = V9P2000 (* No other available (?) *)

        initializer
            mtype <- 100

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

        method deserialize package = () (* TODO *)
    end

class rVersion msize =
    object (self)
        inherit fcall

        val mutable msize = msize

        initializer
            mtype <- 101;
            tag <- notag

        method deserialize package =
            self#check package;
            (* Ignore tag. *)
            msize <- d_int32 package 7;
            let vrsn_str = d_str package 11 in
            if not (vrsn_str = "9P2000") then 
                raise (Unsupported_version vrsn_str)

        method serialize = "" (* TODO *)
        method mtype = mtype
        method msize = msize
    end

class tAttach afid uname aname =
    object
        inherit fcall

        val fid = new_fid ()

        initializer
            mtype <- 104;
            tag <- new_tag ()

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

        method deserialize package = () (* TODO *)

        method mtype = mtype
        method tag = tag
        method fid = fid
    end

class rAttach tag =
    object (self)
        inherit fcall

        val quit = 0

        initializer
            mtype <- 105

        method deserialize package =
            self#check package

        method serialize = "" (* TODO *)

        method mtype = mtype
    end

class rError package tag message =
    object (self)
        inherit fcall

        val mutable message = message

        initializer
            mtype <- 107

        method deserialize package =
            self#check package;
            message <- Some (d_str package 7)

        method serialize = "" (* TODO *)
        method message = message
    end

let tflush  = String.make 1 (char_of_int 108)
let rflush  = String.make 1 (char_of_int 109)

class tWalk fid use_old wname = 
    object
        inherit fcall

        val newfid = if use_old then fid else new_fid ()

        initializer
            mtype <- 110;
            tag <- new_tag ()

        method serialize = 
            let buff = Buffer.create 1024 in
            List.iter (fun e -> Buffer.add_string buff (s_str e)) wname;
            let nwname = (s_int16 (List.length wname)) in
            let wname = Buffer.contents buff in
            let data = concat [
                s_int8 mtype;
                s_int16 tag;
                s_int32 fid;
                s_int32 newfid;
                nwname;
                wname;
            ] in
            s_int32 ((String.length data) + 4) ^ data
                
        method deserialize package = () (* TODO *)

        method mtype = mtype
        method tag = tag
        method newfid = newfid
    end

class rWalk _tag nwquid =
    object (self)
        inherit fcall

        val mutable nwquid = nwquid
        (* val mutable quid = quid *)

        initializer
            mtype <- 111;
            tag <- _tag

        method serialize = "" (* TODO *)
        method deserialize package =
            self#check package;
            nwquid <- d_int16 package 7
    end

class tOpen fid mode =
    object
        inherit fcall

        initializer
            mtype <- 112;
            tag <- new_tag ()

        method serialize =
            let data = concat [
                s_int8 mtype;
                s_int16 tag;
                s_int32 fid;
                s_int8 mode;
            ] in
            (s_int32 ((String.length data) + 4)) ^ data

        method deserialize package = () (* TODO *)

        method tag = tag
    end

class rOpen _tag _iounit =
    object (self)
        inherit fcall

        val mutable iounit = _iounit

        initializer
            mtype <- 113;
            tag <- _tag

        method serialize = "" (* TODO *)

        method deserialize package =
            self#check package;
            iounit <- d_int32 package 20
        
        method iounit = iounit
    end

class tRead fid offset count =
    object
        inherit fcall

        initializer
            mtype <- 116;
            tag <- new_tag ()
        
        method serialize =
            let data = concat [
                s_int8 mtype;
                s_int16 tag;
                s_int32 fid;
                s_int64 offset;
                s_int32 count;
            ] in
            (s_int32 ((String.length data) + 4)) ^ data

        method deserialize package = (* TODO *)
            ()

        method tag = tag
    end

class rRead _tag data =
    object (self)
        inherit fcall

        val mutable data = data

        initializer
            mtype <- 117;
            tag <- _tag

        method serialize = (* TODO *)
            ""

        method deserialize package =
            self#check package;
            let count = d_int32 package 7 in
            data <- (String.sub package 11 count)

        method data = data
        method count = String.length data
    end

class tWrite fid offset count data =
    object (self)
        inherit fcall
        
        initializer
            mtype <- 118;
            tag <- new_tag ();

        method serialize =
            let package = concat [
                s_int8 mtype;
                s_int16 tag;
                s_int32 fid;
                s_int64 offset;
                s_int32 count;
                data;
            ] in
            s_int32 ((String.length package) + 4) ^ package

        method deserialize package = () (* TODO *)

        method tag = tag
    end

class rWrite _tag count =
    object (self)
        inherit fcall

        val mutable count = count
        
        initializer
            mtype <- 119;
            tag <- _tag

        method serialize = "" (* TODO *)
        method deserialize package =
            self#check package;
            count <- d_int32 package 7;

        method count = count
    end

class tClunk fid =
    object
        inherit fcall

        initializer
            reuse_fid fid;
            mtype <- 120;
            tag <- new_tag ()

        method serialize =
            let data = concat [
                s_int8 mtype;
                s_int16 tag;
                s_int32 fid;
            ] in
            s_int32 ((String.length data) + 4) ^ data

        method deserialize package = () (* TODO *)

        method tag = tag
    end

class rClunk _tag =
    object (self)
        inherit fcall

        initializer
            mtype <- 121;
            tag <- _tag

        method serialize = "" (* TODO *)
        method deserialize package =
            self#check package
    end

(* TODO implement these: *)
let tauth = String.make 1 (char_of_int 102)
let rauth = String.make 1 (char_of_int 103)
let tcreate  = String.make 1 (char_of_int 114)
let rcreate  = String.make 1 (char_of_int 115)
let tremove  = String.make 1 (char_of_int 122)
let rremove  = String.make 1 (char_of_int 123)
let tstat  = String.make 1 (char_of_int 124)
let rstat  = String.make 1 (char_of_int 125)
let twstat  = String.make 1 (char_of_int 126)
let rwstat  = String.make 1 (char_of_int 127)
