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

(**
IXP Library serialization / de-serialization interface.
{{:http://v9fs.sourceforge.net/rfc/} http://v9fs.sourceforge.net/rfc/}

Object Oriented interface to the IXP packages.
Primarily meant to be used through the Ixpc module.
 *)

(** Version of the protocol (currently there is only one) *)
type version = V9P2000

(** {2 Records} *)

(** Describes a stat result *)
type stat = {
    ktype : int;
    kdev : int32;
    q_type : int;
    q_vers : int32;
    q_path : int64;
    mode : int32;
    atime : int32;
    mtime : int32;
    length : int64;
    name : string;
    uid : string;
    gid : string;
    muid : string;
}

(** Thrown if some one wants to use an unsupported version *)
exception Unsupported_version of string

(** A package is trying to de-serialize a package which it cannot understand *)
exception Illegal_package_type of int

(** A package is not complete, i.e. it does not have the correct length *)
exception Package_not_complete

(** A package with the wrong tag was de-serialized *)
exception Wrong_tag of int * int

(** {2 Serialization functions} *)

(** 
[s_intx bytes value] serialize integer of value [value]. The integer will
take up [bytes] bytes (octets) in the string returned.
*)
val s_intx : int -> int -> string

(** Serialize a 8 bit integer. *)
val s_int8 : int -> string

(** Serialize a 16 bit integer. *)
val s_int16 : int -> string

(** Serialize a 32 bit integer. *)
val s_int32 : int32 -> string

(** Serialize a 64 bit integer. *)
val s_int64 : int64 -> string

(** Serialize a string. The string must be UTF-8 encoded. *)
val s_str : string -> string

(** {2 De-serialization functions} *)

(** 
FIXME some of these functions should return Int32.t or Int64.t values
*)

(** 
[d_intx data bytes offset] de-serializes an integer of [bytes] bytes
starting at [offset].
*)
val d_intx : string -> int -> int -> int

(** [d_int8 data bytes offset] de-serializes an 8 bit integer *)
val d_int8 : string -> int -> int

(** [d_int16 data bytes offset] de-serializes a 16 bit integer *)
val d_int16 : string -> int -> int

(** [d_int32 data bytes offset] de-serializes a 32 bit integer *)
val d_int32 : string -> int -> int32

(** [d_int64 data bytes offset] de-serializes a 64 bit integer *)
val d_int64 : string -> int -> int64

(** [d_str data offset] de-serializes a string. *)
val d_str : string -> int -> string

val d_stat : string -> int -> stat

(** {2 FCall classes} *)
(**
These classes represent the packages used to communicate with the resource.
*)

(** {3 Abstract base class} *)

(** Base class for all package. *)
class virtual fcall :
  object
    val mutable mtype : int (** The message type. *)

    val mutable tag : int (** Message tag, unique for all messages within a
                              session. *)

    (** Reads a package and sets the fields accordingly. *)
    method virtual deserialize : string -> unit

    (** Serializes the message, returns a sequence of bytes. *)
    method virtual serialize : string

    (** Returns the massage type. *)
    method mtype : int

    (** Returns the tag of the message. *)
    method tag : int
  end

(** {3 Concrete classes} *)

class tVersion :
  Int32.t ->
  object
    val mutable mtype : int
    val mutable tag : int
    val version : version
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rVersion :
  int32 ->
  object
    val mutable msize : int32
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method msize : int32
    method mtype : int
    method serialize : string
    method tag : int
  end

class tAttach :
  int32 option ->
  string ->
  string ->
  object
    val fid : int32
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method fid : int32
    method mtype : int
    method serialize : string
    method tag : int
  end

class rAttach :
  int ->
  object
    val mutable mtype : int
    val quit : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rError :
  int ->
  string ->
  object
    val mutable message : string
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method message : string
    method mtype : int
    method serialize : string
    method tag : int
  end

class tflush :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rflush :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class tWalk :
  int32 ->
  bool ->
  string list ->
  object
    val mutable mtype : int
    val newfid : int32
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method newfid : int32
    method serialize : string
    method tag : int
  end

class rWalk :
  int ->
  int ->
  object
    val mutable mtype : int
    val mutable nwquid : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class tOpen :
  int32 ->
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rOpen :
  int ->
  int32 ->
  object
    val mutable iounit : int32
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method iounit : int32
    method mtype : int
    method serialize : string
    method tag : int
  end

class tCreate :
  int32 ->
  string ->
  int32 ->
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rCreate :
  int ->
  int32 ->
  object
    val mutable iounit : int32
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method iounit : int32
    method mtype : int
    method serialize : string
    method tag : int
  end

class tRead :
  int32 ->
  int64 ->
  int32 ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rRead :
  int ->
  string ->
  object
    val mutable data : string
    val mutable mtype : int
    val mutable tag : int
    method count : int
    method data : string
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class tWrite :
  int32 ->
  int64 ->
  int32 ->
  string ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rWrite :
  int ->
  int32 ->
  object
    val mutable count : int32
    val mutable mtype : int
    val mutable tag : int
    method count : int32
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class tClunk :
  int32 ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rClunk :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class tRemove :
  int32 ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rRemove :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class tStat :
  int32 ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
  end

class rStat :
  int ->
  stat option ->
  object
    val mutable mtype : int
    val mutable tag : int
    method deserialize : string -> unit
    method mtype : int
    method serialize : string
    method tag : int
    method stat : stat
  end
