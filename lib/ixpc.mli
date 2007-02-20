type t

exception Socket_error
exception IXPError

type stat = {
  ktype : int;
  kdev : int;
  q_type : int;
  q_vers : int;
  q_path : int;
  mode : int;
  atime : int;
  mtime : int;
  length : int;
  name : string;
  uid : string;
  gid : string;
  muid : string;
}

val oREAD : int
val oWRITE : int
val oRDWR : int
val oEXEC : int
val oEXCL : int
val oTRUNC : int
val oREXEC : int
val oRCLOSE : int
val oAPPEND : int

val connect : string -> Unix.file_descr
val version : Unix.file_descr -> unit
val attach : Unix.file_descr -> string -> string -> int

val send : Unix.file_descr -> string -> unit
val receive : Unix.file_descr -> string

val walk : Unix.file_descr -> int -> bool -> string -> int
val walk_open : Unix.file_descr -> int -> bool -> string -> int -> int * int

val fopen : Unix.file_descr -> int -> int -> int
val clunk : Unix.file_descr -> int -> unit


val read : Unix.file_descr -> int -> 'a -> int -> int -> string
val write : Unix.file_descr -> int -> int -> int -> int -> string -> int
val fwrite : Unix.file_descr -> int -> string -> int -> int -> string -> int

val create : Unix.file_descr -> int -> string -> int -> int -> int
val remove : Unix.file_descr -> int -> unit

val unpack_files : string -> stat list
