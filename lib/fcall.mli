type version = V9P2000
exception Unsupported_version of string
exception Illegal_package_type of int
exception Package_not_complete
exception Wrong_tag of int * int

val s_xbit_int : int -> int -> string
val s_int8 : int -> string
val s_int16 : int -> string
val s_int32 : int -> string
val s_int64 : int -> string
val s_str : string -> string
val d_intx : string -> int -> int -> int
val d_int8 : string -> int -> int
val d_int16 : string -> int -> int
val d_int32 : string -> int -> int
val d_int64 : string -> int -> int
val d_str : string -> int -> string

class virtual fcall :
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method virtual deserialize : String.t -> unit
    method mtype : int
    method virtual serialize : String.t
    method tag : int
  end
val concat : string list -> string
class tVersion :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    val version : version
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class rVersion :
  int ->
  object
    val mutable msize : int
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method msize : int
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class tAttach :
  int option ->
  string ->
  string ->
  object
    val fid : int
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method fid : int
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class rAttach :
  int ->
  object
    val mutable mtype : int
    val quit : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class rError :
  'a ->
  'b ->
  string option ->
  object
    val mutable message : string option
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method message : string option
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class tflush :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class rflush :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class tWalk :
  int ->
  bool ->
  string list ->
  object
    val mutable mtype : int
    val newfid : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method newfid : int
    method serialize : String.t
    method tag : int
  end
class rWalk :
  int ->
  int ->
  object
    val mutable mtype : int
    val mutable nwquid : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class tOpen :
  int ->
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class rOpen :
  int ->
  int ->
  object
    val mutable iounit : int
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method iounit : int
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class tCreate :
  int ->
  string ->
  int ->
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class rCreate :
  int ->
  int ->
  object
    val mutable iounit : int
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method iounit : int
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class tRead :
  int ->
  int ->
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class rRead :
  int ->
  string ->
  object
    val mutable data : string
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method count : int
    method data : string
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class tWrite :
  int ->
  int ->
  int ->
  string ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class rWrite :
  int ->
  int ->
  object
    val mutable count : int
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method count : int
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
class tClunk :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class rClunk :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class tRemove :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : string -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end

class rRemove :
  int ->
  object
    val mutable mtype : int
    val mutable tag : int
    method check : String.t -> unit
    method deserialize : String.t -> unit
    method mtype : int
    method serialize : String.t
    method tag : int
  end
