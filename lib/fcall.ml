type version = V9P2000

exception Unsupported_version
exception Illegal_package_tag of int
exception Package_not_complete

(* Message tags *)
let tauth = 255
let notag = 255

(* Base class for P9 request transmissions and responses. *)
class virtual fcall =
    object (self)
        method virtual mtype : int
    end

class virtual fcall_trans =
    object (self)
        inherit fcall
        method virtual serialize : String.t

        (* Serialize unsigned int values *)
        method private s_8bit_int v =
            String.make 1 (char_of_int v)
             
        method private s_16bit_int v =
            self#s_xbit_int 2 v

        method private s_32bit_int v =
            self#s_xbit_int 4 v

        method private s_64bit_int v =
            self#s_xbit_int 8 v

        method private s_xbit_int bytes v =
            let str = String.create bytes in
            for i = 0 to (bytes - 1) do
                str.[i] <- char_of_int ((v lsr (i * 8)) land 255)
            done;
            str

        method private s_str s =
            (self#s_16bit_int (String.length s)) ^ s
    end

class virtual fcall_resp =
    object (self)
        inherit fcall

        (* De-serialize unsigned int values. *)
        method private d_8bit_int str offset =
            int_of_char str.[0 + offset]

        method private d_16bit_int str offset =
            self#d_xbit_int str 2 offset

        method private d_32bit_int str offset =
            self#d_xbit_int str 4 offset

        method private d_64bit_int str offset =
            self#d_xbit_int str 8 offset

        method private d_xbit_int str bytes offset =
            let v = ref 0 in
            for i = 0 to bytes - 1 do
                v := !v + ((int_of_char str.[offset + i]) lsl (i * 4))
            done;
            !v

        method private d_str str offset =
            let len = self#d_16bit_int str offset in
            print_newline ();
            String.sub str (offset + 2) len
    end

let concat = String.concat ""

class tVersion msize =
    object (self)
        inherit fcall_trans

        val mtype = 100
        val version = V9P2000 (* No other available (?) *)

        method mtype = mtype

        method serialize =
            let vrsn_str = "9P2000" in
            let data = concat [
                self#s_8bit_int mtype;
                self#s_16bit_int notag;
                self#s_32bit_int msize;
                self#s_str vrsn_str;
            ] in
            self#s_32bit_int ((String.length data) + 4) ^ data
    end

class rVersion package =
    object (self)
        inherit fcall_resp

        val mtype = 101
        val mutable msize = 0

        initializer
            let mt = self#d_8bit_int package 4 in
            if not (mt = mtype) then raise (Illegal_package_tag mt);
            let psize = self#d_32bit_int package 0 in
            if (String.length package) < psize then raise Package_not_complete;
            (* Ignore tag. *)
            msize <- self#d_32bit_int package 7;
            let vrsn_str = self#d_str package 11 in
            if (not (vrsn_str = "9P2000")) then raise Unsupported_version

        method mtype = mtype
        method msize = msize
    end

(* Message types *)
let tauth = String.make 1 (char_of_int 102)
let rauth = String.make 1 (char_of_int 103)
let tattach = String.make 1 (char_of_int 104)
let rattach = String.make 1 (char_of_int 105)
let terror = String.make 1 (char_of_int 106) (* illegal *)
let rerror = String.make 1 (char_of_int 107)
let tflush  = String.make 1 (char_of_int 108)
let rflush  = String.make 1 (char_of_int 109)
let twalk  = String.make 1 (char_of_int 110)
let rwalk  = String.make 1 (char_of_int 111)
let topen  = String.make 1 (char_of_int 112)
let ropen  = String.make 1 (char_of_int 113)
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
