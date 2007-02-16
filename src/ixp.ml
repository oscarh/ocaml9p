open Fcall

let msize = ref 1024

let address_exp = Str.regexp "unix!\\(.+\\)"

let connect () =
    let address = Sys.getenv "WMII_ADDRESS" in
    let sockaddr = Unix.ADDR_UNIX (if Str.string_match address_exp address 0 then
        Str.matched_group 1 address
    else
        address) in
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect fd sockaddr;
    fd

let send fd data =
    let size = String.length data in
    let _ = Unix.send fd data 0 size [] in
    ()

let main () =
    let fd = connect () in
    let send = send fd in
    let pkg = new tVersion !msize in
    send pkg#serialize;
    let buff = String.create !msize in
    let rlen = Unix.recv fd buff 0 !msize [] in
    (try 
        let resp = new rVersion (String.sub buff 0 rlen) in
        print_string "Got a version response! Message size is: \n";
        print_int resp#msize;
        msize := resp#msize;
        print_newline ()
    with Illegal_package_tag t -> 
        print_string "Illegal tag:"; 
        print_int t;
        print_newline ());
    Unix.shutdown fd Unix.SHUTDOWN_ALL


let _ = main ()
