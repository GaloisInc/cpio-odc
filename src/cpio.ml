(*
 * cpio.ml
 *
 * Copyright (C) 2014, Galois, Inc.
 * All Rights Reserved.
 *
 * Written by James Bielman <jamesjb@galois.com>, 20 May 2014
 *)

open Bigarray
open Printf
open Uint32

type buffer = (char, int8_unsigned_elt, c_layout) Array1.t

(** Header for the CPIO portable ASCII (odc) format. *)
type header =
  { c_magic     : uint32
  ; c_dev       : uint32
  ; c_ino       : uint32
  ; c_mode      : uint32
  ; c_uid       : uint32
  ; c_gid       : uint32
  ; c_nlink     : uint32
  ; c_rdev      : uint32
  ; c_mtime     : uint32
  ; c_namesize  : uint32
  ; c_filesize  : uint32
  }

(** Size of the header in bytes. *)
let header_size = 76

(** An entry in a CPIO archive. *)
type entry =
  { e_header    : header
  ; e_name      : string
  ; e_file      : buffer
  }

(** Convert a bigarray of characters to a string. *)
let ba_to_string ba =
  let len = Array1.dim ba in
  let result = String.create len in
  for i = 0 to len - 1 do
    result.[i] <- ba.{i}
  done;
  result

(** Decode "len" characters of an octal string at "offset"
 * from "ba". *)
let decode offset len ba =
  let result = ref Uint32.zero in
  for i = 0 to len - 1 do
    let ch = ba.{offset + i} in
    if ch >= '0' && ch <= '7' then begin
      let n = Uint32.of_int (int_of_char ch - int_of_char '0') in
      result := Uint32.mul !result (Uint32.of_int 8);
      result := Uint32.add !result n
    end else
      raise (Invalid_argument "bad octal string")
  done;
  !result

(** Decode a CPIO header at "offset" in "ba". *)
let get_header offset ba =
  { c_magic     = decode (offset +  0)  6 ba
  ; c_dev       = decode (offset +  6)  6 ba
  ; c_ino       = decode (offset + 12)  6 ba
  ; c_mode      = decode (offset + 18)  6 ba
  ; c_uid       = decode (offset + 24)  6 ba
  ; c_gid       = decode (offset + 30)  6 ba
  ; c_nlink     = decode (offset + 36)  6 ba
  ; c_rdev      = decode (offset + 42)  6 ba
  ; c_mtime     = decode (offset + 48) 11 ba
  ; c_namesize  = decode (offset + 59)  6 ba
  ; c_filesize  = decode (offset + 65) 11 ba
  }

(** Return true if an entry is the CPIO trailer. *)
let is_last_entry e =
  e.e_name = "TRAILER!!!"

(** Decode a CPIO entry at "offset" in "ba".  Returns the
 * entry and the offset of the next header. *)
let get_entry offset ba =
  let hdr       = get_header offset ba in
  let name_size = Uint32.to_int hdr.c_namesize in
  let file_size = Uint32.to_int hdr.c_filesize in
  let name_ba   = Array1.sub ba (offset + header_size) (name_size - 1) in
  let name      = ba_to_string name_ba in
  let file      = Array1.sub ba (offset + header_size + name_size) file_size in
  ({ e_header = hdr
   ; e_name   = name
   ; e_file   = file
   }, offset + header_size + name_size + file_size)

(** Left fold a function over the entries in an archive. *)
let fold_entry f z ba =
  let rec loop f acc ba offset =
    let (e, next) = get_entry offset ba in
      if is_last_entry e
        then acc
        else loop f (f e acc) ba next
  in
    loop f z ba 0

(** Map a function over the entries in an archive. *)
let map_entry f ba =
  fold_entry (fun x l -> f x :: l) [] ba

(** Apply a side-effecting function to each entry in an archive. *)
let iter_entry f ba =
  fold_entry (fun x _ -> f x) () ba

(** Find an entry in a CPIO archive if it exists. *)
let find_entry filename ba =
  let rec loop offset =
    let (e, next) = get_entry offset ba in
      if is_last_entry e then None
      else if e.e_name = filename then Some e
      else loop next
  in
    loop 0

let find_file filename ba =
  match find_entry filename ba with
  | Some e -> Some e.e_file
  | None   -> None

(** {Test Harness} *)

(*

(** Print a CPIO header for debugging. *)
let print_header hdr =
  printf "magic:    %#o\n" (Uint32.to_int hdr.c_magic);
  printf "dev:      %u\n"  (Uint32.to_int hdr.c_dev);
  printf "ino:      %u\n"  (Uint32.to_int hdr.c_ino);
  printf "mode:     %#o\n" (Uint32.to_int hdr.c_mode);
  printf "uid:      %u\n"  (Uint32.to_int hdr.c_uid);
  printf "gid:      %u\n"  (Uint32.to_int hdr.c_gid);
  printf "nlink:    %u\n"  (Uint32.to_int hdr.c_nlink);
  printf "rdev:     %u\n"  (Uint32.to_int hdr.c_rdev);
  printf "mtime:    %u\n"  (Uint32.to_int hdr.c_mtime);
  printf "namesize: %u\n"  (Uint32.to_int hdr.c_namesize);
  printf "filesize: %u\n"  (Uint32.to_int hdr.c_filesize)

(** Print a CPIO entry for debugging. *)
let print_entry e =
  print_header e.e_header;
  printf "filename: %s\n" e.e_name;
  printf "contents:\n%s\n" (ba_to_string e.e_file)

let main =
  let (f1, f2) =
    match Sys.argv with
    | [| _; f1; f2 |] -> (f1, f2)
    | _ -> output_string stderr "usage: cpio ARCHIVE FILENAME\n";
           exit 1
  in
  let fd = Unix.openfile f1 [Unix.O_RDONLY] 0600 in
  let ba = Array1.map_file fd char c_layout false (-1) in
  match find_entry f2 ba with
  | Some e -> print_entry e
  | None   -> print_endline "file not found"

*)
