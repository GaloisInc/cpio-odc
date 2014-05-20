(*
 * cpio.mli
 *
 * Copyright (C) 2014, Galois, Inc.
 * All Rights Reserved.
 *
 * Written by James Bielman <jamesjb@galois.com>, 20 May 2014
 *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** A byte buffer as used by Mirage's Io_page module. *)

val find_file : string -> buffer -> buffer option
(** Find a file by name in an archive and return its contents. *)
