(**
   goal :
   visit all the cmx file in a list of dirs,
   get that list from the environment settings.
visit all the modules and all the symbols in the compiler.
dont have name them all manually, import them all.
print out the signatures.

This is not working well, it turns out that we cannot reference the symbold of the loaded modules.
 **)
open Ocaml_common

let update_search_path () =
  try
    let extra_paths = [
            "/home/mdupont/.opam/4.14.0/lib/alcotest";
            "/home/mdupont/.opam/4.14.0/lib/base";
            "/home/mdupont/.opam/4.14.0/lib/base/base_internalhash_types";
            "/home/mdupont/.opam/4.14.0/lib/base/caml";
            "/home/mdupont/.opam/4.14.0/lib/base/shadow_stdlib";
            "/home/mdupont/.opam/4.14.0/lib/ocaml";
            "/home/mdupont/.opam/4.14.0/lib/ocaml-compiler-libs/common";
            "/home/mdupont/.opam/4.14.0/lib/ocaml-compiler-libs/shadow";
            "/home/mdupont/.opam/4.14.0/lib/ocaml/compiler-libs";
            "/home/mdupont/.opam/4.14.0/lib/ppx_derivers";
            "/home/mdupont/.opam/4.14.0/lib/ppx_tools/ast_lifter";
            "/home/mdupont/.opam/4.14.0/lib/ppxlib";
            "/home/mdupont/.opam/4.14.0/lib/ppxlib/ast";
            "/home/mdupont/.opam/4.14.0/lib/ppxlib/astlib";
            "/home/mdupont/.opam/4.14.0/lib/ppxlib/print_diff";
            "/home/mdupont/.opam/4.14.0/lib/ppxlib/stdppx";
            "/home/mdupont/.opam/4.14.0/lib/ppxlib/traverse_builtins";
            "/home/mdupont/.opam/4.14.0/lib/re";
            "/home/mdupont/.opam/4.14.0/lib/sexplib0";
            "/home/mdupont/.opam/4.14.0/lib/stdlib-shims";
            "/home/mdupont/.opam/4.14.0/lib/uuidm";
                      ]
    in
    Clflags.include_dirs := List.rev_append extra_paths !Clflags.include_dirs
  with
  | e ->
     let backtrace = Printexc.get_backtrace () in
     Printf.eprintf "Caught exception1: %s\n" (Printexc.to_string e);
     Printf.eprintf "Backtrace:\n%s\n" backtrace


let _ = update_search_path()



let load_obj_file fn =
  (*~/2024/08/06/ocaml-macros/toplevel/opttopdirs.ml*)
  Printf.printf "Try loading %s" fn;
  try Dynlink.loadfile fn; "Loaded: true\n"
  with
  | Dynlink.Error err ->
     Printf.printf "Error while loading %s: %s.@."
       fn (Dynlink.error_message err);
     "false"
  | _ ->
     Printf.printf "Error!";
     (* print_exception_outcome ppf exn; *)
     "false"

let load_and_print path =
    Printf.printf "Path %s\n" path;
    let ret = load_obj_file path in
    Printf.printf "ret:%s" ret

let load_object_file_of_type suffix base  dir   =
  if Filename.check_suffix dir suffix
  then
    load_and_print (base ^ "/" ^ dir)
  else
    Printf.printf "Skip suffix %s path: %s/%s\n" suffix base dir



let rec map x f =
  match x with
  | head :: tail -> f head; map tail f
  | [] -> ()

let print_dirs dir dirs =
  Printf.printf "Print Dirs: %s\n" dir;
  let print_dir1 = load_object_file_of_type ".cmxs" dir in
  map dirs print_dir1;
  (* let print_dir2 = load_object_file_of_type ".cmxa" dir in *)
  (* map dirs print_dir2; *)
  let print_dir3 = load_object_file_of_type ".cmo" dir in
  map dirs print_dir3
  (* let print_dir4 = load_object_file_of_type ".cma" dir in *)
  (* map dirs print_dir4 *)

let scan_directory_and_load dir =
  Printf.printf "Loading Path %s\n" dir;
  (print_dirs dir (Array.to_list (Sys.readdir dir)))

let print_root = load_object_file_of_type ".cmxs" "ROOT"
let () = map !Clflags.include_dirs print_root
let () = map !Clflags.include_dirs scan_directory_and_load
let () = map !Clflags.include_dirs scan_directory_and_load (* try a second time*)
let _ = Compmisc.init_path ()
let env = Compmisc.initial_env()

let rec print_path(p:Ocaml_common.Path.t):string =
    match p with
  | Pident i ->  let n = (Ident.name i) in "ident:" ^ n
  | Pdot (a,b) -> let a1 = print_path a in a1  ^ "." ^ b
  | Papply (a,b)  -> let a1 = print_path a in let b1 = print_path b in  a1 ^ "apply" ^ b1

let print_module_names (name : string)
      (_path : Ocaml_common.Path.t)
      (_declaration : Ocaml_common.Types.module_declaration)
      (accumulator : unit) =
  Printf.printf "Path: %s %s\n" (print_path _path) name;
  accumulator

let () =
  Printf.printf "PrintModules:\n";
  try
    Printf.printf "beforefold:\n";
    Ocaml_common.Env.fold_modules print_module_names None env ();
      print_endline "Hello, World!"
        with
          e ->
          let backtrace = Printexc.get_backtrace () in
          Printf.eprintf "Caught exception: %s\n" (Printexc.to_string e);
          Printf.eprintf "Backtrace:\n%s\n" backtrace;
          raise e
