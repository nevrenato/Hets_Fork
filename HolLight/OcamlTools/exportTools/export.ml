exception EnvError of string;;
let getenv env_var =
 try Sys.getenv env_var
 with Not_found -> raise (EnvError (Printf.sprintf "Couldn't find env variable %s" env_var));;
let realpath p = let curr_cwd = Sys.getcwd (); in 
                  Sys.chdir p; let real_p = Sys.getcwd ()
                               in Sys.chdir curr_cwd; real_p;;
let hol_dir = realpath (getenv "HETS_HOL_DIR");;
let ocaml_source_dir = getenv "HETS_OCAML_LIB_DIR";;
let ocaml_tools_dir = getenv "HETS_HOLLIGHT_TOOLS";;
let use_file' s =
  if Toploop.use_file Format.std_formatter s then ()
  else (Format.print_string("Error in included file "^s);
        Format.print_newline());;

use_file' (Filename.concat ocaml_tools_dir "overload_loadfile.ml");;
use_file "hol.ml";;

use_file' (Filename.concat ocaml_tools_dir "export_helper.ml");;

Gc.compact ();;

let oc = open_out "/tmp/exportml_done" in
close_out oc;
