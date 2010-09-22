open Ocamlbuild_plugin
open Command

let clibdir = "-L/usr/lib";;
let clibs = "-lutil";;
let obj_files = "ocaml_forkpty.o";;

(*let cc = env "%.c" in*)
  (*Cmd(S[ocamlc; A "-cc"; A "gcc"; T(tags_of_pathname cc); A cc]);;*)

dispatch begin function
  | After_rules ->
      flag ["link"; "ocaml"; "byte"] (A "-custom");
      flag ["link"; "ocaml"] (S[A obj_files; A "-cclib"; A clibdir; A "-cclib"; A clibs]);
  | _ -> ()
end
