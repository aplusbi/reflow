open Ocamlbuild_plugin
open Command

let ptyutils = "ocaml_forkpty.o";;
let clibdir = "-L/usr/lib";;
let clibs = "-lutil";;

(*let cc = env "%.c" in*)
  (*Cmd(S[ocamlc; A "-cc"; A "gcc"; T(tags_of_pathname cc); A cc]);;*)

dispatch begin function
  | After_rules ->
      flag ["link"; "ocaml"; "byte"] (A "-custom");
      flag ["link"; "ocaml"; "use_ptyutils"] (S[A "-cclib"; A clibdir; A "-cclib"; A clibs]);
      dep ["link"; "ocaml"; "use_ptyutils"] [ptyutils]
  | _ -> ()
end
