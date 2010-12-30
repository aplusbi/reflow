open Ocamlbuild_plugin
open Command

let ptyutils = "ptyutils_stub.o";;
let clibdir = "-L/usr/lib";;
let clibs = "-lutil";;

dispatch begin function
  | After_rules ->
      ocaml_lib ~extern:true ~dir:"+site-lib/extunix" "extunix";
      flag ["link"; "ocaml"; "byte"] (A "-custom");
      flag ["link"; "ocaml"; "use_ptyutils"] (S[A "-cclib"; A clibdir; A "-cclib"; A clibs]);
      dep ["link"; "ocaml"; "use_ptyutils"] [ptyutils]
  | _ -> ()
end
