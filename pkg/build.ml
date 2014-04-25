#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "ripple" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "lib/ripple";
    Pkg.bin ~auto:true ~dst:"ripple-cli" "src/cli";
    Pkg.bin ~auto:true ~dst:"ripple-gettransactions" "src/transactions";
  ]
