open Ocamlbuild_plugin

let tag_atdgen env =
  List.iter (fun f -> tag_file (env f) ["package(atdgen)"])

let () = dispatch begin function

  | After_rules ->

    (* atdgen rules *)

    rule "atdgen: .atd -> _t.ml*"
      ~prods:["%_t.ml";"%_t.mli"]
      ~dep:"%.atd"
      (begin fun env build ->
        let atdgen = "atdgen" in
        tag_atdgen env ["%_t.ml";"%_t.mli"];
        Cmd (S [A atdgen; A "-t"; P (env "%.atd")]);
      end);

    rule "atdgen: .atd -> _j.ml*"
      ~prods:["%_j.ml";"%_j.mli";]
      ~dep:"%.atd"
      (begin fun env build ->
        let atdgen = "atdgen" in
        tag_atdgen env ["%_j.ml"; "%_j.mli"];
        Cmd (S [A atdgen; A "-j"; A "-j-std"; P (env "%.atd")]);
      end);

    rule "atdgen: .atd -> _v.ml*"
      ~prods:["%_v.ml";"%_v.mli";]
      ~dep:"%.atd"
      (begin fun env build ->
        let atdgen = "atdgen" in
        tag_atdgen env ["%_v.ml";"%_v.mli";];
        Cmd (S [A atdgen; A "-v"; P (env "%.atd")]);
      end);

    ()

  | _ -> ()

end
