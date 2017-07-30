#!/usr/bin/env ocaml

#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "BetterErrors" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.bin ~auto:true "src/BetterErrors" ~dst:"berror";
    Pkg.lib ~exts:Exts.library "src/BetterErrors";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/betterErrorsTypes";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/betterErrors";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/betterErrorsParseError";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/betterErrorsMain";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/helpers";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/terminalReporter";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/reportWarning";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/reportError";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/parseError";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/parseWarning";
    Pkg.lib ~exts:(Exts.exts [".cmo"; ".cmx"; ".cmt"; ".cmi"]) "src/Atom";
    Pkg.doc "README.md";
  ]