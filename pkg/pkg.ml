#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let bench = Conf.(key "bench" bool ~absent:false ~doc:"Build benchmarks")

let () =
  Pkg.describe "asn1-combinators" @@ fun c ->
    let bench = Conf.value c bench in
    Ok [ Pkg.mllib ~api:["Asn"] "src/asn1-combinators.mllib";
         Pkg.test "tests/testrunner";
         Pkg.test ~cond:bench ~run:false "tests/bench";
    ]
