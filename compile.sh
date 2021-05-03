#!/usr/bin/bash
ocamlfind ocamlopt automata.mli automata.ml life.mli life.ml -package containers -package graphics -package unix -linkpkg -o life.out
