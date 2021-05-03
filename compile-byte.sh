#!/usr/bin/bash
ocamlfind ocamlc automata.mli automata.ml life.mli life.ml -package containers -package graphics -package unix -linkpkg -o life.byte
