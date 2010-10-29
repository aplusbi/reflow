byte:
	ocamlbuild reflow.byte

native:
	ocamlbuild reflow.native

debug:
	ocamlbuild reflow.d.byte

clean:
	ocamlbuild -clean
