clando: clean clando.lisp run.lisp compile.lisp
	sbcl --script compile.lisp

.PHONY: clean install

install:
	echo "not implemented"

clean:
	rm -f clando
