clando: clando.lisp run.lisp compile.lisp
	sbcl --load compile.lisp

.PHONY: clean install

install:
	echo "not implemented"

clean:
	rm -f clando
