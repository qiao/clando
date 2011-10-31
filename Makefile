clando: clean clando.lisp run.lisp compile.lisp settings.lisp
	sbcl --script compile.lisp

.PHONY: clean install

install:
	cp clando /usr/bin/clando

clean:
	rm -f clando
