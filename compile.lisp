(load "run")
(save-lisp-and-die "clando" :executable t :toplevel #'main :purify t :save-runtime-options t)
