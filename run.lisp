(require :asdf)
(require :sb-md5)

(asdf:load-system :clando)

(defun main ()
  (apply #'clando:main sb-ext:*posix-argv*))
