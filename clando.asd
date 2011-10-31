(defpackage #:clando-asd
  (:use #:cl #:asdf))

(in-package #:clando-asd)

(defsystem #:clando
  :name "Clando"
  :description "Command-line todo list manager."
  :author "Xueqiao Xu <xueqiaoxu@gmail.com>"
  :version "0.1"
  :license "MIT"
  :serial t
  :depends-on (#:sb-md5)
  :components ((:file "package") 
               (:file "settings")
               (:file "clando")))
