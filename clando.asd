(defpackage #:clando-asd
  (:use #:cl #:asdf))

(in-package #:clando-asd)

(defsystem #:clando
  :name "Clando"
  :description "Command-line todo list manager."
  :author "Xueqiao Xu <xueqiaoxu@gmail.com>"
  :version "0.1"
  :license "MIT"
  :depends-on (#:sb-md5)
  :components ((:module "src"
                :components ((:file "package") 
                             (:file "settings" 
                              :depends-on ("package"))
                             (:file "clando" 
                              :depends-on ("package" "settings"))))))
