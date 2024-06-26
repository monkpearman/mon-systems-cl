;;; :FILE mon-systems/mon-test.asd
;;; ==============================

;;; ==============================
;;
;; (translate-logical-pathname "MON:MON-SYSTEMS;tests")
;;
;;; ==============================

(defpackage #:mon-test-system (:use :common-lisp :asdf))

(in-package #:mon-test-system)

(defsystem :mon-test
  ;; :name ""
  :author  "MON KEY"
  :maintainer "MON KEY"
  :license  "BSD"
  :description "MON tests."
  :version "1.0.0"
  :pathname  "tests/"
  :serial t
  :depends-on
  ;; :DARWIN this seems to have been causing circular dependencies WAS (:mon #+sbcl :sb-rt
  ( #+sbcl :sb-rt
    ;; :NOTE Need to add a package-nickname for :sb-rt for following:
    #-sbcl :rt)
  :components
  ((:file "package")
   (:file "test")
   (:file "timing")
   (:file "testing")))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :mon-test))))
  (pushnew :mon-test cl:*features*))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: nil
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF

