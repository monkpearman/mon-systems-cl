;;; :FILE-CREATED <Timestamp: #{2012-02-28T18:11:39-05:00Z}#{12092} - by MON>
;;; :FILE mon-systems/sldb-specials-deprecated.lisp
;;; ==============================

;; :NOTE swank:*sldb-printer-bindings* is no longer present in slime from cvs as of slime-20120208-cvs
;;
;; How is swank:sldb-loop swank::call-with-bindings and swank::with-bindings supposed to allow
;; dynamic binding of (for example) swank::*sldb-bitvector-length*
;; swank::*sldb-string-length* when the variables are in the car positions of
;; swank:*sldb-printer-bindings* are hardwired?
;; 

(in-package #:mon)

;; (swank:simple-completions "*sldb" (find-package "SWANK"))

(defvar *sldb-bitvector-length-no-miser-me* 50)

(defvar *sldb-string-length-no-miser-me* 160)

(defun sldb-printer-bindings-no-miser-me ()
  `((*print-pretty* . t) 
    (*print-level* . 4)
    (*print-length* . 10) 
    (*print-circle* . t) 
    (*print-readably*)
    ;; (*print-pprint-dispatch* . ,*print-pprint-dispatch*)
    (*print-pprint-dispatch* . ,swank::*sldb-pprint-dispatch-table*)
    (*print-gensym* . t) 
    (*print-base* . 10) 
    (*print-radix*)
    (*print-array* . t) 
    (*print-lines*) 
    (*print-escape* . t)
    (*print-right-margin* . 65)
    ;; (swank::*sldb-string-length* . 160))
    (swank::*sldb-string-length* . ,mon:*sldb-string-length-no-miser-me*)
    ;; (swank::*sldb-bitvector-length* . 25)    
    (swank::*sldb-bitvector-length* . ,mon:*sldb-bitvector-length-no-miser-me*)))

;; ;; (export '(*sldb-bitvector-length-no-miser-me*
;; ;;           *sldb-string-length-no-miser-me*
;; ;;           *sldb-printer-bindings-no-miser-me*))
;; ;; swank:*sldb-printer-bindings*
(defvar *sldb-printer-bindings-no-miser-me*
  #+swank 
  (sldb-printer-bindings-no-miser-me)
  #-swank
  `((*print-pretty* . t) (*print-level* . 4)
    (*print-length* . 10) (*print-circle* . t) (*print-readably*)
    (*print-pprint-dispatch* . ,*print-pprint-dispatch*)
    (*print-gensym* . t) (*print-base* . 10) (*print-radix*)
    (*print-array* . t) (*print-lines*) (*print-escape* . t)
    (*print-right-margin* . 65)
    ;; (swank::*sldb-bitvector-length* . 25)
    ;; (swank::*sldb-string-length* . 160)
    ))

(fundoc 'sldb-printer-bindings-no-miser-me
"Return an alist of printer-bindings for use as a dynamic binding for `swank::*sldb-printer-bindings*'.~%~@
Evaluated in an environment where bindings of `*sldb-string-length-no-miser-me*'
`*sldb-bitvector-length-no-miser-me*' are in effect.~%~@
:EXAMPLE~%~
 \(list \(sldb-printer-bindings-no-miser-me\)
      \(let* \(\(mon:*sldb-string-length-no-miser-me* 14\)
             \(mon:*sldb-bitvector-length-no-miser-me* 10\)
             \(dynamic-bindings \(sldb-printer-bindings-no-miser-me\)\)\)
        dynamic-bindings\)\)~%~@
:SEE-ALSO `*sldb-string-length-no-miser-me*',`*sldb-bitvector-length-no-miser-me*',
`*sldb-printer-bindings-no-miser-me*'.~%▶▶▶")

(vardoc '*sldb-string-length-no-miser-me*
"Number of columns that swank debugger will print bit-vectors before trunctating with \"...\".~%~@
:NOTE `swank::*sldb-bitvector-length*' is defaulted to 25.~%~@
:SEE-ALSO `*sldb-string-length-no-miser-me*', `*sldb-printer-bindings-no-miser-me*'.~%▶▶▶")

(vardoc '*sldb-string-length-no-miser-me*
"Number of columns that swank debugger will print strings before trunctating with \"...\".~%~@
:NOTE `swank::*sldb-string-length*' is defaulted to 50.~%~@
:SEE-ALSO `*sldb-bitvector-length-no-miser-me*', `*sldb-printer-bindings-no-miser-me*'.~%▶▶▶")

(vardoc '*sldb-printer-bindings-no-miser-me*
   "An alist of *print-<FOO>* printer bindings for use as a dynamic binding when in swanks debugger loop.~%~@
:SEE-ALSO `mon:*sldb-bitvector-length-no-miser-me*',
`*sldb-string-length-no-miser-me*', `swank::*sldb-pprint-dispatch-table*',
`swank::*sldb-condition-printer*', `swank::with-bindings',
`swank::call-with-bindings', `swank::sldb-loop', `swank:swank-debugger-hook',
`swank:*global-debugger*', `cl:*debugger-hook*', `swank::*sldb-string-length*',
`swank::*sldb-bitvector-length*', `swank:*macroexpand-printer-bindings*'.~%▶▶▶")


;;; ==============================
;;; EOF
