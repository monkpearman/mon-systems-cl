;;; :FILE-CREATED <Timestamp: #{2011-01-08T01:31:56-05:00Z}#{11016} - by MON>
;;; :FILE mon-systems/docs.lisp
;;; ==============================

;;
;; :SOURCE sbcl/doc/manual/docstrings.lisp & :SOURCE alexandria/docs
;;  But an improved version exists at git://gitorious.org/iolib/texinfo-docstrings.git
;;
;; (sb-posix:chdir (sb-posix:getcwd))
;;
;; (mon-docs:document-package :mon
;; 		  (make-pathname :directory '(<PATH>)
;; 				 :name "mon"
;; 				 :type "texinfo"))

;; (mon-docs:document-package :mon
;;                            (make-pathname
;;                             :directory `,(pathname-directory (merge-pathnames "docs/" (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))))
;;                             :name "mon"
;;                             :type "texinfo"))

;;
;; :NOTE The first char in a docstring _can not_ be a space e.g #\Space or this fails.
;;
;;; ==============================
;; :TODO
;;
;; - Currently not identifying `:EXAMPLE` at BOL
;;  Should wrap these in:
;;
;;  @example
;;  :EXAMPLE {... <EXAMPLE> ... }
;;  @end example
;; Or,
;; @lisp
;; :EXAMPLE {... <EXAMPLE> ... }
;; @end lisp
;;
;;  - Currently converting `:SEE-ALSO` at BOL to `@code{:see-also}`
;;    This should be converted to some sort of @xref list, e.g. `@xref{Output Variables}. `(@pxref{Edebug Execution Modes})'
;;
;; :FILE -> @file{<FNAME>}
;;
;; ":NOTE" ->  @strong{Note}:
;;
;;
;; (docstring 'mon::string-trim-whitespace 'function)

;; (collect-lisp-section
;;
;; (let ((*texinfo-escaped-chars* "@{}:"))
;;   (escape-for-texinfo  (docstring 'mon::string-trim-whitespace 'function)))
;;

;;; ==============================
;;; -*- lisp -*-

;;;; A docstring extractor for the sbcl manual.  Creates
;;;; @include-ready documentation from the docstrings of exported
;;;; symbols of specified packages.

;;;; This software is part of the SBCL software system. SBCL is in the
;;;; public domain and is provided with absolutely no warranty. See
;;;; the COPYING file for more information.
;;;;
;;;; Written by Rudi Schlatte <rudi@constantly.at>, mangled
;;;; by Nikodemus Siivola.

;;;; TODO
;;;; * Verbatim text
;;;; * Quotations
;;;; * Method documentation untested
;;;; * Method sorting, somehow
;;;; * Index for macros & constants?
;;;; * This is getting complicated enough that tests would be good
;;;; * Nesting (currently only nested itemizations work)
;;;; * doc -> internal form -> texinfo (so that non-texinfo format are also
;;;;   easily generated)

;;;; FIXME: The description below is no longer complete. This
;;;; should possibly be turned into a contrib with proper documentation.

;;;; Formatting heuristics (tweaked to format SAVE-LISP-AND-DIE sanely):
;;;;
;;;; Formats SYMBOL as @code{symbol}, or @var{symbol} if symbol is in
;;;; the argument list of the defun / defmacro.
;;;;
;;;; Lines starting with * or - that are followed by intented lines
;;;; are marked up with @itemize.
;;;;
;;;; Lines containing only a SYMBOL that are followed by indented
;;;; lines are marked up as @table @code, with the SYMBOL as the item.


;;; ==============================



;;; ==============================

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-introspect))

;; :WAS (defpackage :sb-texinfo
(defpackage :mon-docs
  (:use :cl :sb-mop)
  (:shadow #:documentation)
  (:export
   ;;  documented functions
   #:generate-includes
   #:document-package
   #:collect-documentation
   #:collect-symbol-documentation
   #:collect-gf-documentation
   #:write-texinfo
   #:write-texinfo-string
   #:maybe-table-offset
   #:maybe-itemize-offset
   #:maybe-documentation
   #:escape-for-texinfo
   #:locate-symbols
   #:texinfo-line
   #:lisp-section-p
   #:string-lines
   #:indentation
   #:alphanumize
   #:name     ; I don't liek this symbol-name, rename it.
   #:node-name
   #:title-name
   ;; documented variables and params.
   #:*texinfo-documentation-types*
   #:*texinfo-escaped-chars*
   #:*itemize-start-characters*
   #:*symbol-characters*
   #:*characters-to-drop*
   #:*character-replacements*
   #:*undocumented-packages*
   #:*not-symbols*)

  (:documentation
   "Tools to generate TexInfo documentation from docstrings."))

;; :WAS (in-package :sb-texinfo)
(in-package :mon-docs)

;;;; various specials and parameters

(defvar *texinfo-output* nil)
(defvar *texinfo-variables* nil)
(defvar *documentation-package* nil)

;; :OLD (defvar *base-package* nil)

(defparameter *undocumented-packages* '(sb-pcl sb-int sb-kernel sb-sys sb-c)
"list of SBCL internal packages that won't get documented.~%
:SEE-ALSO `*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'.~%▶▶▶")

(defparameter *texinfo-documentation-types*
  '(compiler-macro
    function
    method-combination
    setf
    ;;structure  ; also handled by `type'
    type
    variable)
  "A list of symbols accepted as second argument of `documentation'
:SEE-ALSO `*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'.~%▶▶▶")

(defparameter *character-replacements*
  '((#\* . "star") (#\/ . "slash") (#\+ . "plus")
    (#\< . "lt") (#\> . "gt")
    ;; :NOTE Note originally present Distinct from SBCL's docstrings.lisp
    (#\= . "equals"))
  "Characters and their replacement names that `alphanumize' uses. If
the replacements contain any of the chars they're supposed to replace,
you deserve to lose.
:SEE-ALSO `*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'~%▶▶▶")

(defparameter *characters-to-drop* '(#\\ #\` #\')
  "Characters that should be removed by `alphanumize'.~%
:SEE-ALSO `*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'.~%▶▶▶")

;; :NOTE If we add "►" here we could get #\@ inserted
(defparameter *texinfo-escaped-chars* "@{}"
  "Characters that must be escaped with #\@ for Texinfo.~%
:SEE-ALSO `*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'.~%▶▶▶")

(defparameter *itemize-start-characters* '(#\* #\-)
  "Characters that might start an itemization in docstrings when at the start of a line.
:SEE-ALSO `*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'.~%▶▶▶")

;; Having ":" here is likely problematic b/c this is the char we use to prefix new
;; sections, :SEE, :SEE-ALSO :EXAMPLE etc.
(defparameter *symbol-characters*
  ;; :OLD "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890*:-+&#'"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ*:-+&#'"
  "List of characters that make up symbols in a docstring.~%
:SEE-ALSO `*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'.~%▶▶▶")

(defparameter *symbol-delimiters* "`," ;; :WAS " ,.!?;"
"The characters that delimit symbols.~%
`*texinfo-documentation-types*', `*texinfo-escaped-chars*',
`*itemize-start-characters*', `*symbol-characters*', `*characters-to-drop*',
`*character-replacements*', `*undocumented-packages*'.~%▶▶▶" )

(defparameter *ordered-documentation-kinds*
  '(package type structure condition class macro))

;;;; utilities

(defun flatten (list)
  (cond ((null list)
         nil)
        ((consp (car list))
         (nconc (flatten (car list)) (flatten (cdr list))))
        ((null (cdr list))
         (cons (car list) nil))
        (t
         (cons (car list) (flatten (cdr list))))))

(defun whitespacep (char)
  (find char #(#\tab #\space #\page)))

(defun setf-name-p (name)
  (or (symbolp name)
      (and (listp name) (= 2 (length name)) (eq (car name) 'setf))))

(defgeneric specializer-name (specializer))

(defmethod specializer-name ((specializer eql-specializer))
  (list 'eql (eql-specializer-object specializer)))

(defmethod specializer-name ((specializer class))
  (class-name specializer))

(defun ensure-class-precedence-list (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (class-precedence-list class))

(defun specialized-lambda-list (method)
  ;; courtesy of AMOP p. 61
  (let* ((specializers (method-specializers method))
         (lambda-list (method-lambda-list method))
         (n-required (length specializers)))
    (append (mapcar (lambda (arg specializer)
                      (if  (eq specializer (find-class 't))
                           arg
                           `(,arg ,(specializer-name specializer))))
                    (subseq lambda-list 0 n-required)
                    specializers)
           (subseq lambda-list n-required))))


(defun string-lines (string)
  "Lines in STRING as a vector.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (coerce (with-input-from-string (s string)
            (loop for line = (read-line s nil nil)
               while line collect line))
          'vector))

(defun indentation (line)
  "Position of first non-SPACE character in LINE.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (position-if-not (lambda (c) (char= c #\Space)) line))

(defun docstring (x doc-type)
  (cl:documentation x doc-type))

(defun flatten-to-string (list)
  (format nil "~{~A~^-~}" (flatten list)))


(defun alphanumize (original)
  "Construct a string without characters like *`' that will f-star-ck up
filename handling. See `*character-replacements*' and `*characters-to-drop*' for
customization.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let ((name (remove-if (lambda (x) (member x *characters-to-drop*))
                         (if (listp original)
                             (flatten-to-string original)
                             (string original))))
        (chars-to-replace (mapcar #'car *character-replacements*)))
    (flet ((replacement-delimiter (index)
             (cond ((or (< index 0) (>= index (length name))) "")
                   ((alphanumericp (char name index)) "-")
                   (t ""))))
      (loop for index = (position-if #'(lambda (x) (member x chars-to-replace))
                                     name)
         while index
         do (setf name (concatenate 'string (subseq name 0 index)
                                    (replacement-delimiter (1- index))
                                    (cdr (assoc (aref name index)
                                                *character-replacements*))
                                    (replacement-delimiter (1+ index))
                                    (subseq name (1+ index))))))
    name))

;;;; generating various names

(defgeneric name (thing)
  (:documentation
   "Name for a documented THING.~%
Names are either symbols or lists of symbols.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"))

(defmethod name ((symbol symbol))
  symbol)

(defmethod name ((cons cons))
  cons)

(defmethod name ((package package))
  ;; :OLD (short-package-name package))
  (package-name package))

(defmethod name ((method method))
  (list
   (generic-function-name (method-generic-function method))
   (method-qualifiers method)
   (specialized-lambda-list method)))

;;; Node names for DOCUMENTATION instances

(defgeneric nname-using-kind/name (kind name doc))

(defmethod name-using-kind/name (kind (name string) doc)
  (declare (ignore kind doc))
  name)

(defmethod name-using-kind/name (kind (name symbol) doc)
  (declare (ignore kind))
  ;; :OLD (format nil "~@[~A:~]~A" (short-package-name (get-package doc)) name))
  ;; (format nil "~A:~A" (package-name (get-package doc)) name))
  (format nil "~A ~A" (package-name (get-package doc)) name))

(defmethod name-using-kind/name (kind (name list) doc)
  (declare (ignore kind))
  (assert (setf-name-p name))
  ;; :OLD (format nil "(setf ~@[~A:~]~A)" (short-package-name (get-package doc)) (second name)))
  ;; :WAS (format nil "(setf ~A:~A)" (package-name (get-package doc)) (second name)))
  (format nil "(setf ~A ~A)" (package-name (get-package doc)) (second name)))

(defmethod name-using-kind/name ((kind (eql 'method)) name doc)
  ;; :WAS
  ;; (format nil "~A~{ ~A~} ~A"
  ;;         (name-using-kind/name nil (first name) doc)
  ;;         (second name)
  ;;         (third name)))
  (flet ((specializers (ll)
           (let (result)
             (dolist (arg ll)
               (cond
                 ((member arg lambda-list-keywords) (return))
                 ((atom arg) (push t result))
                 (t (push (second arg) result))))
             (nreverse result))))
    (format nil "~A~{ ~A~} ~A"
            (name-using-kind/name nil (first name) doc)
            (second name)
            (specializers (third name)))))
x
(defun node-name (doc)
  "Return TexInfo node name as a string for a DOCUMENTATION instance.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let ((kind (get-kind doc)))
    (format nil "~:(~A~) ~(~A~)" kind (name-using-kind/name kind (get-name doc) doc))))

;; :OLD
;; (defun short-package-name (package)
;;   (unless (eq package *base-package*)
;;     (car (sort (copy-list (cons (package-name package) (package-nicknames package)))
;;                #'< :key #'length))))

(defun package-shortest-name (package)
  (let* ((names (cons (package-name package) (package-nicknames package)))
         (sorted (sort (copy-list names) #'< :key #'length)))
    (car sorted)))

(defun package-macro-name (package)
  (let ((short-name (package-shortest-name package)))
    (remove-if-not #'alpha-char-p (string-downcase short-name))))

;;; Definition titles for DOCUMENTATION instances

(defgeneric title-using-kind/name (kind name doc))

(defmethod title-using-kind/name (kind (name string) doc)
  (declare (ignore kind doc))
  name)

(defmethod title-using-kind/name (kind (name symbol) doc)
  (declare (ignore kind))
  ;; :OLD (format nil "~@[~A:~]~A" (short-package-name (get-package doc)) name))
  (let* ((symbol-name (symbol-name name))
         (earmuffsp (and (char= (char symbol-name 0) #\*)
                         (char= (char symbol-name (1- (length symbol-name))) #\*)
                         (some #'alpha-char-p symbol-name))))
    (if earmuffsp
        (format nil "@~A{@earmuffs{~A}}" (package-macro-name (get-package doc)) (subseq symbol-name 1 (1- (length symbol-name))))
        (format nil "@~A{~A}" (package-macro-name (get-package doc)) name))))

(defmethod title-using-kind/name (kind (name list) doc)
  (declare (ignore kind))
  (assert (setf-name-p name))
  ;; :OLD (format nil "(setf ~@[~A:~]~A)" (short-package-name (get-package doc)) (second name)))
  (format nil "@setf{@~A{~A}}" (package-macro-name (get-package doc)) (second name)))

(defmethod title-using-kind/name ((kind (eql 'method)) name doc)
  (format nil "~{~A ~}~A"
          (second name)
          (title-using-kind/name nil (first name) doc)))

(defun title-name (doc)
  "Returns a string to be used as name of the definition.
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (string-downcase (title-using-kind/name (get-kind doc) (get-name doc) doc)))

(defun include-pathname (doc)
  (let* ((kind (get-kind doc))
         (name (nstring-downcase
                (if (eq 'package kind)
                    (format nil "package-~A" (alphanumize (get-name doc)))
                    (format nil "~A-~A-~A"
                            (case (get-kind doc)
                              ((function generic-function) "fun")
                              (structure "struct")
                              (variable "var")
                              (otherwise (symbol-name (get-kind doc))))
                            ;; :OLD (alphanumize (let ((*base-package* nil))
                            ;;                (short-package-name (get-package doc))))
                            (alphanumize (package-name (get-package doc)))
                            (alphanumize (get-name doc)))))))
    (make-pathname :name name  :type "texinfo")))

;;;; documentation class and related methods

;;; :NOTE Would be good to add some additional methods to transform the value in the :string slot.

(defclass documentation ()
  ((name :initarg :name :reader get-name)
   (kind :initarg :kind :reader get-kind)
   (string :initarg :string :reader get-string)
   (children :initarg :children :initform nil :reader get-children)
   (package :initform *documentation-package* :reader get-package)))

(defmethod print-object ((documentation documentation) stream)
  (print-unreadable-object (documentation stream :type t)
    (princ (list (get-kind documentation) (get-name documentation)) stream)))

(defgeneric make-documentation (x doc-type string))

(defmethod make-documentation ((x package) doc-type string)
  (declare (ignore doc-type))
  (make-instance 'documentation
                 :name (name x)
                 :kind 'package
                 :string string))

(defmethod make-documentation (x (doc-type (eql 'function)) string)
  (declare (ignore doc-type))
  (let* ((fdef (and (fboundp x) (fdefinition x)))
         (name x)
         (kind (cond ((and (symbolp x) (special-operator-p x))
                      'special-operator)
                     ((and (symbolp x) (macro-function x))
                      'macro)
                     ((typep fdef 'generic-function)
                      (assert (or (symbolp name) (setf-name-p name)))
                      'generic-function)
                     (fdef
                      (assert (or (symbolp name) (setf-name-p name)))
                      'function)))
         (children (when (eq kind 'generic-function)
                     (collect-gf-documentation fdef))))
    (make-instance 'documentation
                   :name (name x)
                   :string string
                   :kind kind
                   :children children)))

(defmethod make-documentation ((x method) doc-type string)
  (declare (ignore doc-type))
  (make-instance 'documentation
                 :name (name x)
                 :kind 'method
                 :string string))

(defmethod make-documentation (x (doc-type (eql 'type)) string)
  (make-instance 'documentation
                 :name (name x)
                 :string string
                 :kind (etypecase (find-class x nil)
                         (structure-class 'structure)
                         (standard-class 'class)
                         (sb-pcl::condition-class 'condition)
                         ((or built-in-class null) 'type))))

(defmethod make-documentation (x (doc-type (eql 'variable)) string)
  (make-instance 'documentation
                 :name (name x)
                 :string string
                 :kind (if (constantp x)
                           'constant
                           'variable)))

(defmethod make-documentation (x (doc-type (eql 'setf)) string)
  (declare (ignore doc-type))
  (make-instance 'documentation
                 :name (name x)
                 :kind 'setf-expander
                 :string string))

(defmethod make-documentation (x doc-type string)
  (make-instance 'documentation
                 :name (name x)
                 :kind doc-type
                 :string string))

(defun maybe-documentation (x doc-type)
  "Returns a DOCUMENTATION instance for X and DOC-TYPE, or NIL if
there is no corresponding docstring.
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let ((docstring (docstring x doc-type)))
    (when docstring
      (make-documentation x doc-type docstring))))

(defun lambda-list (doc)
  (case (get-kind doc)
    ((package constant variable type structure class condition nil)
     nil)
    (method
     (third (get-name doc)))
    (t
     ;; :KLUDGE Eugh.
     ;;
     ;; believe it or not, the above comment was written before CSR
     ;; came along and obfuscated this.  (2005-07-04)
     (when (symbolp (get-name doc))
       (labels ((clean (x &key optional key)
                  (typecase x
                    (atom x)
                    ((cons (member &optional))
                     (cons (car x) (clean (cdr x) :optional t)))
                    ((cons (member &key))
                     (cons (car x) (clean (cdr x) :key t)))
                    ((cons (member &whole &environment))
                     ;; Skip these
                     (clean (cdr x) :optional optional :key key))
                    ((cons cons)
                     (cons
                      (cond (key (if (consp (caar x))
                                     (caaar x)
                                     (caar x)))
                            (optional (caar x))
                            (t (clean (car x))))
                      (clean (cdr x) :key key :optional optional)))
                    (cons
                     (cons
                      (cond ((or key optional) (car x))
                            (t (clean (car x))))
                      (clean (cdr x) :key key :optional optional))))))
         ;; :WAS (clean (sb-introspect:function-lambda-list (get-name doc))))))))
         ;; :SEE https://sourceforge.net/p/sbcl/sbcl/ci/0c844eaf20701e9551fefbc1e8d98ee9a09b53ad/tree/doc/manual/docstrings.lisp
         (multiple-value-bind (ll unknown) (sb-introspect:function-lambda-list (get-name doc))
           (if unknown
               (values nil t)
               (clean ll))))))))

;;; ==============================
(defun get-string-name (x)
  (let ((name (get-name x)))
    (cond ((symbolp name)
           (symbol-name name))
          ((and (consp name) (eq 'setf (car name)))
           (symbol-name (second name)))
          ((stringp name)
           name)
          (t
           (error "Don't know which symbol to use for name ~S" name)))))

(defun documentation< (x y)
  (let ((p1 (position (get-kind x) *ordered-documentation-kinds*))
        (p2 (position (get-kind y) *ordered-documentation-kinds*)))
    (if (or (not (and p1 p2)) (= p1 p2))
        (string< (get-string-name x) (get-string-name y))
        (< p1 p2))))

;;;; turning text into texinfo

(defun escape-for-texinfo (string &optional downcasep)
  "Return STRING with characters in `*TEXINFO-ESCAPED-CHARS*' escaped with #\@.
Optionally DOWNCASE \(a boolean\) the result.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let ((result (with-output-to-string (s)
                  (loop for char across string
                        when (find char *texinfo-escaped-chars*)
                        do (write-char #\@ s)
                        do (write-char char s)))))
    (if downcasep (nstring-downcase result) result)))

(defun empty-p (line-number lines)
  (and (< -1 line-number (length lines))
       (not (indentation (svref lines line-number)))))

;;; line markups

;; :WAS (defvar *not-symbols* '("ANSI" "CLHS" "UNIX"))
(defvar *not-symbols* '("CLHS"
                        "ANSI"
                        "UNIX"
			;; :ADDED
			"ASCII"
			":SEE-ALSO"
                        "" ;; "<XREF>"
                        "<DOCSTR>"
                        "<EXAMPLE>"
                        ":EXAMPLE"
			":SEE"
                        ":NOTE"
			":EMACS-LISP-COMPAT"
			":FILE"
                        ":NOTE"
                        ":TODO"
                        ":FILE-CREATED"
                        ":FILE"
                        ":WAS"
                        ;; "▶▶▶"
                        ;; "►►►"
                        ;; "... }"
                        ;; "{ ... "
                        ;; "(URL"
                        )
  "List of strings that may appear in documentation but which which are not symbols.~%
:SEE-ALSO .~%▶▶▶")

(defun locate-symbols (line)
  "Return a list of index pairs of symbol-like parts of LINE.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  ;; This would be a good application for a regex ...
  (let (result)
    (flet ((grab (start end)
             ;; :WAS (unless (member (subseq line start end) *not-symbols*)
	     	     (unless (member (subseq line start end) *not-symbols* :test #'string=)
               (push (list start end) result)))
           (got-symbol-p (start)
             (let ((end (when (< start (length line))
                          (position #\space line :start start))))
               (when end
                 (every (lambda (char) (find char *symbol-characters*))
                        (subseq line start end))))))
      (do ((begin nil)
           (maybe-begin t)
           (i 0 (1+ i)))
	  ;; :OLD ((= i (length line))
          ((>= i (length line))
           ;; symbol at end of line
           (when (and begin (or (> i (1+ begin))
                                (not (member (char line begin) '(#\A #\I)))))
             (grab begin i))
           (nreverse result))
        (cond
          ((and begin (find (char line i) *symbol-delimiters*))
           ;; symbol end; remember it if it's not "A" or "I"
           (when (or (> i (1+ begin)) (not (member (char line begin) '(#\A #\I))))
             (grab begin i))
           (setf begin nil
                 maybe-begin t))
          ((and begin (not (find (char line i) *symbol-characters*)))
           ;; Not a symbol: abort
           (setf begin nil))
          ((and maybe-begin (not begin) (find (char line i) *symbol-characters*))
           ;; potential symbol begin at this position
           (setf begin i
                 maybe-begin nil))
          ((find (char line i) *symbol-delimiters*)
           ;; potential symbol begin after this position
           (setf maybe-begin t))
          ((and (eql #\( (char line i)) (got-symbol-p (1+ i)))
           ;; a type designator, or a function call as part of the text?
           (multiple-value-bind (exp end)
               (let ((*package* (find-package :cl-user)))
                 (ignore-errors (read-from-string line nil nil :start i)))
             (when exp
               (grab i end)
               (setf begin nil
                     maybe-begin nil
                     i end))))
          (t
           ;; Not reading a symbol, not at potential start of symbol
           (setf maybe-begin nil)))))))

(defun texinfo-line (line)
  "Format symbols in LINE texinfo-style: either as code or as variables if the
symbol in question is contained in symbols.~%
`*TEXINFO-VARIABLES*'.
:SEE-ALSO r`document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (with-output-to-string (result)
    (let ((last 0))
      (dolist (symbol/index (locate-symbols line))
        (write-string (subseq line last (first symbol/index)) result)
        (let ((symbol-name (apply #'subseq line symbol/index)))
          (format result (if (member symbol-name *texinfo-variables*
                                     :test #'string=)
                             "@var{~A}"
                             "@code{~A}")
                  (string-downcase symbol-name)))
        (setf last (second symbol/index)))
      (write-string (subseq line last) result))))

;;; lisp sections
(defun lisp-section-p (line line-number lines)
  "Return T if the given LINE looks like the start of lisp code, ie. if LINE
starts with whitespace followed by a paren or semicolon, and the previous line is empty.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let ((offset (indentation line)))
    (and offset
         (plusp offset)
         (find (find-if-not #'whitespacep line) "(;")
         (empty-p (1- line-number) lines))))

(defun collect-lisp-section (lines line-number)
  ;; :OLD
  ;; (let ((lisp (loop for index = line-number then (1+ index)
  ;;                   for line = (and (< index (length lines)) (svref lines index))
  ;;                   while (indentation line)
  ;;                   collect line)))
  ;;   (values (length lisp) `("@lisp" ,@lisp "@end lisp"))))
  (flet ((maybe-line (index)
           (and (< index (length lines)) (svref lines index))))
    (let ((lisp (loop for index = line-number then (1+ index)
                      for line = (maybe-line index)
                      while (or (indentation line)
                                ;; Allow empty lines in middle of lisp sections.
                                (let ((next (1+ index)))
                                  (lisp-section-p (maybe-line next) next lines)))
                      collect line)))
     (values (length lisp) `("@lisp" ,@lisp "@end lisp")))))

;;; itemized sections

(defun maybe-itemize-offset (line)
  "Return NIL or the indentation offset if LINE looks like it starts an item in
an itemization.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let* ((offset (indentation line))
         (char (when offset (char line offset))))
    (and offset
         (member char *itemize-start-characters* :test #'char=)
         (char= #\Space (find-if-not (lambda (c) (char= c char))
                                     line :start offset))
         offset)))

(defun collect-maybe-itemized-section (lines starting-line)
  ;; Return index of next line to be processed outside
  (let ((this-offset (maybe-itemize-offset (svref lines starting-line)))
        (result nil)
        (lines-consumed 0))
    (loop for line-number from starting-line below (length lines)
       for line = (svref lines line-number)
       for indentation = (indentation line)
       for offset = (maybe-itemize-offset line)
       do (cond
            ((not indentation)
             ;; empty line -- inserts paragraph.
             (push "" result)
             (incf lines-consumed))
            ((and offset (> indentation this-offset))
             ;; nested itemization -- handle recursively
             ;; FIXME: tables in itemizations go wrong
             (multiple-value-bind (sub-lines-consumed sub-itemization)
                 (collect-maybe-itemized-section lines line-number)
               (when sub-lines-consumed
                 (incf line-number (1- sub-lines-consumed)) ; +1 on next loop
                 (incf lines-consumed sub-lines-consumed)
                 (setf result (nconc (nreverse sub-itemization) result)))))
            ((and offset (= indentation this-offset))
             ;; start of new item
             (push (format nil "@item ~A"
                           (texinfo-line (subseq line (1+ offset))))
                   result)
             (incf lines-consumed))
            ((and (not offset) (> indentation this-offset))
             ;; continued item from previous line
             (push (texinfo-line line) result)
             (incf lines-consumed))
            (t
             ;; end of itemization
             (loop-finish))))
    ;; a single-line itemization isn't.
    (if (> (count-if (lambda (line) (> (length line) 0)) result) 1)
        (values lines-consumed `("@itemize" ,@(reverse result) "@end itemize"))
        nil)))

;;; table sections

(defun tabulation-body-p (offset line-number lines)
  (when (< line-number (length lines))
    (let ((offset2 (indentation (svref lines line-number))))
      (and offset2 (< offset offset2)))))

(defun tabulation-p (offset line-number lines direction)
  (let ((step  (ecase direction
                 (:backwards (1- line-number))
                 (:forwards (1+ line-number)))))
    (when (and (plusp line-number) (< line-number (length lines)))
      (and (eql offset (indentation (svref lines line-number)))
           (or (when (eq direction :backwards)
                 (empty-p step lines))
               (tabulation-p offset step lines direction)
               (tabulation-body-p offset step lines))))))

(defun maybe-table-offset (line-number lines)
  "Return NIL or the indentation offset if LINE looks like it starts an item in
a tabulation. Ie, if it is:~%
 (1) indented,
 (2) preceded by an empty line, another tabulation label, or a tabulation body,
 (3) and followed another tabulation label or a tabulation body.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let* ((line (svref lines line-number))
         (offset (indentation line))
         (prev (1- line-number))
         (next (1+ line-number)))
    (when (and offset (plusp offset))
      (and (or (empty-p prev lines)
               (tabulation-body-p offset prev lines)
               (tabulation-p offset prev lines :backwards))
           (or (tabulation-body-p offset next lines)
               (tabulation-p offset next lines :forwards))
           offset))))

;;; FIXME: This and itemization are very similar: could they share
;;; some code, mayhap?

(defun collect-maybe-table-section (lines starting-line)
  ;; Return index of next line to be processed outside
  (let ((this-offset (maybe-table-offset starting-line lines))
        (result nil)
        (lines-consumed 0))
    (loop for line-number from starting-line below (length lines)
          for line = (svref lines line-number)
          for indentation = (indentation line)
          for offset = (maybe-table-offset line-number lines)
          do (cond
               ((not indentation)
                ;; empty line -- inserts paragraph.
                (push "" result)
                (incf lines-consumed))
               ((and offset (= indentation this-offset))
                ;; start of new item, or continuation of previous item
                (if (and result (search "@item" (car result) :test #'char=))
                    (push (format nil "@itemx ~A" (texinfo-line line))
                          result)
                    (progn
                      (push "" result)
                      (push (format nil "@item ~A" (texinfo-line line))
                            result)))
                (incf lines-consumed))
               ((> indentation this-offset)
                ;; continued item from previous line
                (push (texinfo-line line) result)
                (incf lines-consumed))
               (t
                ;; end of itemization
                (loop-finish))))
     ;; a single-line table isn't.
    (if (> (count-if (lambda (line) (> (length line) 0)) result) 1)
        (values lines-consumed
                `("" "@table @emph" ,@(reverse result) "@end table" ""))
        nil)))

;;; section markup

(defmacro with-maybe-section (index &rest forms)
  `(multiple-value-bind (count collected) (progn ,@forms)
    (when count
      (dolist (line collected)
        (write-line line *texinfo-output*))
      (incf ,index (1- count)))))

(defun write-texinfo-string (string &optional lambda-list)
  "Try to guess as much formatting for a raw docstring as possible.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (let ((*texinfo-variables* (flatten lambda-list))
        (lines (string-lines (escape-for-texinfo string nil))))
      (loop for line-number from 0 below (length lines)
            for line = (svref lines line-number)
            do (cond
                 ((with-maybe-section line-number
                    (and (lisp-section-p line line-number lines)
                         (collect-lisp-section lines line-number))))
                 ((with-maybe-section line-number
                    (and (maybe-itemize-offset line)
                         (collect-maybe-itemized-section lines line-number))))
                 ((with-maybe-section line-number
                    (and (maybe-table-offset line-number lines)
                         (collect-maybe-table-section lines line-number))))
                 (t
                  (write-line (texinfo-line line) *texinfo-output*))))))

;;;; texinfo formatting tools

(defun hide-superclass-p (class-name super-name)
  (let ((super-package (symbol-package super-name)))
    (or
     ;; :KLUDGE We assume that we don't want to advertise internal
     ;; classes in CP-lists, unless the symbol we're documenting is
     ;; internal as well.
     (and (member super-package #.'(mapcar #'find-package *undocumented-packages*))
          (not (eq super-package (symbol-package class-name))))
     ;; KLUDGE: We don't generally want to advertise SIMPLE-ERROR or
     ;; SIMPLE-CONDITION in the CPLs of conditions that inherit them
     ;; simply as a matter of convenience. The assumption here is that
     ;; the inheritance is incidental unless the name of the condition
     ;; begins with SIMPLE-.
     (and (member super-name '(simple-error simple-condition))
          (let ((prefix "SIMPLE-"))
            (mismatch prefix (string class-name) :end2 (length prefix)))
          t ; don't return number from MISMATCH
          ))))

(defun hide-slot-p (symbol slot)
  ;; :FIXME There is no pricipal reason to avoid the slot docs fo
  ;; structures and conditions, but their DOCUMENTATION T doesn't
  ;; currently work with them the way we'd like.
  (not (and (typep (find-class symbol nil) 'standard-class)
            (docstring slot t))))

(defun texinfo-anchor (doc)
  (format *texinfo-output* "@anchor{~A}~%" (node-name doc)))

;; ;WAS
;;; :KLUDGE &AUX *PRINT-PRETTY* here means "no linebreaks please"
;; (defun texinfo-begin (doc &aux *print-pretty*)
;;   (let ((kind (get-kind doc)))
;;     ;; :OLD (format *texinfo-output* "@~A {~:(~A~)} ~({~A}~@[ ~{~A~^ ~}~]~)~%"
;;     (format *texinfo-output* "@~A {~:(~A~)} ~(~A~@[ ~{~A~^ ~}~]~)~%"
;;             (case kind
;;               ((package constant variable)
;;                "defvr")
;;               ((structure class condition type)
;;                "deftp")
;;               (t
;;                "deffn"))
;;             (map 'string (lambda (char) (if (eql char #\-) #\Space char)) (string kind))
;;             (title-name doc)
;;             ;; &foo would be amusingly bold in the pdf thanks to TeX/Texinfo
;;             ;; interactions,so we escape the ampersand -- amusingly for TeX.
;;             ;; sbcl.texinfo defines macros that expand @&key and friends to &key.
;;             (mapcar (lambda (name)
;;                       (if (member name lambda-list-keywords)
;;                           (format nil "@~A" name)
;;                           name))
;;                     (lambda-list doc)))))


;;; KLUDGE: &AUX *PRINT-PRETTY* here means "no linebreaks please"
(defun texinfo-begin (doc &aux *print-pretty*)
  (let ((kind (get-kind doc)))
    (format *texinfo-output* "@~A {~:(~A~)} ~(~A~)"
            (case kind
              ((package constant variable)
               "defvr")
              ((structure class condition type)
               "deftp")
              (t
               "deffn"))
            (map 'string (lambda (char) (if (eql char #\-) #\Space char)) (string kind))
            (title-name doc))
    (multiple-value-bind (lambda-list unknown) (lambda-list doc)
      (cond (unknown
             (format *texinfo-output* " @emph{lambda list not known}"))
            ((not lambda-list))
            (t
             ;; &foo would be amusingly bold in the pdf thanks to
             ;; TeX/Texinfo interactions,so we escape the ampersand --
             ;; amusingly for TeX.  sbcl.texinfo defines macros that
             ;; expand @andkey and friends to &key.
             (format *texinfo-output* " ~(~{~A~^ ~}~)"
                     (mapcar (lambda (name)
                               (if (member name lambda-list-keywords)
                                   (format nil "@and~A{}"
                                           (remove #\- (subseq (string name) 1)))
                                   name))
                             lambda-list)))))
    (format *texinfo-output* "~%")))

                     
;;; ==============================
(defun texinfo-index (doc)
  (let ((title (title-name doc)))
    (case (get-kind doc)
      ((structure type class condition)
       (format *texinfo-output* "@tindex ~A~%" title))
      ((variable constant)
       (format *texinfo-output* "@vindex ~A~%" title))
      ((compiler-macro function method-combination macro generic-function)
       (format *texinfo-output* "@findex ~A~%" title)))))

(defun texinfo-inferred-body (doc)
  (when (member (get-kind doc) '(class structure condition))
    (let ((name (get-name doc)))
      ;; class precedence list
      (format *texinfo-output* "Class precedence list: @code{~(~{@lw{~A}~^, ~}~)}~%~%"
              (remove-if (lambda (class)  (hide-superclass-p name class))
                         (mapcar #'class-name (ensure-class-precedence-list (find-class name)))))
      ;; slots
      (let ((slots (remove-if (lambda (slot) (hide-slot-p name slot))
                              (class-direct-slots (find-class name)))))
        (when slots
          (format *texinfo-output* "Slots:~%@itemize~%")
          (dolist (slot slots)
            (format *texinfo-output*
                    "@item ~(@code{~A}~#[~:; --- ~]~
                      ~:{~2*~@[~2:*~A~P: ~{@code{@w{~S}}~^, ~}~]~:^; ~}~)~%~%"
                    (slot-definition-name slot)
                    (remove
                     nil
                     (mapcar
                      (lambda (name things)
                        (if things
                            (list name (length things) things)))
                      '("initarg" "reader"  "writer")
                      (list
                       (slot-definition-initargs slot)
                       (slot-definition-readers slot)
                       (slot-definition-writers slot)))))
            ;; FIXME: Would be neater to handler as children
            (write-texinfo-string (docstring slot t)))
          (format *texinfo-output* "@end itemize~%~%"))))))

(defun texinfo-body (doc)
  (write-texinfo-string (get-string doc)))

(defun texinfo-end (doc)
  (write-line (case (get-kind doc)
                ((package variable constant) "@end defvr")
                ((structure type class condition) "@end deftp")
                (t "@end deffn"))
              *texinfo-output*))

(defun write-texinfo (doc)
  "Writes TexInfo for a DOCUMENTATION instance to `*TEXINFO-OUTPUT*'.
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (texinfo-anchor doc)
  (texinfo-begin doc)
  ;; :ADDED
  (texinfo-index doc)
  ;;
  (texinfo-inferred-body doc)
  (texinfo-body doc)
  (texinfo-end doc)
  ;; FIXME: Children should be sorted one way or another
  (mapc #'write-texinfo (get-children doc)))

;;;; main logic

(defun collect-gf-documentation (gf)
  "Collects method documentation for the generic function GF.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (loop for method in (generic-function-methods gf)
        for doc = (maybe-documentation method t)
        when doc
        collect doc))

(defun collect-name-documentation (name)
  (loop for type in *texinfo-documentation-types*
        for doc = (maybe-documentation name type)
        when doc
        collect doc))

(defun collect-symbol-documentation (symbol)
  "Collects all docs for a SYMBOL and (SETF SYMBOL), returns a list of
the form DOC instances.~%
:SEE `*texinfo-documentation-types*' for the possible values of doc-type.~%
:SEE-ALSO `generate-includes'.~%▶▶▶"
  (nconc (collect-name-documentation symbol)
         (collect-name-documentation (list 'setf symbol))))


(defun collect-documentation (package)
  "Collect all documentation for all external symbols of PACKAGE, as well as for
PACKAGE itself.~%
:SEE-ALSO `generate-includes'.~%▶▶▶"
  (let* ((*documentation-package* (find-package package))
         (docs nil))
    (check-type package package)
    (do-external-symbols (symbol package)
      (setf docs (nconc (collect-symbol-documentation symbol) docs)))
    (let ((doc (maybe-documentation *documentation-package* t)))
      (when doc
        (push doc docs)))
    docs))

(defmacro with-texinfo-file (pathname &body forms)
  `(with-open-file (*texinfo-output* ,pathname
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
    ,@forms))

;; :ADDED
(defun write-ifnottex ()
  ;; We use @&key, etc to escape & from TeX in lambda lists -- so we need to
  ;; define them for info as well.
  (flet ((macro (name)
                 (let ((string (string-downcase name)))
                   (format *texinfo-output* "@macro ~A~%~A~%@end macro~%" string string))))
    (macro '&allow-other-keys)
    (macro '&optional)
    (macro '&rest)
    (macro '&key)
    (macro '&body))
  ;; :ADDED
  (format *texinfo-output*
	  (mon:mapconcat #'identity
			 '("@c Some index prettification helper macros, for tricking the texindex"
			   "@c collation \"engine\""
			   "@macro earmuffs{name}"
			   "*\\name\\*"
			   "@end macro"
			   "@macro setf{name}"
			   "\(setf \\name\\\)"
			   "@end macro~%")
			 #\newline)))

;;; :NOTER A word of explanation about the iftex branch here is probably
;;; warranted.  The package information should be present for clarity, because
;;; these produce body text as well as index entries (though in info output it's
;;; more important to use a very restricted character set because the info reader
;;; parses the link, and colon as a special character).  In TeX output we make
;;; the package name unconditionally small, and arrange such that the start of
;;; the symbol name is at a constant horizontal offset, that offset being such
;;; that the longest package names have the "sb-" extending into the left margin.
;;; (At the moment, the length of the longest package name, sb-concurrency, is
;;; hard-coded).
(defun write-packageish-macro (package-name macro-name)
  (format *texinfo-output* "~
@iftex
@macro ~A{name}
{@smallertt@phantom{concurrency:}~@[@llap{~(~A~):}~]}\\name\\
@end macro
@end iftex
@ifinfo
@macro ~2:*~A{name}
\\name\\
@end macro
@end ifinfo
@ifnottex
@ifnotinfo
@macro ~:*~A{name}
\\name\\ ~@[[~(~A~)]~]
@end macro
@end ifnotinfo
@end ifnottex~%"
          macro-name package-name))

(defun write-package-macro (package)
  (let* ((package-name (package-shortest-name package))
         (macro-name (package-macro-name package)))
    (write-packageish-macro package-name macro-name)))

;; :OLD (defun generate-includes (directory packages &key (base-package :cl-user))
(defun generate-includes (directory &rest packages)
  "Create files in DIRECTORY containing Texinfo markup of all docstrings of each
exported symbol in PACKAGES.~%
DIRECTORY is created if necessary.~%
If a namestring is supplied for DIRECTORY which doesn't end in a slash, you lose.~%
The generated include files are of the form:~%
\"<doc-type>_<packagename>_<symbol-name>.texinfo\"~%
These can be included via @include statements.~%
Texinfo syntax-significant characters are escaped in symbol names, but if a
docstring contains invalid Texinfo markup, you lose.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (handler-bind ((warning #'muffle-warning))
    (let (;; :OLD (*base-package* (find-package base-package)))
	  (directory (merge-pathnames (pathname directory))))
      (ensure-directories-exist directory)
      (dolist (package packages)
        (dolist (doc (collect-documentation (find-package package)))
          (with-texinfo-file (merge-pathnames (include-pathname doc) directory)
            (write-texinfo doc))))
      ;; :ADDED
      (with-texinfo-file (merge-pathnames "ifnottex.texinfo" directory)
        (write-ifnottex))
      ;;
      (with-texinfo-file (merge-pathnames "package-macros.texinfo" directory)
        (dolist (package packages)
          (write-package-macro package))
        (write-packageish-macro nil "nopkg"))
      directory)))

(defun document-package (package &optional filename)
  "Create a file containing all available documentation for the exported symbols
of PACKAGE in Texinfo format.~%
FILENAME is a pathanme to write the generated Texinfo file to and has the form:~%
 <NAME>.<TYPE>~%
If FILENAME is not supplied, a file with the following namestring is generated:~%
\"<PACKAGENAME>.texinfo\" .~%
The definitions can be referenced using Texinfo statements like:~%
 @ref{<doc-type>_<packagename>_<symbol-name>.texinfo}.~%
Texinfo syntax-significant characters are escaped in symbol names, but if a
docstring contains invalid Texinfo markup, you lose.~%
:SEE-ALSO `document-package', `generate-includes',`collect-documentation',
`collect-symbol-documentation', `collect-gf-documentation', `write-texinfo',
`write-texinfo-string', `maybe-table-offset',`maybe-itemize-offset',
`maybe-documentation', `escape-for-texinfo', `locate-symbols', `texinfo-line',
`lisp-section-p', , `string-lines', `indentation', `alphanumize',
`name', `node-name', `title-name'.~%▶▶▶"
  (handler-bind ((warning #'muffle-warning))
    (let* ((package (find-package package))
           (filename (or filename (make-pathname
				   ;; :OLD  :name (string-downcase (short-package-name package))
                                   :name (string-downcase (package-name package))
                                   :type "texinfo")))
           (docs (sort (collect-documentation package) #'documentation<)))
      ;;
      (with-texinfo-file ;; (merge-pathnames "ifnottex.texinfo" directory)
	  (merge-pathnames
	   (make-pathname :name "ifnottex" :type "texinfo")
	   (directory-namestring filename))
	(write-ifnottex)
	(write-package-macro package))
      ;; (with-texinfo-file ;; (merge-pathnames "package-macros.texinfo" directory)
      ;; 	  (merge-pathnames (make-pathname :name "package-macros" :type "texinfo")
      ;; 			   (directory-namestring filename))
      ;; 	(dolist (package packages)
      ;;     (write-package-macro package))
      ;;   (write-packageish-macro nil "nopkg"))
      (with-texinfo-file filename
        (dolist (doc docs)
          (write-texinfo doc)))
      filename)))


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: nil
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
