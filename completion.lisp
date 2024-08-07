;;; :SOURCE (URL `ftp://ftp.csl.sri.com/pub/users/gilham/xit-mod-fmg.tgz')
;;;
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10.; Package: UT -*-
;;;_____________________________________________________________________________
;;;
;;;                       System: Utilities
;;;                       Module: Completion
;;;                       Version: 1.0
;;;
;;; Copyright (c): Forschungsgruppe DRUID, Matthias Ressel
;;;                Universitaet Stuttgart
;;;
;;; File: /usr/local/lisp/xit/cl-utilities/completion.lisp
;;; File Creation Date: 09/04/92 15:20:39
;;; Last Modification Time: 10/08/92 12:15:19
;;; Last Modification By: Matthias Ressel
;;;
;;;
;;; Changes (worth to be mentioned):
;;; ================================
;;;
;;;_____________________________________________________________________________

#||

(string-completion "Ha" '("Amsterdam" "Berlin" "Bremen" "Hanau"
			"Hannover" "Stuttgart")) ==>
"Han"
("Hanau" "Hannover")

(common-prefix '("Hanau" "Hannover")) ==>
3


(filename-completion "/usr/local/druid/sounds/h") ==>
"/usr/local/druid/sounds/hal"
("/usr/local/druid/sounds/hal" "/usr/local/druid/sounds/hal2.au"
			       "/usr/local/druid/sounds/hal20.au")
27

||#

(in-package :ut)

(eval-when (compile load eval)
  (export '(string-completion file-completion common-prefix
	    filename-completion function-completion symbol-completion
	    variable-completion)))

(defun string-completion  (str list &key
				    (generator #'identity)
				    (key #'identity)
				    (result-type t))
  "Given a string STR and a LIST, finds longest completion of STR, that is in
LIST, or all possible completions in LIST, or both, depending on result-type:
   result-type = string: longest completion,
   result-type = list:   all completions,
   result-type = t:      two values: longest completion plus all completions.
If a GENERATOR is given, it is applied first to the LIST argument to provide
the real list against which completions are matched. In this case LIST may be
of any type. When a KEY is given, this function is applied to the list arguments
before matching."
  (let ((result nil)
	(result-minimal nil)
	(completions nil)
	(length (length str)))
    (dolist (item (funcall generator list)
	      (case result-type
		(list (nreverse completions))
		(string result)
		((t) (values result (nreverse completions)))))
      (let* ((item-string (funcall key item))
	     (mismatch (mismatch str item-string)))
	(when (or (not mismatch) (= mismatch length))
	  (when (and (not result-minimal) (member result-type '(t string)))
	    (cond (mismatch
		   (if result
		       (setq result (common-prefix-2 result item-string))
		     (setq result item-string))
		   (when (string-equal result str) (setq result-minimal t)))
		  (t (setq result str)
		     (setq result-minimal t))))
	  (when (member result-type '(t list))
	    (push item-string completions)))))))

(defun common-prefix-2 (seq1 seq2)
  (let ((mismatch (mismatch seq1 seq2)))
    (if mismatch
	(subseq seq1 0 mismatch)
      seq1)))

(defun common-prefix (sequences)
  "Returns the longest prefix common to sequences (typically strings) 
in argument list."
  (cond ((null sequences) nil)
	((null (cdr sequences)) (car sequences))
	(t ;; works only when sequences has more than 2 elements
	 (reduce #'common-prefix-2 sequences))))


;;; special completions

;;; :NOTE This doesn't appear to work as intended... what am i missing? -MON
(defun filename-completion (str &key (result-type t))
  "For a string STR, depending on RESULT-TYPE, returns (string) best filename 
completion string, (list) list of all possible completions, (t) or both."
  (let* ((directory-name (directory-namestring str))
	 (filename (file-namestring str)))
    (string-completion
     ;; Do not use str here, it might contain Home dir symbols
     (concatenate 'string directory-name filename)
     directory-name
     :generator #'directory
     :key #'namestring
     :result-type result-type)))

(defun lisp-completion (symbol predicate &key package (result-type t))
  (string-completion
   (if (symbolp symbol)
      (symbol-name symbol)
     symbol)
   (let ((result nil))
     (do-symbols (sym (or package
			  (and (symbolp symbol)
				       (symbol-package symbol))
			 *package*) result)
       (when (funcall predicate sym)
	 (push sym result))))
   :key #'symbol-name
   :result-type result-type))

(defun function-completion (symbol &rest args)
  (apply #'lisp-completion symbol #'fboundp args))

(defun symbol-completion (symbol &rest args)
  (flet ((true (x) (declare (ignore x)) t))
    (apply #'lisp-completion symbol #'true args)))

(defun variable-completion (symbol &rest args)
  (apply #'lisp-completion symbol #'boundp args))






  
    
    
    
  
  
