;;; :FILE mon-systems/numbers.lisp
;;; ==============================


;;; ==============================
;;; :TODO CLHS notes that these were in MacLisp and (for me) it makes more
;;;  conceptual sense:
;;;
;;; `lessp'    <- `<'
;;; `greaterp' <- `>'
;;;
;;; ==============================


(in-package #:mon)

;; :SOURCE sclf/directory.lisp :COURTESY Walter C. Pelissero
(defun bits-set-p (x bits)
  (= (logand x bits)
     bits))

(defun coerce-int-float (num)
  (typecase  num
    (integer num)
    (float   num)
    (t (float num 0d0))))

;; :SOURCE sbcl/src/code/target-extensions.lisp :WAS `power-of-two-ceiling'
(defun number-power-of-two-ceiling (unsigned-int)
  (declare (mon:index unsigned-int))
  (ash 1 (integer-length (1- unsigned-int))))

(defun number-sequence-loop (from-n &optional to-n (inc-by 1))
  (declare ;;(optimize (speed 0) (space 1) (compilation-speed 0) (debug 3))
   (type real from-n)
   (type (or real null) to-n)
   (type real inc-by))
  (assert (not (zerop inc-by))
	  nil ":FUNCTION `number-sequence-loop' -- arg INC can not be zerop")
  ;; The `minusp' check keeps a similiar semantics as Emacs' `number-sequence'
  (unless (minusp inc-by)
    (cond ((eql to-n from-n) (list from-n))
	  ((and to-n inc-by
		(or
		 (and (plusp inc-by) (> from-n to-n) nil)
		 (loop
		    :for i :upfrom from-n :upto to-n :by inc-by
		    :collect i))))
	  (to-n (loop
		   :for i :upfrom from-n :upto to-n :by 1
		   :collect i))
	  (t (loop :for i :upfrom from-n :upto from-n
		:collect i)))))

;;; ==============================
;; :NOTE Following unaltered from :FILE lisp/subr.el
;; (if (or (not to) (= from to))
;;     (list from)
;;     (or inc (setq inc 1))
;;     (when (zerop inc) (error "The increment can not be zero"))
;;     (let (seq (n 0) (next from))
;; 	(if (> inc 0)
;; 	    (mon::while (<= next to)
;; 	      (setq seq (cons next seq)
;; 		    n (1+ n)
;; 		    next (+ from (* n inc))))
;; 	    (mon::while (>= next to)
;; 	      (setq seq (cons next seq)
;; 		    n (1+ n)
;; 		    next (+ from (* n inc)))))
;; 	(nreverse seq))))
;;
(defun number-sequence (from &optional to (inc 1))
  (declare (type real from)
	   ((or real null) to inc))
  (assert (not (zerop inc))
	  nil
	  ":FUNCTION `number-sequence' -- arg INC can not be zerop")
  (cond ((or (null to) (eql to from))
	 (list from))
        (t (let ((n 0)
		 (next from)
		 (seq ()))
	     (if (> inc 0)
		 (while (<= next to)
		   (setq seq (cons next seq)
			 n (1+ n)
			 next (+ from (* n inc))))
		 (while (>= next to)
		   (setq seq (cons next seq)
			 n (1+ n)
			 next (+ from  (* n inc)))))
	     (nreverse seq)))))

;;; :SOURCE mcclim-/Tools/gilbert/clim-doc-convert.lisp :WAS `mk-random-list-from-string'
(defun random-number-pairs (n-times random-bounds)
  ;;  (declare (type n-times
  (let (gthr)
    (dotimes (i n-times (setq gthr (nreverse gthr)))
      (let ((rnd-str (format nil "~a" (random random-bounds))))
	(push (cons rnd-str (car (parse-integer-list rnd-str ))) gthr)))))

;;; :SOURCE mcclim-/Tools/gilbert/clim-doc-convert.lisp
(defun parse-integer-list (string &key (start 0) (end (the fixnum-exclusive (length string))))
  (declare (type simple-string string))
  (multiple-value-bind (value start)
      (parse-integer string :start start :end end :junk-allowed t)
    (cond ((null value)
           nil)
          (t
           (cons value (parse-integer-list string :start start :end end))))))

;;; ==============================
;; Following version from Lice:
;; (defun int-to-string (num-int)
;;   "Return the decimal representation of num-int as a string.
;; Argument NUM-INT is an integer or a floating point number.
;; Uses a minus sign if negative."
;;   (check-type num-int number)
;;   (prin1-to-string num-int))

(defun number-to-string (number)
  (check-type number number)
  (write-to-string number :radix 10))

;; (fset 'int-to-string 'number-to-string)

;; :SOURCE arnesi/src/numbers.lisp :WAS `radix-values'
(defun %radix-values (radix)
  (declare ((integer 2 35) radix))
  #-sbcl (assert (<= 2 radix 35)
  		 (radix)
  		 "RADIX must be between 2 and 35 (inclusive), not ~D." radix)
  (make-array radix
  	      :displaced-to "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  	      :displaced-index-offset 0
  	      :element-type
  	      #+lispworks 'base-char
  	      #-lispworks 'character))

;; :SOURCE arnesi/src/numbers.lisp
(defun parse-float (float-string
                    &key (start 0) (end nil) (radix 10)
                         (junk-allowed t)
                         (type 'single-float)
                         (decimal-character #\.))
  ;; Convert FLOAT-STRING to floating point number.
  ;; START is an index into FLOAT-STRING. Default is 0.
  ;; END is an index into FLOAT-STRING designating where parse should stop.
  ;; TYPE is a cl float specifier. Default is single-float
  ;; Keyword DECIMAL-CHARACTER designates the mantissa delimiter. Default is #\\.
  ;; `mon:parse-integer-list' `mon:number-average-seq' `cl:most-positive-double-float', `cl:most-positive-long-float',
  ;; `cl:most-positive-short-float', `cl:most-positive-single-float'
  (declare (type character decimal-character)
	   (type string-not-empty float-string)
	   (type unsigned-byte-29 start)
	   (type (integer 2 35) radix))
  (let ((radix-array (%radix-values radix))
        (integer-part 0)
        (mantissa 0)
        (mantissa-size 1)
        (sign 1))
    `(declare ((and (vector character ,radix) (not simple-array)) radix-array))
    (with-input-from-string (float-stream (string-upcase
					   (string-trim-whitespace float-string
								   :start start
								   :end (or end (length float-string)))))
      (labels ((peek () (peek-char nil float-stream nil nil nil))
               (next () (read-char float-stream nil nil nil))
               (sign () ;; reads the (optional) sign of the number
                 (cond
                   ((char= (peek) #\+) (next) (setf sign 1))
                   ((char= (peek) #\-) (next) (setf sign -1)))
                 (integer-part))
               (integer-part ()
                 (cond
                   ((position (peek) radix-array)
                    ;; the next char is a valid char
                    (setf integer-part (+ (* integer-part radix)
                                          (position (next) radix-array)))
                    ;; again
                    (return-from integer-part (integer-part)))
                   ((null (peek))
                    ;; end of string
                    (done))
                   ((char= decimal-character (peek))
                    ;; the decimal seperator
                    (next)
                    (return-from integer-part (mantissa)))
                   ;; junk
                   (junk-allowed (done))
                   (t (bad-string))))
               (mantissa ()
                 (cond
                   ((position (peek) radix-array)
                    (setf mantissa (+ (* mantissa radix)
                                      (position (next) radix-array))
                          mantissa-size (* mantissa-size radix))
                    (return-from mantissa
                      (mantissa)))
                   ((or (null (peek)) junk-allowed)
                    ;; end of string
                    (done))
                   (t (bad-string))))
               (bad-string ()
                 (error "Unable to parse ~S." float-string))
               (done ()
                 (return-from parse-float
                   (coerce (* sign (+ integer-part (/ mantissa mantissa-size))) type))))
        (sign)))))

(defun string-to-number (convert-string &key (start 0) (end nil) (radix 10)
			 (junk-allowed t) (decimal-character #\.))
  ;; (declare (type simple-string convert-string))
  (let (do-coercion)
    (setf do-coercion
	  (typecase convert-string
	    (character     (char-code convert-string))
	    (float         convert-string)
	    (integer       convert-string)
	    (ratio         (coerce convert-string 'float))
	    (simple-string convert-string) ;;
	    (null nil)
	    (t nil)))
    (if (and (stringp do-coercion) (not (null do-coercion)))
	(values
	 (if (position decimal-character convert-string)
	     ;;(arnesi:parse-float convert-string
	     (parse-float convert-string
			  :start start :end end :radix radix
			  :junk-allowed junk-allowed
			  :decimal-character decimal-character)
	     (parse-integer convert-string
			    :start start :end end :radix radix :junk-allowed junk-allowed)))
	(or do-coercion
	    (error "Arg does not satisfy predicate `stringp' CONVERT-STRING was of type: ~S "
		   (type-of convert-string))))))

;;; ==============================
;; cllib/geo.lisp
;; (deftype index-t () '(unsigned-byte 28))
;; (defun parse-num (str)
;;   "Parse the number from the string str, fixing commas."
;;   (declare (simple-string st))
;;   (fill st #\Space :end
;; 	(let ((pp (position-if
;; 		   (lambda (zz) (or (digit-char-p zz) (eql zz #\$))) st)))
;; 	  (when pp (if (digit-char-p (char st pp)) pp (1+ pp)))))
;;   (nsubstitute #\Space #\% st)
;;   (do ((pos 0 (and next (1+ next))) next res)
;;       ((null pos)
;;        (setf st (apply #'concatenate 'string (nreverse res))
;; 	     (values next pos) (read-from-string st nil nil))
;;        (and next
;; 	    (* next (case (read-from-string st nil nil :start pos)
;; 		      (trillion 1000000000000) (billion 1000000000)
;; 		      (million 1000000) (t 1)))))
;;     (declare (type (or null index-t) pos next))
;;     (push (subseq st pos (setq next (position #\, st :start pos))) res)))


(defun value-in-range-p (value low high &optional error-string)
  (cond ((and (numberp low) (> low value))
	 (and error-string
              (error "~A ~A ~D is not greater than ~D" error-string 'integer value low)))
	((and (consp low) (numberp (first low)) (>= (first low) value))
	 (and error-string
              (error "~A ~A ~D is not greater than or equal to ~D"
                     error-string 'integer value (first low))))
	((and (numberp high) (< high value))
	 (and error-string
              (error "~A ~D ~A is not less than or equal to ~D"
                     error-string 'integer value high)))
	((and (consp high) (numberp (first high)) (<= (first high) value))
	 (and error-string
	      (error
               "~A ~A ~D is not less than ~D"
               error-string 'integer value (first high))))
	(T T)))

;; :NOTE Not sure how good an idea this is...
(defun % (number divisor)
  (declare (type integer number divisor))
  (mod number divisor))

;; :SOURCE cl-docutils-20101006-git/utilities.lisp :WAS `length-unit'
(defun length-unit-get (unit)
  (or (cdr (assoc unit *length-unit*))
      (error "Unacceptable unit ~S - acceptable units are ~S"
             unit
             (mapcar #'car *length-unit*))))

;; :SOURCE cl-docutils-20101006-git/utilities.lisp :WAS `convert-length-unit'
(defun length-unit-convert (size unit)
  (unless (consp size)
    (setf size (cons size :px)))
  (cons
   (* (/ (car size) (length-unit-get (cdr size)))
      (length-unit-get unit))
   unit))

;;; ==============================
;; :PASTE-NUMBER 125253
;; :PASTE-BY pjb
;; :PASTE-DATE 2011-10-11
;; :PASTE-URL (URL `http://paste.lisp.org/+2ON9')
;; :WAS `best-fit'
;; (defun best-fit (target sizes)
;;   (let ((candidates (member target sizes :test (function >=))))
;;     (cond
;;       ((null candidates)           nil)
;;       ((= (car candidates) target) (list target))
;;       (t (cons (car candidates)
;;                (best-fit (- target (car candidates)) (cdr candidates)))))))
;; modifications to pjb's `best-fit':
;;  - Added declarations/assertions.
;;  - Added early exit when TARGET-NUMBER is cl:zerop
;;  - No longer necessary to reverse NUMBER-BAG.
;;  - Now accepts vectors as argument to NUMBER-BAG.
;;  - Now with different semantics w/r/t floats and negatives
;;  - Now with different semantics when TARGET-NUMBER is outside the set represented by NUMBER-BAG
(defun number-nearest-seq (target-number number-bag)
  (declare (type mon:proper-sequence number-bag)
           (type real target-number))
  #-:sbcl (assert (typep number-bag 'mon:proper-sequence))
  #-:sbcl (assert (typep target-number 'number))
  ;; :NOTE cl:length of (1 . 2) should signal an error, so even where NUMBER-BAG
  ;; is not a proper-sequence we would bail here.
  (unless (and (not (zerop (length number-bag)))
               (every #'realp number-bag))
    (error "Arg NUMBER-BAG must be a proper-sequence with each element satisfying `cl:numberp'~% got: ~S" number-bag))
  (let ((ensured-list (etypecase number-bag
                        (list  number-bag) 
                        (vector (coerce number-bag 'list)))))
    (declare (list ensured-list)
             (optimize (speed 3)))
    (labels ((num-near-seq (target bag)
               (declare (real target) (list bag))
               (let* ((candidates      (member target bag :test #'>=))
                      (first-candidate (car candidates)))
                 (cond ((null first-candidate) nil)
                       ((= first-candidate target) (list target))
                       (t (cons (car candidates)
                                (num-near-seq (- target first-candidate) (cdr candidates)))))))
             (rtn-check (rtn-value)
               (case (list-length rtn-value)
                 ;; lets assume we never get a consed pair
                 (0 (setf rtn-value (reduce #'min ensured-list)
                          rtn-value (if (and (plusp target-number)
                                             (minusp rtn-value))
                                        (list rtn-value (abs (- target-number rtn-value)))
                                        (list rtn-value (- target-number rtn-value)))))
                 ;; we found a negative number
                 (1 (push (abs (- target-number (car rtn-value))) rtn-value)
                    (nreverse rtn-value))
                 (2 (if (= (reduce #'+ rtn-value) target-number)
                        rtn-value
                        (list (car rtn-value)
                              (- target-number (car rtn-value)))))
                 ;; we have a number larger than all others.
                 (t (setf rtn-value (reduce #'max ensured-list)
                          rtn-value (list rtn-value
                                          (if (and (minusp target-number)
                                                   (minusp rtn-value))
                                              (abs (- target-number rtn-value))
                                              (- target-number rtn-value))))))))
      (when (member target-number ensured-list :test #'=)
        (return-from number-nearest-seq (list target-number 0)))
      (rtn-check (num-near-seq target-number (delete-duplicates (sort ensured-list #'>)))))))

;;; ==============================
;; NOPE
;; (defun number-nearest-seq (target-number number-bag)
;;   (declare (type mon:proper-sequence number-bag)
;;            (type real target-number))
;;   #-:sbcl (assert (typep number-bag 'mon:proper-sequence))
;;   #-:sbcl (assert (typep target-number 'number))
;;   ;; :NOTE cl:length of (1 . 2) should signal an error, so even where
;;   ;; alexandria:proper-list isn't in the environment we would bail here.
;;   (unless (and (not (zerop (length number-bag)))
;;                (every #'realp number-bag))
;;     (error "Arg NUMBER-BAG must be a proper-sequence with each element satisfying `cl:numberp'~% got: ~S" number-bag))
;;   (let* ((list-ensured
;;           (etypecase number-bag
;;             (list  (copy-seq number-bag))
;;             (vector (coerce number-bag 'list))))
;;          (early-candidate-check
;;           (cond ((member target-number list-ensured :test (function =))
;;                  (return-from number-nearest-seq (list target-number 0)))
;;                 ((zerop target-number)
;;                  (return-from number-nearest-seq
;;                    (if (member-if #'zerop list-ensured)
;;                        (list target-number target-number)
;;                        (if (some #'minusp list-ensured)
;;                            (if (some #'plusp list-ensured)
;;                                (loop
;;                                   for x in list-ensured
;;                                   if (plusp x) minimizing x into plus
;;                                   else
;;                                   maximizing x into minus
;;                                   finally (return (if (>= (abs minus) plus)
;;                                                       plus
;;                                                       minus)))
;;                                (apply #'max list-ensured))
;;                            (apply #'min list-ensured)))))
;;                 (t (mapcar #'(lambda (x)
;;                                (list (- target-number x) x))
;;                            list-ensured)))))
;;     ;; (declare (list list-ensured early-candidate-check)
;;     ;;          (optimize (speed 3)))
;;     (setf list-ensured
;;           (assoc (reduce (if (plusp target-number)
;;                              ;; #'min #'max)
;;                              #'max #'min)
;;                          early-candidate-check :key #'car)
;;                  early-candidate-check))
;;     (rotatef (car list-ensured) (cadr list-ensured))
;;     (list list-ensured early-candidate-check)))
;; (setf list-ensured
;;           (list
;;            (reduce (if (plusp target-number)
;;                        #'max #'min)
;;                          early-candidate-check :key #'car)
;;            (reduce (if (plusp target-number)
;;                        #'max #'min)
;;                    early-candidate-check :key #'cadr)
;;            early-candidate-check
;;           ))))
;;; ==============================

;; :SOURCE (URL `http://paste.lisp.org/display/118915') :COURTESY mathrick :WAS `average'
(defun number-average-seq (seq &key key (weighting-key (constantly 1))
                           weights large-sum-p)
  (when (and weighting-key weights)
    (error "WEIGHTS and WEIGHTING-KEY might not be given at the same time."))
  (loop
     :for i :from 0
     :for elem :in seq
     :with avg = 0
     :for val = (if key
                    (funcall key elem)
                    elem)
     :for weight = (if weights (elt weights i)
                       (funcall weighting-key elem))
     :summing weight :into sum-weight
     :summing (* val weight) :into sum-avg
     :do (when large-sum-p
           (progn
             (setf avg (* avg
                          (/ (- sum-weight weight)
                             sum-weight)))
             (incf avg (* val (/ weight sum-weight)))))
     :finally (return (if large-sum-p
                          avg
                          (/ sum-avg (max sum-weight 1))))))

(defun number-average-seq-simple (seq)
  (declare (sequence seq))
  (or (every #'numberp seq)
      (error "element of arg SEQ not `cl:numberp', got: ~S" seq))
  (/ (reduce #'+ seq) (length seq)))

;;; ==============================
;; :NOTE CL's `=' won't/doesn't handle character types.
;; (cl:defun = (a b)
;;   (cond ((and (characterp a) (characterp b))
;; 	 (char= a b))
;; 	((and (numberp a) (characterp b))
;; 	 (cl:= a (char-code b)))
;; 	((and (characterp a) (numberp b))
;; 	 (cl:= (char-code a) b))
;; 	((and (numberp a) (numberp b))
;; 	 (cl:= a b))
;; 	(t (error "Wrong type argument ~a" (if (or (numberp a) (characterp a))
;; 					       b
;; 					     a)))))

;;; ==============================
;; :SOURCE https://github.com/astine/subship.git :FILE new.lisp
;; (defun greater (x y &optional (test #'>))
;;   "Returns the greater of two alternatives as defined by test."
;;   (if (funcall test x y) x y))
;;
;; (defun greatest (list &optional (test #'>))
;;   "Returns the greatest of a list of alternatives as defined by test."
;;   (reduce (lambda (x y)
;; (greater x y test))
;; list))
;;; ==============================


;;; ==============================
;;; :NUMBERS-DOCUMENTATION
;;; ==============================

(fundoc 'number-sequence-loop
"Like `mon:number-sequence' but uses `cl:loop'. ~%~@
When optional arg TO-N is non-nil it is a number of type `cl:real' to increment to.~%
When optional arg INC-BY is non-nil it is a number of type `cl:real' to incrembent by.
:EXAMPLE~%
 \(number-sequence-loop 3.3 8.6 1.001\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc  'number-sequence
"Return a sequence of numbers from FROM to TO (both inclusive) as a list.~%~@
INC is the increment used between numbers in the sequence and defaults to 1.~%~@
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from zero.~%~@
TO is only included if there is an N for which TO = FROM + N * INC.~%~@
If TO is nil or numerically equal to FROM, return \(FROM).~%~@
If INC is positive and TO is less than FROM, or INC is negative and TO is larger
than FROM, return nil.~%~@
If INC is zero and TO is neither nil nor numerically equal to FROM, signal an error.~%~@
:EXAMPLE~%
 \(number-sequence 0 8 3\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-to-number
  "Parse string as a decimal number and return the number.~%~@
Parses both integers and floating point numbers.~%
Ignore leading spaces and tabs, and all trailing chars.~%~@
When keyword RADIX interpret string as a number in that base.
The radix parameter must be between 2 and 36.~%Default radix is 10.~%~@
:EXAMPLE~%
 \(string-to-number \"3.33\"\)~%
 \(string-to-number \"3.33\" :decimal-character #\\,\)~%
 \(string-to-number \"3\" :decimal-character #\\,\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:bytes-to-int', `mon:coerce-int-float', `mon:number-to-string',
`mon:string-coerce-from'.~%▶▶▶")

(fundoc 'number-to-string
  "Return the decimal representation of NUMBER as a string.~%~@
Uses a minus sign if negative.~%~@
NUMBER may be an integer or a floating point number.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:bytes-to-int', `mon:coerce-int-float', `mon:string-to-number',
`mon:string-coerce-from'.~%▶▶▶")

(fundoc '%
"Return remainder of X divided by Y.~%~@
Both must be integers or markers.~%~@
:EXAMPLE~%
 \(% 8 3\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'coerce-int-float
"Ensure that the argument is either an integer or a float.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:bytes-to-int', `mon:number-to-string',
`mon:string-coerce-from'.~%▶▶▶")

(fundoc 'random-number-pairs
"Return a list of consed pairs with each elt a string and its corresponding wholenum.
elts of list have the format:~%
 \(\"NN\" . NN\)~%~@
N-TIMES is the number consed pairs to generate.~%~@
RANDOM-BOUNDS is an integer or positive float suitable for limiting `random'.~%~@
:EXAMPLE~%
 \(mk-random-list-from-string 8 18\)~%~@
:SEE-ALSO `mon:parse-integer-list', `cl:random-state-p' `cl:*random-state*',
`cl:random', `cl:make-random-state'.~%▶▶▶")

(fundoc 'parse-integer-list
"Parse the integers in string from START to END. Return parse list.~%@
:EXAMPLE~%
 \(parse-integer-list \"83813\" :start 1 :end 2\)~%@
:SEE-ALSO `random-number-pairs'.~%▶▶▶")

(fundoc 'length-unit-get
"Lookup UNIT for `mon:length-unit-get' in `mon:*length-unit*' alist.~%~@
Signal an error if UNIT will not associate.~%~%
:EXAMPLE~%
 \(length-unit-get :cm\)~%~@
:SEE-ALSO .~%▶▶▶")

(fundoc 'length-unit-convert
"Convert SIZE to UNIT.\n~%~@
SIZE is a unit in inches.~%~@
UNIT is a key in `mon:*length-unit*'.~%~@
:EXAMPLE~%
 \(length-unit-convert 1.0  :cm\)~%
 \(length-unit-convert 1.03 :mm\)~%
 \(length-unit-convert 1.03 :in\)~%
 \(length-unit-convert 1.03 nil\)~%~@
:SEE-ALSO `mon:length-unit-get', `mon:*length-unit*'.~%▶▶▶")

(fundoc 'number-average-seq
  "Calculate the average \(arithmetic mean\) value of numbers in sequence SEQ.~%~@
KEY has the standard meaning.~%~@
WEIGHTING-KEY should be a function of one argument returning a number denoting
the weight of an element.~%~@
Alternatively, WEIGHTS might be given, as a sequence of equal length as SEQ,
whose elements give weights of the corresponding SEQ elements.~%~@
LARGE-SUM-P is a flag specifying whether the sum of the elements in SEQ might
exceed `cl:most-positive-*-float'. When non-nil, an alternative algorithm is used,
which will not signal an error as long as the mean of the values is below the
maximum value, but on the other hand might have a worse numerical stability,
since it will perform a multiplication on the average on every step.~%~@
:EXAMPLE~%
 \(let \(\(seq \(loop
	       for i from 1.13332 to 1000.0 by 0.769
	       collect i\)\)\)
   \(abs \(- \(number-average-seq seq :large-sum-p t\)
	   \(number-average-seq seq :large-sum-p nil\)\)\)\)
 ;=> 2.1362305e-4~%~@
:SEE-ALSO `mon:number-average-seq-simple', `cl:most-positive-double-float',
`cl:most-positive-long-float', `cl:most-positive-short-float',
`cl:most-positive-single-float'.~%▶▶▶")

(fundoc 'number-average-seq-simple
        "Return the average of all elts of SEQ.~%~@
SEQ is must be of tyep `cl:sequence' with every elt satisfying `cl:numberp'.~%~@
:EXAMPLE~%
 \(number-average-seq-simple '\(1 2 3 4\)\)~%~@
 \(number-average-seq-simple #\(17/18 2.33\)\)~%~@
 \(number-average-seq-simple \(make-array 4 :element-type 'bit :initial-contents '\(1 1 0 1\)\)\)~%~@
 \(number-average-seq-simple '\( #c\(1.3 -1.2\) #c\(1.1 -1.4\)\)\)~%~@
 (number-average-seq-simple #*01011101000000101)~%~@
;; Following fail succesfully:~%
 \(number-average-seq-simple '\( 8 \"8\"\)\)~%
 \(number-average-seq-simple #\(1 2 3\) \(make-array '\(2 2\) :initial-contents '\(\(1 1\) \(2 2\)\)\)\)~%~@
:SEE-ALSO `mon:number-average-seq'.~%▶▶▶")

(fundoc 'number-power-of-two-ceiling
        "The smallest power of two that is equal to or greater than UNSIGNED-INT.~%~@
UNSIGNED-INT should be of type `mon:index'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `prime-plusp', `prime-or-next-greatest'.~%▶▶▶")


(fundoc 'number-nearest-seq
        "Return a list indicating the number of NUMBER-BAG nearest TARGET-NUMBER.~%~@
TARGET-NUMBER is an object of type `cl:real', an error is signaled if not.~%~@
NUMBER-BAG is a proper-sequence of type `cl:list' or `cl:vector' with each
element of type `cl:real', an error is signaled if not.~%~@
 is an object of type `cl:number' an error is signaled if not.~%~@
Return value has the form:~%
 \( <NEAREST-NUMBER> <DISTANCE-TO-TARGET-NUMBER> \)~%
 - <NEAREST-NUMBER> is a nearest number to TARGET-NUMBER~%
 - <DISTANCE-TO-TARGET-NUMBER> is a value representing a distance from TARGET-NUMBER.~%~@
It should be the case that following will return true:~%~@
 \(= \(apply #'+  \( <NEAREST-NUMBER> <DISTANCE-TO-TARGET-NUMBER> \)\) <TARGET-NUMBER>\)~%
:EXAMPLE~%
 \(number-nearest-seq  12   '\(1 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq  12   #\(1 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq  15   '\(1.8 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq  13.1 '\(1.8 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq -13.1 '\(1.8 -3 5 -7 11 -13 17\)\)~%
 \(number-nearest-seq  1    '\(1.1 1.0 1.3 1\)\)~%
 \(number-nearest-seq  1.0  '\(1.1 1.0 1.3 1\)\)~%
 \(number-nearest-seq  1    '\(0 0 0 0\)\)~%
 \(number-nearest-seq -1    '\(0 0 0 0\)\)~%
 \(number-nearest-seq -15   '\(-1.8 -3 -5 -7 -11 -13 -17\)\)~%
 \(number-nearest-seq -13.1 '\(1.8 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq   3   '\(11 13 17\)\)~%
 \(number-nearest-seq -18   '\(-4 -1 -3 -5 -7 -11 -13 -17\)\)~%
 \(number-nearest-seq  16.9 '\(1 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq  16.9 '\(3 5 7 11 16.8 16.95 17\)\)~%
 \(number-nearest-seq  16.9 '\(3 5 7 11 16.8 17\)\)~%
 \(number-nearest-seq  16.9 '\(3 5 7 11 16.8 17\)\)~%
 \(number-nearest-seq  17.1  #\(1 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq  1    '\(3 5 7 11 13 17\)\)~%
 \(number-nearest-seq -1    '\(1 3 5 7 11 13 17\)\)~%
 \(number-nearest-seq 0.1   '\(1 3 5 7 11 13 17\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
