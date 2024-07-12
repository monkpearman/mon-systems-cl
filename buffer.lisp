;;; :FILE-CREATED <Timestamp: #{2011-03-26T12:17:47-04:00Z}#{11126} - by MON>
;;; :FILE mon-systems/buffer.lisp
;;; ==============================


(in-package #:mon)

;; flex:
;; (let ((in (flex:MAKE-FLEXI-STREAM :element-type '(unsigned-byte 8))))
  
;; (map 'vector #'char-code (with-output-to-string (str)
;;                            (run-program... :external-format :iso...)))


;;; ==============================
;;; :PASTED (URL `http://paste.lisp.org/display/120782')
;;; :AUTHOR Pascal Bourguignon
;;; :PASTE-NUMBER 120782
;;; :PASTE-TITLE buffer
;;; :PASTE-DATE 2011-03-23
;;; ==============================

(defconstant +binary-message-length+ 1020)
(defconstant +carriage-return+       13 "ASCII code of CR.")
(defconstant +line-feed+             10 "ASCII code of LF.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUFFER
;;;
;;; The following functionnal abstraction specifies buffers, that are
;;; used to gather bytes and eat messages in FIFO order.
;;;
;;; This implementation moves eaten bytes down the vector, assuming
;;; there won't be a lot of remaining bytes to move.  If this
;;; assumption reveals false, then another implementation, cord-like,
;;; could be written.  

(defun make-buffer (initial-size)
  (make-array initial-size
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun buffer-length (buffer)
  (length buffer))

(defun buffer-ref (buffer index)
  (aref buffer index))

(defun buffer-subseq (buffer start end)
  (let ((sub (make-array (- end start) :element-type '(unsigned-byte 8))))
    (replace sub buffer :start2 start)
    sub))

(defun buffer-search (subsequence buffer)
  (search subsequence buffer))

(defun buffer-delete-from-head (buffer size-to-remove)
  (replace buffer buffer :start2 size-to-remove)
  (decf (fill-pointer buffer) size-to-remove)
  buffer)

(defun buffer-clear (buffer)
  (setf (fill-pointer buffer) 0))

(defun buffer-append (buffer bytes size)
  (let* ((old-size (length buffer))
         (new-size (+ old-size size)))
    (loop :while (< (array-dimension buffer 0) new-size) :do
       (setf buffer (adjust-array buffer
                                  (* 2 (array-dimension buffer 0))
                                  :element-type (array-element-type buffer)
                                  :fill-pointer (fill-pointer buffer))))
    (setf (fill-pointer buffer) new-size)
    (replace buffer bytes :start1 old-size :end2 size)
    buffer))


;;; ==============================
;;; :PASTED (URL `http://paste.lisp.org/display/120781')
;;; :AUTHOR Pascal Bourguignon
;;; :PASTE-NUMBER 120781
;;; :PASTE-TITLE Concatenated Vectors
;;; :PASTE-DATE  2011-03-23
;;; ==============================

(defun make-vector-concatenation-accessor (vectors)
  (lambda (index)
    (if (minusp index)
        (error "Invalid index (must not be negative)")
        (loop
           :named indexing
           :for vec :in vectors
           :for max = (length vec)
           :sum max :into total
           :if (< index max) :do (return-from indexing (values vec index))
           :else :do (decf index max)
           :finally (error "Invalid index (must be less than ~A)" total)))))

(defstruct concatenated-vector
  reader writer)

(defun concatenate-vectors (&rest vectors)
  (let ((accessor (make-vector-concatenation-accessor vectors)))
    (make-concatenated-vector 
     :reader (lambda (index) (multiple-value-bind (vec ind) (funcall accessor index)
                               (aref vec ind)))
     :writer (lambda (value index) (multiple-value-bind (vec ind) (funcall accessor index)
                                     (setf (aref vec ind) value))))))

(defmethod ref ((vec concatenated-vector) index)
  (funcall (concatenated-vector-reader vec) index))

(defmethod (setf ref) (new-value (vec concatenated-vector) index)
  (funcall (concatenated-vector-reader vec) new-value index))


;;; ==============================
;;; EOF
