;;;; Interface definition

(defpackage :swank-fancy-trace
  (:use :cl)
  (:import-from :swank :defslimefun :from-string :to-string)
  (:import-from :swank-backend :toggle-fancy-trace)
  (:export #:*unfancy-follows-fancy*
           #:*traces*
           #:*traced-specs*
           #:fancy-trace
           #:id-of
           #:depth-of
           #:spec-of
           #:args-of
           #:retval-of
           #:backtrace-of
           #:expanded-p-of
           #:children-of
           #:parent-of))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (rename-package :swank-fancy-trace :swank-fancy-trace (list :sft)))
(in-package :swank-fancy-trace)



(defparameter *unfancy-follows-fancy* nil)
(defvar *traced-specs*                '())
(in-package :swank-backend)

(definterface toggle-fancy-trace (spec)
  "TODO, document this interface careflly"
  (if swank-fancy-trace:*unfancy-follows-fancy*
      (values :would-be-reverting)
      ;; The reasoning for deciding whether to call the "fallback"
      ;; interface, `swank-backend:toggle-trace', is:
      ;;
      ;; If this interface is unimplemented, only call it if
      ;; `*unfancy-follows-fancy*' is nil, since otherwise, our caller,
      ;; `swank:swank-toggle-fancy-trace', has already called it and we
      ;; would be reverting that action.
      ;;
      ;; Also, if we do call the fallback interface, signal our caller
      ;; what the echo message is.
      ;;
      (values :nonfancy-fallback (swank:swank-toggle-trace (swank::to-string spec)))))

(in-package :swank)

(defslimefun swank-toggle-fancy-trace (spec-string)
  (let ((message))
    (when swank-fancy-trace:*unfancy-follows-fancy*
      (setq message (swank:swank-toggle-trace spec-string)))
    (let* ((spec (from-string spec-string))
           (traced-p (multiple-value-list (swank-backend:toggle-fancy-trace spec))))
      (format t "traced-p is ~a~%" traced-p)
      (case (first traced-p)
        ((:nonfancy-fallback)
         (second traced-p))
        ((:would-be-reverting)
         message)
        ((nil)
         (setf swank-fancy-trace:*traced-specs*
               (remove spec swank-fancy-trace:*traced-specs* :test #'equal))
         (format nil "~S is now fancy untraced." spec))
        (t
         (pushnew spec swank-fancy-trace:*traced-specs* :test #'equal)
         (format nil "~S is now fancy traced." spec))))))

(defslimefun swank-fancy-untrace-all ()
  (mapcar #'swank:swank-toggle-fancy-trace (mapcar #'to-string swank-fancy-trace:*traced-specs*)))

(in-package :swank-fancy-trace)

(defvar       *depth*                   0)
(defvar       *current-trace*           0)
(defparameter *id*                      0)
(defparameter *traces*                '())

(defclass fancy-trace ()
  ((id         :initarg  :id           :accessor id-of)
   (depth      :initarg  :depth        :accessor depth-of)
   (spec       :initarg  :spec         :accessor spec-of)
   (args       :initarg  :args         :accessor args-of)
   (retval     :initform :still-inside :accessor retval-of)
   (backtrace  :initarg  :backtrace    :accessor backtrace-of)
   (expanded-p :initform  t            :accessor expanded-p-of)
   (children   :initform nil           :accessor children-of)
   (parent     :initform nil           :accessor parent-of)))


(defun fancy-trace (spec function &key args backtrace)
  (incf *id*)
  (let* ((*current-trace* (make-instance 'fancy-trace :id        *id*
                                                :depth     *depth*
                                                :spec      spec
                                                :args      args
                                                :backtrace backtrace))
         (*depth* (1+ *depth*))
         (retval (progn (push *current-trace* *traces*)
                        (multiple-value-list (funcall function)))))
    (setf (retval-of *current-trace*) retval)
    (values-list retval)))


;;;; Presentation

(in-package :swank)

(defmethod emacs-inspect ((sym (eql 'swank-fancy-trace:*traces*)))
  (let* ((traces (symbol-value sym))
         (size (length traces))
         (traced-specs swank-fancy-trace:*traced-specs*))
    (append
     (label-value-line
      "Traces collected so far" size)
     '((:newline))
     '("Traced specs:      ")
     (unless (zerop size)
       `((:action "[untrace all traced specs]"
                  ,#'swank-fancy-untrace-all)))

     '((:newline) (:newline))
     (if traced-specs
         (loop for spec in traced-specs
               append `("    "
                        (:action "[untrace]"
                                 ,(lambda () (swank-toggle-fancy-trace (to-string spec))))
                        " "
                        ,(format nil "~s" spec)
                        (:newline)))
         "None")

     `((:newline))
     `("Traces follow:     ")
     (unless (zerop (length traced-specs))
       `((:action "[clear collected traces]"
                  ,(lambda () (setq swank-fancy-trace:*traces* nil)))))

     '((:newline)(:newline))
     (loop for trace in (reverse traces)
           append `(,(cond ((null (sft:children-of trace))
                            "    ")
                           ((sft:expanded-p-of trace)
                            `(:action "[-] " ,(lambda () (setf (sft:expanded-p-of trace) nil))))
                           (t
                            `(:action "[+] " ,(lambda () (setf (sft:expanded-p-of trace) t)))))
                    ,(format nil "~a: " (sft:id-of trace)) (:value ,(sft:spec-of trace))
                    " args: "   (:value ,(sft:args-of trace))
                    " retval: " (:value ,(sft:retval-of trace))
                    (:newline))))))




#+nil
(progn
  (defun fact (n) (if (= n 1) 1 (* n (fact (1- n)))))
  (defun fact2 (n) (if (= n 1) 1 (* n (fact2 (1- n)))))
  (defun fact3 (n) (if (= n 1) 1 (* n (fact3 (1- n)))))
  (fact 3)
  (fact3 20)
  (fact2 2))
