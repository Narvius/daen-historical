(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS EFFECT
;;;;;;;;;;;;;;;;;;;;
; Effects can be used to apply functions to a bullet group, but with a delay.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass effect ()
  ((time :initform 0 :initarg :time :accessor @time)
   (fn :initarg :fn :accessor @fn)
   (final-fn :initarg :final-fn :accessor @final-fn)))

(defun add-effect (group wait &key fn final-fn)
  (push (make-instance 'effect :time wait :fn fn :final-fn final-fn) (@effects group)))

(defmacro add-repeated-effect (repeats group delay &key (fn nil) (immediately t))
  (let ((i (gensym "add-repeated-effect"))
	(g (gensym "add-repeated-effect"))
	(e (gensym "add-repeated-effect")))
    `(let ((,i ,repeats))
       (add-effect ,group ,(if immediately 0 delay) :fn ,fn :final-fn #'(lambda (,g ,e)
									  (declare (ignore ,g))
									  (unless (< (decf ,i) 0)
									    (reset-time ,e ,delay)))))))
