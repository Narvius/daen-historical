(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS DIRECTOR
;;;;;;;;;;;;;;;;;;;;
; A director is responsible for spawning bullets and/or other directors. Every
; level has one main director who directs (see what I did there?) the entire
; danmaku for it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass director (entity)
  ((action :initform nil :initarg :action :accessor @action)
   (time :initform 0 :initarg :delay :accessor @time)
   (width :initform 8 :initarg :radius :accessor @radius)))

(defun new-director (action &key (pos nil) (vel (sdl:point)) (delay 0))
  (make-instance 'director :action action :delay delay
		 :pos (when pos
			(sdl:copy-point pos))
		 :vel (when vel
			(sdl:copy-point vel))))

(defun add-director (director)
  (push director (@directors *screen*)))

(defun copy-director (d)
  (new-director (funcall (@action d)) :pos (@position d) :vel (@velocity d) :delay (@time d)))

(defun add-new-director (action &key (pos nil) (vel (sdl:point)) (delay 0))
  (push (new-director action :delay delay :pos pos :vel vel) (@directors *screen*)))

(defun kill-director (d)
  (setf (@dead d) t (@directors *screen*) (delete d (@directors *screen*))))

(defun set-action (d action delay)
  (setf (@action d) action)
  (reset-time d delay))

(defun reset-time (d delay)
  (incf (@time d) delay))

(defmethod tick ((d director) dt)
  (decf (@time d) dt)
  (loop while (and (not (@dead d)) (@action d) (<= (@time d) 0)) do
    (funcall (@action d) d))
  (when (and (@position d) (@velocity d))
    (call-next-method)))

(defmethod draw ((d director))
  (when (@position d)
    (sdl:draw-filled-circle-* (round (~ d @position @x)) (round (~ d @position @y)) (@radius d)
			      :color (@color d) :stroke-color sdl:*white*)))

(defmacro do-repeat ((dir-name &key (interval 1000) (times nil)) &body body)
  (let ((i (gensym "repeat")))
    `(let ((,i 0))
       (lambda (,dir-name)
	 ,@body
	 (incf ,i)
	 ,(if times
	    `(if (= ,i ,times)
	       (kill-director ,dir-name)
	       (reset-time ,dir-name ,interval))
	    `(reset-time ,dir-name ,interval))))))

;(do-phases-2 (d c)
;  (key :initial
;       :normal
;       :final
;       :next-phase
;       :interval
;       :duration
;       :finish-check))

(defmacro do-phases-subclauses ((counter phase) &body body)
  `(progn
     ,(when (getf body :initial)
	`(when (= 0 ,counter)
	   (expand-to-single-form ,(getf body :initial))))
     ,(if (getf body :interval)
	`(when (and (<= ,(or (getf body :delay) 0) ,counter)
		    (= ,(mod (or (getf body :delay) 0) (getf body :interval))
		       (mod ,counter ,(getf body :interval))))
	   (expand-to-single-form ,(getf body :normal)))
	`(progn
	   (expand-to-single-form ,(getf body :normal))))
     ,(if (getf body :duration)
	`(when (= ,counter ,(1- (getf body :duration)))
	   (setf ,phase (expand-to-single-form ,(getf body :next-phase))
		 ,counter -1))
	(when (getf body :finish-check)
	  `(when (expand-to-single-form ,(getf body :finish-check))
	     (setf ,phase (expand-to-single-form ,(getf body :next-phase))
		   ,counter -1))))
     ,(when (getf body :final)
	`(when (= ,counter -1)
	   (expand-to-single-form ,(getf body :final))))))

(defmacro do-phases ((dir-name counter-name &key (initial-phase nil) (granularity 10) (pre-fn nil) (post-fn nil))
		     &body body)
  (let ((phase (gensym "phase")))
    `(let ((,phase ,(or initial-phase (caar body)))
	   (,counter-name -1))
       (do-repeat (,dir-name :interval ,granularity)
	 (incf ,counter-name)
	 ,(when pre-fn
	    `(when ,pre-fn
	       (funcall ,pre-fn ,dir-name)))
	 (case ,phase
	   ,@(mapcar #'(lambda (x)
			 `(,(car x) (do-phases-subclauses (,counter-name ,phase) ,@(cdr x)))) body))
	 ,(when post-fn
	    `(when ,post-fn
	       (funcall ,post-fn ,dir-name)))))))