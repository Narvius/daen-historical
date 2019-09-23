(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS BULLET-GROUP
;;;;;;;;;;;;;;;;;;;;
; A group of bullets. Grouping bullets together has the benefit of being able to
; grant them special behavior (like chasing the player) via effects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bullet-group ()
  ((bullets :initform (make-array 1 :fill-pointer 0 :adjustable t) :accessor @bullets)
   (effects :initform nil :accessor @effects)
   (compact :initform t :initarg :compact :accessor @compact)))

(defun new-group (&key (compact t))
  (let ((g (make-instance 'bullet-group :compact compact)))
    (push g (@groups *screen*))
    g))

(defun add-to-group (bullet group)
  (let ((i (position nil (@bullets group))))
    (if (and (@compact group) i)
      (setf (elt (@bullets group) i) bullet)
      (vector-push-extend bullet (@bullets group)))))

(defmethod kill-group (group)
  (setf (@groups *screen*) (delete group (@groups *screen*))))

(defun apply-to-group (group fn &rest args)
  (loop for bullet across (@bullets group) do
    (unless (@dead bullet)
      (apply fn bullet args))))

(defmethod tick ((bg bullet-group) dt)
  (dolist (effect (@effects bg))
    (decf (@time effect) dt)
    (when (<= (@time effect) 0)
      (when-let (fn (@fn effect))
	(loop for bullet across (@bullets bg) do
	  (unless (or (not bullet) (@dead bullet))
	    (funcall fn bullet))))
      (when-let (final-fn (@final-fn effect))
	(funcall final-fn bg effect))))
  (setf (@effects bg) (delete-if #'(lambda (e) (<= (@time e) 0)) (@effects bg)))
  (map-into (@bullets bg) #'(lambda (b) (when (and b (not (@dead b))) b)) (@bullets bg)))

; Allows you to define effect final functions that do things to bullets one by
; one, with a time interval. The first list contains the names of the arguments
; to the generated functions, with one - the interval - being required.
; The second list holds the used names for the group, effect and counter,
; respectively. The third list is for optional settings, via keywords.
(defmacro defbyone (name (interval &rest arglist) (group effect i) (&key reversed kill-group) &body body)
  `(defun ,name ,(cons interval arglist)
     (let ((,i ,(if reversed `(length (@bullets ,group)) 0)))
       (lambda (,group ,effect)
	 (if (< ,i (length (@bullets ,group)))
	   (progn
	     ,@body
	     (incf (@time ,effect) ,interval)
	     (incf ,i))
	   ,(when kill-group
		  `(kill-group ,group)))))))

(defbyone aim-by-one (delay &key (vel nil)) (group effect i) ()
  (let ((bullet (elt (@bullets group) i)))
    (when vel
      (set-length (@velocity bullet) vel))
    (set-angle (@velocity bullet) (angle-between (@position bullet) (@position (@player *screen*))))))

(defbyone split-by-one (delay count spread &key (vel nil) (at-player nil) (rand-spread nil)) (group effect i) ()
  (let* ((bullet (elt (@bullets group) i)))
    (setf (@dead bullet) t)
    (fire-bullets (@position bullet) (if vel vel (get-length (@velocity bullet)))
		  (if at-player
		    (angle-between (@position bullet) (@position (@player *screen*)))
		    (get-angle (@velocity bullet))) count :spread spread :rand-spread rand-spread)))
