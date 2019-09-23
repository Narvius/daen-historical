(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS ENTITY
;;;;;;;;;;;;;;;;;;;;
; Base class for something that has a physical representation in the game world.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass entity ()
  ((position :initform (sdl:point) :initarg :pos :accessor @position)
   (velocity :initform (sdl:point) :initarg :vel :accessor @velocity)
   (width :initform 40 :initarg :w :accessor @width)
   (height :initform 40 :initarg :h :accessor @height)
   (color :initform sdl:*black* :initarg :color :accessor @color)
   (rect :initform (sdl:rectangle) :accessor @rect)
   (dead :initform nil :initarg :dead :accessor @dead)))

(defgeneric tick (e dt))
(defgeneric get-hitbox (e))
(defgeneric get-drawbox (e))
(defgeneric draw (e))

(defmethod tick ((p entity) dt)
  (add-point-* (@position p) (* (@x (@velocity p)) dt 1/1000) (* (@y (@velocity p)) dt 1/1000)))

(defmethod get-hitbox ((p entity))
  (sdl:rectangle-from-midpoint-* (@x (@position p)) (@y (@position p))
				 (@width p) (@height p) (@rect p)))

(defmethod get-drawbox ((p entity))
  (get-hitbox p))

(defmethod draw ((p entity))
  (sdl:draw-box (get-drawbox p) :color (@color p) :stroke-color sdl:*white*))

(defun distance (e1 e2)
  (sdl:distance (@position e1) (@position e2)))

(defun circular-move-around (ph p turn)
  (let ((angle (+ (angle-between p (@position ph)) turn))
	(dist (sdl:distance (@position ph) p)))
    (sdl:set-point-* (@position ph) :x (+ (@x p) (* dist (cos angle))) :y (+ (@y p) (* dist (sin angle))))))
