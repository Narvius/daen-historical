(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS PLAYER
;;;;;;;;;;;;;;;;;;;;
; Holds a lot of information regarding the player, including, but not limited
; to: Speed, Hit Count, Immunity Time (after being hit).
; Also has different drawing behavior than regular physicals and is prepared
; to interact with the key input system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass player (entity)
  ((speed :initform 320 :accessor @speed)
   (slow-speed :initform 160 :accessor @slow-speed)
   (slow-mode :initform nil :accessor @slow-mode)
   (dirs :initform nil :accessor @dirs)
   (hit-width :initform 8 :accessor @hit-width)
   (hit-height :initform 8 :accessor @hit-height)
   (hit-count :initform 0 :accessor @hit-count)
   (graze-count :initform 0 :accessor @graze-count)
   (immune-time :initform 0 :accessor @immune-time)
   (immune-time-on-hit :initform 1500 :accessor immune-time-@on-hit)))

(defun handle-player-collision (b)
  (let ((p (@player *screen*)))
    (when (and (not (@dead b))
	       (intersects-p (get-drawbox p) (get-hitbox b))
	       (<= (@immune-time p) 0))
      (if (inside-p (get-hitbox b) (@position p))
	  (setf (@immune-time p) (immune-time-@on-hit p) (@hit-count p) (1+ (@hit-count p)))
	  (when (not (@grazed b))
	    (graze b)
	    (incf (@graze-count p)))))))

(defun angle-to-player (e)
  (angle-between (@position e) (@position (get-player))))

(defun distance-to-player (e)
  (sdl:distance (@position e) (@position (get-player))))

(defmethod tick :after ((p player) dt)
  ; Handle immunity.
  (decf (@immune-time p) dt)
  (setf (@color p) (if (< 0 (@immune-time p)) sdl:*blue* sdl:*black*))
  ; Set move speed.
  (if (= 0 (length (@dirs p)))
    (set-length (@velocity p) 0)
    (let ((pm (apply #'mapcar #'+ (@dirs p))))
      (sdl:set-point-* (@velocity p) :x (first pm) :y (second pm))
      (set-length (@velocity p) (if (@slow-mode p) (@slow-speed p) (@speed p)))))
  ; Handle borders.
  (setf (~ p @position @x) (clamp (~ p @position @x) 0 (@w *play-rect*))
	(~ p @position @y) (clamp (~ p @position @y) 0 (@h *play-rect*))))

(defmethod get-hitbox ((p player))
  (sdl:rectangle-from-midpoint-* (~ p @position @x) (~ p @position @y) 3 3 (@rect p)))

(defmethod get-drawbox ((p player))
  (sdl:rectangle-from-midpoint-* (@x (@position p)) (@y (@position p))
				 (@width p) (@height p) (@rect p)))

(defmethod draw :after ((p player))
  (when (@slow-mode p)
    (sdl:draw-box (get-hitbox p) :color sdl:*red*)))
