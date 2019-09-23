(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS BULLET
;;;;;;;;;;;;;;;;;;;;
; Well, it's a bullet. It hits the player. Use fire-bullet(s) to spawn them.
; Cannot have special behavior - for that, use bullet-groups.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bullet (entity)
  ((width :initform 20)
   (height :initform 20)
   (dead :initform nil :accessor @dead)
   (grazed :initform nil :accessor @grazed)))

(defun spawn-bullet (pos vel &key (group nil) (color sdl:*red*) (w 15) (h 15))
  (let ((bullet (make-instance 'bullet :pos (sdl:copy-point pos) :vel (sdl:copy-point vel)
			  :color color :w w :h h)))
    (push bullet (@bullets *screen*))
    (when group
      (add-to-group bullet group))
    bullet))

(defun fire-bullets (pos vel dir count
		     &key (extra-fn nil) (group nil) (vel-rand 0) (spread 0) (rand-spread nil))
  (let ((p (sdl:point))
	(angle (- (/ spread 2))))
    (dotimes (i count)
      (let ((b (spawn-bullet pos (to-carthesian (+ vel (if (> vel-rand 0) (random vel-rand) 0))
						(+ dir angle (if rand-spread (random spread) 0)) p)
			     :group group)))
	(unless (or rand-spread (= count 1))
	  (incf angle (/ spread count)))
	(when extra-fn
	  (funcall extra-fn b))))))

(defun fire-bullet (pos vel dir &key (group nil))
  (fire-bullets pos vel dir 1 :group group))

(defun split-bullet (b count &key (dir nil) (vel nil) (group nil) (spread 0) (rand-spread nil))
  (setf (@dead b) t)
  (fire-bullets (@position b) (or vel (get-length (@velocity b))) (or dir (get-angle (@velocity b))) count
		:group group :spread spread :rand-spread rand-spread))

(defun player-redirect (bullet &key (angle-mod 0) (randomize-angle nil))
  (let ((angle (angle-between (@position bullet) (~ *screen* @player @position))))
    (set-angle (@velocity bullet) (+ angle (if (and (> angle-mod 0) randomize-angle)
					     (- (random angle-mod) (/ angle-mod 2))
					     angle-mod)))))

(defun random-redirect (bullet &key (min 0) (max (* 2 pi)))
  (set-angle (@velocity bullet) (+ min (random (- max min)))))

(defun accel-bullet (bullet change &key (multiply nil) (random-factor))
  (let ((change (+ change (if (= 0 random-factor) 0 (random random-factor)))))
    (set-length (@velocity bullet) (funcall (if multiply #'* #'+) (get-length (@velocity bullet)) change))))

(defun turn-bullet (bullet change &key (random-factor 0))
  (let ((change (+ change (if (= 0 random-factor) 0 (random random-factor)))))
    (set-angle (@velocity bullet) (+ (get-angle (@velocity bullet)) change))))

(defun stop-bullet (bullet)
  (sdl:set-point-* (@velocity bullet) :x 0 :y 0))

(defun bullet-removable-p (b)
  (or (@dead b) (not (inside-p *play-rect* (@position b)))))

(defun graze (b)
  (setf (@color b) sdl:*magenta*)
  (setf (@grazed b) t))
