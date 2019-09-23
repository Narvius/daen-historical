(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS LEVEL PLAYER
;;;;;;;;;;;;;;;;;;;;
; The heartpiece. This screen basically runs the game itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass level-player (screen)
  ((player :initform (make-instance 'player :pos (sdl:point :x 300 :y 300)) :accessor @player)
   (directors :initform nil :accessor @directors)
   (bullets :initform nil :accessor @bullets)
   (groups :initform nil :accessor @groups)
   (time :initform 0 :initarg :time :accessor @time)
   (max-time :initform 0 :initarg :max-time :accessor @max-time)
   (wait-for-bullets :initform t :accessor @wait-for-bullets)
   (initial-wait :initform t :accessor @initial-wait)
   (paused :initform nil :accessor @paused) ; Also doubles as selection for the pause menu.
   ))

(defun create-level-player (lvl)
  (let ((s (make-instance 'level-player :time 1000 :max-time (@duration lvl))))
    (setf (@directors s) (mapcar #'copy-director (@directors lvl)))
    s))

(defun get-player ()
  (~ *screen* @player))

(defun time-out-p (s)
  (< (@time s) 0))

(defmethod render ((s level-player))
  ; Entities
  (draw (@player s))
  (unless (time-out-p s)
    (dolist (director (@directors s))
      (draw director)))
  (dolist (bullet (@bullets s))
    (draw bullet))
  ; Outlines
  (sdl:draw-rectangle *size-rect* :color sdl:*white*)
  (sdl:draw-box-* (@w *play-rect*) 0 (- *size-x* (@w *play-rect*)) *size-y*
		  :color sdl:*black* :stroke-color sdl:*white*)
  ; Time Bar
  (sdl:draw-string-solid-* "Time" 630 560 :color sdl:*white* :justify :center :font *font-1*)
  (sdl:draw-rectangle-* 615 50 30 500 :color sdl:*white*)
  (let ((remaining (if (@initial-wait s)
		     (round (* 498 (@time s) (/ 1000)))
		     (round (* 498 (max 0 (@time s)) (/ (@max-time s)))))))
    (sdl:draw-box-* 616 (- 549 remaining) 28 remaining :color (if (@initial-wait s) sdl:*green* sdl:*red*)))
  ; Time Bar subdivisions
  (dotimes (i 4)
    (let ((y (+ 50 (* i 125))))
      (sdl:draw-line-* 615 y 630 y :color sdl:*white*)
      (dotimes (j 4)
	(let ((y2 (+ y (* (1+ j) 25))))
	  (sdl:draw-line-* 615 y2 622 y2)))))
  ; Stats
  (sdl:draw-string-solid-* "Hits" 660 70 :color sdl:*white* :font *font-2*)
  (sdl:draw-string-solid-* (write-to-string (@hit-count (@player s))) 740 110
			   :justify :right :color sdl:*white* :font *font-2*)
  (sdl:draw-string-solid-* "Graze" 660 160 :color sdl:*white* :font *font-2*)
  (sdl:draw-string-solid-* (write-to-string (@graze-count (@player s))) 740 190
			   :justify :right :color sdl:*white* :font *font-2*)
  ; Pause Menu, if applicable
  (when (@paused s)
    (sdl:draw-box-* 250 150 300 180 :color sdl:*black* :stroke-color sdl:*white*)
    (sdl:draw-string-solid-* "PAUSED" 400 180 :justify :center :font *font-2*)
    (sdl:draw-string-shaded-* "Continue" 400 240 sdl:*white* (if (= (@paused s) 0) sdl:*blue* sdl:*black*)
			      :justify :center :font *font-1*)
    (sdl:draw-string-shaded-* "Quit" 400 280 sdl:*white* (if (= (@paused s) 1) sdl:*blue* sdl:*black*)
			      :justify :center :font *font-1*)))

(defmethod tick ((s level-player) dt)
  (unless (@paused s)
    (tick (@player s) dt)
    (dolist (bullet (@bullets s))
      (tick bullet dt)
      (handle-player-collision bullet))
    (unless (@initial-wait s)
      (dolist (group (@groups s))
	(tick group dt))
      (unless (time-out-p s)
	(dolist (director (@directors s))
	  (tick director dt))))
    (setf (@bullets s) (delete-if #'bullet-removable-p (@bullets s)))
    (decf (@time s) dt)
    (when (and (time-out-p s)
	       (@initial-wait s))
      (setf (@time s) (@max-time s) (@initial-wait s) nil))
    (when (and (time-out-p s)
	       (or (not (@wait-for-bullets s))
		   (= 0 (length (@bullets s)))))
      (push-close-screen-event))))

(defun handle-dir (s dir downp)
  (if downp
    (push dir (~ s @player @dirs))
    (setf (@dirs (@player s)) (remove dir (~ s @player @dirs)))))

(defmethod keyboard-input ((s level-player) key downp)
  (if (not (@paused s))
    (case key
      ((:sdl-key-up    :sdl-key-w) (handle-dir s '( 0 -1) downp))
      ((:sdl-key-down  :sdl-key-s) (handle-dir s '( 0  1) downp))
      ((:sdl-key-left  :sdl-key-a) (handle-dir s '(-1  0) downp))
      ((:sdl-key-right :sdl-key-d) (handle-dir s '( 1  0) downp))
      (:sdl-key-lshift (setf (~ s @player @slow-mode) downp))
      (:sdl-key-escape (setf (@paused s) 0)))
    (when downp
      (case key
	((:sdl-key-up :sdl-key-down :sdl-key-w :sdl-key-s) (setf (@paused s) (- 1 (@paused s))))
	((:sdl-key-enter :sdl-key-space) (if (= (@paused s) 0)
					     (setf (@paused s) nil)
					     (progn
					       (prepare-next-screen (create-main-menu))
					       (push-close-screen-event))))))))

(defmethod close-screen ((s level-player))
  (unless *next-screen*
    (prepare-next-screen (create-level-summary (~ s @player @hit-count) (~ s @player @graze-count)))))