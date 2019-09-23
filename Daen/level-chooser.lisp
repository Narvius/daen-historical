(in-package :narvius.game.daen)

(defclass level-chooser (screen)
  ((grid-selection :initform 0 :initarg :grid-selection :accessor @grid-selection)
   (selection :initform 0 :initarg :selection :accessor @selection)))

(defun create-level-chooser ()
  (make-instance 'level-chooser))

(defun draw-grid-square (x y number highlight-p rect)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* x y 30 30 rect)
		:stroke-color sdl:*white*
		:color (if highlight-p sdl:*blue* sdl:*black*))
  (sdl:draw-string-solid-* (format nil "~2,'0d" number) x y :justify :center :font *font-2*))

(defun to-readable-time (ms)
  (let ((result "")
	(mins (floor (/ ms 60000)))
	(secs (mod (floor (/ ms 1000)) 60)))
    (when (> mins 0)
      (setf result (format nil "~amin " mins)))
    (when (or (> mins 0) (> secs 0))
      (setf result (concatenate 'string result (format nil "~as" secs))))
    result))

(defun difficulty-color (n)
  (case n
    ((1 2 3 4) sdl:*green*)
    ((5 6 7) sdl:*yellow*)
    ((8 9) sdl:*red*)
    (10 sdl:*white*)))

(defun first-column-p (s)
  (= 0 (mod (@grid-selection s) 16)))

(defun last-column-p (s)
  (or (= 15 (mod (@grid-selection s) 16))
      (= (1- (length *levels*)) (@grid-selection s))))

(defmethod render ((s level-chooser))
  ; Selection Grid
  (sdl:draw-rectangle-* 75 30 650 290)
  (let ((rect (sdl:rectangle)))
    (dotimes (i (length *levels*))
      (draw-grid-square (+ 100 (* 40 (mod i 16)))
			(+ 60 (* 40 (floor (/ i 16))))
			i (and (= (@selection s) 0) (= (@grid-selection s) i)) rect)))
  ; Remaining Menu Items
  (sdl:draw-rectangle-* 90 340 620 40)
  (sdl:draw-string-shaded-* "Return" 400 355 sdl:*white* (if (= (@selection s) 1) sdl:*blue* sdl:*black*)
			    :justify :center :font *font-2*)
  ; Info Panel
  (sdl:draw-rectangle-* 75 400 650 180)
  (sdl:draw-rectangle-* 110 420 175 40)
  (sdl:draw-rectangle-* 110 470 175 40)
  (sdl:draw-rectangle-* 110 520 175 40)
  (sdl:draw-string-solid-* "Title"      115 425 :font *font-1*)
  (sdl:draw-string-solid-* "Difficulty" 115 475 :font *font-1*)
  (sdl:draw-string-solid-* "Duration"   115 525 :font *font-1*)
  (dotimes (i 10)
    (sdl:draw-box-* (- 265 (* 15 i)) 495 10 10 :stroke-color sdl:*white* :color sdl:*black*))
  (when-let (lvl (and (= (@selection s) 0) (elt *levels* (@grid-selection s))))
    (sdl:draw-string-solid-* (@name lvl) 280 440 :justify :right :font *font-2*)
    (sdl:draw-string-solid-* (to-readable-time (@duration lvl)) 280 540 :justify :right :font *font-2*)
    (let ((color (difficulty-color (@difficulty lvl))))
      (dotimes (i (@difficulty lvl))
	(sdl:draw-box-* (- 265 (* 15 i)) 495 10 10 :stroke-color sdl:*white* :color color)))))

(defmethod keyboard-input ((s level-chooser) key downp)
  (when downp
    (if (= 0 (@selection s))
      (case key
	((:sdl-key-up    :sdl-key-w) (when (< (decf (@grid-selection s) 16) 0)
				       (setf (@grid-selection s) (mod (@grid-selection s) 16)
					     (@selection s) 1)))
	((:sdl-key-down  :sdl-key-s) (when (<= (length *levels*) (incf (@grid-selection s) 16))
				       (setf (@grid-selection s) (mod (@grid-selection s) 16)
					     (@selection s) 1)))
	((:sdl-key-left  :sdl-key-a) (when (= 15 (mod (decf (@grid-selection s)) 16))
				       (setf (@grid-selection s) (min (+ (@grid-selection s) 16)
								      (~ *levels* length 1-)))))
	((:sdl-key-right :sdl-key-d) (when (or (= 0 (mod (incf (@grid-selection s)) 16))
					       (= (length *levels*) (@grid-selection s)))
				       (decf (@grid-selection s) (mod (@grid-selection s) 16))))
	((:sdl-key-enter :sdl-key-space) (push-close-screen-event))
	((:sdl-key-escape) (setf (@selection s) 1) (push-close-screen-event)))
      (case key
	((:sdl-key-up :sdl-key-w) (progn
				    (setf (@selection s) 0
					  (@grid-selection s) (+ (* 16 (floor (length *levels*) 16))
								 (@grid-selection s)))
				    (when (<= (length *levels*) (@grid-selection s))
				      (decf (@grid-selection s) 16))))
	((:sdl-key-down :sdl-key-s) (setf (@selection s) 0))
	((:sdl-key-enter :sdl-key-space :sdl-key-escape) (push-close-screen-event))))))

(defmethod close-screen ((s level-chooser))
  (case (@selection s)
    (0 (prepare-next-screen (create-level-player (elt *levels* (@grid-selection s)))))
    (1 (prepare-next-screen (create-main-menu)))))