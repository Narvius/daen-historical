(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS MAIN MENU
;;;;;;;;;;;;;;;;;;;;
; The initial screen whenever the game is launched.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass main-menu (screen)
  ((selection :initform 0 :initarg :selection :accessor @selection)
   (items :initform '("Level Select" "Quit") :accessor @items)))

(defun create-main-menu ()
  (make-instance 'main-menu))

(defmethod render ((s main-menu))
  (sdl:draw-rectangle *size-rect* :color sdl:*white*)
  ; The "title".
  (sdl:draw-string-solid-* "DanmakuEngine v4 (13-01-2013)" 70 35 :font *font-2*)
  (sdl:draw-string-solid-* "- by Narvius" 135 70 :font *font-1*)
  ; The items.
  (let ((i -1)
	(y (- 570 (* 35 (length (@items s))))))
    (dolist (item (@items s))
      (sdl:draw-string-shaded-* item (- 765 (if (= (@selection s) (incf i)) 60 0)) (incf y 35) sdl:*white*
				(if (= (@selection s) i) sdl:*blue* sdl:*black*)
				:justify :right :font *font-2*))))

(defmethod keyboard-input ((s main-menu) key downp)
  (when downp
    (case key
      ((:sdl-key-up   :sdl-key-w) (setf (@selection s) (mod (1- (@selection s)) (length (@items s)))))
      ((:sdl-key-down :sdl-key-s) (setf (@selection s) (mod (1+ (@selection s)) (length (@items s)))))
      ((:sdl-key-enter :sdl-key-space) (push-close-screen-event)))))

(defmethod close-screen ((s main-menu))
  (prepare-next-screen (case (@selection s)
			 (0 (create-level-chooser)))))