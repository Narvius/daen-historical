(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS LEVEL SUMMARY
;;;;;;;;;;;;;;;;;;;;
; The screen that is showed after a level is finished. Shows the score and
; will later probably display highscore tables and allow you to save replays.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass level-summary (screen)
  ((hits :initform 0 :initarg :hits :accessor @hits)
   (graze :initform 0 :initarg :graze :accessor @graze)))

(defun create-level-summary (hits graze)
  (make-instance 'level-summary :hits hits :graze graze))

(defmethod render ((s level-summary))
  (sdl:draw-rectangle *size-rect* :color sdl:*white*)
  (sdl:draw-string-solid-* "Level finished" 400 50 :justify :center :font *font-2*)
  (sdl:draw-string-solid-* "Hits taken" 400 100 :justify :center :font *font-2*)
  (sdl:draw-string-solid-* (write-to-string (@hits s)) 400 120 :justify :center :font *font-1*)
  (sdl:draw-string-solid-* "Bullets grazed" 400 150 :justify :center :font *font-2*)
  (sdl:draw-string-solid-* (write-to-string (@graze s)) 400 170 :justify :center :font *font-1*))

(defmethod keyboard-input ((s level-summary) key downp)
  (declare (ignore key))
  (when downp
    (push-close-screen-event)))

(defmethod close-screen ((s level-summary))
  (prepare-next-screen (create-main-menu)))
