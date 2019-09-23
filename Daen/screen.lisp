(in-package :narvius.game.daen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS SCREEN
;;;;;;;;;;;;;;;;;;;;
; The very basic base class for a screen, which describes an entire part of the
; game, like the main menu, or an actual gameplay screen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass screen ()
  ())

(defun prepare-next-screen (screen)
  (setf *next-screen* screen))

(defun push-close-screen-event ()
  (sdl:push-user-event :code 0 :data1 ccl:+null-ptr+ :data2 ccl:+null-ptr+))

(defmethod render ((s screen)))
(defmethod tick ((s screen) dt)
  (declare (ignore dt)))
(defmethod keyboard-input ((s screen) key downp)
  (declare (ignore key downp)))
(defmethod close-screen ((s screen)))
