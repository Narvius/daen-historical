;; TODO LIST:
; 1) Settings - for example hold/switch for slow-move
; 2) More content
; 3) Write vecto-based string draw function (for cffi-less ttf-support)

(defpackage :narvius.game.daen
  (:use :common-lisp)
  (:export :main))

(in-package :narvius.game.daen)

(ql:quickload '(:lispbuilder-sdl))

(load (first (directory "Daen/utils.lisp")))
(dolist (f (directory "Daen/*.lisp"))
  (unless (search "main.lisp" (write-to-string f))
    (load f))) 

(defparameter *size-x* 800)
(defparameter *size-y* 600)

(defvar *size-rect* nil)
(defvar *play-rect* nil)

(defparameter *font-1* (sdl:initialise-font sdl:*font-8x13*))
(defparameter *font-2* (sdl:initialise-font sdl:*font-9x15*))

(defvar *levels* nil)
(defvar *screen* nil)
(defvar *next-screen* nil)
(defvar *settings* nil)

(defun game-tick (ticks dt)
  (declare (ignore ticks))
  (when *screen*
    (tick *screen* dt)))

(defun event-loop (screen)
  (sdl:with-events (:poll)
    (:quit-event ()
      t)
    (:user-event ()
      (close-screen screen)
      (setf *screen* nil)
      (sdl:push-quit-event))
    (:key-down-event (:key key)
      (keyboard-input screen key t))
    (:key-up-event (:key key)
      (keyboard-input screen key nil))
    (:idle ()
      (sdl:clear-display sdl:*black*)
      (render screen)
      (sdl:update-display))))

(defun run-screen ()
  (setf *screen* *next-screen*)
  (setf *next-screen* nil)
  (event-loop *screen*))

(defun init-levels ()
  (let ((*package* (find-package :narvius.game.daen)))
    (dolist (path (directory "levels/*.lvl"))
      (with-open-file (f path)
	(push (eval (read f)) *levels*))))
  (setf *levels* (nreverse *levels*)))

(defun init ()
  (setf *next-screen* (create-main-menu)
        *settings* (make-instance 'settings)
	*size-rect* (sdl:rectangle :w 800 :h 600)
	*play-rect* (sdl:rectangle :w 600 :h 600))
  (sdl:init-subsystems sdl:sdl-init-everything)
  (sdl:window *size-x* *size-y* :fullscreen nil
	      :bpp 32 :any-format t :title-caption "DanmakuEngine" :position t
	      :fps (make-instance 'sdl:fps-unlocked :ps-fn #'game-tick))
  (init-levels))

(defun clean ()
  (setf *screen* nil
	*next-screen* nil
	*levels* nil
	*settings* nil
	*size-rect* nil
	*play-rect* nil)
  (sdl:quit-subsystems sdl:sdl-init-everything))

(defun main ()
  (init)
  (loop while *next-screen* do
    (run-screen))
  (clean))