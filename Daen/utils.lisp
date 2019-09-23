(in-package :narvius.game.daen)

(defmacro doc (fun)
  "Returns the documentation string of function `FUN`, if any."
  `(documentation ',fun 'function))

(defmacro when-let (binding &body body)
  "Takes a single binding and a body of forms. If the value in the binding is
  true, then the body is executed with that binding."
  `(let (,binding)
     (when ,(first binding)
       ,@body)))

(defmacro if-let (binding true &optional false)
  `(let (,binding)
     (if ,(first binding)
       ,true
       ,false)))

(defmacro internal-step~ (sym &rest syms)
  (if (null syms)
    sym
    `(,sym (internal-step~ ,@syms))))

(defmacro ~ (&rest syms)
  `(internal-step~ ,@(nreverse syms)))

(defmacro expand-to-single-form (l)
  (if (and l (listp l) (listp (car l))) `(progn ,@l) l))

(defun @x  (x) (sdl:x x))
(defun @y  (x) (sdl:y x))
(defun @x2 (x) (sdl:x2 x))
(defun @y2 (x) (sdl:y2 x))
(defun @w  (x) (sdl:width x))
(defun @h  (x) (sdl:height x))
(defun s@x  (x v) (setf (sdl:x x)      v))
(defun s@y  (x v) (setf (sdl:y x)      v))
(defun s@x2 (x v) (setf (sdl:x2 x)     v))
(defun s@y2 (x v) (setf (sdl:y2 x)     v))
(defun s@w  (x v) (setf (sdl:width x)  v))
(defun s@h  (x v) (setf (sdl:height x) v))
(defsetf @x  s@x)
(defsetf @y  s@y)
(defsetf @x2 s@x2)
(defsetf @y2 s@y2)
(defsetf @w  s@w)
(defsetf @h  s@h)

(defun angle-between (p1 p2)
  "Returns the angle from `P1` to `P2`."
  (atan (- (@y p2) (@y p1)) (- (@x p2) (@x p1))))

(defun inside-p (rect point)
  "Returns whether the point is inside the rectangle."
  (and (<= (@x rect) (@x point) (@x2 rect))
       (<= (@y rect) (@y point) (@y2 rect))))

(defun intersects-p (r1 r2)
  "Returns whether two rectangles intersect."
  (and (< 0 (- (min (@x2 r1) (@x2 r2)) (max (@x r1) (@x r2))))
       (< 0 (- (min (@y2 r1) (@y2 r2)) (max (@y r1) (@y r2))))))

(defun clamp (val lower-limit upper-limit)
  "Returns `VAL`, or the closest limit, if it is outside of them."
  (max lower-limit (min val upper-limit)))

(defun get-length (p)
  "Gets the points distance from #(0 0)."
  (sqrt (+ (* (@x p) (@x p)) (* (@y p) (@y p)))))

(defun get-angle (p)
  "Gets the angle between #(0 0) and the point."
  (atan (@y p) (@x p)))

(defun set-length (p length)
  "Sets the point to have it's length (distance from #(0 0)) be equal to `LENGTH`,
  while maintaining the same angle."
  (let ((angle (get-angle p)))
    (sdl:set-point-* p :x (* length (cos angle)) :y (* length (sin angle)))))

(defun set-angle (p angle)
  (let ((length (get-length p)))
    (sdl:set-point-* p :x (* length (cos angle)) :y (* length (sin angle)))))

(defun add-point-* (p1 x y)
  (sdl:set-point-* p1 :x (+ (@x p1) x) :y (+ (@y p1) y)))

(defun add-point (p1 p2)
  (multiple-value-call #'add-point-* p1 (sdl:point-* p2)))

(defun to-carthesian (r phi &optional (p (sdl:point)))
  (sdl:set-point-* p :x (* r (cos phi)) :y (* r (sin phi))))

(defun to-polar (p)
  (values (get-length p) (get-angle p)))

(defun random-elt (seq)
  (elt seq (random (length seq))))