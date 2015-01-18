;;;; asteroid-wars.lisp

(in-package #:asteroid-wars)

;;;;;;;;;;;;;;;;;;;;;;;; CONFIG/PRESETS ;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *data-root* (asdf:system-source-directory 'asteroid-wars))
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *game-width* 1024)
(defparameter *game-height* 768)
(defparameter *game-state* 0) ; 0:menu/intro, 1:in game, 2:game over

(defparameter *pause* nil)
(defparameter *game-tick* 0)
(defparameter *wave* 1)

;;;; Player Params
(defparameter *player* nil)
(defparameter *player-laser* nil)
(defparameter *player-lives* 0) ; default 3
(defparameter *player-score* 0)
(defparameter *player-shield* nil)
(defparameter *player-shield-timer* 0)
(defparameter *thrust* nil)

(defparameter +acceleration+ 0.1)
(defparameter +turn-speed+ 3)
(defparameter +max-speed+ 5)

;;;; Asteroid Params
(defparameter *asteroid-count* 0) ; default (11 + wave) large per wave
(defparameter *asteroids* nil)
(defparameter *asteroid-field* 65)
(defparameter *asteroid-schedule* nil)

;;;; Enemy Params
(defparameter *enemy* nil)
(defparameter *enemy-laser* nil)
(defparameter *enemy-schedule* nil)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *sound-thrust* nil)
(defparameter *soundfx* nil)

;;;; GFX Params
;(defparameter *gfx-bg* (merge-pathnames "gfx-bg.jpg" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf-12* 
  (make-instance 'SDL:ttf-font-definition
		 :size 12
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-18* 
  (make-instance 'SDL:ttf-font-definition
		 :size 18
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-24* 
  (make-instance 'SDL:ttf-font-definition
		 :size 24
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-32* 
  (make-instance 'SDL:ttf-font-definition
		 :size 32
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *ttf-font-small* nil)
(defparameter *ttf-font-normal* nil)
(defparameter *ttf-font-large* nil)
(defparameter *ttf-font-huge* nil)


;;;;;;;;;;;;;;;;;;;;;;;; SHIP/ASTEROID TEMPLATES ;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tmpl-player* '((0 -20) (-12 20) (-5 15) (5 15) (12 20)))
(defparameter *tmpl-player-flame* '((-5 15) (0 30) (5 15)))

(defparameter *tmpl-asteroid-1* '((0 -60) (40 -40) (60 0) (40 40) (0 60)
				  (-40 40) (-60 0) (-40 -40)))
(defparameter *tmpl-asteroid-2* '((0 -30) (20 -30) (30 0) (20 20) (0 30)
				  (-20 20) (-30 0) (-20 -20)))
(defparameter *tmpl-asteroid-3* '((0 -15) (10 -15) (15 0) (10 10) (0 15)
				  (-10 10) (-15 0) (-10 -10)))

(defparameter *tmpl-ship-large-top* '((8 -15) (12 -8) (-12 -8) (-8 -15)))
(defparameter *tmpl-ship-large-middle* '((-12 -8) (-35 0) (35 0) (12 -8)))
(defparameter *tmpl-ship-large-bottom* '((-35 0) (-15 10) (15 10) (35 0)))

;;;;;;;;;;;;;;;;;;;;;;;; STRUCTS/CLASSES ;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct player
  (x 0)
  (y 0)
  (vx 0)
  (vy 0)
  (angle 0))


(defstruct player-laser
  (x 0)
  (y 0)
  (vx 0)
  (vy 0)
  (angle 0)
  (time 0))
  

(defstruct asteroid
  (x 0)
  (y 0)
  (vx 0)
  (vy 0)
  (angle 0)
  (rot 0)
  (stage 1)
  (shape nil))


(defstruct enemy
  (x 0)
  (y 0)
  (vx 0)
  (vy 0)
  (angle 0)
  (ship-type nil))


(defstruct enemy-laser
  (x 0)
  (y 0)
  (vx 0)
  (vy 0)
  (angle 0)
  (time 0))


;;;;;;;;;;;;;;;;;;;;;;;; SLIME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CONTINUABLE macro

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


;;;; UPDATE-SWANK function

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
			 (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))


;;;;;;;;;;;;;;;;;;;;;;;; MATHS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; SQUARE function

(defun square (x)
  (* x x))


;;;; DEG-TO-RAD function

(defun deg-to-rad (degs)
  (/ (* degs pi) 180))


;;;; RAD-TO-DEG function

(defun rad-to-deg (rads)
  (/ (* rads 180) pi))


;;;; ROTATE function

(defun rotate (pt-x pt-y pos-x pos-y angle)
  (let* ((dx (- (* pt-x (cos (deg-to-rad angle)))
		(* pt-y (sin (deg-to-rad angle)))))
	 (dy (+ (* pt-x (sin (deg-to-rad angle)))
		(* pt-y (cos (deg-to-rad angle))))))
    (list (+ (floor dx) pos-x) (+ (floor dy) pos-y))))


;;;;;;;;;;;;;;;;;;;;;;;; PHYSICS ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; SHIP-COLLIDE-ASTEROID function

(defun ship-collide-asteroid ()
  (let ((p *player*))
    (loop for a in *asteroids*
       do (ship-collide-asteroid-p a p))))


;;;; SHIP-COLLIDE-ASTEROID-P function

(defun ship-collide-asteroid-p (a p)
  (if (<= (sqrt (+ (square (- (asteroid-x a) (player-x p)))
		   (square (- (asteroid-y a) (player-y p)))))
	  (+ (asteroid-field-size (asteroid-stage a)) 10))
      (progn (split-asteroid a)
	     (unless (eq *player-shield* t)
	       (player-destroyed)
	       (play-sound 6)))))


;;;; SHIP-COLLIDE-ENEMY function

(defun ship-collide-enemy ()
  (let ((p *player*))
    (loop for e in *enemy*
       do (ship-collide-enemy-p e p))))


;;;; SHIP-COLLIDE-ENEMY-P function

(defun ship-collide-enemy-p (e p)
  (if (<= (sqrt (+ (square (- (enemy-x e) (player-x p)))
		   (square (- (enemy-y e) (player-y p)))))
	  (+ (enemy-field-size (enemy-ship-type e)) 10))
      (progn (setf *enemy* (remove e *enemy*))
	     (unless (eq *player-shield* t)
	       (player-destroyed)
	       (play-sound 6)))))


;;;; LASER-COLLIDE-ASTEROID function

(defun laser-collide-asteroid (l)
  (loop for a in *asteroids*
     do (if (<= (sqrt (+ (square (- (asteroid-x a) (player-laser-x l)))
			 (square (- (asteroid-y a) (player-laser-y l)))))
		(asteroid-field-size (asteroid-stage a)))
	    (progn (split-asteroid a)
		   (setf *player-laser* (remove l *player-laser*))
		   (update-score-kill 'asteroid (asteroid-stage a))
		   (play-sound (+ (random 4) 1))))))


;;;; LASER-COLLIDE-ASTEROID function

(defun laser-collide-enemy (l)
  (loop for e in *enemy*
     do (if (<= (sqrt (+ (square (- (enemy-x e) (player-laser-x l)))
			 (square (- (enemy-y e) (player-laser-y l)))))
		(enemy-field-size (enemy-ship-type e)))
	    (progn (setf *enemy* (remove e *enemy*))
		   (setf *player-laser* (remove l *player-laser*))
		   (update-score-kill 'enemy (enemy-ship-type e))
		   (play-sound (+ (random 4) 1))))))


;;;; ENEMY-LASER-COLLIDE-PLAYER function

(defun enemy-laser-collide-player (l)
  (let ((p *player*))
    (if (<= (sqrt (+ (square (- (enemy-laser-x l) (player-x p)))
		     (square (- (enemy-laser-y l) (player-y p)))))
	    20)
	(progn (setf *enemy-laser* (remove l *enemy-laser*))
	       (unless (eq *player-shield* t)
		 (player-destroyed)
		 (play-sound 6))))))

;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b &optional (font *ttf-font-normal*))
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)
			   :font font))


;;;; DRAW-BOX function

(defun draw-box (x y w h r g b)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* x y w h)
		:color (sdl:color :r r :g g :b b)))


;;;; DRAW-LINE function

(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		   :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE function

(defun draw-circle (x y rad r g b)
  (sdl:draw-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE-FILLED function

(defun draw-circle-filled (x y rad r g b)
  (sdl:draw-filled-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-ELLIPSE-FILLED function

(defun draw-ellipse-filled (x y rx ry r g b)
  (sdl:draw-filled-ellipse-* x y rx ry
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-POLYGON function

(defun draw-polygon (vertices r g b)
  (sdl:draw-polygon vertices :color (sdl:color :r r :g g :b b)))


;;;; DRAW-POLYGON-FILLED function

(defun draw-polygon-filled (vertices r g b)
  (sdl:draw-filled-polygon vertices :color (sdl:color :r r :g g :b b)))

;;;; PLAY-SOUND function

(defun play-sound (s &optional (loop-p nil))
  (sdl-mixer:play-sample (aref *soundfx* s) :loop loop-p))

(defun stop-sound (s &optional (loop-p nil))
  (sdl-mixer:halt-sample (aref *soundfx* s)))

;;;;;;;;;;;;;;;;;;;;;;;; ENEMY ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-ENEMY-SCHEDULE function

(defun create-enemy-schedule ()
  (setf *enemy-schedule* nil)

  (dotimes (x 5)
    (push (random (* 60 (+ 60 (* 5 *wave*)))) *enemy-schedule*)))


;;;; CHECK-ENEMY-START-TIME function

(defun check-enemy-start-time ()
  (dolist (e *enemy-schedule*)
    (if (<= e *game-tick*)
	(progn (create-enemy)
	       (setf *enemy-schedule* (remove e *enemy-schedule*))))))


;;;; CREATE-ENEMY function

(defun create-enemy (&optional (ship-type 1))
  (let ((x 0)
	(y 0)
	(vx (* (- (random 2.0) 1.0) 2))
	(vy (* (- (random 2.0) 1.0) 2))
	(enter-lr (random 2))
	(enter-any (random 4)))

    (cond ((= ship-type 1)
	   (if (zerop enter-lr)
	       (progn (setf x -10)
		      (setf y (+ (random 500) 150))
		      (setf vx 1.5)
		      (setf vy 0))
	       (progn (setf x *game-width*)
		      (setf y (+ (random 500) 150))
		      (setf vx -1.5)
		      (setf vy 0))))
	  
	  ((= ship-type 2)
	   (cond ((zerop enter-any) ;top
		  (progn (setf x (random *game-width*))
			 (setf y -10)))

		 ((= enter-any 1) ;right
		  (progn (setf x (+ *game-width* 10))
			 (setf y (random *game-height*))))

		 ((= enter-any 2) ;bottom
		  (progn (setf x (random *game-width*))
			 (setf y (+ *game-height* 10))))

		 (t (progn (setf x -10)
			   (setf y (random *game-height*)))))))
	

    (push (make-enemy :x x :y y :vx vx :vy vy :angle 0 :ship-type ship-type) *enemy*)))


;;;; UPDATE-ENEMIES function

(defun update-enemies ()
  (loop for e in *enemy*
     do (update-enemy-position e)))


;;;; UPDATE-ENEMY-POSITION function

(defun update-enemy-position (e)
  (let ((dx 60)
	(dy 40))

    (setf (enemy-x e) (+ (enemy-x e) (enemy-vx e)))
    (setf (enemy-y e) (+ (enemy-y e) (enemy-vy e)))

    (if (< (enemy-x e) (- dx))
	(if (= (enemy-ship-type e) 1)
	    (setf *enemy* (remove e *enemy*))
	    (setf (enemy-x e) (+ *game-width* dx))))

    (if (> (enemy-x e) (+ *game-width* dx))
	(if (= (enemy-ship-type e) 1)
	    (setf *enemy* (remove e *enemy*))
	    (setf (enemy-x e) (- dx))))

    (if (< (enemy-y e) (- dy))
	(setf (enemy-y e) (+ *game-height* dy)))

    (if (> (enemy-y e) (+ *game-height* dy))
	(setf (enemy-y e) (- dy))))

  (if (< (random 1000) (+ 10 (* 2 *wave*)))
      (enemy-fire-shot e)))


;;;; ENEMY-FIRE-SHOT function

(defun enemy-fire-shot (e)
  (let* ((vec-x (sin (deg-to-rad (random 360))))
	 (vec-y (cos (deg-to-rad (random 360))))
	 (vx (* 5 vec-x))
	 (vy (* 5 vec-y)))
    (if (= (enemy-ship-type e) 1)
	(push (make-enemy-laser :x (round (enemy-x e)) :y (round (enemy-y e))
				:vx vx :vy vy
				:time 90) *enemy-laser*)))

  (play-sound 7))


;;;; DRAW-ENEMIES function

(defun draw-enemies ()
    (loop for e in *enemy*
       do (progn (draw-enemy-ship e))))


(defun draw-enemy-ship (e)
  (let ((ship-top nil)
	(ship-middle nil)
	(ship-bottom nil))

    (dolist (s *tmpl-ship-large-top*)
      (push (rotate (first s) (second s) (enemy-x e) (enemy-y e) 0) ship-top))

    (dolist (s *tmpl-ship-large-middle*)
      (push (rotate (first s) (second s) (enemy-x e) (enemy-y e) 0) ship-middle))

    (dolist (s *tmpl-ship-large-bottom*)
      (push (rotate (first s) (second s) (enemy-x e) (enemy-y e) 0) ship-bottom))

    (draw-polygon ship-top 200 200 200)
    (draw-polygon ship-middle 200 200 200)
    (draw-polygon ship-bottom 200 200 200)))


;;;; DRAW-ENEMY-LASER function

(defun draw-enemy-laser ()
  (loop for l in *enemy-laser*
     do (draw-circle (round (enemy-laser-x l)) (round (enemy-laser-y l)) 1 255 0 0)))


;;;; UPDATE-ENEMY-LASER function

(defun update-enemy-laser ()
  (loop for l in *enemy-laser*
     do (update-enemy-laser-position l)))


;;;; UPDATE-ENEMY-LASER-POSITION function

(defun update-enemy-laser-position (l)
  (setf (enemy-laser-time l) (decf (enemy-laser-time l)))

  (if (<= (enemy-laser-time l) 0)
      (setf *enemy-laser* (remove l *enemy-laser*))
      (progn (setf (enemy-laser-x l) (+ (enemy-laser-x l) (enemy-laser-vx l)))
	     (setf (enemy-laser-y l) (- (enemy-laser-y l) (enemy-laser-vy l)))
	     (enemy-laser-collide-player l)

	     (if (< (enemy-laser-x l) -1)
		 (setf (enemy-laser-x l) (+ *game-width* 1)))

	     (if (> (enemy-laser-x l) (+ *game-width* 1))
		 (setf (enemy-laser-x l) -1))

	     (if (< (enemy-laser-y l) -1)
		 (setf (enemy-laser-y l) (+ *game-height* 1)))

	     (if (> (enemy-laser-y l) (+ *game-height* 1))
		 (setf (enemy-laser-y l) -1)))))


;;;; ENEMY-FIELD-SIZE function

(defun enemy-field-size (ship-type)
  (if (= ship-type 1)
      20))


;;;;;;;;;;;;;;;;;;;;;;;; ASTEROIDS ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CREATE-ASTEROID-SCHEDULE function

(defun create-asteroid-schedule ()
  (setf *asteroid-schedule* nil)

  (dotimes (x 4)
    (create-asteroid))

  (dotimes (x (+ 5 *wave*))
    (push (random (* 60 (+ 75 (* 5 *wave*)))) *asteroid-schedule*)))


;;;; CHECK-ASTEROID-START-TIME function

(defun check-asteroid-start-time ()
  (dolist (a *asteroid-schedule*)
    (if (<= a *game-tick*)
	(progn (create-asteroid)
	       (setf *asteroid-schedule* (remove a *asteroid-schedule*))))))


;;;; CREATE-ASTEROID function

(defun create-asteroid (&optional (stage 1) (x (+ (random 1020) 1)) (y (+ (random 700) 700)))
  (let ((asteroid (create-asteroid-shape stage)))
    (push (make-asteroid :x x
			 :y y
			 :vx (* (- (random 2.0) 1.0) stage)
			 :vy (* (- (random 2.0) 1.0) stage)
			 :angle 0
			 :rot (- (random 6.0) 3.0)
			 :stage stage
			 :shape asteroid) *asteroids*)))


;;;; CREATE-ASTEROID-SHAPE function

(defun create-asteroid-shape (stage)
  (let* ((asteroid nil)
	 (tmpl (cond ((= stage 2) *tmpl-asteroid-2*)
		     ((= stage 3) *tmpl-asteroid-3*)
		     (t *tmpl-asteroid-1*))))
    (dolist (s tmpl)
      (push (list (+ (first s) (- (random 16) 8))
		  (+ (second s ) (- (random 16) 8)))
	    asteroid))
    asteroid))


;;;; SPLIT-ASTEROID function

(defun split-asteroid (a)
  (if (< (asteroid-stage a) 3)
      (progn (create-asteroid (+ (asteroid-stage a) 1) (asteroid-x a) (asteroid-y a))
	     (create-asteroid (+ (asteroid-stage a) 1) (asteroid-x a) (asteroid-y a))))
  (setf *asteroids* (remove a *asteroids*)))


;;;; ASTEROID-FIELD-SIZE function

(defun asteroid-field-size (stage)
  (ash *asteroid-field* (- 1 stage)))


;;;; DRAW-ASTEROIDS function

(defun draw-asteroids ()
  (let ((asteroid nil))
    (loop for a in *asteroids*
       do (progn (dolist (roid (asteroid-shape a))
		   (push (rotate (first roid) (second roid)
				 (asteroid-x a) (asteroid-y a)
				 (asteroid-angle a)) asteroid))

		 (draw-polygon asteroid 255 255 255)
		 (setf asteroid nil)))))
    

;;;; UPDATE-ASTERIODS function

(defun update-asteroids ()
  (loop for a in *asteroids*
     do (update-asteroid-position a)))


;;;; UPDATE-ASTEROID-POSITION function

(defun update-asteroid-position (a)
  (let ((dx (asteroid-field-size (asteroid-stage a)))
	(dy (asteroid-field-size (asteroid-stage a))))

    (setf (asteroid-angle a) (+ (asteroid-angle a) (asteroid-rot a)))

    (setf (asteroid-x a) (+ (asteroid-x a) (asteroid-vx a)))
    (setf (asteroid-y a) (+ (asteroid-y a) (asteroid-vy a)))

    (if (< (asteroid-x a) (- dx))
	(setf (asteroid-x a) (+ *game-width* dx)))

    (if (> (asteroid-x a) (+ *game-width* dx))
	(setf (asteroid-x a) (- dx)))

    (if (< (asteroid-y a) (- dy))
	(setf (asteroid-y a) (+ *game-height* dy)))

    (if (> (asteroid-y a) (+ *game-height* dy))
	(setf (asteroid-y a) (- dy)))))


;;;;;;;;;;;;;;;;;;;;;;;; PLAYER ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CREATE-PLAYER function

(defun create-player ()
  (setf *player* (make-player :x (/ *game-width* 2)
			      :y (/ *game-height* 2)
			      :vx 0
			      :vy 0
			      :angle 0)))


;;;; DRAW-PLAYER function

(defun draw-player ()
  (let* ((p *player*)
	 (x (player-x p))
	 (y (player-y p))
	 (angle (player-angle p))
	 (ship nil)
	 (flame nil))

    (dolist (s *tmpl-player*) 
      (push (rotate (first s) (second s) x y angle) ship))

    (dolist (s *tmpl-player-flame*) 
      (push (rotate (first s) (second s) x y angle) flame))

    (draw-polygon ship 255 255 255)
    
    (if (and (eq *thrust* t) (zerop (mod *game-tick* 4)))
	(draw-polygon flame 255 0 0))

    (if (and (eq *player-shield* t) (zerop (mod *game-tick* 4)))
	(draw-circle (round (player-x p)) (round (player-y p)) 30 255 0 0))))



;;;; UPDATE-PLAYER function

(defun update-player ()
  (let* ((p *player*))
    
    (setf (player-x p) (+ (player-x p) (player-vx p)))
    (setf (player-y p) (+ (player-y p) (player-vy p)))

    (if (< (player-x p) -10)
	(setf (player-x p) (+ *game-width* 10)))

    (if (> (player-x p) (+ *game-width* 10))
	(setf (player-x p) 0))

    (if (< (player-y p) -10)
	(setf (player-y p) (+ *game-height* 10)))

    (if (> (player-y p) (+ *game-height* 10))
	(setf (player-y p) 0))))


;;;; PLAYER-MOVE function

(defun player-move (direction)
  (let* ((p *player*)
	 (vec-x (sin (deg-to-rad (player-angle p))))
	 (vec-y (cos (deg-to-rad (player-angle p)))))

    (cond ((equalp direction 'left)
	   (setf (player-angle p) (- (player-angle p) +turn-speed+)))
	  
	  ((equalp direction 'right)
	   (setf (player-angle p) (+ (player-angle p) +turn-speed+)))
	  
	  ((equalp direction 'forward)
	   (progn (setf (player-vx p) (+ (player-vx p) (* vec-x +acceleration+)))
		  (setf (player-vy p) (- (player-vy p) (* vec-y +acceleration+)))
		  (setf *thrust* t)
		  (check-max-speed p)))
		  
		   
	  ((equalp direction 'back)
	   (progn (setf (player-vx p) (- (player-vx p) (* vec-x +acceleration+)))
		  (setf (player-vy p) (+ (player-vy p) (* vec-y +acceleration+))))))))


;;;; CHECK-MAX-SPEED function

(defun check-max-speed (p)
  (if (< (player-vx p) (- +max-speed+))
      (setf (player-vx p) (- +max-speed+)))

  (if (> (player-vx p) +max-speed+)
      (setf (player-vx p) +max-speed+))

  (if (< (player-vy p) (- +max-speed+))
      (setf (player-vy p) (- +max-speed+)))

  (if (> (player-vy p) +max-speed+)
      (setf (player-vy p) +max-speed+)))


;;;; FIRE-LASER function

(defun fire-laser ()
  (let* ((p *player*)
	 (vec-x (sin (deg-to-rad (player-angle p))))
	 (vec-y (cos (deg-to-rad (player-angle p))))
	 (vx (* 8 vec-x))
	 (vy (* 8 vec-y)))
    (push (make-player-laser :x (round (player-x p)) :y (round (player-y p))
			     :vx vx :vy vy
			     :angle (rem (player-angle p) 360)
			     :time 75) *player-laser*))
  (play-sound 0))


;;;; DRAW-LASER function

(defun draw-laser ()
  (loop for l in *player-laser*
     do (draw-circle (round (player-laser-x l)) (round (player-laser-y l)) 1 255 255 0)))


;;;; UPDATE-LASER function

(defun update-laser ()
  (loop for l in *player-laser*
     do (update-laser-position l)))


;;;; UPDATE-LASER-POSITION function

(defun update-laser-position (l)
  (setf (player-laser-time l) (decf (player-laser-time l)))

  (if (<= (player-laser-time l) 0)
      (setf *player-laser* (remove l *player-laser*))
      (progn (setf (player-laser-x l) (+ (player-laser-x l) (player-laser-vx l)))
	     (setf (player-laser-y l) (- (player-laser-y l) (player-laser-vy l)))
	     (laser-collide-asteroid l)
	     (laser-collide-enemy l)

	     (if (< (player-laser-x l) -1)
		 (setf (player-laser-x l) (+ *game-width* 1)))

	     (if (> (player-laser-x l) (+ *game-width* 1))
		 (setf (player-laser-x l) -1))

	     (if (< (player-laser-y l) -1)
		 (setf (player-laser-y l) (+ *game-height* 1)))

	     (if (> (player-laser-y l) (+ *game-height* 1))
		 (setf (player-laser-y l) -1)))))


;;;; PLAYER-DESTROYED function

(defun player-destroyed ()
  (setf *player-lives* (decf *player-lives*))
  (create-player)
  (setf *player-shield* t)
  (setf *player-shield-timer* 120)
  (play-sound 5))


;;;; PLAYER-SHIELD-ACTIVATED function

(defun player-shield-activated ()
  (if (eq *player-shield* t)
      (progn (setf *player-shield-timer* (decf *player-shield-timer*))
	     (if (<= *player-shield-timer* 0)
		 (setf *player-shield* nil)))))


;;;; PLAY-SOUND-THRUST function

(defun play-sound-thrust ()
  (sdl-mixer:play-music *sound-thrust* :loop t))


;;;; STOP-SOUND-THRUST function

(defun stop-sound-thrust ()
  (sdl-mixer:halt-music 100))

;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-LEVEL function

(defun display-level ()
  ;(let ((p *player*))
  ;  (draw-text (format nil "vx: ~a" (player-vx p)) 10 10 255 255 255 *ttf-font-small*)
  ;  (draw-text (format nil "vy: ~a" (player-vy p)) 10 20 255 255 255 *ttf-font-small*))

  (draw-text (format nil "Lives: ~a" *player-lives*) 10 10 255 255 255)
  (draw-text (format nil "Wave: ~a" *wave*) 400 10 255 255 255)
  (draw-text (format nil "Score: ~a" *player-score*) 900 10 255 255 255))


;;;; DRAW-GAME-UI function

(defun draw-game-ui ()
  (if (eql *pause* t)
      (draw-text "Paused" 
	     380 280 255 255 255 *ttf-font-large*)))

(defun update-score-kill (type size)
  (cond ((equalp type 'asteroid) 
	 (cond ((= size 2) (setf *player-score* (+ *player-score* 20)))
	       ((= size 3) (setf *player-score* (+ *player-score* 30)))
	       (t (setf *player-score* (+ *player-score* 10)))))

	((equalp type 'enemy)
	 (cond ((= size 1) (setf *player-score* (+ *player-score* 150)))))))


;;;; UPDATE-GAME-TICK function

(defun update-game-tick ()
  (setf *game-tick* (incf *game-tick*)))


;;;; END-OF-WAVE-P function

(defun end-of-wave-p ()
  (if (and (zerop (length *asteroid-schedule*))
	   (zerop (length *asteroids*)))
      (progn (setf *player-score* (+ *player-score* 
				     (* 1000 *wave*)
				     (* 250 *player-lives* *wave*)))
	     (setf *wave* (incf *wave*))
	     (new-wave))))


;;;; GAME-OVER-P function

(defun game-over-p ()
  (if (zerop *player-lives*)
      (change-game-state)))


;;;; NEW-WAVE function

(defun new-wave ()
  (setf *game-tick* 0)
  (create-asteroid-schedule)
  (create-enemy-schedule))


;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  (draw-text "Asteroid Wars" 450 20 255 255 255 *ttf-font-huge*)

  (draw-text "Game Over" 480 150 255 255 255 *ttf-font-huge*)

  (draw-text (format nil "Score: ~a" *player-score*) 420 300 255 255 255 *ttf-font-huge*)

  (draw-text "Press SPACE to Continue..." 420 740 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (draw-text "Asteroid Wars" 450 20 255 255 255 *ttf-font-huge*)

  (draw-text "Intro Screen Goes Here..." 450 350 255 255 0)

  (draw-text "Press SPACE to Continue..." 420 740 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; PAUSE-GAME function

(defun pause-game ()
  (if (eql *pause* nil)
      (setf *pause* t)
      (setf *pause* nil)))


;;;; STATE-IN-PLAY function

(defun state-in-play ()
  
  (unless (eql *pause* t)
    (update-game-tick)
    (player-shield-activated)
    (update-player)
    (update-asteroids)
    (update-enemies)
    (ship-collide-asteroid)
    (ship-collide-enemy)
    (update-laser)
    (update-enemy-laser)
    (check-asteroid-start-time)
    (check-enemy-start-time)
    (end-of-wave-p)
    (game-over-p)
    )

  (display-level)
  (draw-player)
  (draw-asteroids)
  (draw-enemies)
  (draw-laser)
  (draw-enemy-laser)
  (draw-game-ui))


;;;; CONTINUE-OPTION function

(defun continue-option ()
  (cond ((zerop *game-state*) (change-game-state))
	((= *game-state* 2) (change-game-state))
	(t ())))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (cond ((zerop *game-state*) 
	 (progn (reset-game)
		(setf *game-state* 1)
		(play-sound 5)))

	((= *game-state* 1) (setf *game-state* 2))
	
	((= *game-state* 2) (setf *game-state* 0))
	
	(t ())))


;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)

  (cond ((= *game-state* 1) (state-in-play))

	((= *game-state* 2) (display-end-game))

	(t (display-menu)))

  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *random-state* (make-random-state t))
  (create-player)
  (setf *enemy* nil)
  (setf *asteroids* nil)
  (setf *player-laser* nil)
  (setf *enemy-laser* nil)
  (setf *pause* nil)
  (setf *game-tick* 0)
  (setf *player-score* 0)
  (setf *wave* 1)
  (setf *player-lives* 3)
  (setf *player-shield* t)
  (setf *player-shield-timer* 120)
  (create-asteroid-schedule)
  (create-enemy-schedule))


;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 8))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "laser-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "explosion-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 2) (sdl-mixer:load-sample (sdl:create-path "explosion-2.ogg" *audio-root*)))
    (setf (aref *soundfx* 3) (sdl-mixer:load-sample (sdl:create-path "explosion-3.ogg" *audio-root*)))
    (setf (aref *soundfx* 4) (sdl-mixer:load-sample (sdl:create-path "explosion-4.ogg" *audio-root*)))
    (setf (aref *soundfx* 5) (sdl-mixer:load-sample (sdl:create-path "shield-1.ogg" *audio-root*)))
    (setf (aref *soundfx* 6) (sdl-mixer:load-sample (sdl:create-path "player-explosion.ogg" *audio-root*)))
    (setf (aref *soundfx* 7) (sdl-mixer:load-sample (sdl:create-path "laser-2.ogg" *audio-root*)))
    (setf *sound-thrust* (sdl-mixer:load-music (sdl:create-path "thrust.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when *sound-thrust*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *sound-thrust*)
    (setf *sound-thrust* nil))

  (when *music*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *music*)
    (setf *music* nil))

  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height*
		:title-caption "Asteroid Wars")
    (setf (sdl:frame-rate) 60)

    (setup-audio)

    ;(sdl-mixer:play-music *music-intro* :loop t)

    (unless (sdl:initialise-default-font *terminus-ttf-18*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    (setf *ttf-font-small* (sdl:initialise-font *terminus-ttf-12*))
    (setf *ttf-font-normal* (sdl:initialise-font *terminus-ttf-18*))
    (setf *ttf-font-large* (sdl:initialise-font *terminus-ttf-24*))
    (setf *ttf-font-huge* (sdl:initialise-font *terminus-ttf-32*))

    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-a (if (= *game-state* 1)
					 (create-enemy)))
			 (:sdl-key-z (if (= *game-state* 1)
					 (fire-laser)))
			 (:sdl-key-p (if (= *game-state* 1)
					 (pause-game)))
			 (:sdl-key-q (if (= *game-state* 1)
					 (change-game-state)))
			 (:sdl-key-up (if (= *game-state* 1)
					  (play-sound-thrust)))
			 (:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key
		       (:sdl-key-up (if (= *game-state* 1)
					(stop-sound-thrust)))))
      (:idle ()
	     (setf *thrust* nil)
	     (when (sdl:get-key-state :sdl-key-left) (player-move 'left))
	     (when (sdl:get-key-state :sdl-key-right) (player-move 'right))
	     (when (sdl:get-key-state :sdl-key-up) (player-move 'forward))
	     ;(when (sdl:get-key-state :sdl-key-down) (player-move 'back))
	     (render)))))
