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

;;;; Player Params
(defparameter *player* nil)
(defparameter *player-laser* nil)

(defparameter +acceleration+ 0.1)
(defparameter +turn-speed+ 3)
(defparameter +max-speed+ 5)


;;;; Asteroid Params
(defparameter *asteroids* nil)
(defparameter *asteroid-field* 60)

;;;; Enemy Params


;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *soundfx* nil)

;;;; GFX Params
;(defparameter *gfx-snake* (merge-pathnames "logo.png" *gfx-root*))

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

(defparameter *tmpl-player* '((0 -20) (-10 20) (0 10) (10 20)))
(defparameter *tmpl-laser* '((0 10) (0 0)))
(defparameter *tmpl-asteroid-1* '((0 -60) (40 -40) (60 0) (40 40) (0 60)
				  (-40 40) (-60 0) (-40 -40)))
(defparameter *tmpl-asteroid-2* '((0 -30) (20 -30) (30 0) (20 20) (0 30)
				  (-20 20) (-30 0) (-20 -20)))
(defparameter *tmpl-asteroid-3* '((0 -15) (10 -15) (15 0) (10 10) (0 15)
				  (-10 10) (-15 0) (-10 -10)))

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


(defun ship-collide-asteroid ()
  (let ((p *player*))
    (loop for a in *asteroids*
       do (ship-collide a p))))

(defun ship-collide (a p)
  (if (<= (sqrt (+ (square (- (asteroid-x a) (player-x p)))
		   (square (- (asteroid-y a) (player-y p)))))
	  (asteroid-field-size (asteroid-stage a)))
      (format t "hit~%")))


(defun laser-collide-asteroid (l)
  (loop for a in *asteroids*
     do (if (<= (sqrt (+ (square (- (asteroid-x a) (player-laser-x l)))
			 (square (- (asteroid-y a) (player-laser-y l)))))
		(asteroid-field-size (asteroid-stage a)))
	    (progn (if (< (asteroid-stage a) 3)
		       (progn (create-asteroid (+ (asteroid-stage a) 1) (asteroid-x a) (asteroid-y a))
			      (create-asteroid (+ (asteroid-stage a) 1) (asteroid-x a) (asteroid-y a))))
		   (setf *asteroids* (remove a *asteroids*))
		   (setf *player-laser* (remove l *player-laser*))))))

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

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;;;;;;;;;;;;;;;;;;;;;; ENEMY ;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; ASTEROIDS ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CREATE-ASTEROID function

(defun create-asteroid (&optional (stage 1) (x (+ (random 700) 700)) (y (+ (random 700) 700)))
  (let ((asteroid (create-asteroid-shape stage)))
    (push (make-asteroid :x x
			 :y y
			 :vx (- (random 2.0) 1.0)
			 :vy (- (random 2.0) 1.0)
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
	 (ship nil))

    (dolist (s *tmpl-player*) 
      (push (rotate (first s) (second s) x y angle) ship))

    (draw-polygon ship 255 255 255)))



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
			     :time 75) *player-laser*)))


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

	     (if (< (player-laser-x l) -1)
		 (setf (player-laser-x l) (+ *game-width* 1)))

	     (if (> (player-laser-x l) (+ *game-width* 1))
		 (setf (player-laser-x l) -1))

	     (if (< (player-laser-y l) -1)
		 (setf (player-laser-y l) (+ *game-height* 1)))

	     (if (> (player-laser-y l) (+ *game-height* 1))
		 (setf (player-laser-y l) -1)))))

;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-LEVEL function

(defun display-level ()
  (let ((p *player*))
    (draw-text (format nil "vx: ~a" (player-vx p)) 10 10 255 255 255 *ttf-font-small*)
    (draw-text (format nil "vy: ~a" (player-vy p)) 10 20 255 255 255 *ttf-font-small*)))


;;;; DRAW-GAME-UI function

(defun draw-game-ui ()
  (if (eql *pause* t)
      (draw-text "Paused" 
	     380 280 255 255 255 *ttf-font-large*)))


;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  (draw-text "Game Over" 20 100 255 255 255)

  (draw-text "Press SPACE to Continue..." 290 570 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (gl:color 1 0 1)

  (draw-text "Asteroid Wars" 20 20 255 255 255 *ttf-font-huge*)

  (draw-text "Intro Screen" 20 100 255 255 255)

  (draw-text "Press SPACE to Continue..." 290 570 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; PAUSE-GAME function

(defun pause-game ()
  (if (eql *pause* nil)
      (setf *pause* t)
      (setf *pause* nil)))


;;;; STATE-IN-PLAY function

(defun state-in-play ()
  
  (unless (eql *pause* t)
    (update-player)
    (update-asteroids)
    (ship-collide-asteroid)
    (update-laser)
    )

  (display-level)
  (draw-player)
  (draw-asteroids)
  (draw-laser)
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
		(setf *game-state* 1)))

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
  (create-player)
  (setf *asteroids* nil)
  (setf *player-laser* nil)
  (setf *pause* nil))


;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 1))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    ;(setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "beep.ogg" *audio-root*)))
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
					 (create-asteroid 1)))
			 (:sdl-key-z (if (= *game-state* 1)
					 (fire-laser)))
			 (:sdl-key-p (if (= *game-state* 1)
					 (pause-game)))
			 (:sdl-key-q (if (= *game-state* 1)
					 (change-game-state)))
			 (:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:idle ()
	     (when (sdl:get-key-state :sdl-key-left) (player-move 'left))
	     (when (sdl:get-key-state :sdl-key-right) (player-move 'right))
	     (when (sdl:get-key-state :sdl-key-up) (player-move 'forward))
	     ;(when (sdl:get-key-state :sdl-key-down) (player-move 'back))
	     (render)))))
