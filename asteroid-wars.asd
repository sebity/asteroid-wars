;;;; asteroid-wars.asd

(asdf:defsystem #:asteroid-wars
  :description "A remake of the classic game Asteroids"
  :author "Jan Tatham <jan@sebity.com>"
  :license "GPL v2"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf)
  :serial t
  :components ((:file "package")
               (:file "asteroid-wars")))

