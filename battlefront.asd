;;;; battlefront.asd

(asdf:defsystem #:battlefront
  :description "chaotic 2D overhead shooter inspired by Star Wars: Battlefront"
  :author "Stephen Whitmore <sww@eight45.net>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2
               #:cl-opengl
               #:pngload)
  :components ((:file "package")
               (:file "tex-png")
               (:file "engine")
               (:file "ecs")
               (:file "battlefront")))
