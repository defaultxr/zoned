;;;; zoned.asd

(asdf:defsystem #:zoned
  :description "Tile-based 2D video game map editor"
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:mutility
               #:zoned/zone
               #:mcclim
               #:mcclim-bitmaps)
  :components ((:file "zoned-package")
               (:file "zoned")))

(asdf:defsystem #:zoned/zone
  :description "Zoned zone classes and functionality without the GUI bits"
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:alexandria
               #:split-sequence
               #:cl-strings)
  :components ((:file "zone-package")
               (:file "utility")
               (:file "zone")))
