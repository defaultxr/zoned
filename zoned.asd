;;;; zoned.asd - asdf system definitions for zoned and subsystems.

(asdf:defsystem #:zoned
  :name "zoned"
  :description "Tile-based 2D video game map editor"
  :author "modula t."
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/defaultxr/zoned"
  :bug-tracker "https://github.com/defaultxr/zoned/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/zoned.git")
  :depends-on (#:zoned/zone
               #:mcclim
               #:mcclim-bitmaps)
  :pathname "src/"
  :serial t
  :components ((:file "zoned-package")
               (:file "zoned")))

(asdf:defsystem #:zoned/zone
  :name "zoned/zone"
  :description "Zoned zone classes and functionality without the GUI bits"
  :author "modula t."
  :license "MIT"
  :version "0.1"
  :depends-on (#:alexandria
               #:mutility
               #:mutility/loopy
               #:str)
  :pathname "src/"
  :serial t
  :components ((:file "zone-package")
               (:file "utility")
               (:file "zone")))
