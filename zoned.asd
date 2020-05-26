;;;; zoned.asd

(asdf:defsystem #:zoned
  :name "zoned"
  :description "Tile-based 2D video game map editor"
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/defaultxr/zoned"
  :bug-tracker "https://github.com/defaultxr/zoned/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/zoned.git")
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
               #:cl-strings)
  :components ((:file "zone-package")
               (:file "utility")
               (:file "zone")))
