;;;; zoned-package.lisp

(defpackage #:zoned
  (:shadowing-import-from #:alexandria ;; using alexandria causes a name conflict if we don't do this...
                          :simple-parse-error)
  (:shadow #:save-tileset)
  (:use #:clim-lisp
        #:clim
        #:alexandria
        #:mutility
        #:zone)
  (:import-from #:zone
                :with-swank-output
                :file-name-sans-suffix)
  (:export :zoned))
