(in-package #:zone)

;;; debug stuff

(defparameter *swank-output* *standard-output*)

(defmacro with-swank-output (&body body)
  "Run BODY with *standard-output* bound to the swank output (so `print'/etc don't print to the CLIM stream). Useful for debugging."
  `(let ((*standard-output* *swank-output*))
     ,@body))

;;; math stuff

(defun coordinate-to-tile-index (x y &key (tile-width 32) (tile-height 32) (zone-columns 32) (zone-rows 32))
  "Map an X,Y coordinate to a tile index in the zone. Returns NIL if the X,Y coordinate is out of range of the map."
  (let ((column (floor (/ x tile-width)))
        (row (floor (/ y tile-height))))
    (when (and (<= column zone-columns)
               (<= row zone-rows))
      (+ column (* zone-columns row)))))

;;; list stuff

(defun common-subseqs-left (list-1 list-2 &key (test #'equal))
  "Get the number of items that are that same at the beginning of the provided lists.

See also: `path-relative-to'"
  (loop :for i :from 0
     :for item-1 :in list-1
     :for item-2 :in list-2
     :if (not (funcall test item-1 item-2))
     :return i))

;;; file stuff

(defun image-p (filename)
  "True if FILENAME is a supported image type."
  (when-let* ((ext-pos (position #\. filename :from-end t))
              (extension (subseq filename (1+ ext-pos))))
    (member (my-intern extension :keyword) (list :png :gif))))

(defun file-name-sans-suffix (filename)
  "Get the name of FILENAME sans its file extension.

Example:

;; (file-name-sans-suffix \"/home/blah/foo.txt\")
;; ;=> \"foo\""
  (subseq filename (1+ (position #\/ filename :from-end t)) (position #\. filename :from-end t)))

(defvar *path-relative-to* :filename
  "Where to save the filename paths as relative to when saving a tileset to a file.")

(defun path-relative-to (filename relative-to)
  "Return FILENAME as relative to RELATIVE-TO.

Example:

;; (path-relative-to \"/home/user/blah/foo.txt\" \"/home/user/my-folder/\")
;; ;=> \"../blah/foo.txt\""
  (let* ((split-file (split-sequence #\/ filename :remove-empty-subseqs t))
         (split-dir (split-sequence #\/ relative-to :remove-empty-subseqs t))
         (common (common-subseqs-left split-file split-dir)))
    (concatenate 'string
                 (str:join "/" (make-list (length (subseq split-dir common)) :initial-element ".."))
                 "/"
                 (str:join "/" (subseq split-file common)))))

