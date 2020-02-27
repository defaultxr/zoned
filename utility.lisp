(in-package #:zone)

(defvar *path-relative-to* :filename
  "Where to save the filename paths as relative to when saving a tileset to a file.")

(defun common-subseqs-left (list-1 list-2 &key (test #'equal))
  "Get the number of items that are that same at the beginning of the provided lists.

See also: `path-relative-to'"
  (loop :for i :from 0
     :for item-1 :in list-1
     :for item-2 :in list-2
     :if (not (funcall test item-1 item-2))
     :return i))

(defun path-relative-to (filename relative-to)
  "Return FILENAME as relative to RELATIVE-TO.

Example:

;; (path-relative-to \"/home/user/blah/foo.txt\" \"/home/user/my-folder/\")
;; ;=> \"../blah/foo.txt\""
  (let* ((split-file (split-sequence #\/ filename :remove-empty-subseqs t))
         (split-dir (split-sequence #\/ relative-to :remove-empty-subseqs t))
         (common (common-subseqs-left split-file split-dir)))
    (concatenate 'string
                 (cl-strings:join (make-list (length (subseq split-dir common)) :initial-element "..") :separator "/")
                 "/"
                 (cl-strings:join (subseq split-file common) :separator "/"))))

