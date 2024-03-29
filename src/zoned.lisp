;;;; zoned.lisp

(in-package #:zoned)

;;; FIX: look into print-items system for printing?:
;;; https://40ants.com/lisp-project-of-the-day/2020/07/0145-utilities.print-items.html

;;; theme (FIX: move to mutility?)

(defvar *theme* (list
                 :background (make-gray-color 0.3)
                 :foreground +black+
                 :grid (make-gray-color 0.8)))

(defun get-color (element)
  "Get the theme's color for a type of GUI element, i.e. :foreground, :background, :accent, etc.

See also: `*theme*'"
  (getf *theme* element))

;;; gui

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass graphical-view (view)
    ()))

(defconstant +graphical-view+ (make-instance 'graphical-view))

;; (define-presentation-type tile ()
;;   :options (tile rotation)
;;   :inherit-from tile)

(define-presentation-method present (tile (type zone-tile) stream (view graphical-view) &key)
  (let* ((sprite (sprite-of tile))
         (width (tile-width tile))
         (height (tile-height tile))
         (pos (pos-of tile))
         (rotation 0))
    (draw-design stream (pattern-for sprite)
                 :transformation (compose-transformations
                                  (make-translation-transformation (* width (elt pos 0)) (* height (elt pos 1)))
                                  (make-rotation-transformation* rotation (/ width 2) (/ height 2))))))

(define-presentation-method present (object (type zone-object) stream (view graphical-view) &key)
  (let* ((sprite (sprite-of object))
         (width (tile-width object))
         (height (tile-height object))
         (pos (pos-of object))
         (rotation 0))
    (draw-design stream (pattern-for sprite)
                 :transformation (compose-transformations
                                  (make-translation-transformation (elt pos 0) (elt pos 1))
                                  (make-rotation-transformation* rotation (/ width 2) (/ height 2))))))

(define-presentation-method present ((tile null) (type tile) stream (view graphical-view) &key)
  (let ((width (tile-width (tileset-of (pane-frame stream))))
        (height (tile-height (tileset-of (pane-frame stream)))))
    (draw-rectangle* stream 0 0 width height :ink +pink1+)
    (draw-text* stream "erase" (/ width 2) (/ height 2) :align-x :center :align-y :center :text-size 11)))

(define-presentation-method present (tile (type tile) stream (view graphical-view) &key)
  ;; for the tileset view
  (let* ((sprite (sprite-of tile))
         (width (tile-width tile))
         (height (tile-height tile))
         (cols (floor (/ (rectangle-width (sheet-region stream)) width)))
         (index (1+ (index-of tile)))
         (pattern (pattern-for sprite)))
    (draw-design stream pattern
                 :transformation (make-translation-transformation (* width (mod index cols)) (* height (floor (/ index cols)))))))

(define-presentation-method present (layer (type zone-layer) stream (view graphical-view) &key)
  (let* ((layers (layers-of *application-frame*))
         (num-layers (length layers))
         (index (position layer layers))
         (r-index (- num-layers index 1))
         (y-pos (+ 15 (* 16 r-index)))
         (text (format nil "~s. ~a" index (or (name-of layer)
                                              (type-of layer)))))
    (when (eql (current-layer-of *application-frame*) index)
      (let ((width (rectangle-width (sheet-region (find-pane-named (zoned) 'layers-pane)))))
        (draw-rectangle* stream 0 (- y-pos 15) width (+ 4 y-pos) :ink (make-rgb-color 0.4 0.4 1))))
    (draw-text* stream text 0 y-pos)))

;; (defclass tileset-pane (action-gadget application-pane immediate-repainting-mixin)
;;   ())

;; (defmethod handle-event ((this tileset-pane) (event pointer-button-press-event))
;;   (setf tmp event)
;;   (format *swank-output* "x: ~a y: ~a~%" (clim:pointer-event-x event) (clim:pointer-event-y event)))

;; (defmethod handle-repaint ((this tileset-pane) region)
;;   (print 'hi *swank-output*)
;;   (draw-tileset (pane-frame this) this))

(define-application-frame zoned ()
  ((zone :initform (zone) :accessor zone-of :documentation "The actual zone object being edited.")
   (brush :initform nil :accessor brush-of :documentation "The current brush to paint tiles with. Can be either an integer representing the index into the tileset, or NIL, representing the eraser.")
   (current-layer :initform 0 :reader current-layer-of :documentation "The layer of the zone that we are currently editing.")
   (image-patterns :initform nil :documentation "The CLIM image patterns for the tile sprites.")
   (zone-filename :initform nil :accessor filename-of :documentation "The name of the file that the zone should be written to.")
   (modified-p :initform nil :accessor modified-p :documentation "Whether the zone has been modified since the last save.")
   (draw-grid-p :initform t :accessor draw-grid-p :documentation "Whether to draw the grid for the current layer."))
  (:command-table (zoned
		   :inherit-from (zoned-file-command-table
                                  zoned-edit-command-table
                                  zoned-view-command-table
                                  zoned-help-command-table)
		   :menu (("File" :menu zoned-file-command-table)
                          ("Edit" :menu zoned-edit-command-table)
                          ("View" :menu zoned-view-command-table)
			  ("Help" :menu zoned-help-command-table))))
  (:panes
   (zone-pane (make-clim-application-pane
               :name 'zone
               :scroll-bars t
               :incremental-redisplay t
               :display-function 'draw-zone
               :default-view +graphical-view+
               :foreground (get-color :foreground)
               :background (get-color :background)))
   (layers-pane (make-clim-application-pane
                 :name 'layers
                 :scroll-bar :vertical
                 :display-function 'draw-layers
                 :default-view +graphical-view+
                 :foreground (get-color :foreground)
                 :background (get-color :background)))
   (tileset-pane (make-clim-application-pane
                  :name 'tileset
                  :scroll-bar :vertical
                  :display-function 'draw-tileset
                  :default-view +graphical-view+
                  :foreground (get-color :foreground)
                  :background (get-color :background)))
   (int-pane (make-clim-interactor-pane
              :name 'interactor
              :foreground (get-color :foreground)
              :background (get-color :background))))
  (:layouts
   (default
       (vertically ()
         (4/5 (horizontally ()
                (4/5 zone-pane)
                (make-pane 'clime:box-adjuster-gadget)
                (1/5 (vertically ()
                       (2/5 layers-pane)
                       (make-pane 'clime:box-adjuster-gadget)
                       (3/5 tileset-pane)
                       ))))
         (make-pane 'clime:box-adjuster-gadget)
         (1/5 int-pane))))
  (:menu-bar t)
  (:pointer-documentation t))

;; (defmethod frame-standard-output ((frame zoned))
;;   (find-pane-named frame 'interactor))

(defmethod (setf current-layer-of) (value (zoned zoned))
  (let ((pane (find-pane-named zoned 'layers-pane)))
    (setf (slot-value zoned 'current-layer) value
          (pane-needs-redisplay pane) t)
    (redisplay-frame-pane zoned pane :force-p t)
    (com-refresh)))

(define-command-table zoned-file-command-table)

(define-command (com-new :name t :menu t
                         :command-table zoned-file-command-table
                         :keystroke (#\n :control))
    ()
  (setf (zone-of *application-frame*) (zone)))

(define-command (com-save :name t :menu t
                          :command-table zoned-file-command-table
                          :keystroke (#\s :control))
    ()
  (unless (filename-of *application-frame*)
    (let ((frame-input (frame-standard-input *application-frame*)))
      (accepting-values (frame-input :align-prompts t)
        (setf (filename-of *application-frame*)
              (accept 'pathname :stream frame-input
                      :prompt "Filename")))))
  (with-open-file (stream (filename-of *application-frame*) :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;; (let ((*print-readably* t)))
    (write (zone-of *application-frame*) :stream stream :escape t :readably t))
  (setf (modified-p *application-frame*) nil))

(define-command (com-load :name t :menu t
                          :command-table zoned-file-command-table
                          :keystroke (#\o :control))
    ()
  (format t "~&Sorry, loading is not yet implemented."))

(define-command (com-quit :name t :menu t
                          :command-table zoned-file-command-table
                          :keystroke (#\q :control))
    ()
  (frame-exit *application-frame*))

(define-command-table zoned-edit-command-table)

(add-menu-item-to-command-table 'zoned-edit-command-table "Zone" :divider nil)

(define-command (com-resize-zone :name t :menu ("Resize Zone" :after "Zone")
                                 :command-table zoned-edit-command-table)
    ()
  (format t "~&Note: Currently, resizing the zone will add to or remove from the bottom right of the zone.~%")
  (accepting-values (t :align-prompts t)
    (accept '(integer 1) :default (width (zone-of *application-frame*)) :prompt "Width")
    (fresh-line)
    (accept '(integer 1) :default (height (zone-of *application-frame*)) :prompt "Height")))

(add-menu-item-to-command-table 'zoned-edit-command-table "Tileset" :divider nil)

(define-command (com-add-tile :name t :menu ("Add Tile" :after "Tileset")
                              :command-table zoned-edit-command-table)
    ()
  (let (name path)
    (accepting-values (t :align-prompts t)
      (setf path (accept 'pathname :prompt "Sprite path"
                                   :default (if-let ((tile (lastcar (tiles-of (tileset-of (zone-of (zoned)))))))
                                              (directory-namestring (cadr tile))
                                              (or #+quicklisp (when-let ((dir (car ql:*local-project-directories*)))
                                                                (namestring dir))
                                                  (uiop:getenv "HOME")
                                                  ""))))
      (fresh-line)
      (setf name (let ((sym (accept 'symbol :prompt "Tile name (optional)")))
                   (if (null sym)
                       (upcase-intern (file-name-sans-suffix (namestring path)) :keyword)
                       sym))))
    (add-tile (tileset-of *application-frame*) name path)))

(add-menu-item-to-command-table 'zoned-edit-command-table "Layers" :divider nil)

(define-command (com-add-layer :name t :menu ("Add Layer" :after "Layers")
                               :command-table zoned-edit-command-table)
    ()
  (add-layer *application-frame*)
  (setf (pane-needs-redisplay (find-pane-named *application-frame* 'zone-pane)) t))

(define-command-table zoned-view-command-table)

(define-command (com-refresh :name t :menu t
                             :command-table zoned-view-command-table
                             :keystroke (#\r :control))
    ()
  nil)

(define-command-table zoned-help-command-table)

(define-command (com-readme :name "README" :menu t
                            :command-table zoned-help-command-table)
    ()
  (ed (asdf:system-relative-pathname :zoned "README.org")))

(define-command (com-repo :name t :menu t
                          :command-table zoned-help-command-table)
    ()
  (open-url (asdf:system-homepage (asdf:find-system :zoned t))))

(define-command (com-bugs :name t :menu t
                          :command-table zoned-help-command-table)
    ()
  (open-url (asdf:system-bug-tracker (asdf:find-system :zoned t))))

(define-command (com-about :name t :menu t
                           :command-table zoned-help-command-table)
    ()
  (let* ((system (asdf:find-system "zoned"))
         (version (asdf:component-version system)))
    (format t "~&zoned ~a~%video game zone editor~%a struct.ws project by modula t. worm and contributors~%" version)))

(define-zoned-command (paint-tile :name t) ((brush tile) (index integer))
  (setf (tile-elt (layer-elt *application-frame* (current-layer-of *application-frame*))
                  index)
        brush))

(define-presentation-to-command-translator paint-tile
    (zone-tile paint-tile zoned
               :pointer-documentation
               ((tile index stream)
                (let ((brush (brush-of *application-frame*))
                      (index (or index (index-of tile))))
                  (if brush
                      (format stream "Paint tile ~a with brush ~a" index brush)
                      (format stream "Erase tile ~a" index)))))
    (tile index)
  (list (brush-of *application-frame*)
        (or index (index-of tile))))

(define-zoned-command (select-layer :name t) ((layer '(or zone-layer integer)))
  (setf (current-layer-of *application-frame*) (typecase layer
                                                 (integer layer)
                                                 (zone-layer (position layer (layers-of *application-frame*))))))

(define-presentation-to-command-translator select-layer
    (zone-layer select-layer zoned
                :pointer-documentation
                ((layer stream)
                 (format stream "Select layer ~a" (index-of layer))))
    (layer)
  (list layer))

(define-zoned-command (set-brush :name t) ((tile tile :prompt "Brush"))
  (setf (brush-of *application-frame*) (name-of tile)))

(define-presentation-to-command-translator set-brush (tile set-brush zoned
                                                           :pointer-documentation
                                                           ((tile stream)
                                                            (if tile
                                                                (format stream "Set brush to ~a" (name-of tile))
                                                                (format stream "Use eraser tool"))))
    (tile)
  (list tile))

(defmethod draw-layer ((layer zone-tile-layer) index frame stream)
  (dolist (tile (tiles-of layer))
    (updating-output (stream :unique-id (list index (index-of tile)) :id-test #'equal :cache-value (name-of (tile-of tile)) :cache-test #'eql)
      (present tile 'zone-tile :stream stream))))

(defmethod draw-layer ((layer zone-object-layer) index frame stream)
  (dolist* (obj-index object (objects-of layer))
    (updating-output (stream :unique-id (list index obj-index) :id-test #'equal :cache-value (list (pos-of object)) :cache-test #'equal)
      (present object 'zone-object :stream stream))))

(defvar *image-patterns* (list)
  "The plist of CLIM patterns generated from loaded image files.")

(defun pattern-for (image)
  "Get a CLIM pattern for the image located at IMAGE."
  (when (typep image 'tile)
    (return-from pattern-for (pattern-for (sprite-of image))))
  (let ((image (etypecase image
                 (string image)
                 (pathname (namestring (truename image)))
                 (null image))))
    (or (loop :for (path pattern) :on *image-patterns* :by #'cddr
           :if (string= path image)
           :return pattern)
        (let ((pattern (etypecase image
                         (string (make-pattern-from-bitmap-file image))
                         (null (make-pattern (make-array (list 32 32)) ;; FIX: use tile-width and tile-height
                                             (make-list (* 32 32) :initial-element (make-opacity 0.0)))))))
          (push pattern *image-patterns*)
          (push image *image-patterns*)
          pattern))))

(defun draw-grid (frame stream)
  (let* ((color (get-color :grid))
         (zone (zone-of frame))
         ;; number of rows/columns in the zone:
         (zone-width (width zone))
         (zone-height (height zone))
         ;; tile dimensions in pixels:
         (tile-width (tile-width zone))
         (tile-height (tile-height zone))
         ;; zone dimensions in pixels:
         (map-width (* zone-width tile-width))
         (map-height (* zone-height tile-height)))
    (dotimes (x (1+ zone-width))
      (let ((x-pos (* x tile-width)))
        (draw-line* stream x-pos 0 x-pos map-height
                    :ink color)))
    (dotimes (y (1+ zone-width))
      (let ((y-pos (* y tile-width)))
        (draw-line* stream 0 y-pos map-width y-pos
                    :ink color)))))

(defun draw-zone (frame stream)
  (dolist* (index layer (layers-of frame))
    ;; (updating-output (stream :unique-id index :cache-value layer))
    (when (and (draw-grid-p frame)
               (current-layer-of frame))
      (draw-grid frame stream))
    (draw-layer layer index frame stream)))

(defun draw-layers (frame stream)
  (let ((layers (layers-of frame)))
    (dolist* (index layer layers)
      (present layer 'zone-layer :stream stream))))

(defun draw-tileset (frame stream)
  (declare (ignorable stream))
  (let ((tileset (tileset-of frame)))
    (present nil 'tile :stream stream)
    (dolist (tile (tiles-of tileset))
      (let ((tile (make-instance 'tile :name (car tile) :tileset tileset)))
        (present tile 'tile :stream stream)))))

;;; zone methods

(defmethod name-of ((this null))
  nil)

(defmethod name-of ((this zoned))
  (name-of (zone-of this)))

(defmethod (setf name-of) (value (this zoned))
  (setf (name-of (zone-of this)) value)
  ;; (setf (pane-needs-redisplay (find-pane-named *application-frame* 'tileset-pane)) t) ;; FIX
  )

(defmethod tileset-of ((this zoned))
  (slot-value (zone-of this) 'tileset))

(defmethod (setf tileset-of) (value (this zoned))
  (setf (tileset-of (zone-of this)) value)
  (setf (pane-needs-redisplay (find-pane-named *application-frame* 'tileset-pane)) t))

(defmethod tile-width ((this zoned))
  (tile-width (tileset-of this)))

(defmethod tile-height ((this zoned))
  (tile-height (tileset-of this)))

(defmethod tiles-of ((this zoned))
  (tiles-of (tileset-of this)))

(defmethod properties-of ((this zoned))
  (profperties-of (tileset-of this)))

(defun load-tileset (file)
  (setf (tileset-of (zoned)) (load-tileset file)))

(defmethod save-tileset ((this zoned) file &key (paths-relative-to :filename))
  (save-tileset (tileset-of (zoned)) file :paths-relative-to paths-relative-to))

(defmethod tile-elt ((this zoned) tile)
  (tile-elt (tileset-of this) tile))

(defmethod add-tile ((this zoned) name file)
  (add-tile (tileset-of this) name file)
  (setf (pane-needs-redisplay (find-pane-named *application-frame* 'tileset-pane)) t))

(defmethod width ((this zoned))
  (width (zone-of this)))

(defmethod height ((this zoned))
  (height (zone-of this)))

(defmethod layers-of ((this zoned))
  (layers-of (zone-of this)))

(defmethod layer-elt ((this zoned) index)
  (layer-elt (zone-of this) index))

;; (defmethod (setf layer-elt) ((this zoned) index)
;;   ;; FIX
;;   )

(defmethod add-layer ((this zoned) &rest keys &key index (type :tile))
  (declare (ignorable index type))
  (apply #'add-layer (zone-of this) keys)
  (setf (current-layer-of this) (or index (1- (length (layers-of this)))))
  (dolist (pane (list 'layers-pane 'zone-pane))
    (setf (pane-needs-redisplay (find-pane-named *application-frame* pane)) t)))

(defmethod zone-of ((this zoned))
  (slot-value this 'zone))

(defmethod (setf zone-of) (value (this zoned))
  (setf (slot-value this 'zone) value)
  (dolist (pane (list 'layers-pane 'zone-pane 'tileset-pane ;; 'status-pane
                      ))
    (setf (pane-needs-redisplay (find-pane-named this pane)) t)))

;;;

(defun zoned (&key make-new zone)
  "Start the zoned interface.

With MAKE-NEW, always make a new instance even if one already exists."
  (if make-new
      (run-frame-top-level (apply 'make-application-frame 'zoned (when zone (list :zone zone))))
      (find-application-frame 'zoned)))

(defun test-zoned ()
  "Start a test instance of zoned."
  (let* ((zoned (zoned))
         (zone (zone-of zoned))
         (tileset (tileset-of zoned)))
    (add-tile tileset 'grass "~/misc/lisp/worm/res/images/grass.png")
    (add-tile tileset 'arrow "~/misc/lisp/worm/res/images/arrow.png")
    (add-tile tileset 'bullet "~/misc/lisp/worm/res/images/bullet1.png")
    (dotimes (index (length (layers-of zone)))
      (remove-layer zone 0))
    (add-layer (zone-of zoned))
    (let ((layer (layer-elt (zone-of zoned) 0))
          (tiles (mapcar 'car (tiles-of (tileset-of zoned)))))
      (dotimes (n (length (tiles-of layer)))
        (setf (tile-elt layer n) (random-elt tiles))))))
