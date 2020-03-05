;;;; zoned.lisp

(in-package #:zoned)

;;; theme

(defparameter *theme* (list
                       :background +white+
                       :foreground +black+))

(defun get-color (element)
  "Get the theme's color for a type of GUI element, i.e. :foreground, :background, :accent, etc.

See also: `*theme*'"
  (getf *theme* element))

(defparameter tmp nil)

;;; gui

(defclass graphical-view (view)
  ())

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
         (width 32 ;; (tile-width object)
           )
         (height 32 ;; (tile-height object)
           )
         (pos (pos-of object))
         (rotation 0))
    (draw-design stream (pattern-for sprite)
                 :transformation (compose-transformations
                                  (make-translation-transformation (elt pos 0) (elt pos 1))
                                  (make-rotation-transformation* rotation (/ width 2) (/ height 2))))))

(defparameter tmp3 nil)

(define-presentation-method present (tile (type tile) stream (view graphical-view) &key)
  (let* ((sprite (sprite-of tile))
         (width (tile-width tile))
         (height (tile-height tile))
         (index (index-of tile))
         (pattern (pattern-for sprite)))
    (draw-design stream pattern
                 :transformation (make-translation-transformation (* width index) 0))))

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
  ((zone :initform (zone) :accessor zone-of)
   (brush :initform 0 :accessor brush-of)
   (current-layer :initform 0 :reader current-layer-of)
   (image-patterns :initform nil))
  (:panes
   (int-pane (make-clim-interactor-pane
              :name 'interactor
              :foreground (get-color :foreground)
              :background (get-color :background)))
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
                  :background (get-color :background))))
  (:layouts
   (default
       (vertically ()
         (4/5 (horizontally ()
                (4/5 zone-pane)
                (1/5 (vertically ()
                       (2/5 layers-pane)
                       (3/5 tileset-pane)
                       ))))
         (1/5 int-pane))))
  (:menu-bar t)
  (:pointer-documentation t))

(defmethod (setf current-layer-of) (value (zoned zoned))
  (let ((pane (find-pane-named zoned 'layers-pane)))
    (setf (slot-value zoned 'current-layer) value
          (pane-needs-redisplay pane) t)
    (redisplay-frame-pane zoned pane :force-p t)
    (com-refresh)))

(define-zoned-command (com-quit :name t :menu t) ()
  (frame-exit *application-frame*))

(define-zoned-command (com-refresh :name t :menu t) ()
  nil)

(define-zoned-command (com-add-layer :name t :menu t) ()
  (add-layer *application-frame*)
  (setf (pane-needs-redisplay (find-pane-named *application-frame* 'zone-pane)) t))

(define-zoned-command (com-test :name t :menu t) ()
  (test-zoned))

(define-zoned-command (paint-tile :name t) ((tile tile) (index integer))
  (setf (tile-elt (layer-elt *application-frame* (current-layer-of *application-frame*))
                  index)
        tile))

(define-presentation-to-command-translator paint-tile (zone-tile paint-tile zoned) (tile index)
  (list (brush-of *application-frame*)
        (or index (index-of tile))))

(define-zoned-command (select-current-layer :name t) ((layer '(or zone-layer integer)))
  (setf (current-layer-of *application-frame*) (typecase layer
                                                 (integer layer)
                                                 (zone-layer (position layer (layers-of *application-frame*))))))

(define-presentation-to-command-translator select-current-layer (zone-layer select-current-layer zoned) (layer)
  (list layer))

;; (define-town-example-command (com-get-distance :name t :menu t
;;                                                :keystroke (#\d :meta))
;;     ((town-a town :prompt "Town a")
;;      (town-b town :prompt "Town b"))
;;   (notify-user *application-frame*
;; 	       (format nil "It's ~d pixels from ~a to ~a."
;; 		       (get-distance-between-points (town-coordinates town-a)
;; 						    (town-coordinates town-b))
;; 		       (town-name town-a)
;; 		       (town-name town-b))
;; 	       :title "Distance"
;; 	       :text-style '(:serif :roman 15)))

(define-zoned-command (set-brush :name t) ((tile tile :prompt "Brush"))
  (setf (brush-of *application-frame*) (name-of tile)))

(define-presentation-to-command-translator set-brush (tile set-brush zoned) (tile)
  (list tile))

(defmethod draw-layer ((layer zone-tile-layer) index frame stream)
  (dolist (tile (tiles-of layer))
    (updating-output (stream :unique-id (list index (index-of tile)) :id-test #'equal :cache-value (name-of (tile-of tile)))
      (present tile 'zone-tile))))

(defmethod draw-layer ((layer zone-object-layer) index frame stream)
  (dolist* (object obj-index (objects-of layer))
    (updating-output (stream :unique-id (list index obj-index) :id-test #'equal :cache-value (list (pos-of object)) :cache-test #'equal)
      (present tile 'zone-object))))

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
                         (null (make-pattern (make-array (list 32 32)) ;; FIX: use tile-widht and tile-height
                                             (make-list (* 32 32) :initial-element (make-opacity 0.0)))))))
          (push pattern *image-patterns*)
          (push image *image-patterns*)
          pattern))))

(defun draw-zone (frame stream)
  (dolist* (layer index (layers-of frame))
    ;; (updating-output (stream :unique-id index :cache-value layer))
    (draw-layer layer index frame stream)))

(defun draw-layers (frame stream)
  (let ((layers (layers-of frame)))
    (dolist* (layer index layers)
      (present layer 'zone-layer :stream stream))))

(defparameter tmp2 nil)

(defun draw-tileset (frame stream)
  (declare (ignorable stream))
  (let ((tileset (tileset-of frame)))
    (dolist (tile (tiles-of tileset))
      (let ((tile (make-instance 'tile :name (car tile) :tileset tileset)))
        (present tile 'tile :stream stream)))))

;;; zone methods

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
  (dolist (pane (list 'layers-pane 'zone-pane 'tileset-pane 'status-pane))
    (setf (pane-needs-redisplay (find-pane-named *application-frame* pane)) t)))

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
