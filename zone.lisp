(in-package #:zone)

(defclass tile ()
  ((name :initarg :name :accessor name-of :documentation "The name of the tile.")
   (tileset :initarg :tileset :accessor tileset-of :documentation "The tileset that the tile belongs to.")))

(defmethod print-object ((this tile) stream)
  (with-slots (name) this
    ;; FIX: print pathnames relative to `*path-relative-to*'
    (format stream "(~s :name ~s)" 'tile name)))

(defmethod sprite-of ((this tile))
  (cadr (assoc (name-of this) (tiles-of (tileset-of this)))))

(defmethod index-of ((this tile))
  (let ((tiles (tiles-of (tileset-of this))))
    (position-if (lambda (tile)
                   (eql (name-of this) (car tile)))
                 tiles)))

(defmethod tile-width ((this tile))
  (tile-width (tileset-of this)))

(defmethod tile-height ((this tile))
  (tile-height (tileset-of this)))

(defclass tileset ()
  ((tile-width :initarg :tile-width :initform 32 :accessor tile-width :type integer)
   (tile-height :initarg :tile-height :initform 32 :accessor tile-height :type integer)
   (tiles :initarg :tiles :initform nil :accessor tiles-of :type list :documentation "Alist mapping tile names to their definitions.") ;; FIX: maybe make this a hashtable?
   (properties :initarg :properties :initform nil :accessor properties-of :type list)))

(defmethod print-object ((this tileset) stream)
  (with-slots (tile-width tile-height tiles properties) this
    (format stream "(~s :tile-width ~s :tile-height ~s :tiles ~s :properties ~s)" 'tileset tile-width tile-height tiles properties)))

(defun tileset (&key (tile-width 32) (tile-height 32) tiles properties)
  (make-instance 'tileset
                 :tile-width tile-width
                 :tile-height tile-height
                 :tiles tiles
                 :properties properties))

;; (defmethod tiles-of ((this tileset))
;;   )

(defun load-tileset (file)
  (with-open-file (s file)
    (read s)))

(defmethod save-tileset ((tileset tileset) filename &key (paths-relative-to :filename))
  "Write TILESET to a file located at FILENAME.

PATHS-RELATIVE-TO specifies where to make the paths for image filenames relative to. It can be any of the following: :filename (for the filename the tileset is being saved to), :absolute (to use the full absolute pathnames), or a string (to specify a pathname directly)."
  (with-open-file (s filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print tileset s)))

(defgeneric tile-elt (object index)
  (:documentation "Get a tile by name or index from a tileset or zone-tile-layer."))

(defmethod tile-elt ((tileset tileset) tile) ;; FIX: allow index here too?
  (let* ((tiles (tiles-of tileset))
         (assoc (etypecase tile
                  (symbol (assoc tile tiles))
                  (integer (elt tiles tile)))))
    (make-instance 'tile
                   :name (car assoc)
                   :tileset tileset)))

(defgeneric add-tile (tileset name file)
  (:documentation "Add a new tile definition to a tileset."))

(defmethod add-tile ((tileset tileset) name file)
  (let ((file (namestring (truename file)))) ;; FIX: maybe we should just leave it relative here?
    (if-let (v (assoc name (tiles-of tileset)))
      (setf (cadr v) file)
      (appendf (tiles-of tileset) (list (list name file))))))

(defun add-directory-images-as-tiles (tileset directory)
  "Add all the images in DIRECTORY as tiles to TILESET, using their filename sans extension as the tile name."
  (loop :for file :in (directory (concat directory "/*.*"))
     :for filename := (namestring file)
     :for tile-name := (file-name-sans-suffix filename)
     :if (image-p filename)
     :do (add-tile tileset (my-intern tile-name :keyword) filename)))

(defclass zone ()
  ((name :initarg :name :initform nil :accessor name-of :type (or nil string) :documentation "The name of the zone.")
   (width :initarg :width :initform 32 :accessor width :type integer :documentation "The width of the zone, as a number of tiles.")
   (height :initarg :height :initform 32 :accessor height :type integer :documentation "The height of the zone, as a number of tiles.")
   (tileset :initarg :tileset :initform (tileset) :accessor tileset-of :type (or tileset string) :documentation "The tileset of the zone, or the pathname to its external tileset.")
   (layers :initarg :layers :initform nil :accessor layers-of :type list :documentation "List of the layers for the zone, from bottom to top.")
   (properties :initarg :properties :initform nil :accessor properties-of :type list :documentation "Plist of additional data about the zone.")))

(defmethod print-object ((this zone) stream)
  (with-slots (width height tileset layers properties) this
    (format stream "(~s :width ~s :height ~s :tileset ~s :layers ~s :properties ~s)" 'zone width height tileset layers properties)))

(defun zone (&key (width 32) (height 32) tileset layers properties)
  (let ((zone (make-instance 'zone
                             :width width
                             :height height
                             :tileset (or tileset (tileset))
                             :layers (or layers (list (make-instance 'zone-tile-layer :tiles (make-list (* width height)))))
                             :properties properties)))
    (dolist (layer (layers-of zone))
      (setf (zone-of layer) zone))
    zone))

(defgeneric layer-elt (zone index)
  (:documentation "Get a layer from ZONE by its index."))

(defmethod layer-elt ((zone zone) index)
  (elt (layers-of zone) index))

(defgeneric add-layer (zone &key index type)
  (:documentation "Add a layer to a zone."))

(defmethod add-layer ((zone zone) &key index (type :tile))
  (let* ((length (* (width zone) (height zone)))
         (layers (layers-of zone))
         (index (or index (length layers)))
         (layer (apply 'make-instance
                       (ecase type
                         (:tile 'zone-tile-layer)
                         (:object 'zone-object-layer))
                       :zone zone
                       (ecase type
                         (:tile (list :tiles (make-list length)))
                         (:object (list :objects nil))))))
    (setf (layers-of zone) (append (subseq layers 0 index) (list layer) (subseq layers index)))))

(defgeneric remove-layer (zone index)
  (:documentation "Remove a layer from a zone."))

(defmethod remove-layer ((zone zone) index)
  (setf (layers-of zone) (delete (layer-elt zone index) (layers-of zone))))

(defclass zone-layer ()
  ((name :initarg :name :initform nil :accessor name-of :type (or null string) :documentation "The name of the layer.")
   (zone :initarg :zone :accessor zone-of :type zone :documentation "The zone that this layer is associated with.")))

(defmethod tileset-of ((this zone-layer))
  (slot-value (zone-of this) 'tileset))

(defmethod tile-width ((this zone-layer))
  (tile-width (tileset-of (zone-of this))))

(defmethod tile-height ((this zone-layer))
  (tile-height (tileset-of (zone-of this))))

(defmethod index-of ((this zone-layer))
  (position this (layers-of (zone-of this))))

(defclass zone-tile-layer (zone-layer)
  ((tiles :initarg :tiles :initform nil :documentation "The list of tiles in the layer.")
   (zone :initarg :zone :reader zone-of :type zone :documentation "The zone that the layer is a part of."))
  (:documentation "A zone layer that is aligned to the tile grid."))

(defmethod print-object ((this zone-tile-layer) stream)
  (format stream "(~s :tiles ~s)" 'zone-tile-layer (slot-value this 'tiles)))

(defmethod tile-elt (tile-layer elt)
  (make-instance 'zone-tile
                 :index elt
                 :layer tile-layer))

(defun (setf tile-elt) (value tile-layer elt) ;; FIX: if no such tile is known, offer a restart to add it to the tileset.
  (setf (elt (slot-value tile-layer 'tiles) elt) value))

(defmethod tiles-of ((layer zone-tile-layer))
  (loop :for index :from 0 :below (length (slot-value layer 'tiles))
     :collect (tile-elt layer index)))

(defclass zone-tile ()
  ((index :initarg :index :reader index-of :type integer :documentation "A vector representing the index of the tile in its layer. See also: `pos-of' for a vector representing the tile's X,Y position.")
   (layer :initarg :layer :reader layer-of :documentation "The layer that this tile belongs to.")))

(defmethod pos-of ((this zone-tile))
  (let ((index (index-of this))
        (width (width (zone-of (layer-of this)))))
    (vector (mod index width)
            (floor (/ index width)))))

(defmethod tile-of ((this zone-tile))
  (tile-elt (tileset-of (layer-of this))
            (elt (slot-value (layer-of this) 'tiles) (index-of this))))

(defmethod (setf tile-of) (value (this zone-tile))
  (setf (tile-elt (layer-of this) (index-of this)) value))

(defmethod sprite-of ((this zone-tile))
  (sprite-of (tile-of this)))

(defmethod tile-width ((this zone-tile))
  (tile-width (layer-of this)))

(defmethod tile-height ((this zone-tile))
  (tile-height (layer-of this)))

(defclass zone-object-layer (zone-layer)
  ((objects :accessor objects-of :type list :documentation "The list of objects in the layer."))
  (:documentation "A zone layer of `zone-object' objects that are not necessarily grid-aligned."))

(defmethod print-object ((this zone-object-layer) stream)
  (with-slots (objects) this
    (format stream "(~s :objects ~s)" 'zone-tile-layer objects)))

(defgeneric pos-of (object)
  (:documentation "The center position of an object as vector coordinates."))

(defclass zone-object ()
  ((pos :initarg :pos :accessor pos-of :initform (vector 0 0) :type vector :documentation "The center position of the object in the zone, in pixel coordinates.")
   (tile :initarg :tile :accessor tile-of :type integer :documentation "The tile number that the object gets its image from.")
   (layer :initarg :layer :reader layer-of :type layer :documentation "The layer that the object is a part of.")
   (properties :initarg :properties :accessor properties-of :type list :documentation "Additional data associated with the object."))
  (:documentation "An object in a zone-object-layer; not necessarily positioned within the grid."))

(defmethod zone-of ((this zone-object))
  (zone-of (layer-of this)))

(defmethod sprite-of ((this zone-object))
  (sprite-of (tile-elt (tileset-of (zone-of this)) (tile-of this))))

