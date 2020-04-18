;;;; zone-package.lisp

(defpackage #:zone
  (:use #:cl
        #:alexandria
        #:mutility)
  (:export
   #:tile
   #:name-of
   #:tileset-of
   #:sprite-of

   #:tileset
   #:tile-width
   #:tile-height
   #:tiles-of
   #:properties-of
   #:load-tileset
   #:save-tileset
   #:tile-elt
   #:add-tile
   #:add-directory-images-as-tiles

   #:zone
   #:width
   #:height
   #:layers-of
   #:layer-elt
   #:add-layer
   #:remove-layer

   #:zone-layer
   #:zone-of

   #:zone-tile-layer

   #:zone-tile
   #:index-of
   #:layer-of
   #:tile-of
   #:pos-of

   #:zone-object-layer
   #:objects-of

   #:zone-object))
