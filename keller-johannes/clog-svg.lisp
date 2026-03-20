(in-package :clog)

(defclass clog-svg-toplevel (clog-element) ()
  (:documentation "CLOG SVG extension: SVG element containing SVG data."))

(defgeneric create-svg-toplevel (clog-obj
                                 &key version xmlns width height hidden class html-id))

(defmethod create-svg-toplevel ((obj clog-obj)
                                &key (version "1.1")
                                  (xmlns "http://www.w3.org/2000/svg")
                                  (width "300")
                                  (height "200")
                                  (viewbox nil)
                                  (hidden nil)
                                  (class nil)
                                  (html-id nil))
  (create-child obj (format nil "<svg ~a ~a ~a ~a~@[~a~]~@[~a~]~@[~a~]><circle cx='50' cy='50' r='5'></circle></svg>"
                            (format nil "version='~a'" version)
                            (format nil "xmlns='~a'" xmlns)
                            (format nil "width='~a'" width)
                            (format nil "height='~a'" height)
                            (when viewbox (format nil " viewbox='~a'" viewbox))
                            (if hidden " visibility='hidden'" " visibility='visible'")
                            (when class (format nil " class='~a'"
                                                (escape-string class :html t))))
                :clog-type 'clog-svg-toplevel
                :html-id html-id))






(defclass clog-svg-circle (clog-element) ()
  (:documentation "CLOG SVG extension: SVG circle element."))

(defgeneric create-svg-circle (clog-obj &key cx cy r))

(defmethod create-svg-circle ((obj clog-obj) &key cx cy r html-id)
  (create-child obj (format nil "<circle xmlns='http://www.w3.org/2000/svg' ~a ~a ~a fill='gold' visibility='visible'></circle>"
                            (format nil "cx='~a'" cx)
                            (format nil "cy='~a'" cy)
                            (format nil "r='~a'" r))
                :clog-type 'clog-svg-circle
                :html-id html-id))


(defgeneric cx (clog-svg-circle)
  (:documentation "Get/Setf the CX of a SVG circle."))

(defmethod cx ((obj clog-svg-circle))
  (property obj "cx"))

(defgeneric (setf cx) (value clog-svg-circle)
  (:documentation "Set cx VALUE for CLOG-SVG-CIRCLE"))

(defmethod (setf cx) (value (obj clog-svg-circle))
  (setf (property obj "cx") value))
