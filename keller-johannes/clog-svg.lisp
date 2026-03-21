(in-package :clog)




(defun create-SVG-with-ns (connection-id svg-element-name attributes-list
                         &key (clog-type 'clog-element) (html-id nil))
  "Low-level create a new CLOG-ELEMENT and attach it to HTML element on
CONNECTION-ID. There must be a single outer block that will be set to
an internal id. The returned CLOG-ELEMENT requires placement or will
not be visible, ie. place-after, etc. as it exists in the javascript
clog array but is not in the DOM. If HTML-ID is nil, one is generated.
(private)"
  (let ((web-id (if html-id
                    html-id
                    (format nil "CLOG~A" (generate-id)))))
    (cached-execute connection-id
                    (format nil "let svgNS = 'http://www.w3.org/2000/svg';
let newElement = document.createElementNS(svgNS, '~a');
newElement.id = '~a';
~{newElement.setAttribute('~a', '~a');~}
clog['~a'] = newElement;"
                            svg-element-name
                            web-id
                            (alexandria:flatten attributes-list)
                            web-id))
    (make-clog-element connection-id web-id :clog-type clog-type)))






(defgeneric create-SVG-child (clog-obj svg-element-name attributes-list
                              &key html-id auto-place clog-type)
  (:documentation ""))

(defmethod create-SVG-child ((obj clog-obj) svg-element-name attributes-list
                             &key (html-id nil)
                               (auto-place t)
                               (clog-type 'clog-element))
  (let ((child (create-SVG-with-ns (connection-id obj)
                                   svg-element-name
                                   attributes-list
                                   :clog-type clog-type
                                   :html-id   html-id)))
    (setf (parent child) obj)
    (if auto-place
        (case auto-place
          (:bottom (place-inside-bottom-of obj child))
          (:top (place-inside-top-of obj child))
          (t (place-inside-bottom-of obj child)))
        (setf (connection-data-item obj html-id) obj))
    child))








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
  (create-child obj (format nil "<svg ~a ~a ~a ~a~@[~a~]~@[~a~]~@[~a~]></svg>"
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




(defmethod (setf svg-attribute) (value (obj clog-obj) attr-name)
  (js-execute obj (format nil "clog['~a'].setAttribute('~A', '~A');"
                          (html-id obj)
                          attr-name
                          (escape-string value :html t)))
  value)





(defclass clog-svg-circle (clog-element) ()
  (:documentation "CLOG SVG extension: SVG circle element."))

(defmethod create-svg-circle ((obj clog-obj) &key cx cy r fill html-id)
  (create-SVG-child obj "circle" `(("cx" ,cx) ("cy" ,cy) ("r" ,r)
                                   ("fill" ,fill) ("visibility" "visible"))
                :clog-type 'clog-svg-circle
                :html-id html-id))



(defclass clog-svg-rect (clog-element) ()
  (:documentation "CLOG SVG extension: SVG rect element."))

(defmethod create-svg-rect ((obj clog-obj) &key x y width height fill html-id)
  (create-SVG-child obj "rect" `(("x" ,x) ("y" ,y)
                                 ("width" ,width) ("height" ,height)
                                 ("fill" ,fill))
                    :clog-type 'clog-svg-rect
                    :html-id html-id))


(defclass clog-svg-ortho-shape (clog-element) ()
  (:documentation "CLOG SVG extension: SVG path using online horizontal and vertical lines."))

(defun generate-path-string (deltas)
  (let ((result "")
        (direction "h"))
    (loop for delta in deltas do
      (progn (setf result (format nil "~a ~a ~a " result direction delta))
             (if (string= direction "h")
                 (setf direction "v")
                 (setf direction "h"))))
    result))

(defmethod create-svg-ortho-shape ((obj clog-obj)
                                   &key x-origin y-origin html-id deltas fill stroke)
  (create-SVG-child obj "path" (list (list "d" (format nil "M ~a ~a ~a Z"
                                                       x-origin y-origin
                                                       (generate-path-string deltas)))
                                     (list "fill" fill)
                                     (list "stroke" stroke))
                    :clog-type 'clog-svg-ortho-shape
                    :html-id html-id))



(export 'create-svg-ortho-shape)
(export 'create-svg-rect)
(export 'create-svg-circle)
(export 'create-svg-toplevel)
(export 'svg-attribute)
