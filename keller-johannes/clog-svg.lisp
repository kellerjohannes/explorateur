(in-package :clog)




(defun create-SVG-with-html (connection-id html
                         &key (clog-type 'clog-element) (html-id nil))
  "Low-level create a new CLOG-ELEMENT and attach it to HTML element on
CONNECTION-ID. There must be a single outer block that will be set to
an internal id. The returned CLOG-ELEMENT requires placement or will
not be visible, ie. place-after, etc. as it exists in the javascript
clog array but is not in the DOM. If HTML-ID is nil, one is generated.
(private)"
  (format t "~&HTML: '~a'." (escape-string html :html t))
  (let ((web-id (if html-id
                    html-id
                    (format nil "CLOG~A" (generate-id)))))

    (cached-execute connection-id
                    (format nil "let svgNS = 'http://www.w3.org/2000/svg';
  let newElement = document.createElementNS(svgNS, '~a');
  clog['~A'] = newElement;
  newElement.id = '~A';"
                            html
                            web-id web-id))
    (make-clog-element connection-id web-id :clog-type clog-type)))





(defgeneric create-SVG-element (clog-obj html-tag &rest all-args
                                &key content clog-type html-id auto-place
                                &allow-other-keys)
  (:documentation "Create a new CLOG-ELEMENT as child of CLOG-OBJ with any
possible tag and keywords."))

(defmethod create-SVG-element (clog-obj html-tag &rest all-args
                               &key (content "")
                                 clog-type
                                 html-id
                                 (auto-place t)
                               &allow-other-keys)
  (let* ((extra-args (alexandria:remove-from-plist all-args
                                                   :content :clog-type
                                                   :html-id :auto-place))
         (html (with-output-to-string (*standard-output*)
                 (format t "<~(~a~) " html-tag)
                 (loop for (key value) on extra-args by #'cddr
                       do (format t "~(~a~)=~s" key value))
                 (format t " id=~s>~A</~(~a~)>" html-id content html-tag)))
         (clog-type (or clog-type
                        (let* ((class-name (intern (string-upcase
                                                    (format nil "CLOG-~a"
                                                            html-tag))
                                                   :clog)))
                          (when (find-class class-name nil)
                            class-name))
                        'clog-element)))
    (create-SVG-child clog-obj html
                      :clog-type  clog-type
                      :html-id    html-id
                      :auto-place auto-place)))





(defgeneric create-SVG-child (clog-obj html &key html-id auto-place clog-type)
  (:documentation "Create a new CLOG-ELEMENT or sub-type of CLOG-TYPE from HTML
as child of CLOG-OBJ and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ, you can also set auto-place to :bottom or :top. If HTML-ID is nil one
will be generated. If auto-place is nil the object is stored in the connection-data
and not subject to browser-gc requests."))

(defmethod create-SVG-child ((obj clog-obj) html &key (html-id nil)
                                               (auto-place t)
                                               (clog-type 'clog-element))
  (let ((child (create-SVG-with-html (connection-id obj) (escape-string html)
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






(defclass clog-svg-circle (clog-element) ()
  (:documentation "CLOG SVG extension: SVG circle element."))

(defgeneric create-svg-circle (clog-obj &key cx cy r))

(defmethod create-svg-circle ((obj clog-obj) &key cx cy r html-id)
  (create-SVG-child obj (format nil "<circle xmlns='http://www.w3.org/2000/svg' ~a ~a ~a fill='gold' visibility='visible'></circle>"
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
