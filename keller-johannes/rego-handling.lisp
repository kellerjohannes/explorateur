(in-package :explorateur)

(defun dummy (num)
  (format t "~&Dummy output: ~a.~%" num))

(funcall
  (incudine:regofile->function (merge-pathnames "rego-scores/test1.rego"
                                                (asdf:system-source-directory :explorateur))))
