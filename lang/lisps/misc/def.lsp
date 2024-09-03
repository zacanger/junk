(defmacro defn (name params &body body)
  '(progn
     (setf (symbol-function ',name)
           #'(lambda ,params (block ,name ,@body)))
     ',name))

;; or
(defmacro defn (name lambda-list &rest body)
  `(setf (fdefinition ',name)
         (lambda ,lambda-list
           (block ,name ,@body))))
