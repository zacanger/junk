(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

; or as a macro
(defmacro avg (&rest args)
  '(/ (+ ,@args) ,(length args)))
