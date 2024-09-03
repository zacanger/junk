(defmacro while (test &body body)
  '(do ()
     ((not ,test))
     ,@body))
