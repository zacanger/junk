(defmacro for ((var start stop) &body body)
  '(do ((,var ,start (1+ ,var))
        (limit ,stop))
     ((> ,var limit))
     ,@body))

; (for (a 0 2) (princ a))
