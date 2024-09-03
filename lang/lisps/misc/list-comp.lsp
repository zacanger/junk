(defmacro lcomp (expr for var in list condi condi-test)
  (let ((res (gensym)))
    `(let ((,res nil))
       (loop for ,var in ,list
             ,coni ,condi-test
             do (setq ,res (append ,res (list ,expr))))
       ,res)))
