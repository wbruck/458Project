(defun get-time-string ()
  (multiple-value-bind (s m h d mo y) (get-decoded-time)
    (format nil "~4D-~2,'0D-~2,'0D,~2,'0D:~2,'0D:~2,'0D" y mo d h m s)))

(defmacro test-code (&rest forms)
  `(dolist (form ',forms)
     (format t "~2%At ~A, evaluating: ~2%~A~%"
             (get-time-string) form)
     (format t "~2%Result: ~%~S~2%" (eval form))))

