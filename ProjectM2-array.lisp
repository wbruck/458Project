;;; use arrays so that they can be more easily navigated
;;; and are not consumed by the pop funtion
(defun copy-into-arrays ()
  (defparameter *position-array* (make-array 9))
  (setf *goalie-array* (make-array 
                        (length *list-of-goalies*)
                        :initial-contents *list-of-goalies*))
  (setf (aref *position-array* 0) *goalie-array*)
  (setf *right-wing-array* (make-array
                            (length *list-of-right-wings*)
                                    :initial-contents *list-of-right-wings*))
  (setf (aref *position-array* 1) *right-wing-array*)
  (setf *left-wing-array* (make-array 
                   (length *list-of-left-wings*)
                           :initial-contents *list-of-left-wings*))
  (setf (aref *position-array* 2) *left-wing-array*)
  )

;
;;
;;;
;;;; RECURSIVELY GO THROUGH LISTS
;;;
;;
;
(defun search-for-lineup (list-of-all-positions lineup)
  (declare (notinline search-for-lineup));to make trace work
  (let ((player (pop (car list-of-all-positions))))
    ; (print player)
    (setq lineup (cons player lineup))
    (print (calculate-salary lineup))
    (cond ; call recursively DFS
     ((> (calculate-salary lineup) 20000) ; salary too high
      (if (null (car (car list-of-all-positions))) ; check position list length !0
        lineup ; return lineup
        (search-for-lineup list-of-all-positions ; same position list
                         (cdr lineup)))) ; remove last player from lineup
     ((eq (length (cdr list-of-all-positions)) 0) lineup) ; terminator
      (t (search-for-lineup (cdr list-of-all-positions) lineup)); position lists iteration
      )
    )) ; position lists

;
;;
;;;
;;;; CALCUALTE SALARY AND SCORE
;;;
;;
;
(defun calculate-salary (lineup)
  (loop with salary = 0
      for player in lineup
      do (incf salary (player-salary player))
      finally (return salary)))

;score
(defun calculate-score (lineup)
  (loop with score = 0
      for player in lineup
      do (incf score (player-score player))
        finally (return score)))
;
;;
;;;
;;;; RECURSIVELY GO THROUGH LISTS WITH ARRAYS!!
;;;
;;
;
(defun search-for-lineup-a (position-x player-x lineup)
  (declare (notinline search-for-lineup));to make trace work
  (let* ((position (aref *position-array* position-x))
         (player (aref position player-x)))
    (print player)
    (setq lineup (cons player lineup))
    (print (calculate-salary lineup))
    (print lineup)
    (cond ; call recursively DFS
     ((> (calculate-salary lineup) 20000) ; salary too high
      (if (> (1+ player-x) (length position)) ; check next call player lenght 
        lineup ; return lineup TODO ->(really move up position)
        (search-for-lineup-a ;else
         position-x ; same position list
         (1+ player-x) ; next player
         (cdr lineup)))) ; remove last player from lineup
     ((> (1+ position-x) 2) lineup) ; terminator if for complete LEGAL lineup
     (t (search-for-lineup-a 
         (1+ position-x) ; next position
         0 ; first player in array
       lineup ))))) ; TODO - append lineup to LEGAL list start over


(SEARCH-FOR-LINEUP-A 0 0 'NIL)
