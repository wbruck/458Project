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
; position should just be length of lineup-1
(defun search-for-lineup-a (position-x player-x lineup)
  (declare (notinline search-for-lineup));to make trace work
  (let* ((position (aref *position-array* position-x))
         (player (aref position player-x)))
    (print player)
    (setq lineup (cons player lineup))
    (print lineup)
    (print (calculate-salary lineup))
    (print (calculate-score lineup))
    ;put whole cond in if to check if best score
    ; use loop to go through each position array
    ; use recursion to get up and down on array
    (cond ; call recursively DFS      
     ((> (calculate-salary lineup) 20000) ; salary too high
      (if (> (1+ player-x) (length position)) ; check next call player lenght 
        lineup ; return lineup TODO ->(really move up position)
        (search-for-lineup-a ;else
         position-x ; same position list
         (1+ player-x) ; next player
         (cdr lineup)))) ; remove last player from lineup
     ((> (1+ position-x) 2) ; terminator if for complete LEGAL lineup
      ; check if a finished lineup has best score
      (if (> (calculate-score lineup) *best-score*)
          lineup ; return lineup
        "not best score"))
     (t (search-for-lineup-a 
         (1+ position-x) ; next position
         0 ; first player in array
       lineup ))))) ; TODO - append lineup to LEGAL list start over

(defun loop-recursive (position-x lineup)
  (declare (notinline loop-recursive))
  (let* ((position (aref *position-array* position-x)))
    (loop
      for player across position
      do (setf lineup (cons player lineup))
      (print player)
;      (print lineup)
      (print (calculate-salary lineup))
         (cond
          ((> (1+ position-x) 2) ; TERMINATOR done with all positions
           (if (and (> (calculate-score lineup) *best-score*) ; check score
                    (< (calculate-salary lineup) *max-salary*))
               ; if best: append lineup, set best, cdr lineup, loop
               (progn (print "LEGAL LINEUP!!!!")
                 (setf *legal-lineup* (cons lineup *legal-lineup*))
                 (setf *best-score* (calculate-score lineup))
                 (setf lineup (cdr lineup))
                 lineup ) ; return lineup from the progn
              ;else remove player from list
             (setf lineup (cdr lineup))))
          ((< (calculate-salary lineup) *max-salary*) ; salary < max
           (loop-recursive (1+ position-x) lineup) ; go recursively to next position
           (setf lineup (cdr lineup))) ; remove player to loop to next
          ((> (calculate-salary lineup) *max-salary*) ; loop to next player
           (setf lineup (cdr lineup))
           ) ; remove player added before going to next
      (t (print "it worked"))) ; truth cond... do nothing
      (print lineup)
      (print (calculate-score lineup)))
    (print "out of loop")
      lineup)) ; return lineup
    
    
;;; TODO - add check score/append to not only find local max
(defparameter *max-salary* 21000)
(defparameter *best-score* 0)
(defparameter *legal-lineup* '())
(loop-recursive 0 '())
(SEARCH-FOR-LINEUP-A 0 0 'NIL)


