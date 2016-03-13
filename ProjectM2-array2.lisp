;;; sort list
(defun sort-list (list-of-players)
  (let* ((listx '())
         (sub-list (cdr list-of-players)))
    (setf listx (cons (car list-of-players) listx))
    (loop
      for x in sub-list
      do (cond
          ((> (player-salary x) (player-salary (car listx)))
           (setf listx (cons x listx)))
          ((equalp (player-salary x) (player-salary (car listx)))
           (setf listx (cons x listx)))
          ;find position in list to insert
          ((< (player-salary x) (player-salary (car (last listx))))
         ;  (print "poop")
           (setf listx (append listx (list x))))
       (t ; loop through list until you find < next
        ;  (print (player-salary x))
        ;  (print listx)
        ;  (print (length listx))
          (loop
            for y upto (length listx)
            ; find first salaryX > salaryY
            when (>= (player-salary x) (player-salary (nth y listx)))
                   ; let num-less be (length loop-list)
            return (progn
                     (let ((num-less y)) 
                       ; (cons player (last sub-list num-less)
                     ;  (print num-less)
                     ;  (print (butlast listx num-less))
                     ;  (print "end")
                     ;  (print (cons x (last listx num-less)))
                       (setf listx (append (butlast listx num-less) 
                                           (cons x (last listx num-less))))))
       )
          )
       ))
  listx))
  
;  find position its < append there

;(sort-list *list-of-left-wings*)
;(reverse *)
;(loop
;  for x in *
;  do (print (player-salary x)))
;        
    

;;; use arrays so that they can be more easily navigated
;;; and are not consumed by the pop funtion
(defun copy-into-arrays ()
  (defparameter *position-array* (make-array 9))
  (setf *goalie-array* (make-array 
                        (length *list-of-goalies*)
                        :initial-contents (reverse (sort-list *list-of-goalies*))))
  (setf (aref *position-array* 0) *goalie-array*)
  (setf *right-wing-array* (make-array
                            (length *list-of-right-wings*)
                            :initial-contents (reverse 
                                               (sort-list *list-of-right-wings*))))
  (setf (aref *position-array* 1) *right-wing-array*)
  (setf *left-wing-array* (make-array 
                   (length *list-of-left-wings*)
                           :initial-contents (reverse
                                              (sort-list *list-of-left-wings*))))
  (setf (aref *position-array* 2) *left-wing-array*)
  (setf *right-wing-array2* (make-array
                            (length *list-of-right-wings*)
                             :initial-contents (reverse
                                                (sort-list *list-of-right-wings*))))
  (setf (aref *position-array* 3) *right-wing-array2*)
  (setf *left-wing-array2* (make-array 
                   (length *list-of-left-wings*)
                            :initial-contents (reverse
                                               (sort-list *list-of-left-wings*))))
  (setf (aref *position-array* 4) *left-wing-array2*)
  (setf *center-array* (make-array
                        (length *list-of-centers*)
                        :initial-contents (reverse
                                           (sort-list *list-of-centers*))))
  (setf (aref *position-array* 5) *center-array*)
  (setf *defenseman-array* (make-array
                                  (length *list-of-defensemen*)
                            :initial-contents (reverse 
                                              (sort-list *list-of-defensemen*))))
  (setf (aref *position-array* 6) *defenseman-array*)
  (setf *center-array2* (make-array
                        (length *list-of-centers*)
                         :initial-contents (reverse
                                            (sort-list *list-of-centers*))))
  (setf (aref *position-array* 7) *center-array2*)
  (setf *defenseman-array2* (make-array
                                  (length *list-of-defensemen*)
                             :initial-contents (reverse
                                                (sort-list *list-of-defensemen*))))
  (setf (aref *position-array* 8) *defenseman-array2*)
  )


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

; check if player already in lineup
(defun in-lineup-p (lineup player)
  (setq val nil)
  (loop
    for dude in lineup
    do (if (equalp (player-name dude) (player-name player))
           (progn
          ;   (print (player-name dude))
          ;   (print (player-name player))
             (setq val T))
        ; (print "not"))
    ))
  val)


;
;;
;;;
;;;; RECURSIVELY GO THROUGH LISTS WITH ARRAYS!!
;;;
;;
; position should just be length of lineup-1


(defun loop-recursive (position-x lineup)
  (declare (notinline loop-recursive))
  (let* ((position (aref *position-array* position-x)))
    (loop
      for player across position
    ;  when (> (calculate-salary (cons player lineup))
   ;           *max-salary*) ; loop to next player
  ;    return lineup ;(progn
            ;   (print (cons player lineup))
                ;      (print "bonk"))
      do (if (null (in-lineup-p lineup player))
             (progn 
            ;   (print player)
               (setf lineup (cons player lineup))
               (let ((line-salary (calculate-salary lineup))
                     (line-score (calculate-score lineup)))
          ;  (print lineup)
               (cond
                ((> (1+ position-x) 8) ; TERMINATOR done with all positions
                 (if (and (> line-score *best-score*) ; check score
                          (< line-salary *max-salary*))
                     ; if best: append lineup, set best, cdr lineup, loop
                     (progn (print "LEGAL LINEUP!!!!")
                       (setf *legal-lineup* (cons lineup *legal-lineup*))
                       (setf *best-score* line-score)
                       (setf lineup (cdr lineup))
                       lineup ) ; return lineup from the progn
                   ;else remove player from list
                   (setf lineup (cdr lineup))))
                ((< line-salary *max-salary*) ; salary < max
                 (loop-recursive (1+ position-x) lineup) ; go recursively to next position
                 (setf lineup (cdr lineup))) ; remove player to loop to next
                ((> line-salary *max-salary*) ; salary < max
                 ;(loop-recursive (1+ position-x) lineup) ; go recursively to next position
                 (setf lineup (cdr lineup))) ; remove player to loop to next
                ; return if salary is too high
                (t (1+ 1))) ; truth cond... do nothing
               ))))
    lineup))
    
;;; TODO - add check score/append to not only find local max
(copy-into-arrays)
(defparameter *max-salary* 55000)
(defparameter *best-score* 0)
(defparameter *legal-lineup* '())
(time (loop-recursive 0 '()))

;(SEARCH-FOR-LINEUP-A 0 0 'NIL) .
(loop
  for line in *legal-lineup*
  do (print line))
(loop
  for line in *legal-lineup*
  do (print (calculate-salary line)))
(loop
  for line in *legal-lineup*
  do (print (calculate-score line)))

