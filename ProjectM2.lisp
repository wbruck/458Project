;M2

;arrays for all the players for each position
; sort the lists so the the highest values are first
;  this will facilitate pruning
; *center* 
; once a player is selected as the 1st for a group remove from "master"



;start on position one
;DFS 
;if salary goes over limit (55000) prune
;if score is less than 'best path' prune
;- not as easy because score can improve

;does the state-visited every need to be checked?
;no



;
;;
;;;
;;;;successor functions will loop throug list of arrays, of possitions
;;;
;;
;

; GOAL STATE - the highest points total of all players, yet found
;recursively loop through arrays of players
; ?? add to a queue??? try to recurse...
; 
; returning the next player on the list
; for each return check the salary, if over 55k break
; for each return check "goal state" 

;(defun recursive-DFS-player-scorer (position array index, player array index)
;  (cond 
;   ((if salary > 55000) break )
;   ((if number_players_selected < 9) 
;    RECURSIVE-CALL((1+ position_array_index) player_array_index))))
   
;(defun recursive-dfs (array-arg)
;  (loop
;    while player in list
;    (let player (pop list)
;      (if (eq list last)
;          (loop
;            while
;            pop.list)
;        (recursive-dfs (1+ list))))))
    
;  pop player from array
;  ;do some calculation
;  (cond 
;  recursive-dfs (next-array))
     
    
;
;;
;;;
;;;; copy lists into new lists to work with
;;;
;;
;
;(defparameter *list-of-position-lists* '())

(defun copy-into-list-of-lists ()
  (defparameter *list-of-position-lists* '())
  (setq right-wings1 (copy-list *list-of-right-wings*))
  (setq *list-of-position-lists* (cons right-wings1 *list-of-position-lists*))
  (setq left-wings1 (copy-list *list-of-left-wings*))
  (setq *list-of-position-lists* (cons left-wings1 *list-of-position-lists*))
  (setq goalies (copy-list *list-of-goalies*))
  (setq *list-of-position-lists* (cons goalies *list-of-position-lists*))
  )


;;; use arrays so that they can be more easily navigated
;;; and are not consumed by the pop funtion
(defun copy-into-arrays ()
  (defparameter *position-array* (make-array 9))
  (vector-push *list-of-goalies*
               (setf *goalie-array* (make-array 
                                     (length *list-of-goalies*)
                                     :fill-pointer 0
                                     :adjustable t)))
  (setf (aref *position-array* 0) *goalie-array*)
  (vector-push *list-of-right-wings*
               (setf *right-wing-array* (make-array 
                                         (length *list-of-right-wings*)
                                         :fill-pointer 0
                                         :adjustable t)))
  (setf (aref *position-array* 1) *right-wing-array*)
  (setf *left-wing-array* (make-array 
                   (length *list-of-left-wings*)
                           :initial-contents *list-of-left-wings*))
  (setf (aref *position-array* 2) *left-wing-array*)
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

;;;;;PROBLEMS 
; 1. lists get consumed, making second runs, impossible
; 2. calling recursive "back" to the previous list of players not possible
; 3. 
  


;;;; Execute lines to save typing
(copy-into-list-of-lists)
(setq lineup1 (search-for-lineup *list-of-position-lists* '()))
(calculate-salary lineup1)
(calculate-score lineup1)



(defparameter *lineup* '())
; set-difference will return the intersection of 2 lists  