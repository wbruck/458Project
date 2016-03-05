;M2

;arrays for all the players for each position
; sort the lists so the the highest values are first
;  this will facilitate pruning
; *center* 
; once a player is selected as the 1st for a group remove from "master"



start on position one
DFS 
if salary goes over limit (55000) prune
if score is less than 'best path' prune
- not as easy because score can improve

does the state-visited every need to be checked?
no



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

(defun recursive-DFS-player-scorer (position array index, player array index)
  (cond 
   ((if salary > 55000) break )
   ((if number_players_selected < 9) 
    RECURSIVE-CALL((1+ position_array_index) player_array_index))))
   
(defun recursive-dfs (array-arg)
  (loop
    while player in list
    (let player (pop list)
      (if (eq list last)
          (loop
            while
            pop.list)
        (recursive-dfs (1+ list))))))
    
  pop player from array
  ;do some calculation
  (cond 
  recursive-dfs (next-array)
     
     ;
   

set-difference will return the intersection of 2 lists 