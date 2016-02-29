;;; This is the IDE's built-in-editor, where you create and edit
;;; lisp source code.  You could use some other editor instead,
;;; though the IDE's menu-bar commands would not be applicable there.
;;; 
;;; This editor has a tab for each file that it's editing.  You can
;;; create a new editor buffer at any time with the File | New command.
;;; Other commands such as Search | Find Definitions will create
;;; editor buffers automatically for existing code.
;;; 
;;; You can use the File | Compile and Load command to compile and
;;; load an entire file, or compile an individual definition by
;;; placing the text cursor inside it and using Tools | Incremental
;;; Compile.  You can similarly evaluate test expressions in the
;;; editor by using Tools | Incremental Evaluation; the returned
;;; values and any printed output will appear in a lisp listener
;;; in the Debug Window.
;;; 
;;; For a brief introduction to other IDE tools, try the
;;; Help | Interactive IDE Intro command.  And be sure to explore
;;; the other facilities on the Help menu.

;;;; PLAYER CLASS
(defclass player()
  ((name :accessor player-name
         :initform 'blank
         :initarg :name)
   (score :accessor player-score
          :initform 0
          :initarg :score)
   (salary :accessor player-salary
           :initform 0
           :initarg :salary)
   (position :accessor player-position
             :initform 'x
             :initarg :position)))
  
;
;;
;;;
;;;; READ IN FILE
;;;
;;
;
;;; puts into list of strings
(WITH-OPEN-FILE (STREAM "C:\\Users\\admin\\New folder\\smallplayers.csv"
                             :DIRECTION :INPUT)
              (DO ((LINE NIL) (RESULT NIL (CONS LINE RESULT)))
                  ((EQ LINE :EOF) (NREVERSE RESULT))
                (SETQ LINE (READ-LINE STREAM NIL :EOF))))

;;; function to do same
(defun read-all-lines (input-stream)
  (DO ((LINE NIL) (RESULT NIL (CONS LINE RESULT)))   ; var init step
                  ((EQ LINE :EOF) (NREVERSE RESULT)) ; end-test result
    (SETQ LINE (READ-LINE STREAM NIL :EOF))))        ; statement

;
;;
;;;
;;;; PARSE STRING 
;;;
;;
;
; take string and make a list
;;;;;;;;;TODO - use array not list
(defun split-string-comma (string)
  (unless (eq (length string) 0)
  (let ((delimiter (position #\, string :test #'equal))) ; find first comma
    (cons (subseq string 0 delimiter)                    ; cons list of first place 
          (split-string-comma (subseq string (1+ delimiter))))))) ; with recursive call trimming string
                                                                     ; trimming +1 to lose ,
;;;;;;notes on sting parsing
;;find position
(position #\, line :test #'equal)
;;trim until position
(subseq line 0 position)

;
;;
;;;
;;;; PUT LIST INTO PLAYER CLASS
;;;
;;
;
(defun list-into-player-class (list)
  (let ((sam (MAKE-INSTANCE 'PLAYER)))  ;make instance
    (LOOP FOR ITEM IN LIst ; items in list
    FOR I upto 12     ; number of items "index"
    do (cond ((= i 1) (setf (player-position sam) item))
             ((= i 2) (setf (player-name sam) item))
             ((= i 3) (setf (player-name sam) 
                        (concatenate 'string (player-name sam) item)))
             ((= i 4) (setf (player-score sam) item)) ; TODO use string-to-number
             ((= i 6) (setf (player-salary sam) item)))) ; TOD use string-to-number
    sam))


string-to-number


 (loop with players = ()
                for line in read-in
                    do (cons (list-into-player-class (split-string-comma (line))) players)
                    players)