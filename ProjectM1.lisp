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
             :initarg :position)
   (played :accessor player-played
           :initform 0
           :initarg :played)
   (injury :accessor player-injury
           :initform 'blank
           :initarg :injury)))

;;; print player class ; from textbook
(defmethod print-object ((object player) stream)
  (format stream "#<PLAYER ~A (POSITION: ~A, SAL: ~A)>"
    (player-name object)
    (player-position object)
    (player-salary object)))
  
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
(defun read-all-lines (input-file)
  (with-open-file (stream input-file 
                          :direction :input)
  (DO ((LINE NIL) (RESULT NIL (CONS LINE RESULT)))   ; var init step
                  ((EQ LINE :EOF) (NREVERSE RESULT)) ; end-test result
    (SETQ LINE (READ-LINE STREAM NIL :EOF)))))        ; statement

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
;(position #\, line :test #'equal)
;;trim until position
;(subseq line 0 position)

;
;;
;;;
;;;; PUT LIST INTO PLAYER CLASS
;;;
;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;FOR UNKNOWN REASONS the csv file has to have an empty row or it will 
;;;; not parse the last column, i guess theres  a bug in my parser
(defun list-into-player-class (list)
  (let ((sam (MAKE-INSTANCE 'PLAYER)))  ;make instance
    (LOOP FOR ITEM IN LIst ; items in list
        FOR I upto 13     ; number of items "index"
        do (cond 
            ((= i 1) (setf (player-position sam) item))
            ((= i 2) (setf (player-name sam) item))
            ((= i 3) (setf (player-name sam) 
                       (concatenate 'string (player-name sam) item)))
            ((= i 4) (setf (player-score sam) (read-from-string item)))
            ((= i 5) (setf (player-played sam) (parse-integer item)))
            ((= i 6) (setf (player-salary sam) (parse-integer item)))
            ((= i 10) (setf (player-injury sam) item))))
    sam))

;
;;
;;;
;;;; Whole function to parse csv into classes
;;;
;;
;
; preliminary function to put all stats into list of lists
(defun csv-into-player-lists (input-file)
  (let* ((lines (reverse (cdr (reverse                ; hack to get rid of :EOF garbage
                      (cdr (read-all-lines input-file)))))))
    (loop
      with add-player-list = ()
      for line in lines
      do (setf add-player-list (cons (split-string-comma line) add-player-list))
      finally (return add-player-list))))

;copy of prelim, with all stats into class
;;
;;; DOES NOT WORK, but I think it would be faster
;;
;(defun csv-into-player-class (input-file)
;  (let* ((lines (reverse (cdr (reverse                ; hack to get rid of :EOF garbage
;                      (cdr (read-all-lines input-file)))))))
;    (loop
;      with add-player-class-list = ()
;      for line in lines
;      do ((setf add-player (split-string-comma line))
;          (setf add-player-class-list (cons 
;                                       (list-into-player-class add-player)
;                                       add-player-class-list))
;          finally (return add-player-class-list)))))

; read in csv and put players into player classes
(defun csv-into-list-of-player-classes (file-input)
  (LOOP
    WITH PLAYER-CLASSES = NIL 
    FOR LIST IN (CSV-INTO-PLAYER-LISTS FILE-INPUT)
    DO (setf player-classes (CONS (LIST-INTO-PLAYER-CLASS LIST)
                                  PLAYER-CLASSES))
    FINALLY (RETURN PLAYER-CLASSES)))

;
;;
;;;
;;;; SORT Player Classes into position lists
;;;
;;
;
;Seems more efficient to do this in the initial function but I couldnt figure it out
;;
;defined these in a file that the function loads to "refresh" them every time
;(defparameter *list-of-centers* '())
;(defparameter *list-of-right-wings* '())
;(defparameter *list-of-left-wings* '())
;(defparameter *list-of-goalies* '())
; (defparameter *list-of-defensemen* '())

;player-class-list is output of csv-into-loist-of-player-classes
; TODO - can probably add that into the function proper to make it easier
(defun sort-player-classes-into-list-by-position (file-input)
  (let ((player-class-list (csv-into-list-of-player-classes file-input)))
  (load "C:/458Project/defPlayerLists.lisp")
  (loop
    for player in player-class-list
    do (cond
        ((< (player-played player) 20)
         (print "bum"))
        ((equal (player-injury player) "")
         (print "not hurt"))
        ((equal (player-position player) "C")
         (setf *list-of-centers*
           (cons player *list-of-centers*)))
        ((equal (player-position player) "LW")
         (setf *list-of-left-wings*
           (cons player *list-of-left-wings*)))
        ((equal (player-position player) "RW")
         (setf *list-of-right-wings*
           (cons player *list-of-right-wings*)))
        ((equal (player-position player) "D")
         (setf *list-of-defensemen*
           (cons player *list-of-defensemen*)))
        ((equal (player-position player) "G")
         (setf *list-of-goalies*
           (cons player *list-of-goalies*)))))))

(setq file-in "C:/458Project/smallplayers.csv")
(setq file-2 "C:/458Project/big2-29.csv")
(setq full-file "C:/458Project/NHL-2016-03-08-14899-players-list.csv")
(setq file-mini "C:/458Project/miniplayer.csv")
(setq med-file "C:/458Project/med-small.csv")

(sort-player-classes-into-list-by-position med-file)
;(sort-player-classes-into-list-by-position
 ;(loop with players = ()
  ;              for line in read-in
   ;                 do (cons (list-into-player-class (split-string-comma (line))) players)
    ;                players)