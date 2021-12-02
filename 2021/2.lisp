#!/usr/local/bin/sbcl --script

;;2021-12-02T20:21:12ZUTC+8

(load "~/quicklisp/setup.lisp")

(ql:quickload :split-sequence)

(setf the-input-string 
"forward 2
down 9
up 6
forward 1
down 5
down 7
down 9
forward 9
down 8
up 7
forward 2
up 6
forward 4
down 5
down 9
up 1
down 9
forward 8
forward 6
forward 6
forward 5
forward 9
up 3
up 5
forward 1
down 4
down 7
forward 2
up 3
down 8
forward 1
down 2
forward 3
up 1
up 1
up 7
forward 5
up 8
forward 8
forward 8
down 6
forward 1
forward 5
forward 4
forward 6
forward 5
down 6
down 9
forward 9
down 8
forward 6
down 5
forward 9
up 3
up 1
down 8
down 7
down 9
forward 7
down 8
down 9
down 5
down 3
forward 1
forward 6
down 1
forward 9
down 5
forward 7
up 2
down 8
forward 1
down 4
down 9
down 4
up 5
forward 4
forward 6
forward 1
down 3
forward 1
down 6
up 5
up 4
forward 6
forward 1
forward 1
down 2
up 4
up 3
up 2
up 6
down 6
forward 1
down 8
forward 1
up 6
forward 7
down 5
forward 4
forward 6
down 4
forward 4
down 4
down 4
forward 2
forward 8
down 5
down 1
down 8
up 5
up 8
down 5
forward 4
down 6
up 7
forward 2
down 3
forward 2
forward 2
down 9
down 3
up 6
forward 8
up 2
up 9
forward 4
down 1
down 5
forward 4
down 2
down 3
forward 5
down 4
forward 7
up 4
forward 6
up 8
forward 1
up 9
down 4
forward 2
down 1
forward 7
down 3
down 2
forward 5
down 3
down 9
down 9
up 5
forward 2
down 8
up 9
forward 4
down 3
forward 3
forward 6
up 2
forward 3
down 1
down 1
down 1
forward 7
forward 4
forward 7
down 5
down 6
down 2
forward 6
down 3
up 6
forward 4
down 8
up 1
forward 8
down 2
down 5
forward 4
down 9
forward 2
forward 2
down 3
forward 3
down 1
forward 2
down 7
forward 3
forward 9
up 9
forward 6
forward 2
down 1
down 5
forward 6
forward 6
down 3
up 3
forward 9
down 7
down 2
down 4
down 7
forward 5
up 4
forward 8
down 5
forward 7
down 7
up 7
down 8
forward 9
up 5
forward 1
down 2
forward 5
down 9
forward 3
down 5
forward 8
forward 3
up 5
down 2
up 3
forward 2
up 1
up 5
down 8
forward 2
down 5
up 4
up 5
up 2
forward 9
forward 6
down 9
up 9
forward 6
forward 4
forward 3
forward 7
up 1
down 2
down 6
down 1
forward 8
down 1
forward 6
down 8
forward 8
down 7
down 6
down 5
forward 2
up 8
up 6
up 5
down 1
forward 1
down 1
down 5
forward 7
forward 3
down 1
forward 5
forward 5
forward 8
down 1
up 2
down 6
up 6
forward 6
forward 6
down 3
forward 9
up 4
forward 4
down 6
up 1
forward 6
down 2
down 5
down 2
down 6
up 5
down 1
down 1
forward 3
forward 7
forward 3
up 2
down 8
down 4
down 1
down 5
down 1
down 9
forward 6
down 6
down 4
down 6
down 8
forward 4
down 6
down 7
forward 8
down 4
up 4
down 1
forward 1
forward 4
forward 1
up 9
down 7
forward 7
down 4
forward 1
up 4
forward 4
down 5
down 7
forward 5
forward 7
forward 1
forward 1
forward 9
forward 9
up 3
forward 4
down 2
forward 9
up 8
forward 3
up 5
down 3
down 8
forward 8
down 6
forward 1
down 6
down 6
up 9
down 2
forward 8
up 9
down 7
up 9
up 8
up 1
forward 6
forward 9
down 2
forward 8
down 1
up 4
forward 4
forward 7
up 2
forward 4
down 5
forward 3
down 2
down 7
down 4
down 2
up 5
down 5
down 5
down 4
up 1
forward 7
down 6
forward 5
forward 1
down 4
up 9
down 5
forward 7
forward 5
down 6
down 3
down 9
down 1
forward 6
up 2
down 7
down 3
down 6
up 3
down 4
down 4
forward 9
down 3
forward 2
down 9
down 8
up 4
down 2
forward 2
down 5
down 4
down 4
down 2
forward 6
down 3
forward 1
down 4
forward 7
down 5
up 4
down 6
forward 8
down 6
forward 2
forward 4
forward 5
forward 7
forward 4
forward 5
down 8
down 7
forward 3
forward 5
up 7
forward 1
down 4
forward 5
forward 4
forward 4
down 5
down 8
forward 8
down 1
down 1
down 5
up 5
forward 6
down 6
forward 3
forward 4
forward 7
forward 4
down 8
forward 2
down 4
forward 4
down 1
up 2
forward 6
up 1
down 7
down 9
forward 7
forward 2
up 3
down 2
down 9
down 5
up 7
forward 1
forward 8
down 8
up 3
down 3
forward 9
up 4
down 5
up 5
down 1
up 8
forward 9
down 3
up 6
forward 6
forward 1
down 1
forward 9
down 8
forward 8
down 6
up 9
down 4
up 3
up 9
forward 2
down 2
down 2
forward 3
down 2
forward 5
forward 4
up 8
forward 9
up 7
forward 2
down 5
down 6
forward 8
up 7
forward 4
forward 3
up 5
down 8
forward 3
up 2
down 3
forward 6
down 9
down 2
down 6
down 2
forward 7
forward 5
forward 7
down 8
forward 2
down 2
forward 8
up 8
forward 4
forward 3
up 5
down 3
forward 3
up 8
up 7
down 4
down 1
forward 2
down 1
up 6
up 4
down 3
up 1
forward 7
forward 7
forward 7
forward 8
down 1
forward 5
down 6
forward 9
forward 7
forward 7
down 4
up 4
down 6
down 9
up 4
up 2
up 6
forward 4
up 4
up 6
down 2
forward 4
down 9
forward 9
forward 9
down 1
forward 7
down 2
down 7
down 8
down 8
down 9
up 9
down 5
forward 5
forward 7
forward 4
down 7
forward 8
forward 1
down 8
up 9
down 7
forward 9
forward 4
forward 8
down 9
forward 4
down 3
forward 3
down 1
down 1
down 2
up 5
down 2
down 1
down 8
forward 3
up 2
forward 7
down 3
down 8
down 1
forward 4
forward 7
down 5
forward 6
down 6
down 2
forward 6
down 3
up 4
down 7
forward 7
up 1
up 9
down 1
down 2
down 8
down 7
up 1
forward 7
down 2
forward 4
forward 6
forward 9
down 6
forward 2
up 8
down 2
up 2
up 5
down 8
up 6
down 9
forward 6
down 8
down 6
down 1
up 7
up 6
down 8
forward 2
up 7
forward 5
forward 7
forward 7
up 5
forward 2
down 9
up 2
up 8
up 2
down 3
down 7
forward 9
down 3
up 9
forward 8
up 8
forward 4
forward 8
forward 6
up 1
down 3
up 1
down 1
forward 2
forward 1
forward 4
forward 7
up 8
down 9
up 2
down 7
forward 4
down 3
forward 4
forward 2
down 9
forward 8
forward 5
forward 3
down 6
forward 4
forward 4
forward 9
forward 4
up 5
down 7
up 6
forward 5
down 5
forward 4
down 5
forward 7
forward 3
forward 5
down 5
forward 4
down 5
up 4
down 8
up 3
down 3
up 5
forward 4
forward 5
down 6
forward 6
forward 1
forward 8
down 6
down 9
up 5
forward 2
forward 8
up 6
down 6
forward 2
down 8
forward 7
forward 7
down 5
forward 5
forward 8
forward 1
down 4
down 2
down 5
up 4
forward 3
forward 5
down 4
down 7
down 4
up 9
up 6
forward 1
down 8
up 8
up 9
forward 2
forward 1
down 6
forward 6
down 4
forward 7
up 2
up 1
forward 4
down 1
forward 8
forward 3
up 7
up 5
down 1
forward 8
forward 6
up 6
forward 9
down 5
down 9
forward 2
down 3
up 1
up 7
down 1
forward 8
up 9
down 1
down 5
down 7
down 5
down 5
down 5
up 9
forward 9
forward 7
forward 4
forward 6
down 5
down 3
forward 9
forward 1
down 1
down 8
up 4
down 9
forward 9
up 1
down 5
forward 8
up 6
forward 3
down 6
up 8
down 7
forward 3
forward 6
down 7
forward 6
forward 4
forward 4
down 4
forward 6
forward 5
down 6
forward 6
down 7
forward 6
forward 3
up 4
up 2
up 6
down 2
down 8
forward 5
forward 1
up 4
forward 7
forward 9
up 6
down 7
down 3
up 5
forward 5
down 8
up 1
down 1
down 3
down 2
down 1
forward 5
down 3
down 5
forward 7
forward 9
down 3
forward 7
forward 5
forward 4
forward 2
forward 7
forward 8
forward 6
down 8
forward 5
forward 6
forward 6
down 8
down 2
forward 4
down 7
forward 6
down 7
down 4
forward 6
up 6
forward 4
forward 9
forward 2
forward 3
forward 1
down 8
down 3
forward 4
up 3
forward 7
forward 1
down 7
down 8
forward 1
up 8
forward 8
up 8
down 5
forward 6
down 8
down 4
down 9
up 1
down 3
forward 6
down 6
forward 7
forward 3
down 6
down 6
forward 4
down 4
down 1
down 8
forward 2
forward 8
forward 8
down 6
forward 9
down 9
down 5
down 5
forward 7
down 1
forward 1
down 1
down 6
down 1
forward 1
up 6
up 9
forward 5
down 6
forward 8
forward 6
down 7
forward 1
forward 4
forward 9
forward 2
forward 4
down 2
forward 1
forward 8
down 1
down 1
forward 4
down 5
down 3
down 9
down 2
up 8
down 7
down 1
down 9
forward 2
forward 2
up 3
forward 3
down 3
forward 5
forward 9
down 7
up 7
down 9
forward 3
forward 7
down 1
forward 8
down 8
forward 1
down 8
down 6
forward 2
down 3
down 1
down 8
forward 3
up 5
down 7
up 2
up 8
forward 5
up 7
down 6
up 7
down 9
forward 5
up 4
forward 9
down 5
up 7
down 2
up 2
up 7
forward 5
down 6
forward 4
down 4
down 3
forward 2
up 2
down 5
forward 8
down 3
up 7
down 1
down 7
forward 7
forward 4
forward 7
down 2
down 9
down 6
down 9
down 2
down 9
down 7
down 5
forward 4
up 5
up 7
forward 2
forward 7
down 3
down 3
forward 4")

(setf the-input-list 
    (mapcar 
        (lambda (a)
            (cons (car a) (parse-integer (cadr a))))
        (remove-if #'null
            (mapcar 
                (lambda (a) 
                    (split-sequence:split-sequence #\space a :remove-empty-subseqs t)) 
                (split-sequence:split-sequence #\newline the-input-string)))))

;;;part-1
(defun moving ()
    (let (the-move-x the-move-y)
        (setf the-move-x 0 the-move-y 0)
        (loop for i in the-input-list do
            (let (  (the-task (car i))
                    (the-number (cdr i)))
                (cond   ((equal "forward" the-task) (setf the-move-x (+ the-move-x the-number)))
                        ((equal "up" the-task) (setf the-move-y (- the-move-y the-number)))
                        ((equal "down" the-task) (setf the-move-y (+ the-move-y the-number))))))
        (print (* the-move-x the-move-y))))

(time (moving))  ;;2070300
;;229,604 processor cycles

;;;part-2
(defun moving-with-aim ()
    (let (the-move-a the-move-x the-move-y)
        (setf the-move-a 0 the-move-x 0 the-move-y 0)
        (loop for i in the-input-list do
            (let (  (the-task (car i))
                    (the-number (cdr i)))
                (cond   ((equal "forward" the-task) 
                            (progn  (setf the-move-x (+ the-move-x the-number))
                                    (setf the-move-y (+ the-move-y (* the-move-a the-number)))))
                        ((equal "up" the-task) (setf the-move-a (- the-move-a the-number)))
                        ((equal "down" the-task) (setf the-move-a (+ the-move-a the-number))))))
        (print (* the-move-x the-move-y))))

(time (moving-with-aim))  ;;2078985210
;;240,844 processor cycles
