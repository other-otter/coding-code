#!/usr/local/bin/sbcl --script

;;2021-12-11T24:00:00ZUTC+8

(declaim (sb-ext:muffle-conditions cl:warning))

(load "~/quicklisp/setup.lisp")

(ql:quickload :split-sequence)

(setf the-input-string 
"4871252763
8533428173
7182186813
2128441541
3722272272
8751683443
3135571153
5816321572
2651347271
7788154252")

(defun output-list ()
    (mapcar 
        (lambda (a)
            (loop for i from 0 below (length a) collect
                (parse-integer (format nil "~A" (char a i)))))
        (split-sequence:split-sequence #\newline the-input-string)))

;;;part-1
(defun a (the-output-list the-count)
    (let (  (the-set nil) 
            (the-boolean nil))
        (labels (   (add-one () 
                        (loop for i from 0 to 99 do
                            (multiple-value-bind (y x) (floor i 10)
                                (setf   (nth x (nth y the-output-list)) 
                                        (1+ (nth x (nth y the-output-list)))))))
                    (add-zero () 
                        (setf the-boolean nil)
                            (loop for i from 0 to 99 do
                                (multiple-value-bind (y x) (floor i 10)
                                    (let ((c (nth x (nth y the-output-list))))
                                        (if (> c 9)
                                            (progn 
                                                (setf the-boolean t)
                                                (push (cons x y) the-set)
                                                (setf (nth x (nth y the-output-list)) 0)
                                                (add-around x y)
                                                (setf the-count (1+ the-count)))
                                            nil))))
                            (if the-boolean 
                                (add-zero)
                                nil))
                    (add-around (x y)     
                        (let (  (the-q (cons (1- x) (1+ y)))
                                (the-w (cons x      (1+ y)))
                                (the-e (cons (1+ x) (1+ y)))
                                (the-a (cons (1- x) y))
                                (the-s (cons x      y))
                                (the-d (cons (1+ x) y))
                                (the-z (cons (1- x) (1- y)))
                                (the-x (cons x      (1- y)))
                                (the-c (cons (1+ x) (1- y))))
                            (mapcar 
                                (lambda (a) 
                                    (if (and (<= 0 (car a) 9) (<= 0 (cdr a) 9) (null (member a the-set :test #'equal)))
                                        (setf (nth (car a) (nth (cdr a) the-output-list)) (1+ (nth (car a) (nth (cdr a) the-output-list))))
                                        nil))
                                (list the-q the-w the-e the-a the-d the-z the-x the-c)))))
            (progn  (add-one) 
                    (add-zero)
                    (list the-output-list the-count)))))

(defun b ()
    (let (  (the-output-list (output-list)) 
            (the-count 0))
        (loop for i from 1 to 100 do 
            (let ((the-value (a the-output-list the-count)))
                (setf   the-output-list (car the-value)
                        the-count (cadr the-value))))
        (print the-count)))

(time (b))  ;;1747
;;14,296,072 processor cycles

;;;part-2
(defun c ()
    (let (  (the-output-list (output-list)) 
            (count-number 0))
        (block k
            (loop 
                (progn  (setf   the-output-list (car (a the-output-list 0))
                                count-number (1+ count-number))
                        (if (let (l)
                                (loop for i from 0 to 99 do
                                    (multiple-value-bind (y x) (floor i 10)
                                        (let ((c (nth x (nth y the-output-list))))
                                            (if (= c 0)
                                                nil
                                                (setf l t))))) l) 
                            nil 
                            (return-from k count-number))))) 
        (print count-number)))

(time (c))  ;;505
;;74,170,104 processor cycles
