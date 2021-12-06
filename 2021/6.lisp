#!/usr/local/bin/sbcl --script

;;2021-12-06T24:00:00ZUTC+8

(declaim (sb-ext:muffle-conditions cl:warning))

(load "~/quicklisp/setup.lisp")

(ql:quickload :split-sequence)

(setf the-input-string "5,1,1,4,1,1,4,1,1,1,1,1,1,1,1,1,1,1,4,2,1,1,1,3,5,1,1,1,5,4,1,1,1,2,2,1,1,1,2,1,1,1,2,5,2,1,2,2,3,1,1,1,1,1,1,1,1,5,1,1,4,1,1,1,5,4,1,1,3,3,2,1,1,1,5,1,1,4,1,1,5,1,1,5,1,2,3,1,5,1,3,2,1,3,1,1,4,1,1,1,1,2,1,2,1,1,2,1,1,1,4,4,1,5,1,1,3,5,1,1,5,1,4,1,1,1,1,1,1,1,1,1,2,2,3,1,1,1,1,1,2,1,1,1,1,1,1,2,1,1,1,5,1,1,1,1,4,1,1,1,1,4,1,1,1,1,3,1,2,1,2,1,3,1,3,4,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,4,1,1,2,2,1,2,4,1,1,3,1,1,1,5,1,3,1,1,1,5,5,1,1,1,1,2,3,4,1,1,1,1,1,1,1,1,1,1,1,1,5,1,4,3,1,1,1,2,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,3,3,1,2,2,1,4,1,5,1,5,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,5,1,1,1,4,3,1,1,4")

(setf the-input-list 
      (mapcar 
            #'parse-integer 
            (split-sequence:split-sequence #\, the-input-string)))

;;;part-1
(defun a (b c)
    (if (= 8 (car b))
        (b (append '(8) (mapcar #'1- '(2 3 4))) c)
        (print (b b c))))

(defun b (c d)
(gc :full t)
    (if (= d 0)
        (length c)
        (b (c c) (1- d))))

(defun c (d)
    (let* ( (e nil)
            (f (remove-if (lambda (a) (if (= a 0) (push a e) nil)) d))
            (g (mapcar #'1- f))
            (h (loop for i from 1 to (length e) do (push 6 g) (push 8 g))))
        g))

(time (a the-input-list 80))  ;;391888
;;5,210,989,656 processor cycles

;;;part-2

;(time (a the-input-list 256)) ;]

(setf the-number-menu (make-array 10))

(defun d (e)
    (mapcar (lambda (a) (setf (aref the-number-menu a)  (1+ (aref the-number-menu a)))) e))

(defun e ()
    (let ((f (make-array 10)))
        (setf (aref f 8) (aref the-number-menu 0))
        (loop for i from 1 to 8 do
            (setf (aref f (1- i)) (aref the-number-menu i)))
        (setf (aref f 6) (+ (aref the-number-menu 0) (aref the-number-menu 7)))
        (loop for i from 0 to 8 do
            (setf (aref the-number-menu i) (aref f i)))))

(defun f (g)
    (d the-input-list)
    (loop for i from 1 to g do (e))
    (let (h) 
         (loop for ii from 0 to 8 do (push (aref the-number-menu ii) h))
         (print (apply #'+ h))))

(time (f 256))  ;;1754597645339
;;215,848 processor cycles
