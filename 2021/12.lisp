#!/usr/local/bin/sbcl --script

;;2021-12-12T24:00:00ZUTC+8

(declaim (sb-ext:muffle-conditions cl:warning))

(require :sb-concurrency)

(load "~/quicklisp/setup.lisp")

(ql:quickload :split-sequence)

(setf the-input-string 
"xx-end
EG-xx
iy-FP
iy-qc
AB-end
yi-KG
KG-xx
start-LS
qe-FP
qc-AB
yi-start
AB-iy
FP-start
iy-LS
yi-LS
xx-AB
end-KG
iy-KG
qc-KG
FP-xx
LS-qc
FP-yi")

(setf the-input-list 
    (mapcar 
        (lambda (a) 
            (split-sequence:split-sequence #\- a)) 
    (split-sequence:split-sequence #\newline the-input-string)))

(setf   lower-case-map  (make-hash-table :test #'equal)
        node-map        (make-hash-table :test #'equal)
        search-queue    (sb-concurrency:make-queue)
        path-list       nil)
        
(defun draw-the-map ()
    (mapcar 
        (lambda (a) 
            (let (  (x (car a))
                    (y (cadr a)))
                (when (<= 97 (char-code (char x 0)) 122) 
                    (setf (gethash x lower-case-map) t))
                (when (<= 97 (char-code (char y 0)) 122) 
                    (setf (gethash y lower-case-map) t))
                (push y (gethash x node-map))
                (push x (gethash y node-map))))
        the-input-list))

(draw-the-map)

;;;part-1
(defun search-for (the-path)  ;mini Breadth First Search
    (let ((the-node (car (last the-path))))
        (if (equal the-node "end")
            (push the-path path-list)
            (let ((the-next (gethash the-node node-map)))
                (if the-next
                    (mapcar 
                        (lambda (a) 
                            (if (and (gethash a lower-case-map) (member a the-path :test #'equal))
                                nil
                                (let ((new-path (append the-path (list a))))
                                    (sb-concurrency:enqueue new-path search-queue))))
                        the-next)
                    nil)))))
                    
(defun search-loop ()
    (let ((the-path (sb-concurrency:dequeue search-queue)))
        (if the-path
            (progn  (search-for the-path)
                    (search-loop))
            nil)))

(defun search-start ()
    (setf path-list nil)
    (sb-concurrency:enqueue (list "start") search-queue)
    (search-loop)
    (print (length path-list)))
    
(time (search-start))  ;;
;;

;;;part-2
(defun check-path (the-path the-node)
    (let (  (count-map (make-hash-table :test #'equal))
            (the-boolean nil) 
            (the-string nil) 
            (the-value nil))
        (mapcar 
            (lambda (a)
                (if (gethash a lower-case-map)
                    (let ((the-count (gethash a count-map)))
                        (if the-count
                            (setf (gethash a count-map) (1+ the-count))
                            (setf (gethash a count-map) 1))) 
                    nil))
            the-path)
        (when (equal the-node "start")
            (setf the-boolean t the-value t))
        (maphash 
            (lambda (k v)
                (cond   ((> v 2) (setf the-boolean t the-value t))
                        ((= v 2) (setf the-boolean t the-value nil the-string k))
                        ((< v 2) nil)))
            count-map)
        (when (and (null the-value) the-boolean)
            (when (equal the-string the-node) 
                (setf the-value t))
            (let ((node-value (gethash the-node count-map)))
                (if node-value
                    (setf the-value t)
                    nil)))
        the-value))

(defun search-for (the-path)
    (let ((the-node (car (last the-path))))
        (if (equal the-node "end")
            (push the-path path-list)
            (let ((the-next (gethash the-node node-map)))
                (if the-next
                    (mapcar 
                        (lambda (a) 
                            (if (check-path the-path a)
                                nil
                                (let ((new-path (append the-path (list a))))
                                    (sb-concurrency:enqueue new-path search-queue))))
                        the-next)
                    nil)))))

(time (search-start))  ;;
;;
