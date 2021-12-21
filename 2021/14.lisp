#!/usr/local/bin/sbcl --script

;;2021-12-20T24:00:00ZUTC+8

(declaim (sb-ext:muffle-conditions cl:warning))

(declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))

(setf sb-impl::*default-external-format* :UTF-8)

(load "~/quicklisp/setup.lisp")

(ql:quickload :split-sequence)

(setf the-input-string "PHVCVBFHCVPFKBNHKNBO")

(setf the-dictionary-string 
"HK -> F
VN -> S
NB -> F
HF -> B
CK -> N
VP -> B
HO -> P
NH -> N
CC -> N
FC -> P
OK -> S
OO -> P
ON -> C
VF -> B
NN -> O
KS -> P
FK -> K
HB -> V
SH -> O
OB -> K
PB -> V
BO -> O
NV -> K
CV -> H
PH -> H
KO -> B
BC -> B
KC -> B
SO -> P
CF -> V
VS -> F
OV -> N
NS -> K
KV -> O
OP -> O
HH -> C
FB -> S
CO -> K
SB -> K
SN -> V
OF -> F
BN -> F
CP -> C
NC -> H
VH -> S
HV -> V
NF -> B
SS -> K
FO -> F
VO -> H
KK -> C
PF -> V
OS -> F
OC -> H
SK -> V
FF -> H
PK -> N
PC -> O
SP -> B
CB -> B
CH -> H
FN -> V
SV -> O
SC -> P
NP -> B
BB -> S
PV -> S
VB -> P
SF -> H
VC -> O
HN -> V
BF -> O
NO -> O
HP -> N
VV -> K
HS -> P
FH -> N
KB -> F
KF -> B
PN -> K
KH -> K
CN -> S
PP -> O
BP -> O
OH -> B
FS -> O
BK -> B
PO -> V
CS -> C
BV -> N
KP -> O
KN -> B
VK -> F
HC -> O
BH -> B
FP -> H
NK -> V
BS -> C
FV -> F
PS -> P")

(defun split-string (string &key (delimiter (string #\space)) (max -1))
    (let ((pos (search delimiter string)))
        (if (or (= max 0) (eq nil pos))
            (list string)
            (cons
                (subseq string 0 pos)
                (split-string (subseq string (+ pos (length delimiter)))
                    :delimiter delimiter
                    :max (if (= max -1) -1 (- max 1)))))))

(setf the-dictionary-list 
    (mapcar 
        (lambda (a)
            (split-string a :delimiter #(#\space #\- #\> #\space)))
        (split-string the-dictionary-string :delimiter #(#\newline))))

(setf dictionary-map (make-hash-table :test #'equal))

(setf the-input-map 
    (let ((count-map (make-hash-table :test #'equal)))
        (loop for i from 0 below (1- (length the-input-string)) do
            (let* ( (char-a (char the-input-string i))
                    (char-b (char the-input-string (1+ i)))
                    (char-string (format nil "~c~c" char-a char-b))
                    (the-count (gethash char-string count-map)))
            (if the-count
                (setf (gethash char-string count-map) (1+ the-count))
                (setf (gethash char-string count-map) 1))))
        count-map))

(setf the-output-array (make-array 100))

(setf (aref the-output-array 0) the-input-map)

(defun draw-the-map ()
    (mapcar 
        (lambda (a) 
            (setf (gethash (car a) dictionary-map) (char (cadr a) 0)))
        the-dictionary-list))

(draw-the-map)

(defun add-once (the-number)
    (let* ( (map-number (1- the-number))
            (input-map (aref the-output-array map-number))
            (output-map (make-hash-table :test #'equal)))
        (labels (   (set-table (the-string the-value)
                        (let ((map-value (gethash the-string output-map)))
                            (if map-value
                                (setf (gethash the-string output-map) (+ map-value the-value))
                                (setf (gethash the-string output-map) the-value))))
                    (add-char (the-string the-count)
                        (let* ( (next-char (gethash the-string dictionary-map))
                                (first-char (char the-string 0))
                                (second-char (char the-string 1))
                                (first-string (format nil "~c~c" first-char next-char))
                                (second-string (format nil "~c~c" next-char second-char)))
                            (progn  (set-table first-string the-count)
                                    (set-table second-string the-count)
                                    nil))))
            (maphash
                (lambda (k v)
                    (add-char k v))
                input-map))
            (setf (aref the-output-array the-number) output-map)))

(defun add-for (the-number)
    (loop for i from 0 to the-number do
        (if (hash-table-p (aref the-output-array i))
            nil
            (add-once i))))

(defun map-count (the-map)
    (let (  (new-map (make-hash-table))
            (first-string nil)
            (count-list nil))
        (labels (   (set-table (the-char the-value)
                        (let ((map-value (gethash the-char new-map)))
                            (if map-value
                                (setf (gethash the-char new-map) (+ map-value the-value))
                                (setf (gethash the-char new-map) the-value))))
                    (second-char (the-string the-value)
                        (set-table (char the-string 1) the-value)))
        (maphash
            (lambda (k v)
                (when   (null first-string) 
                        (setf first-string k))
                (second-char k v))
            the-map)
        (progn  (set-table (char first-string 0) 1)
                (set-table (char first-string 1) 1)))
        (maphash 
            (lambda (k v)
                (push v count-list))
            new-map)
        (print (- (apply #'max count-list) (apply #'min count-list)))))

(defun add-times (the-times)
    (add-for the-times)
    (map-count (aref the-output-array the-times)))

;;;part-1
(time (add-times 10))  ;;3555
;;1,051,312 processor cycles

;;;part-2
(time (add-times 40))  ;;4439442043739
;;3,506,284 processor cycles
