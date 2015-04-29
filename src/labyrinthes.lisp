;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          labyrinthes.lisp
;;;; Purpose:       The Pacman levels.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of clacman, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; clacman users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :clacman)


(defun show-matrix (matrix max-x max-y &optional (stream *standard-output*))
  "Print to STREAM the MATRIX."
  (when matrix
    (format stream "~&  ")
    (loop for y from 0 to (1- max-y)
       do (format t "~2,'0D" y))
    (loop for y from 0 to (1- max-y)
       do
         (format stream "~&~2,'0D" y)
         (loop for x from 0 to (1- max-x)
            do (if (listp (aref matrix y x))
                   (format stream "~2A" "1")
                   (format stream "~2A" "")))
         (format stream "~2,'0D" y))
    (format stream "~%")
    (format stream "  ")
    (loop for y from 0 to (1- max-y)
       do (format t "~2,'0D" y))
    (format stream "~%")))


(defun get-neighbours (matrix x y)
  "Search for empty neighbours."
  (when *debug*
    (format t "~&Search for neighbours ~Ax~A" y x))
  (loop for pos in '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))
     unless (= 0 (aref matrix (+ x (car pos)) (+ y (cdr pos))))
     collect (list (+ y (cdr pos)) (+ x (car pos)))))


(defun next-position (matrix from-x from-y to-x to-y)
  "Search for the next position to go from (FROM-X, FROM-Y) to (TO-X, TO-Y)."
  (let ((open '())
        (close '()))
    (push (list from-x from-y) close)
    (loop
       do (let ((neighbours (get-neighbours matrix from-y from-x)))
            (when *debug*
              (format t "~&Neighbours : ~A" neighbours))
            (loop for pos in neighbours
               unless (find pos close)
               do (push (list (first pos)
                              (second pos)
                              (+ (abs (- (first pos) to-x))
                                 (abs (- (second pos) to-y))))
                        open))
            (setf open (sort open #'< :key #'third))
            (when *debug*
              (format t "~&First ~A Open ~A~&Close : ~A"
                      (first open)
                      open close))
            (if (or (and (= (first (first open)) to-x)
                         (= (second (first open)) to-y))
                    (find (first open) close :test #'equal))
                (return (second (reverse close)))
                (setf from-x (first (first open))
                      from-y (second (first open))
                      close (append (list (first open)) close)
                      open (nthcdr 1 open)))))))



(defun load-level (number)
  "Load level from file.
Returns a new matrix for the game, pacman position, a list of the
balls' position and a list of the 4 ghosts' positions."
  (let ((filename (concatenate 'string
                               *clacman-directory*
                               +levels-directory+
                               number
                               ".dat"))
        (matrix (make-array (list +matrix-height+ +matrix-width+))) ;; 16 21)))
        (ghosts '())
        (balls '())
        pacman)
    (when *debug*
      (format t "~&File level ~A~%" filename))
    (with-open-file (is filename :direction :input)
        (loop for line = (read-line is nil nil)
           as y = 0 then (1+ y)
           until (null line)
           do
             (loop for x from 0 below (length line)
                for ch =  (char line x)
                do (cond ((char= #\1 ch) ;; pacman
                          (progn
                            (setf pacman (cons x y))
                            (setf (aref matrix y x) 1)))
                         ((char= #\2 ch) ;; ghost
                          (progn
                            (push (cons x y) ghosts)
                            (setf (aref matrix y x) 2)))
                         ((char= #\. ch) ;; balls
                          (progn
                            (push (cons x y) balls)
                            (setf (aref matrix y x) 3)))
                         ((char= #\# ch) ;; wall
                          (setf (aref matrix y x) 0))
                         (t ;; empty
                          (setf (aref matrix y x) 4))))))
    (values matrix pacman balls ghosts)))
