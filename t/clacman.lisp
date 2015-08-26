;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clacman.lisp
;;;; Purpose:       Clacman unit tests.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of clacman, is Copyright (c) 2007, 2015 by Nicolas Lamirault
;;;;
;;;; clacman users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :clacman-test)


(plan 12)


(multiple-value-bind (matrix pacman-pos balls-pos ghosts-pos)
    (clacman::load-level "0")
  (declare (ignore balls-pos))
  ;; game
  (is 16 (array-dimension matrix 0))
  (is 21 (array-dimension matrix 1))
  ;; pacman position
  (is 9 (car pacman-pos))
  (is 10 (cdr pacman-pos))
  ;; ghosts positions
  (is 10 (car (first ghosts-pos)))
  (is 5 (cdr (first ghosts-pos)))
  (is 9 (car (second ghosts-pos)))
  (is 5 (cdr (second ghosts-pos)))
  (is 8 (car (third ghosts-pos)))
  (is 5 (cdr (third ghosts-pos)))
  (is 7 (car (fourth ghosts-pos)))
  (is 5 (cdr (fourth ghosts-pos)))
  )


(finalize)
