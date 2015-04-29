;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Some specials variables of the Clacman game
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


(defparameter *version* (asdf:component-version (asdf:find-system "clacman")))

(defparameter *debug* nil "When T, active some logs for debugging.")

(defparameter *clacman-directory*
  (namestring (asdf:component-relative-pathname (asdf:find-system :clacman)))
  "Directory with contains CLACMAN source files.")

;; screen

(defparameter *speed* 10)

(define-constant +width+ 640 "The default width.")

(define-constant +height+ 480 "The default height.")

(define-constant +matrix-width+ 21)

(define-constant +matrix-height+ 16)

(define-constant +game-left-corner-x+ 200)

(define-constant +game-left-corner-y+ 50)

(define-constant +matrix-background+ '(50 50 50))

;; multimedia files

(define-constant +levels-directory+ "/var/")

(define-constant +background+ "img/background.png")

(define-constant +pacman+ "img/pacman.png")

(define-constant +blinky+ "img/Ghost_Blinky.png")

(define-constant +clyde+ "img/Ghost_Clyde.png")

(define-constant +inky+ "img/Ghost_Inky.png")

(define-constant +pinky+ "img/Ghost_Pinky.png")

