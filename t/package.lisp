;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package file for Clacman unit tests
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of clacman, is Copyright (c) 2007, 2015 by Nicolas Lamirault
;;;;
;;;; clacman users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

(defpackage :clacman-test
  (:use :cl :clacman :prove)
  (:documentation "Clacman unit tests package.")
  )
