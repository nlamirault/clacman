;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Some specials variables of the Clacman game
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of clacman, is Copyright (c) 2007, 2015 by Nicolas Lamirault
;;;;
;;;; clacman users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************


(in-package :clacman)


(defmacro define-constant (name value &optional doc)
  "Macro which defines constant if NAME is unbound."
  `(defconstant ,name
     (if (boundp ',name)
         (symbol-value ',name)
         ,value)
    ,@(when doc (list doc))))
