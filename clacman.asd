;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clacman.asd
;;;; Purpose:       ASDF definition for Clacman
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of clacman, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; clacman users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :asdf)



(defsystem clacman
  :name "clacman"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :version "0.2.O"
  :licence "Lisp Lesser GNU General Public License"
  :description "A pacman game."
  :depends-on (:pal)
  :components
  ((:module :src
            :components
            ((:file "package")
             (:file "tools" :depends-on ("package"))
             (:file "specials" :depends-on ("tools"))
             (:file "labyrinthes" :depends-on ("specials"))
             (:file "clacman" :depends-on ("specials" "labyrinthes"))))))
