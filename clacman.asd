;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clacman.asd
;;;; Purpose:       ASDF definition for Clacman
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of clacman, is Copyright (c) 2007, 2015 by Nicolas Lamirault
;;;;
;;;; clacman users are granted the rights to distribute and use this software
;;;; as governed by the terms of the MIT License :
;;;; http://www.opensource.org/licenses/mit-license.php
;;;;
;;;; *************************************************************************

(in-package :cl-user)
(defpackage clacman-asd
  (:use :cl :asdf))
(in-package :clacman-asd)

(defsystem clacman
  :version "0.5.0"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :license "MIT License"
  :depends-on (:pal)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "tools" :depends-on ("package"))
                 (:file "specials" :depends-on ("tools"))
                 (:file "labyrinthes" :depends-on ("specials"))
                 (:file "clacman" :depends-on ("specials" "labyrinthes")))))
  :description "A pacman game."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clacman-test))))
