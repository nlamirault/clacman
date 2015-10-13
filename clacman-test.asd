;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clacman-test.asd
;;;; Purpose:       ASDF definition for Clacman unit tests
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
(defpackage clacman-test-asd
  (:use :cl :asdf))
(in-package :clacman-test-asd)

(defsystem clacman-test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:clacman
               :prove)
  :components ((:module "t"
                :components
                ((:file "package")
                 (:test-file "clacman" :depends-on ("package")))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)
                    (asdf:clear-system c)))
