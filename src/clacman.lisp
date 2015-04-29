;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          clacman.lisp
;;;; Purpose:       A Pacman game.
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


(if *clacman-directory*
      (pal:define-tags background
                       (pal:load-image (concatenate 'string
                                                    *clacman-directory*
                                                    +background+)
                                       t)
                       pacman
                       (pal:load-image (concatenate 'string
                                                    *clacman-directory*
                                                    +pacman+)
                                       t)
                       ghost-inky
                       (pal:load-image (concatenate 'string
                                                    *clacman-directory*
                                                    +inky+)
                                       t)
                       ghost-pinky
                       (pal:load-image (concatenate 'string
                                                    *clacman-directory*
                                                    +pinky+)
                                        t)
                       ghost-blinky
                       (pal:load-image (concatenate 'string
                                                    *clacman-directory*
                                                    +blinky+)
                                        t)
                       ghost-clyde
                       (pal:load-image (concatenate 'string
                                                    *clacman-directory*
                                                    +clyde+)
                                       t))
      (error "CLACMAN directory isn't defined : *CLACMAN-DIRECTORY*"))


(defun original-coordonates (position)
  "Find the original coordonates on the game.
POSITION is a cons of coordonates (x . y)."
  (cons (+ +game-left-corner-x+ (* (car position) 20))
        (+ +game-left-corner-y+ (* (cdr position) 20))))


(defclass entity-base ()
  ((image :accessor entity-base-image :initarg :image)))


(defclass phantom-background (entity-base)
  ()
  (:default-initargs :image (pal:tag 'background)))


(defgeneric draw (entity-base x y)
  (:documentation "Draw a Pacman entity."))


(defmethod draw ((entity-base entity-base) x y)
  (pal:draw-image (entity-base-image entity-base) (pal:v x y)))


(defun draw-square (x y r g b)
  "Draw a square from (X,Y) with color defined by R G B codes."
  (pal:draw-polygon (list (pal:v x y)
                          (pal:v (+ x 18) y)
                          (pal:v (+ x 18) (+ y 18))
                          (pal:v x (+ y 18)))
                    r g b                                  
                    255))


(defun draw-ball (x y r g b)
  "Draw a ball."
  (pal:draw-circle (pal:v x y) 2 r g b 255))


(defclass entity ()
  ((position :initform nil
             :initarg :position
             :accessor entity-position)
   (image :initform nil
          :initarg :image
          :accessor entity-image))
  (:documentation "A game entity."))


(defun collision-p (matrix x y)
  "Check if (X,Y) is in the matrix. and if place is empty."
  (when *debug*
     (format t "~&Check ~Ax~A ~A"
             x y (aref matrix y x)))
  (or (<= x 0)
      ;;(>= x 20)
      (> x +matrix-width+)
      (<= y 0)
      ;;(>= y 15)
      (> y +matrix-height+)
      (= 0 (aref matrix y x))
      (= 1 (aref matrix y x))))


(defun get-points (matrix x y)
  "Check if in (X,Y) position there is a ball."
  (if (= 3 (aref matrix y x)) 1 0))


(defgeneric move (entity matrix new-x new-y)
  (:documentation "Change the location of ENTITY to (X,Y) on the matrix."))


(defmethod move ((entity entity) matrix new-x new-y)
  (with-slots (position x y) entity
    (unless (collision-p matrix new-x new-y)
      (setf (aref matrix (cdr position) (car position)) 4
            position (cons new-x new-y)
            (aref matrix new-y new-x) 1))))


(defclass phantom (entity)
  ((eaten :initform nil
          :initarg :eaten
          :accessor phantom-eaten))
  (:documentation "The phantom entity."))


(defclass pacman (entity)
  ((force :initform 0
          :initarg :force
          :accessor pacman-force)
   (invincible :initform nil
               :initarg :invicible
               :accessor pacman-invincible))
  (:documentation "The Pacman entity."))


(defclass game ()
  ((matrix :initform nil
           :initarg :matrix
           :accessor game-matrix)
   (phantoms :initform nil
             :initarg :phantoms
             :accessor game-phantoms)
   (pacman :initform nil
           :initarg :pacman
           :accessor game-pacman))
  (:documentation "A pacman game."))


(defgeneric draw-game (game level points &optional username)
  (:documentation "Draw the MATRIX game, LEVEL is for the user."))


(defmethod draw-game ((game game) level points &optional username)
  (pal:clear-screen 0 0 0)
  (with-slots (matrix phantoms pacman) game
    (loop ;;for y from 0 to 15 ;;29
       for y below +matrix-height+
       as y0 = (+ (* y 20) +game-left-corner-y+)
       do (loop ;; for x from 0 to 20 ;;28
             for x below +matrix-width+
             as x0 = (+ (* x 20) +game-left-corner-x+)
             do (let (r g b)
                  (cond ((= 0 (aref matrix y x))
                         (setf r 0
                               g 0
                               b 155))
;;                         ((= 3 (aref matrix y x))
;;                          (setf r 255
;;                                g 255
;;                                b 255))
                        (t (setf r (first +matrix-background+)
                                 g (second +matrix-background+)
                                 b (third +matrix-background+))))
                  (draw-square x0 y0 r g b)
                  (when (= 3 (aref matrix y x))
                    (draw-ball (+ 10 x0) (+ 10 y0) 255 255 255)))))
    (pal:draw-text (format nil "Level ~A" level) (pal:v 15 20))
    (pal:draw-text (format nil "Points ~A" points) (pal:v 15 105))
    (when username
      (pal:draw-text (format nil "~A" username) (pal:v 15 145)))
    (with-slots (image position) pacman
      (let ((coords (original-coordonates position)))
        (draw image (car coords) (cdr coords))))
    (loop for phantom in phantoms
       as image = (entity-image phantom)
       as pos = (entity-position phantom)
       as coords = (original-coordonates pos)
       do (draw image (car coords) (cdr coords)))))


(defun init-game ()
  "Creates a new game object."
  (let* ((rs (make-random-state t))
         (matrix (make-array (list +matrix-height+ +matrix-width+))) ;;30 29)))
         (pacman (make-instance 'pacman
                                :image (make-instance 'entity-base
                                                      :image (pal:tag 'pacman))
                                :force nil
                                :invicible nil
                                :position (cons 10 15)))
         (phantoms
          (loop for ghost in '(ghost-inky ghost-pinky ghost-blinky ghost-clyde)
             as x = (random 25 rs) then (1+ (random 25 rs))
             as y = (random 25 rs) then (1+ (random 25 rs))
             collect (make-instance 'phantom
                                    :image (make-instance 'entity-base
                                                          :image (pal:tag ghost))
                                    :position (cons x y)
                                    :eaten nil))))
    (loop for phantom in phantoms
       do (with-slots (position) phantom
            (setf (aref matrix (cdr position) (car position)) 1)))
    (with-slots (position) pacman
      (setf (aref matrix (cdr position) (car position)) 1))
    (make-instance 'game
                   :matrix matrix
                   :pacman pacman
                   :phantoms phantoms)))


(defun load-game (number)
  "Creates a new game object from an identified level."
  (multiple-value-bind (matrix pacman-pos balls-pos ghosts-pos)
      (load-level number)
    (declare (ignore balls-pos))
    (let ((pacman (make-instance 'pacman
                                 :image (make-instance 'entity-base
                                                       :image (pal:tag 'pacman))
                                 :force nil
                                 :invicible nil
                                 :position pacman-pos))
          (phantoms
           (loop for ghost in '(ghost-inky ghost-pinky ghost-blinky ghost-clyde)
              as i = 0 then (1+ i)
              collect (make-instance 'phantom
                                     :image (make-instance 'entity-base
                                                           :image (pal:tag ghost))
                                     :position (nth i ghosts-pos)
                                     :eaten nil))))
      (loop for phantom in phantoms
         do (with-slots (position) phantom
              (setf (aref matrix (cdr position) (car position)) 1)))
      (with-slots (position) pacman
        (setf (aref matrix (cdr position) (car position)) 1))
      (make-instance 'game
                     :matrix matrix
                     :pacman pacman
                     :phantoms phantoms))))


(defun clacman (&optional username)
  "Start a new Clacman game."
  (pal:with-pal (:width +width+ :height +height+
                 :fullscreenp nil :fps *speed*
                 :paths (concatenate 'string *clacman-directory* "font/"))
    (let ((game (load-game "0")) ;;(init-game))
          (level 0)
          (points 0)
          (state :ready))
      (with-slots (matrix phantoms pacman) game
        (pal:event-loop ()
          (case state
            
            (:ready
             (pal:test-keys            
               ;;(:key-a (display-about))
               (:key-s (setf state :playing))
               (:key-q (return-from pal:event-loop))))
          
            (:paused
             (pal:test-keys
               (:key-p (setf state :playing))
               (:key-q (return-from pal:event-loop))))
          
            (:playing
             (pal:test-keys
               (:key-p (setf state :paused))
               (:key-left (when *debug*
                            (format t "~&Left "))
                          (with-slots (position) pacman
                            (let ((new-x (1- (car position)))
                                  (new-y (cdr position)))
                              (setf points
                                    (+ points (get-points matrix new-x new-y)))
                              (move pacman matrix new-x new-y))))
               (:key-right (when *debug*
                             (format t "~&Right"))
                           (with-slots (position) pacman
                             (let ((new-x (1+ (car position)))
                                   (new-y (cdr position)))
                               (setf points
                                     (+ points (get-points matrix new-x new-y)))
                               (move pacman matrix new-x new-y))))
               (:key-up (when *debug*
                          (format t "~&Up"))
                        (with-slots (position) pacman
                          (let ((new-x (car position))
                                (new-y (1- (cdr position))))
                            (setf points
                                  (+ points (get-points matrix new-x new-y)))
                             (move pacman matrix new-x new-y))))
               (:key-down (when *debug*
                            (format t "~&Down"))
                          (with-slots (position) pacman
                            (let ((new-x (car position))
                                  (new-y (1+ (cdr position))))
                              (setf points
                                  (+ points (get-points matrix new-x new-y)))
                             (move pacman matrix new-x new-y))))
;;                (:key-space (when *debug*
;;                              (format t "~&Space"))
;;                            (loop for ghost in phantoms
;;                               as pos = (next-position matrix
;;                                                       (car (entity-position ghost))
;;                                                       (cdr (entity-position ghost))
;;                                                       (car (entity-position pacman))
;;                                                       (cdr (entity-position pacman)))
;;                               unless (or (null (first pos))
;;                                          (null (second pos)))
;;                               do (move ghost matrix (first pos) (second pos))))
               (:key-h (when *debug*
                         (format t "~&Help")))
               (:key-q (return-from pal:event-loop)))))

          (when (eq state :playing)
            (loop for ghost in phantoms
               as pos = (next-position matrix
                                       (car (entity-position ghost))
                                       (cdr (entity-position ghost))
                                       (car (entity-position pacman))
                                       (cdr (entity-position pacman)))
               unless (or (null (first pos))
                          (null (second pos)))
               do (move ghost matrix (first pos) (second pos))))
            
          (draw-game game level points username))))))