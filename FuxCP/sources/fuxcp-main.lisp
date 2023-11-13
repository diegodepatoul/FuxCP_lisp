(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the functions that:
;   - dispatch to the right species functions
;   - set the global variables of the CSP
;   - manage the search for solutions

(print "Loading fux-cp...")

; get the value at key @k in the hash table @h as a list
(defun geth-dom (h k)
    (list (gethash k h))
)

; get the value at key @k in the parameters table as a list
(defun getparam-val (k)
    (geth-dom *params* k)
)

; get the value at key @k in the parameters table as a domain
(defun getparam-dom (k)
    (list 0 (getparam k))
)

; get the value at key @k in the parameters table
(defun getparam (k)
    (gethash k *params*)
)

; get if borrow-mode param is allowed
(defun is-borrow-allowed ()
    (not (equal (getparam 'borrow-mode) "None"))
)


; define all the constants that are going to be used
(defun define-global-constants ()
    ;; CONSTANTS
    ; Number of costs added
    (defparameter *n-cost-added 0)
    ; Motion types
    (defparameter DIRECT 2)
    (defparameter OBLIQUE 1)
    (defparameter CONTRARY 0)

    ; Integer constants (to represent costs or intervals)
    ; 0 in IntVar
    (defparameter ZERO (gil::add-int-var-dom *sp* (list 0)))
    ; 1 in IntVar
    (defparameter ONE (gil::add-int-var-dom *sp* (list 1)))
    ; 3 in IntVar (minor third)
    (defparameter THREE (gil::add-int-var-dom *sp* (list 3)))
    ; 9 in IntVar (major sixth)
    (defparameter NINE (gil::add-int-var-dom *sp* (list 9)))

    ; Boolean constants
    ; 0 in BoolVar
    (defparameter FALSE (gil::add-bool-var *sp* 0 0))
    ; 1 in BoolVar
    (defparameter TRUE (gil::add-bool-var *sp* 1 1))

    ; Intervals constants
    ; perfect consonances intervals
    (defparameter P_CONS (list 0 7))
    ; imperfect consonances intervals
    (defparameter IMP_CONS (list 3 4 8 9))
    ; all consonances intervals
    (defparameter ALL_CONS (union P_CONS IMP_CONS))
    ; dissonances intervals
    (defparameter DIS (list 1 2 5 6 10 11))
    ; penultimate intervals, i.e. minor third and major sixth
    (defparameter PENULT_CONS (list 3 9))
    ; penultimate thesis intervals, i.e. perfect fifth and sixth
    (defparameter PENULT_THESIS (list 7 8 9))
    ; penultimate 1st quarter note intervals, i.e. minor third, major sixth and octave/unisson
    (defparameter PENULT_1Q (list 0 3 8))
    ; penultimate syncope intervals, i.e. seconds and sevenths
    (defparameter PENULT_SYNCOPE (list 1 2 10 11))

    ; P_CONS in IntVar
    (defparameter P_CONS_VAR (gil::add-int-var-const-array *sp* P_CONS))
    ; IMP_CONS in IntVar
    (defparameter IMP_CONS_VAR (gil::add-int-var-const-array *sp* IMP_CONS))
    ; ALL_CONS in IntVar
    (defparameter ALL_CONS_VAR (gil::add-int-var-const-array *sp* ALL_CONS))
    ; PENULT_CONS in IntVar
    (defparameter PENULT_CONS_VAR (gil::add-int-var-const-array *sp* PENULT_CONS))
    ; PENULT_THESIS in IntVar
    (defparameter PENULT_THESIS_VAR (gil::add-int-var-const-array *sp* PENULT_THESIS))
    ; PENULT_1Q in IntVar
    (defparameter PENULT_1Q_VAR (gil::add-int-var-const-array *sp* PENULT_1Q))
    ; PENULT_SYNCOPE in IntVar
    (defparameter PENULT_SYNCOPE_VAR (gil::add-int-var-const-array *sp* PENULT_SYNCOPE))

    ; *cf-brut-intervals is the list of brut melodic intervals in the cantus firmus
    (setq *cf-brut-m-intervals (gil::add-int-var-array *sp* *cf-last-index -127 127))
    ; array representing the brut melodic intervals of the cantus firmus
    (create-cf-brut-m-intervals *cf *cf-brut-m-intervals)

    ;; COSTS
    ;; Melodic costs
    (defparameter *m-step-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-step-cost)))
    (defparameter *m-third-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-third-cost)))
    (defparameter *m-fourth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-fourth-cost)))
    (defparameter *m-tritone-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-tritone-cost)))
    (defparameter *m-fifth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-fifth-cost)))
    (defparameter *m-sixth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-sixth-cost)))
    (defparameter *m-seventh-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-seventh-cost)))
    (defparameter *m-octave-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-octave-cost)))
    ;; General costs
    (defparameter *borrow-cost* (gil::add-int-var-dom *sp* (getparam-val 'borrow-cost)))
    (defparameter *h-fifth-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-fifth-cost)))
    (defparameter *h-octave-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-octave-cost)))
    (defparameter *con-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'con-motion-cost)))
    (defparameter *obl-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'obl-motion-cost)))
    (defparameter *dir-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'dir-motion-cost)))
    ;; Species specific costs
    (defparameter *penult-sixth-cost* (gil::add-int-var-dom *sp* (getparam-val 'penult-sixth-cost)))
    (defparameter *non-cambiata-cost* (gil::add-int-var-dom *sp* (getparam-val 'non-cambiata-cost)))
    (defparameter *two-beats-apart-cost* (gil::add-int-var-dom *sp* (getparam-val 'two-beats-apart-cost)))
    (defparameter *two-bars-apart-cost* (gil::add-int-var-dom *sp* (getparam-val 'two-bars-apart-cost)))
    (defparameter *no-syncopation-cost* (gil::add-int-var-dom *sp* (getparam-val 'no-syncopation-cost)))

    ;; Params domains
    (defparameter *motions-domain*
        (remove-duplicates (mapcar (lambda (x) (getparam x))
            (list 'con-motion-cost 'obl-motion-cost 'dir-motion-cost)
        ))
    )
)

(defclass counterpoint () (
    ; voice variables
    (cp-range :accessor cp-range :initarg :cp-range :initform nil)
    (cp-domain :accessor cp-domain :initarg :cp-domain :initform nil)
    (chromatic-cp-domain :accessor chromatic-cp-domain :initarg :chromatic-cp-domain :initform nil)
    (extended-cp-domain :accessor extended-cp-domain :initarg :extended-cp-domain :initform nil)
    (off-domain :accessor off-domain :initarg :off-domain :initform nil)

    ; 1st species variables
    (cp :accessor cp :initarg :cp :initform (list nil nil nil nil)) ; represents the notes of the counterpoint
    (h-intervals :accessor h-intervals :initarg :h-intervals :initform (list nil nil nil nil))
    (m-intervals-brut :accessor m-intervals-brut :initarg :m-intervals-brut :initform (list nil nil nil nil))
    (m-intervals :accessor m-intervals :initarg :m-intervals :initform (list nil nil nil nil))
    (motions :accessor motions :initarg :motions :initform (list nil nil nil nil))
    (motions-cost :accessor motions-cost :initarg :motions-cost :initform (list nil nil nil nil))
    (is-cf-bass-arr :accessor is-cf-bass-arr :initarg :is-cf-bass-arr :initform (list nil nil nil nil))
    (m2-intervals-brut :accessor m2-intervals-brut :initarg :m2-intervals-brut :initform nil)
    (m2-intervals :accessor m2-intervals :initarg :m2-intervals :initform nil)
    (cf-brut-m-intervals :accessor cf-brut-m-intervals :initarg :cf-brut-m-intervals :initform nil)
    (is-p-cons-arr :accessor is-p-cons-arr :initarg :is-p-cons-arr :initform nil)
    (is-cp-off-key-arr :accessor is-cp-off-key-arr :initarg :is-cp-off-key-arr :initform nil)
    (p-cons-cost :accessor p-cons-cost :initarg :p-cons-cost :initform nil)
    (fifth-cost :accessor fifth-cost :initarg :fifth-cost :initform nil)
    (octave-cost :accessor octave-cost :initarg :octave-cost :initform nil)
    (m-degrees-cost :accessor m-degrees-cost :initarg :m-degrees-cost :initform nil)
    (m-degrees-type :accessor m-degrees-type :initarg :m-degrees-type :initform nil)
    (off-key-cost :accessor off-key-cost :initarg :off-key-cost :initform nil)
    (m-all-intervals :accessor m-all-intervals :initarg :m-all-intervals :initform nil)

    ; 2nd species variables
    (h-intervals-abs :accessor h-intervals-abs :initarg :h-intervals-abs :initform (list nil nil nil nil))
    (h-intervals-brut :accessor h-intervals-brut :initarg :h-intervals-brut :initform (list nil nil nil nil))
    (m-succ-intervals :accessor m-succ-intervals :initarg :m-succ-intervals :initform (list nil nil nil nil)) 
    (m-succ-intervals-brut :accessor m-succ-intervals-brut :initarg :m-succ-intervals-brut :initform (list nil nil nil nil)) 
    (m2-len :accessor m2-len :initarg :m2-len :initform nil)
    (total-m-len :accessor total-m-len :initarg :total-m-len :initform nil)
    (m-all-intervals-brut :accessor m-all-intervals-brut :initarg :m-all-intervals-brut :initform nil)
    (real-motions :accessor real-motions :initarg :real-motions :initform nil)
    (real-motions-cost :accessor real-motions-cost :initarg :real-motions-cost :initform nil)
    (is-ta-dim-arr :accessor is-ta-dim-arr :initarg :is-ta-dim-arr :initform nil)
    (is-nbour-arr :accessor is-nbour-arr :initarg :is-nbour-arr :initform nil)
    (penult-thesis-cost :accessor penult-thesis-cost :initarg :penult-thesis-cost :initform nil) 

    ; 3rd species variables
    (is-5qn-linked-arr :accessor is-5qn-linked-arr :initarg :is-5qn-linked-arr :initform nil)
    (is-not-cambiata-arr :accessor is-not-cambiata-arr :initarg :is-not-cambiata-arr :initform nil)
    (not-cambiata-cost :accessor not-cambiata-cost :initarg :not-cambiata-cost :initform nil)
    (m2-eq-zero-cost :accessor m2-eq-zero-cost :initarg :m2-eq-zero-cost :initform nil)
    (is-cons-arr :accessor is-cons-arr :initarg :is-cons-arr :initform (list nil nil nil nil))
    (cons-cost :accessor cons-cost :initarg :cons-cost :initform (list nil nil nil nil))

    ; 4th species variables
    (is-no-syncope-arr :accessor is-no-syncope-arr :initarg :is-no-syncope-arr :initform nil)
    (no-syncope-cost :accessor no-syncope-cost :initarg :no-syncope-cost :initform nil)

    ; 6st species variables
    (direct-move-to-p-cons-cost :accessor direct-move-to-p-cons-cost :initarg :direct-move-to-p-cons-cost :initform (list nil nil nil nil))
    (variety-cost :accessor variety-cost :initarg :variety-cost :initform nil)
))

(defun init-counterpoint (voice-type)
    ; Lower bound and upper bound related to the cantus firmus pitch
    (let (
        (range-upper-bound (+ 12 (* 6 voice-type)))
        (range-lower-bound (+ -6 (* 6 voice-type)))
        )
        (let (
            ; set the pitch range of the counterpoint
            (cp-range (range (+ *tone-pitch-cf range-upper-bound) :min (+ *tone-pitch-cf range-lower-bound))) ; arbitrary range
            )
            (let (
            ; set counterpoint pitch domain
            (cp-domain (intersection cp-range *scale))
            ; penultimate (first *cp) note domain
            (chromatic-cp-domain (intersection cp-range *chromatic-scale))
            ; set counterpoint extended pitch domain
            (extended-cp-domain (intersection cp-range (union *scale *borrowed-scale)))
            ; set the domain of the only barrowed notes
            (off-domain (intersection cp-range *off-scale))
            )
            (make-instance 'counterpoint :cp-range cp-range
                                         :cp-domain cp-domain
                                         :chromatic-cp-domain chromatic-cp-domain
                                         :extended-cp-domain extended-cp-domain
                                         :off-domain off-domain)
            )
        )
    )
)

; re/define all the variables the CSP needs
(defun get-counterpoint (species) (case species 

    (5 (progn
        ;; FIFTH SPECIES COUNTERPOINT GLOBAL VARIABLES
        (defvar *species-arr) ; 0: no constraint, 1: first species, 2: second species, 3: third species, 4: fourth species
        (defvar *sp-arr) ; represents *species-arr by position in the measure
        (defparameter *is-nth-species-arr (list nil nil nil nil nil)) ; if *species-arr is n, then *is-nth-species-arr is true
        (defparameter *is-3rd-species-arr (list nil nil nil nil)) ; if *species-arr is 3, then *is-3rd-species-arr is true
        (defparameter *is-4th-species-arr (list nil nil nil nil)) ; if *species-arr is 4, then *is-4th-species-arr is true
        (defvar *is-2nd-or-3rd-species-arr) ; if *species-arr is 2 or 3, then *is-2nd-or-3rd-species-arr is true
        (defvar *m-ta-intervals) ; represents the m-intervals between the thesis note and the arsis note of the same measure
        (defvar *m-ta-intervals-brut) ; same but without the absolute reduction
        (defvar *is-mostly-3rd-arr) ; true if second, third and fourth notes are from the 3rd species
        (defvar *is-constrained-arr) ; represents !(*is-0th-species-arr) i.e. there are species constraints
        (defparameter *is-cst-arr (list nil nil nil nil)) ; represents *is-constrained-arr for all beats of the measure

        (defparameter *m-succ-intervals-brut (list nil nil nil))
        (defparameter *m-succ-intervals (list nil nil nil))
        (defparameter *is-cons-arr (list nil nil nil nil))
        (defparameter *cons-cost (list nil nil nil nil))
    ))
))



;; DISPATCHER FUNCTION
(defun fux-cp (species-list)
    "Dispatches the counterpoint generation to the appropriate function according to the species."
    ; THE CSP SPACE 
    (defparameter *sp* (gil::new-space))
    (defparameter *sp* (gil::new-space))
    (setf *is-first-run 1) ; 1 if we are computing the first counterpoint, 0 if it is the second

    ; re/set global variables
    (define-global-constants)
    ;(set-space-variables species)
    
    (print (list "Choosing species: " species-list))
    (setq counterpoint-1 (init-counterpoint (first *voices-types)))
    (setq counterpoint-2 (init-counterpoint (second *voices-types)))
    (setf species (first species-list))
    (case species ; [1, 2, 3, 4, 5, 6, 7]
        (1 (progn
            (setq *N-COST-FACTORS 5)
            (fux-cp-1st counterpoint-1)
        ))
        (2 (progn
            (setq *N-COST-FACTORS 6)
            (fux-cp-2nd counterpoint-1)
        ))
        (3 (progn
            (setq *N-COST-FACTORS 7)
            (fux-cp-3rd counterpoint-1)
        ))
        (4 (progn
            (setq *N-COST-FACTORS 6)
            (fux-cp-4th counterpoint-1)
        ))
        (5 (progn
            (setq *N-COST-FACTORS 8)
            (fux-cp-5th)
        ))
        (6 (progn
            (setq *N-COST-FACTORS 15)
            (fux-cp-6th counterpoint-1 counterpoint-2)
        ))
        (7 (progn
            (setq *N-COST-FACTORS 15) 
            (fux-cp-7th counterpoint-1 counterpoint-2)
        ))
        (8 (progn
            (setq *N-COST-FACTORS 15)
            (fux-cp-8th counterpoint-1 counterpoint-2)
        ))
        (otherwise (error "Species ~A not implemented" species))
    )
    
    ;(print toreturn)
    ;toreturn
)

(defun fux-search-engine (the-cp &optional (species '(1)) (voice-type 0))
    (let (se tstop sopts)
        ; TOTAL COST
        (print (list "Starting fux-search-engine with species = " species))
        ;(gil::g-sum *sp* *total-cost *cost-factors) ; sum of all the cost factors
        (gil::g-cost *sp* *cost-factors) ; set the cost function
        ;(gil::g-cost *sp* *total-cost) ; set the cost function

        ;; SPECIFY SOLUTION VARIABLES
        (print "Specifying solution variables...")
        (gil::g-specify-sol-variables *sp* the-cp)
        (gil::g-specify-percent-diff *sp* 0)
        
        ;; BRANCHING
        (print "Branching...")
        (setq var-branch-type gil::INT_VAR_DEGREE_MAX)
        ;(setq var-branch-type gil::INT_VAR_SIZE_MIN)
        (setq val-branch-type gil::INT_VAL_SPLIT_MIN)

        ; 5th species specific
        (if (member 5 species) ; otherwise there is no species array
            (gil::g-branch *sp* *species-arr var-branch-type gil::INT_VAL_RND)
        )

        ; 3rd and 5th species specific
        (if (or (member 3 species) (member 5 species)) (progn
            (gil::g-branch *sp* (m-degrees-cost counterpoint-1) var-branch-type val-branch-type)
            (gil::g-branch *sp* (off-key-cost counterpoint-1) var-branch-type val-branch-type)
        ))

        ; 5th species specific
        (if (and (member 5 species) (>= voice-type 0)) ; otherwise there is no species array
        (progn
            (gil::g-branch *sp* *no-syncope-cost var-branch-type val-branch-type)
            (gil::g-branch *sp* *not-cambiata-cost var-branch-type val-branch-type)
        )
        )

        ; branching *total-cost
        ;(gil::g-branch *sp* *total-cost var-branch-type val-branch-type)
        (if (member 2 species)
            (gil::g-branch *sp* *cost-factors var-branch-type val-branch-type)
        )
    
        ;; Solution variables branching
        (gil::g-branch *sp* the-cp var-branch-type val-branch-type)

        ; time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (setq timeout 5)
        (gil::time-stop-init tstop (* timeout 1000)); initialize it (time is expressed in ms)

        ; search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        ; (gil::set-n-threads sopts 1)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ;; SEARCH ENGINE
        (print "Search engine...")
        (setq se (gil::search-engine *sp* (gil::opts sopts) gil::BAB));
        (print se)

        (print "CSP constructed")
        (list se the-cp tstop sopts)
    )
)



; SEARCH-NEXT-SOLUTION
; <l> is a list containing in that order the search engine for the problem, the variables
; this function finds the next solution of the CSP using the search engine given as an argument
(defun search-next-fux-cp (l)
    (print "Searching next solution...")
    (let (
        (se (first l))
        (the-cp (second l))
        (tstop (third l))
        (sopts (fourth l))
        (species-list (fifth l))
        (check t)
        sol sol-pitches sol-species
        )

        (time (om::while check :do
            ; reset the tstop timer before launching the search
            (gil::time-stop-reset tstop)
            ; try to find a solution
            (time (setq sol (try-find-solution se)))
            (if (null sol)
                ; then check if there are solutions left and if the user wishes to continue searching
                (stopped-or-ended (gil::stopped se) (getparam 'is-stopped))
                ; else we have found a solution so break the loop
                (setf check nil)
            )
        ))

        ; print the solution from GiL
        (print "Solution: ")

        #|
        (case species
            (1 (progn
                (print "PRINT 1st species")
                (print (list "(first *m-intervals-brut)" (gil::g-values sol (first *m-intervals-brut))))
                (print (list "*cf-brut-m-intervals     " (gil::g-values sol *cf-brut-m-intervals)))
                (print (list "(first *motions)       " (gil::g-values sol (first *motions))))
                (print (list "(first *h-intervals)     " (gil::g-values sol (first *h-intervals))))
            ))
            (2 (progn
                (print "PRINT 2nd species")
                (print (list "(first *cp)         " (gil::g-values sol (first *cp))))
                (print (list "(third *cp)         " (gil::g-values sol (third *cp))))
                (print (list "(third *h-intervals)" (gil::g-values sol (third *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                (print (list "*real-motions" (gil::g-values sol *real-motions)))
                (print (list "*penult-thesis-cost" (gil::g-values sol *penult-thesis-cost)))
            ))
            (3 (progn
                (print "PRINT 3rd species")
                (print (list "(first *cp) " (gil::g-values sol (first *cp))))
                (print (list "(second *cp)" (gil::g-values sol (second *cp))))
                (print (list "(third *cp) " (gil::g-values sol (third *cp))))
                (print (list "(fourth *cp)" (gil::g-values sol (fourth *cp))))
                (print (list "*extended-cp-domain" *extended-cp-domain))
                (print (list "(first *h-intervals) " (gil::g-values sol (first *h-intervals))))
                (print (list "(second *h-intervals)" (gil::g-values sol (second *h-intervals))))
                (print (list "(third *h-intervals) " (gil::g-values sol (third *h-intervals))))
                (print (list "(fourth *h-intervals)" (gil::g-values sol (fourth *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                ; (print (list "(fourth *m-intervals-brut)" (gil::g-values sol (fourth *m-intervals-brut))))
                ; (print (list "(first *motions) " (gil::g-values sol (first *motions))))
                (print (list "(fourth *motions)" (gil::g-values sol (fourth *motions))))
                (print (list "*not-cambiata-cost " (gil::g-values sol *not-cambiata-cost)))
                (print (list "*m2-eq-zero-cost   " (gil::g-values sol *m2-eq-zero-cost)))
                ; (print (list "(first *cons-cost)  " (gil::g-values sol (first *cons-cost))))
                ; (print (list "(second *cons-cost) " (gil::g-values sol (second *cons-cost))))
                ; (print (list "(third *cons-cost)  " (gil::g-values sol (third *cons-cost))))
                ; (print (list "(fourth *cons-cost) " (gil::g-values sol (fourth *cons-cost))))
            ))
            (4 (progn
                (print "PRINT 4th species")
                (print (list "(first *cp)         " (gil::g-values sol (first *cp))))
                (print (list "(third *cp)         " (gil::g-values sol (third *cp))))
                (print (list "(first *h-intervals)" (gil::g-values sol (first *h-intervals))))
                (print (list "(third *h-intervals)" (gil::g-values sol (third *h-intervals))))
                (print (list "*m-all-intervals         " (gil::g-values sol *m-all-intervals)))
                (print (list "(third *m-intervals)     " (gil::g-values sol (third *m-intervals))))
                (print (list "(first *m-succ-intervals) " (gil::g-values sol (first *m-succ-intervals))))
                (print (list "*no-syncope-cost" (gil::g-values sol *no-syncope-cost)))
            ))
            (5 (progn
                (print "PRINT 5th species")
                (print (list "(first *cp) " (gil::g-values sol (first *cp))))
                (print (list "(second *cp)" (gil::g-values sol (second *cp))))
                (print (list "(third *cp) " (gil::g-values sol (third *cp))))
                (print (list "(fourth *cp)" (gil::g-values sol (fourth *cp))))
                (print (list "(first *h-intervals) " (gil::g-values sol (first *h-intervals))))
                (print (list "(second *h-intervals)" (gil::g-values sol (second *h-intervals))))
                (print (list "(third *h-intervals) " (gil::g-values sol (third *h-intervals))))
                (print (list "(fourth *h-intervals)" (gil::g-values sol (fourth *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                ; (print (list "(fourth *m-intervals-brut)" (gil::g-values sol (fourth *m-intervals-brut))))
                ; (print (list "(first *motions) " (gil::g-values sol (first *motions))))
                (print (list "(fourth *motions)" (gil::g-values sol (fourth *motions))))
                (print (list "*not-cambiata-cost " (gil::g-values sol *not-cambiata-cost)))
                (print (list "*m2-eq-zero-cost   " (gil::g-values sol *m2-eq-zero-cost)))
                ; (print (list "(first *cons-cost)  " (gil::g-values sol (first *cons-cost))))
                (print (list "(second *cons-cost) " (gil::g-values sol (second *cons-cost))))
                (print (list "(third *cons-cost)  " (gil::g-values sol (third *cons-cost))))
                (print (list "(fourth *cons-cost) " (gil::g-values sol (fourth *cons-cost))))
                (print (list "*species-arr" sol-species))
                (print (list "*sp-arr1" (gil::g-values sol (first *sp-arr))))
                (print (list "*sp-arr2" (gil::g-values sol (second *sp-arr))))
                (print (list "*sp-arr3" (gil::g-values sol (third *sp-arr))))
                (print (list "*sp-arr4" (gil::g-values sol (fourth *sp-arr))))
            ))
            (6 (progn
                (print "PRINT 6th species")
                (print (list "(first *h-intervals)      " (gil::g-values sol (first *h-intervals))))
                (print (list "(first *h-intervals)2     " (gil::g-values sol (first *h-intervals2))))
                (print (list "(first *m-intervals-brut) " (gil::g-values sol (first *m-intervals-brut))))
                (print (list "(first *m-intervals-brut2)" (gil::g-values sol (first *m-intervals-brut2))))
                (print (list "*cf-brut-m-intervals      " (gil::g-values sol *cf-brut-m-intervals)))
                (print (list "*cf-brut-m-intervals2     " (gil::g-values sol *cf-brut-m-intervals2)))
                (print (list "(first *motions)          " (gil::g-values sol (first *motions))))
                (print (list "(first *motions)2         " (gil::g-values sol (first *motions2))))
                (print (list "(first *motions-costs)    " (gil::g-values sol (first *motions-cost))))
                (print (list "(first *motions-costs2)   " (gil::g-values sol (first *motions-cost2))))
                (print (list "*m-degrees-cost    " (gil::g-values sol *m-degrees-cost)))
                (print (list "*m-degrees-type    " (gil::g-values sol *m-degrees-type)))
                (print (list "(first *direct-move-to-p-cons-cost) " (gil::g-values sol (first *direct-move-to-p-cons-cost))))
                (print (list "(first *direct-move-to-p-cons-cost2)" (gil::g-values sol (first *direct-move-to-p-cons-cost2))))
                ;(print (list "*p-chords-cost            " (gil::g-values sol *p-chords-cost)))
                ;(print (list "*variety-cost           " (gil::g-values sol *variety-cost)))
                ;(print (list "*variety-cost2          " (gil::g-values sol *variety-cost2)))
            ))
            (7 (progn
                (print "PRINT 6th species")
                (print (list "(first *h-intervals)      " (gil::g-values sol (first *h-intervals))))
                (print (list "(first *h-intervals)2     " (gil::g-values sol (first *h-intervals2))))
                (print (list "(first *m-intervals-brut) " (gil::g-values sol (first *m-intervals-brut))))
                (print (list "(first *m-intervals-brut2)" (gil::g-values sol (first *m-intervals-brut2))))
                (print (list "*cf-brut-m-intervals      " (gil::g-values sol *cf-brut-m-intervals)))
                (print (list "*cf-brut-m-intervals2     " (gil::g-values sol *cf-brut-m-intervals2)))
                (print (list "(first *motions)          " (gil::g-values sol (first *motions))))
                (print (list "(first *motions)2         " (gil::g-values sol (first *motions2))))
                (print (list "(first *motions-costs)    " (gil::g-values sol (first *motions-cost))))
                (print (list "(first *motions-costs2)   " (gil::g-values sol (first *motions-cost2))))
                ;(print (list "(first *direct-move-to-p-cons-cost) " (gil::g-values sol (first *direct-move-to-p-cons-cost))))
                ;(print (list "(first *direct-move-to-p-cons-cost2)" (gil::g-values sol (first *direct-move-to-p-cons-cost2))))
                ;(print (list "*p-chords-cost            " (gil::g-values sol *p-chords-cost)))
                ;(print (list "*variety-cost           " (gil::g-values sol *variety-cost)))
                ;(print (list "*variety-cost2          " (gil::g-values sol *variety-cost2)))
            ))
        )
        (if (< species 6) 
            (progn
            (print (list "(first *motions)          " (gil::g-values sol (first *motions))))
            (print (list "(first *motions-costs)    " (gil::g-values sol (first *motions-cost))))
            (print (list "*m-degrees-cost    " (gil::g-values sol *m-degrees-cost)))
            (print (list "*m-degrees-type    " (gil::g-values sol *m-degrees-type)))
            (print (list "*off-key-cost     " (gil::g-values sol *off-key-cost)))
            (print (list "*fifth-cost  " (gil::g-values sol *fifth-cost)))
            (print (list "*octave-cost " (gil::g-values sol *octave-cost)))
            (print (list "*cost-factors" (gil::g-values sol *cost-factors)))
            (print (list "### COST ### " (gil::g-values sol *total-cost)))
            (print (list "scale         " *scale))
            (print (list "borrowed-scale" *borrowed-scale))
            (print (list "off-scale     " (reverse *off-scale))) 
        ) (progn
            (print (list "*extended-cp-domain " *extended-cp-domain))
            #|(print (list "*m-degrees-cost     " (gil::g-values sol *m-degrees-cost)))
            (print (list "*m-degrees-cost2    " (gil::g-values sol *m-degrees-cost2)))
            (print (list "*m-degrees-type     " (gil::g-values sol *m-degrees-type)))
            (print (list "*m-degrees-type2    " (gil::g-values sol *m-degrees-type2)))
            (print (list "*off-key-cost      " (gil::g-values sol *off-key-cost)))
            (print (list "*off-key-cost2     " (gil::g-values sol *off-key-cost2)))
            (print (list "*fifth-cost   " (gil::g-values sol *fifth-cost)))
            (print (list "*fifth-cost2  " (gil::g-values sol *fifth-cost2)))
            (print (list "*octave-cost  " (gil::g-values sol *octave-cost)))
            (print (list "*octave-cost2 " (gil::g-values sol *octave-cost2)))|#
            (print (list "*cost-factors " (gil::g-values sol *cost-factors)))
            ;(print (list "*cost-factors2" (gil::g-values sol *cost-factors2)))
            (print (list "### COST ### " (gil::g-values sol *total-cost)))
            (print (list "scale         " *scale))
            (print (list "borrowed-scale" *borrowed-scale))
            (print (list "off-scale     " (reverse *off-scale))) 
        )
        )|#

        
        (print (list "*cost-factors" (gil::g-values sol *cost-factors)))
        (print (list "current-cost = " (reduce #'+ (gil::g-values sol *cost-factors) :initial-value 0)))
        (print species)
        (setq sol-pitches (gil::g-values sol the-cp)) ; store the values of the solution
        (if (eq (length species-list) 1) (progn
            (case (first species-list)
                (4 (progn
                    (setq rythmic+pitches (get-basic-rythmic 4 *cf-len sol-pitches)) ; get the rythmic correpsonding to the species
                    (setq rythmic-om (first rythmic+pitches))
                    (setq pitches-om (second rythmic+pitches))
                ))
                (5 (progn
                    (setq sol-species (gil::g-values sol *species-arr)) ; store the values of the solution
                    (setq rythmic+pitches (parse-species-to-om-rythmic sol-species sol-pitches))
                    (setq rythmic-om (first rythmic+pitches))
                    ; (print (list "rythmic-om" rythmic-om))
                    (setq pitches-om (second rythmic+pitches))
                    ; (print (list "pitches-om" pitches-om))
                    (setq check (checksum-sol pitches-om rythmic-om))
                    ; (print (list "check" check))
                    (if (not (null *prev-sol-check))
                        ; then compare the pitches of the previous solution with the current one
                        ; if they are the same launch a new search
                        (if (member check *prev-sol-check)
                            (progn
                                (search-next-fux-cp l)
                            )
                            (progn
                                (print *prev-sol-check)
                                (setq *prev-sol-check (append *prev-sol-check (list check)))
                            )
                        )
                        ; else register the pitches of the current solution
                        (progn
                            (setq *prev-sol-check (list check))
                        )
                    )
                ))
                (otherwise (progn
                    (setq rythmic-om (get-basic-rythmic species *cf-len sol-pitches)) ; get the rythmic correpsonding to the species
                    (setq pitches-om sol-pitches)
                ))
            )
            (print (list "pitches-om = " pitches-om))
            (print rythmic-om)
            (print *cf-tempo)
            (if (< species 6) ; for species 1 to 5, create only 1 additional voice, else create 2 voices
                (make-instance 'voice :chords (to-midicent pitches-om) :tree (om::mktree rythmic-om '(4 4)) :tempo *cf-tempo)
                (progn
                    (print (list "Species = " species))
                    (case species
                        (6 (progn 
                            (setf first-cp (subseq pitches-om 0 *cf-len))
                            (setf second-cp (subseq pitches-om *cf-len))
                        ))
                        (7 (progn
                            (setf first-cp (subseq pitches-om 0 *cf-len))
                            (setf second-cp (subseq pitches-om *cf-len))
                        ))
                        (8 (progn
                            (setf first-cp (subseq pitches-om 0 *cf-len))
                            (setf second-cp (subseq pitches-om *cf-len))
                        ))
                    )
                    (make-instance 'poly 
                        :voices (
                            list
                            (make-instance 'voice :chords (to-midicent second-cp) :tree (om::mktree (second rythmic-om) '(4 4)) :tempo *cf-tempo)
                            (make-instance 'voice :chords (to-midicent first-cp) :tree (om::mktree (first rythmic-om) '(4 4)) :tempo *cf-tempo)
                            )
                    )
                )
            )
        ))
    )
)

; try to find a solution, catch errors from GiL and Gecode and restart the search
(defun try-find-solution (se)
    (handler-case
        (gil::search-next se) ; search the next solution, sol is the space of the solution
        (error (c)
            (print "gil::Unexpected error. Please investigate.")
            ;(try-find-solution se)
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user)
    (print (list "stopped-se" stopped-se "stop-user" stop-user))
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "There are no more solutions.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search has been stopped. Press next to continue the search.")
    )
)
