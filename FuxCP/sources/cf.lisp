(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the first species.

;;==========================#
;; FIRST SPECIES            #
;;==========================#
(defun fux-cp-cf (cantus-firmus &optional (species 1))
    (print "########## FIRST SPECIES ##########")
    "Create the CSP for the first species of Fux's cantus-firmus."

    ;============================================ CREATING GIL ARRAYS =============================
    ;; initialize the variables
    (print "Initializing variables...")
        
    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")

    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the cantus-firmus
    (setf (first (h-intervals cantus-firmus)) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp cantus-firmus)) (first (cp *bass)) (first (h-intervals cantus-firmus)))
#|
    ;(create-h-intervals (first (cp *upper-voice)) (first (cp *bass-notes)) (first (h-intervals *upper-voice)))
    ;(create-h-intervals (first (cp *bass-notes))  *cf                      (first (h-intervals *bass-notes )))

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the absolute intervals between two notes in a row of the cantus-firmus
    (setf (first (m-intervals cantus-firmus)) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setf (first (m-intervals-brut cantus-firmus)) (gil::add-int-var-array *sp* *cf-last-index -12 12))
    (create-m-intervals-self (first (cp cantus-firmus)) (first (m-intervals cantus-firmus)) (first (m-intervals-brut cantus-firmus)))
    
    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the cantus-firmus is a perfect consonance
    (setf (is-p-cons-arr cantus-firmus) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first (h-intervals cantus-firmus)) (is-p-cons-arr cantus-firmus))


    ; creating motion array
    (print "Creating motion array...")
    (setf (first (motions cantus-firmus)) (gil::add-int-var-array *sp* *cf-last-index 0 2)) ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (setf (first (motions-cost cantus-firmus)) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    ;(create-motions (first (m-intervals-brut cantus-firmus)) *cf-brut-m-intervals (first (motions cantus-firmus)) (first (motions-cost cantus-firmus)))
    (create-motions (first (m-intervals-brut cantus-firmus)) (first (m-intervals-brut *bass)) (first (motions cantus-firmus)) (first (motions-cost cantus-firmus)))
    (set-motions-cost (first (motions cantus-firmus)) (first (motions-cost cantus-firmus)) (rest (first (is-cp-bass cantus-firmus))))
     |#
    ;============================================ HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all intervals between the cantus firmus and the cantus-firmus, the interval must be a consonance
    (print "Harmonic consonances...")
    (case species
        ((1 ) (progn
            (add-h-cons-cst *cf-len *cf-penult-index (first (h-intervals cantus-firmus)) (first (is-cp-bass cantus-firmus)))
        ))
        ((2 ) (add-h-cons-cst *cf-len *cf-penult-index (first (h-intervals cantus-firmus)) PENULT_THESIS_VAR))
        ((3 ) (add-h-cons-cst *cf-len *cf-penult-index (first (h-intervals cantus-firmus)) PENULT_1Q_VAR))
        ;(otherwise (error "Species not supported"))
    )

#|
    (case species ((1 2) 
        ; then
        (progn
        ; must start with a perfect consonance
        (print "Perfect consonance at the beginning...")
        (add-p-cons-start-cst (first (h-intervals cantus-firmus)))

        ; must end with a perfect consonance
        (print "Perfect consonance at the end...")
        (add-p-cons-end-cst (first (h-intervals cantus-firmus)))
        )
    ))

    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    (add-penult-cons-cst-for-cf (penult (first (is-cp-bass cantus-firmus))) (penult (first (h-intervals cantus-firmus))))  

    ;==================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    (if (eq species 1) ; for the 6th species, it isn't a constraint but a cost
    ; no direct motion to reach a perfect consonance
        (progn
            (print "No direct motion to reach a perfect consonance...")
            (add-no-direct-move-to-p-cons-cst (first (motions cantus-firmus)) (is-p-cons-arr cantus-firmus) (first (is-cp-bass cantus-firmus)))
        )
    )
    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, cantus-firmus melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst (first (motions cantus-firmus)) (first (h-intervals cantus-firmus)) (first (m-intervals-brut cantus-firmus)) (first (is-cp-bass cantus-firmus)))

    ; @completely new or reworked
    ; ========= 2 cantus-firmuss specific
    (if (eq *N-VOICES 2)
        (progn
            (print "No successive perfect consonances (cantus-firmus to cantus firmus)")
            (add-no-successive-p-cons-cst (is-p-cons-arr *upper-voice))
        )
    )
    ; =========
    
    ;============================================ COST FACTORS ====================================
    (print "Cost function...")

    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (print "Imperfect consonances are preferred to perfect consonances...")
    (add-p-cons-cost-cst (h-intervals cantus-firmus))
    ; 3) motion costs
    (print "add motion costs")
    ;(add-cost-to-factors (first (motions-cost counterpoint)) 'motions-cost)    
     |#
)