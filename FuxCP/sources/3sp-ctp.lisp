(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the third species.

;;==========================#
;; THIRD SPECIES            #
;;==========================#
;; Note: fux-cp-3rd execute the first species algorithm without some constraints.
;; In this function, 4 quarter notes by measure are assumed.
(defun fux-cp-3rd (counterpoint &optional (species 3))
    "Create the CSP for the 3rd species of Fux's counterpoint, with the cantus firmus as input"
    (print "Creating the CSP for the 3rd species of Fux's counterpoint...")

    ;; ADD FIRST SPECIES CONSTRAINTS
    (fux-cp-1st counterpoint 3)

    (print "########## THIRD SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    
    (loop for i from 1 to 3 do
        ; add all quarter notes to the space with the domain (cp-domain counterpoint)
        (setf (nth i (cp counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index (extended-cp-domain counterpoint)))
        
        (if (and (eq i 3) (is-borrow-allowed))
            ; then add to the penultimate note more possibilities
            (setf (nth *cf-penult-index (nth i (cp counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint)))
        )
    )

    (loop for i from 1 to 3 do
        (setq i-1 (- i 1))
        ; creating harmonic intervals array
        ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
        (setf (nth i (h-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 11))
        (create-h-intervals (nth i (cp counterpoint)) (butlast *cf) (nth i (h-intervals counterpoint)))

        ; array of IntVar representing the absolute intervals between a thesis and an arsis note of the same measure the counterpoint
        (setf (nth i-1 (m-succ-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 1 12))
        (setf (nth i-1 (m-succ-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12))
        (create-intervals (nth i-1 (cp counterpoint)) (nth i (cp counterpoint)) (nth i-1 (m-succ-intervals counterpoint)) (nth i-1 (m-succ-intervals-brut counterpoint)))
    )
    
    ; merging cp and cp-arsis into one array
    (print "Mergin cps...")
    (setq total-cp-len (+ *cf-len (* *cf-last-index 3))) ; total length of the counterpoint array
    (setq total-cp (gil::add-int-var-array *sp* total-cp-len 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp (cp counterpoint) total-cp) ; merge the four counterpoint arrays into one

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the absolute intervals
    ; between the last note of measure m and the first note of measure m+1 of the counterpoint
    (setf (fourth (m-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 1 12))
    (setf (fourth (m-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (fourth (cp counterpoint)) (first (cp counterpoint)) (fourth (m-intervals counterpoint)) (fourth (m-intervals-brut counterpoint)))
    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setf (m2-len counterpoint) (- (* *cf-last-index 4) 1)) ; number of melodic intervals between n and n+2 for the total counterpoint
    (setf (m2-intervals counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) 0 12))
    (setf (m2-intervals-brut counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) -12 12))
    (create-m2-intervals total-cp (m2-intervals counterpoint) (m2-intervals-brut counterpoint))
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setf (total-m-len counterpoint) (* *cf-last-index 4)) ; number of melodic intervals between n and n+1 for the total counterpoint
    (setf (m-all-intervals counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) 0 12))
    (setf (m-all-intervals-brut counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) -12 12))
    (create-m-intervals-self total-cp (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint))

    ; creating motion array
    ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (print "Creating motion array...")
    (setf (fourth (motions counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (setf (fourth (motions-cost counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (fourth (m-intervals-brut counterpoint)) *cf-brut-m-intervals (fourth (motions counterpoint)) (fourth (motions-cost counterpoint)))

    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (fourth (is-cf-bass-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-bass-arr (fourth (cp counterpoint)) (butlast *cf) (fourth (is-cf-bass-arr counterpoint)))

    ; creating boolean are five consecutive notes by joint degree array
    (print "Creating are five consecutive notes by joint degree array...")
    ; array of BoolVar representing if the five consecutive notes are by joint degree
    (setf (is-5qn-linked-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-5qn-linked-arr (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint) (is-5qn-linked-arr counterpoint))

    ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setf (is-ta-dim-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-ta-dim-arr (second (m-succ-intervals counterpoint)) (collect-by-4 (m2-intervals counterpoint) 1 T) (third (m-succ-intervals counterpoint)) (is-ta-dim-arr counterpoint))

    ; creating boolean is consonant array
    (print "Creating is consonant array...")
    (loop for i from 0 to 3 do
        ; array of BoolVar representing if the interval is consonant
        (if (eq i 0)
            (setf (nth i (is-cons-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-len 0 1))
            (setf (nth i (is-cons-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
        )
        (create-is-member-arr (nth i (h-intervals counterpoint)) (nth i (is-cons-arr counterpoint)))
    )

    ; creating boolean is not cambiata array
    (print "Creating is not cambiata array...")
    (setf (is-not-cambiata-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-not-cambiata-arr (second (is-cons-arr counterpoint)) (third (is-cons-arr counterpoint)) (second (m-succ-intervals counterpoint)) (is-not-cambiata-arr counterpoint))

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setf (is-cp-off-key-arr counterpoint) (gil::add-bool-var-array *sp* total-cp-len 0 1))
    (create-is-member-arr total-cp (is-cp-off-key-arr counterpoint) (off-domain counterpoint))


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")
    ; must start with a perfect consonance
    (print "Perfect consonance at the beginning...")
    (add-p-cons-start-cst (first (h-intervals counterpoint)))

    ; must end with a perfect consonance
    (print "Perfect consonance at the end...")
    (add-p-cons-end-cst (first (h-intervals counterpoint)))
    
    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    (add-penult-cons-cst (lastone (fourth (is-cf-bass-arr counterpoint))) (lastone (fourth (h-intervals counterpoint))))
    ; the third note of the penultimate measure must be below the fourth one.
    (gil::g-rel *sp* (lastone (third (m-succ-intervals-brut counterpoint))) gil::IRT_GR 1)
    ; the second note and the third note of the penultimate measure must be distant by greater than 1 semi-tone from the fourth note
    (gil::g-rel *sp* (penult (m2-intervals counterpoint)) gil::IRT_NQ 1)
    

    ; five consecutive notes by joint degree implies that the first and the third note are consonants
    (print "Five consecutive notes by joint degree...")
    (add-linked-5qn-cst (third (is-cons-arr counterpoint)) (is-5qn-linked-arr counterpoint))

    ; any dissonant note implies that it is surrounded by consonant notes
    (print "Any dissonant note...")
    (add-h-dis-or-cons-3rd-cst (second (is-cons-arr counterpoint)) (third (is-cons-arr counterpoint)) (fourth (is-cons-arr counterpoint)) (is-ta-dim-arr counterpoint))


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no melodic interval between 9 and 11
    (loop for m in (m-succ-intervals counterpoint) do
        (add-no-m-jump-extend-cst m)
    )
    (add-no-m-jump-extend-cst (fourth (m-intervals counterpoint)))

    ; no *chromatic motion between three consecutive notes
    (print "No chromatic motion...")
    (add-no-chromatic-m-cst (m-all-intervals-brut counterpoint) (m2-intervals-brut counterpoint))

    ; Marcel's rule: contrary melodic step after skip
    (print "Marcel's rule...")
    (add-contrary-step-after-skip-cst (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint))

    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; no direct motion to reach a perfect consonance
    (print "No direct motion to reach a perfect consonance...")
    (add-no-direct-move-to-p-cons-cst (fourth (motions counterpoint)) (is-p-cons-arr counterpoint))

    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst (fourth (motions counterpoint)) (first (h-intervals counterpoint)) (fourth (m-intervals-brut counterpoint)) (fourth (is-cf-bass-arr counterpoint))) ; TODO 

    ;======================================== COST FACTORS ====================================
    (if (eq *is-first-run 1) (set-cost-factors  (m-all-intervals counterpoint)))
    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (print "Imperfect consonances are preferred to perfect consonances...")
    (add-p-cons-cost-cst (h-intervals counterpoint))

    ; 3, 4) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst counterpoint total-cp-len)
    
    ; 5) contrary motion is preferred
    (add-cost-to-factors (fourth (motions-cost counterpoint)))

    ; 6) cambiata notes are preferred (cons - dis - cons > cons - cons - cons)
    (print "Cambiata notes are preferred...")
    ; IntVar array representing the cost to have cambiata notes
    (setf (not-cambiata-cost counterpoint) (gil::add-int-var-array-dom *sp* *cf-last-index (getparam-dom 'non-cambiata-cost)))
    (add-cost-bool-cst (is-not-cambiata-arr counterpoint) (not-cambiata-cost counterpoint) *non-cambiata-cost*)
    (add-cost-to-factors (not-cambiata-cost counterpoint))

    ; 7) intervals between notes n and n+2 are prefered greater than zero
    (print "Intervals between notes n and n+2 are prefered different than zero...")
    ; IntVar array representing the cost to have intervals between notes n and n+2 equal to zero
    (setf (m2-eq-zero-cost counterpoint) (gil::add-int-var-array-dom *sp* (m2-len counterpoint) (getparam-dom 'two-beats-apart-cost)))
    (add-cost-cst (m2-intervals counterpoint) gil::IRT_EQ 0 (m2-eq-zero-cost counterpoint) *two-beats-apart-cost*)
    (add-cost-to-factors (m2-eq-zero-cost counterpoint))


    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")


    ; RETURN
    #|(if (eq species 3)
        ; then create the search engine
        (append (fux-search-engine total-cp 3) (list species))
        ; else
        nil
    )|#
    (case species 
        (3 (append (fux-search-engine total-cp '(3)) '((3))))
        (8 total-cp)
        (otherwise nil)
    )
)