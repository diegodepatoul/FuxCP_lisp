(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the second species.

;;==========================#
;; SECOND SPECIES           #
;;==========================#
;; Note: fux-cp-2nd execute the first species algorithm without some constraints.
;; In this function, all the variable names without the arsis-suffix refers to thesis notes AKA the first species notes.
;; All the variable names with the arsis-suffix refers to arsis notes AKA notes on the upbeat.
(defun fux-cp-6th (&optional (species 6))
    (print "########## SIXTH SPECIES ##########")

    ; Creating the first counterpoint
    (fux-cp-1st 6)
    
    ; Backuping all its variables 
    (setf *cp2 (copy-list *cp))
    (setf *h-intervals2 (copy-list *h-intervals))
    (setf *m-intervals-brut2 (copy-list *m-intervals-brut))
    (setf *m-intervals2 (copy-list *m-intervals))
    (setf *m2-intervals-brut2 *m2-intervals-brut)
    (setf *m2-intervals2 *m2-intervals)
    (setf *cf-brut-m-intervals2 *cf-brut-m-intervals)
    (setf *is-p-cons-arr2 *is-p-cons-arr)
    (setf *motions2 (copy-list *motions))
    (setf *motions-cost2 (copy-list *motions-cost))
    ;(setf *is-cf-bass2 *is-cf-bass)
    (setf *is-cf-bass-arr2 (copy-list *is-cf-bass-arr))
    (setf *is-cp-off-key-arr2 *is-cp-off-key-arr)
    ;(setf *p-cons-cost2 *p-cons-cost)
    (setf *fifth-cost2 *fifth-cost)
    (setf *octave-cost2 *octave-cost)
    (setf *m-degrees-cost2 *m-degrees-cost)
    (setf *m-degrees-type2 *m-degrees-type)
    (setf *off-key-cost2 *off-key-cost)
    
    (setf *direct-move-to-p-cons-cost2 (copy-list *direct-move-to-p-cons-cost))
    (setf *diversity-cost2 (copy-list *diversity-cost))

    ; Creating the second counterpoint
    (setf *is-first-run 0)
    ; Swap first and second voices
    (setf VOICE_TYPE VOICE_TYPE2)
    (setf RANGE_UB (+ 12 (* 6 VOICE_TYPE)))
    (setf RANGE_LB (+ -6 (* 6 VOICE_TYPE)))
    ; set the pitch range of the counterpoint
    (setf *cp-range (range (+ *tone-pitch-cf RANGE_UB) :min (+ *tone-pitch-cf RANGE_LB))) ; arbitrary range
    ; set counterpoint pitch domain
    (setf *cp-domain (intersection *cp-range *scale))
    ; penultimate (first *cp) note domain
    (setf *chromatic-cp-domain (intersection *cp-range *chromatic-scale))
    ; set counterpoint extended pitch domain
    (setf *extended-cp-domain (intersection *cp-range (union *scale *borrowed-scale)))
    ; set the domain of the only barrowed notes
    (setf *off-domain (intersection *cp-range *off-scale))

    (if (eq species 6) (progn
        (fux-cp-1st 6)

        ; Joining both counterpoints in a single array
        (setq *total-cp-len (* 2 *cf-len))
        (setq *total-cp (gil::add-int-var-array *sp* *total-cp-len 0 127)) ; array of IntVar representing thesis and arsis notes combined
        (merge-cp-same-len (list (first *cp) (first *cp2)) *total-cp) ; merge the two counterpoint arrays into one

        ; Constraints on the two counterpoints
        (print "no unisson between cp1 and cp2")
        (add-no-unisson-cst (first *cp) (first *cp2) species)

        (print "all voices can't go in the same direction")
        (add-no-together-move-cst (first *motions) (first *motions2))

        (print "No successive perfect consonances (counterpoint1 to counterpoint2)")
        (setf h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
        (create-h-intervals (first *cp) (first *cp2) h-intervals-1-2)

        (setf are-cp1-cp2-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
        (create-is-p-cons-arr h-intervals-1-2 are-cp1-cp2-cons-arr)
        (add-no-successive-p-cons-cst are-cp1-cp2-cons-arr)

        (print "prefer perfect chords")
        (setf *p-chords-cost (gil::add-int-var-array-dom *sp* *cf-len (list 0 1)))
        (add-prefer-p-chords-cost (first *h-intervals) (first *h-intervals2) *p-chords-cost)
        (add-cost-to-factors *p-chords-cost)

        (print "Perfect chord at the beginning...")
        ;(add-p-chord-cst (first (first *h-intervals)) (first (first *h-intervals2)))
        (print "Perfect chord at the end...")
        ;(add-p-chord-cst (last (first *h-intervals)) (last (first *h-intervals2)))
    ))

    ; RETURN
    (if (eq species 6)
        ; then create the search engine
        (append (fux-search-engine *total-cp 6) (list species))
        ; else
        nil
    )
    
)