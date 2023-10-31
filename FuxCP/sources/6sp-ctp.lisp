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
    
    ; saving values of cp2 and now working with cp1
    (backup-cp2-values)

    (if (eq species 6) (progn
        ; creating the second counterpoint
        (fux-cp-1st 6)

        ; Joining both counterpoints in a single array
        (create-2v-cp)

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