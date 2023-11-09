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
(defun fux-cp-6th (counterpoint-1 counterpoint-2 &optional (species 6))
    (print "########## SIXTH SPECIES ##########")
    ; Creating the first counterpoint
    (fux-cp-1st counterpoint-1 6)
   
    (setf *is-first-run 0)
    (fux-cp-1st counterpoint-2 6)
    
    (setf solution-array (append (first (cp counterpoint-1)) (first (cp counterpoint-2))))


    ; Constraints on the two counterpoints
    (print "no unisson between cp1 and cp2")
    (add-no-unisson-cst (first (cp counterpoint-1)) (first (cp counterpoint-2)) species)


    (print "all voices can't go in the same direction")
    (add-no-together-move-cst (first (motions counterpoint-1)) (first (motions counterpoint-2)))

    (print "No successive perfect consonances (counterpoint1 to counterpoint2)")
    (setf h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-1)) (first (cp counterpoint-2)) h-intervals-1-2)

    (setf are-cp1-cp2-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr h-intervals-1-2 are-cp1-cp2-cons-arr)
    (add-no-successive-p-cons-cst are-cp1-cp2-cons-arr)

    (print "prefer perfect chords") ; isn't this kind of the 1st and 2nd costs?
    
    (setq *p-chords-cost (gil::add-int-var-array-dom *sp* *cf-len (list 0 1)))
    (add-prefer-p-chords-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) *p-chords-cost)
    ; 15
    (add-cost-to-factors *p-chords-cost)

    ;(print "Perfect chord at the beginning...")
    ;(add-p-chord-cst (first (first *h-intervals)) (first (first *h-intervals2)))
    (print "Perfect chord at the end...")
    (add-p-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2))))

    (last-chord-not-minor-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2)))) ; redundant with last constraint

    ; RETURN
    (if (eq species 6)
        ; then create the search engine
        (append (fux-search-engine solution-array 6) (list species))
        ; else
        nil
    )
    
)