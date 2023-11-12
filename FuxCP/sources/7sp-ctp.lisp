(in-package :fuxcp)

; Author: Anton Lamotte
; Date: November 2023
; This file contains the function that adds all the necessary constraints to the second species for three voices.

;;================================#
;; Second species for three voices#
;;================================#
;; Note: fux-cp-7th execute the first species algorithm without some constraints.
;; In this function, all the variable names without the arsis-suffix refers to thesis notes AKA the first species notes.
;; All the variable names with the arsis-suffix refers to arsis notes AKA notes on the upbeat.
(defun fux-cp-7th (counterpoint-1 counterpoint-2 &optional (species 7))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"
    (print "########## SEVENTH SPECIES ##########")
    ;; compute the first counterpoint (1st species)
    (fux-cp-1st counterpoint-1 6)

    (setf *is-first-run 0)
    ; compute the first counterpoint (2nd species)
    (setf solution-array (append (first (cp counterpoint-1)) (fux-cp-2nd counterpoint-2 7))) ; the final array with both counterpoints

    ; Fux says "must be concordant" -> what is concordance?
    ;(setf h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
    ;(create-h-intervals (first (cp counterpoint-1)) (third (cp counterpoint-2)) h-intervals-1-2)
    ;(add-h-cons-cst *cf-len *cf-penult-index h-intervals-1-2 PENULT_THESIS_VAR)
    
    ; Constraints on the two counterpoints
    (print "no unisson between cp1 and cp2")
    (add-no-unisson-cst (first (cp counterpoint-1)) (first (cp counterpoint-2)) species)

    (print "all voices can't go in the same direction")
    (add-no-together-move-cst (first (motions counterpoint-1)) (first (motions counterpoint-2)))

    (print "no successive perfect consonances (cp1 to cp2)")
    (setf h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-1)) (first (cp counterpoint-2)) h-intervals-1-2)
    (setf are-cp1-cp2-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr h-intervals-1-2 are-cp1-cp2-cons-arr)
    (add-no-successive-p-cons-cst are-cp1-cp2-cons-arr)

    ; Cost #14
    (print "prefer perfect chords") ; todo check dependency with 1st and 2nd cost
    (setq *p-chords-cost (gil::add-int-var-array-dom *sp* *cf-len (list 0 1)))
    (compute-prefer-p-chords-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) *p-chords-cost)
    (add-cost-to-factors *p-chords-cost)
 
    ; RETURN
    (if (eq species 7)
        ; then create the search engine
        (append (fux-search-engine solution-array 7) (list species))
        ; else
        nil
    )
)