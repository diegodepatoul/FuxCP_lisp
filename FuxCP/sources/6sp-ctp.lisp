(in-package :fuxcp)

; Author: Anton Lamotte
; Date: October 2023
; This file contains the function that adds all the necessary constraints to the first species for three voices.

;;================================#
;; First species for three voices #
;;================================#
;; Note: fux-cp-6th executes the first species algorithm with some modified constraints.
(defun fux-cp-6th (species-list counterpoints &optional (species 6))
    (print "########## SIXTH SPECIES ##########")
    (setf counterpoint-1 (first counterpoints))
    (setf counterpoint-2 (second counterpoints))
    (print (list "species list = " species-list))

    (loop for i from 0 below *N-VOICES do (progn
        (case (nth i species-list)
            (1 (incf *N-COST-FACTORS 7))
            (2 (incf *N-COST-FACTORS 7))
            (3 (incf *N-COST-FACTORS 7))
        )
    ))

    (loop for i from 0 below *N-VOICES do (progn
        (case (nth i species-list)
            (1 (fux-cp-1st (nth i counterpoints) 6))
            (2 (fux-cp-2nd (nth i counterpoints) 7))
            (3 (fux-cp-3rd (nth i counterpoints) 8))
        )
        (setf *is-first-run 0)
    ))
    
    (setf solution-array (append (solution-array counterpoint-1) (solution-array counterpoint-2))) ; the final array with both counterpoints

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

    ; Cost #15
    (print "prefer perfect chords") ; todo check dependency with 1st and 2nd cost
    (setq *p-chords-cost (gil::add-int-var-array-dom *sp* *cf-len (list 0 1)))
    (compute-prefer-p-chords-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) *p-chords-cost)
    (add-cost-to-factors *p-chords-cost)

    (print "Last chord cannot be minor")
    (add-no-minor-third-in-last-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2)))) 
    ; buggy with 3rd species
    #| (print "Last chord cannot include a tenth")
    (add-no-tenth-in-last-chord-cst (last (h-intervals-brut counterpoint-1)) (last (h-intervals-brut counterpoint-2))) |#
    (print "Last chord must be a ... chord") 
    (add-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2))))

    ; RETURN
    (if (eq species 6)
        ; then create the search engine
        (append (fux-search-engine solution-array species-list) (list species-list))
        ; else
        nil
    )
    
)