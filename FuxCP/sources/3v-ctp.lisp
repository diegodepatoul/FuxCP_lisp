(in-package :fuxcp)

; Author: Anton Lamotte
; Date: October 2023
; This file contains the function that adds all the necessary constraints to the five species for three voices.

;;===================================#
;; Three voices counterpoint handler #
;;===================================#
(defun fux-cp-3v (species-list counterpoints)
    (print "########## SIXTH SPECIES ##########")
    (setf counterpoint-1 (first counterpoints))
    (setf counterpoint-2 (second counterpoints))
    (print (list "species list = " species-list))
    (setq *N-COST-FACTORS 5)
    (loop for i from 0 below *N-VOICES do (progn
        (case (nth i species-list)
            (1 (incf *N-COST-FACTORS 5))
            (2 (incf *N-COST-FACTORS 6))
            (3 (incf *N-COST-FACTORS 7))
            (4 (incf *N-COST-FACTORS 5)) ; + 6 from fux-cp-4th and -1 not used in fux-cp-3v
            (otherwise (error "Unexpected value in the species list, when calling fux-cp-3v."))
        )
    ))

    ; creating order/role of pitch array (if cantus firmus is higher or lower than counterpoint)
    ; 0 for being the bass, 1 for being above
    ;(create-is-voice-bass-arr counterpoint-1 counterpoint-2 cf)

    (loop for i from 0 below *N-VOICES do (progn
        (case (nth i species-list)
            (1 (fux-cp-1st (nth i counterpoints) 6))
            (2 (fux-cp-2nd (nth i counterpoints) 7))
            (3 (fux-cp-3rd (nth i counterpoints) 8))
            (4 (fux-cp-4th (nth i counterpoints) 9))
            (otherwise (error "Unexpected value in the species list, when calling fux-cp-3v."))
        )
        (setf *is-first-run 0)
    ))
    
    (setf solution-array (append (solution-array counterpoint-1) (solution-array counterpoint-2))) ; the final array with both counterpoints

    ;================================================================================;
    ;                                CONSTRAINTS                                     ;
    ;================================================================================;
    (print "no unisson between cp1 and cp2")
    (add-no-unisson-cst (first (cp counterpoint-1)) (first (cp counterpoint-2)))

    (print "all voices can't go in the same direction")
    (add-no-together-move-cst (first (motions counterpoint-1)) (first (motions counterpoint-2)))

    (print "no successive perfect consonances (cp1 to cp2)")
    (setf h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-1)) (first (cp counterpoint-2)) h-intervals-1-2)
    (setf are-cp1-cp2-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr h-intervals-1-2 are-cp1-cp2-cons-arr)
    (add-no-successive-p-cons-cst are-cp1-cp2-cons-arr)

    (add-h-cons-cst-2v PENULT_CONS_VAR counterpoint-1 counterpoint-2 h-intervals-1-2)

    (print "Last chord cannot be minor")
    (add-no-minor-third-in-last-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2)))) 
    ; buggy with 3rd species
    (if (or (member 3 species-list) (member 4 species-list))
        nil ; debug
        (progn 
            (print "Last chord cannot include a tenth")
            (add-no-tenth-in-last-chord-cst 
                (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2))
                (h-intervals-brut counterpoint-1) (h-intervals-brut counterpoint-2)
            ) 
        )
    )

    (print "Last chord must be a perfect chord") 
    (add-last-chord-perfect-cst (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)))

    ;================================================================================;
    ;                                    COSTS                                       ;
    ;================================================================================;
    (dolist (counterpoint counterpoints) (progn
        (print "as few direct motion to reach a perfect consonance as possible")
        ; Cost #1: as few direct motion to reach a perfect consonance as possible
        (if (eq (species counterpoint) 4)
            nil ; pass, this cost doesn't apply to 4th species
            (let ((direct-move-to-p-cons-cost (gil::add-int-var-array-dom *sp* *cf-last-index (list 0 8))))
                (case (species counterpoint)
                    (1 (compute-no-direct-move-to-p-cons-costs-cst (first (motions counterpoint)) direct-move-to-p-cons-cost (is-p-cons-arr counterpoint)))
                    (2 (compute-no-direct-move-to-p-cons-costs-cst (real-motions counterpoint) direct-move-to-p-cons-cost (is-p-cons-arr counterpoint)))
                    (3 (compute-no-direct-move-to-p-cons-costs-cst (fourth (motions counterpoint)) direct-move-to-p-cons-cost (is-p-cons-arr counterpoint)))
                )
                (add-cost-to-factors direct-move-to-p-cons-cost)
            )
        )
        
        ; Cost #2: as many different notes as possible
        (print "as many different notes as possible")
        (setf (variety-cost counterpoint) (gil::add-int-var-array *sp* (* 3 (- (length (first (cp counterpoint))) 2)) 0 1))
        (compute-variety-cost (first (cp counterpoint)) (variety-cost counterpoint))
        (add-cost-to-factors (variety-cost counterpoint))
    ))

    ; Cost #3
    (print "prefer perfect chords") ; todo check interdependency with 1st and 2nd cost
    (if (member 4 species-list)
        (progn
            (setq *p-chords-cost (gil::add-int-var-array-dom *sp* *cf-last-index (list 0 1)))
            (if (eq (species counterpoint-1) 4)
                (if (eq (species counterpoint-2) 4) 
                    ; both are of fourth species
                    (compute-prefer-p-chords-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) *p-chords-cost)
                    ; only the first is of fourth species
                    (compute-prefer-p-chords-cost (first (h-intervals counterpoint-1)) (rest (first (h-intervals counterpoint-2))) *p-chords-cost)
                )
                ; only the second is of fourth species
                (compute-prefer-p-chords-cost (rest (first (h-intervals counterpoint-1))) (first (h-intervals counterpoint-2)) *p-chords-cost)
            )
        )
        (progn
            (setq *p-chords-cost (gil::add-int-var-array-dom *sp* *cf-len (list 0 1)))
            (compute-prefer-p-chords-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) *p-chords-cost)
        )
    )
    (add-cost-to-factors *p-chords-cost)

    ;================================================================================;
    ;                                    RETURN                                      ;
    ;================================================================================;
    (append (fux-search-engine solution-array species-list) (list species-list))
    
)