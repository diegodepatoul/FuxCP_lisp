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

    (loop for i from 0 below *N-VOICES do (progn
        (case (nth i species-list)
            (1 (fux-cp-1st (nth i counterpoints) 6))
            (2 (fux-cp-2nd (nth i counterpoints) 7))
            (3 (fux-cp-3rd (nth i counterpoints) 8))
            (4 (fux-cp-4th (nth i counterpoints) 9))
            (5 (fux-cp-5th (nth i counterpoints) 10))
            (otherwise (error "Unexpected value in the species list, when calling fux-cp-3v."))
        )
    ))
    
    (setf solution-array (append (solution-array counterpoint-1) (solution-array counterpoint-2))) ; the final array with both counterpoints
        
    ;================================================================================;
    ;                                CONSTRAINTS                                     ;
    ;================================================================================;
    ; all voices must be consonant with the lowest one
    ; implemented in the first species
    
    (print "no unisson between cp1 and cp2")
    (add-no-unisson-cst (first (cp counterpoint-1)) (first (cp counterpoint-2)))

    (print "all voices can't go in the same direction")
    (add-no-together-move-cst (first (motions counterpoint-1)) (first (motions counterpoint-2)) (first (motions *cantus-firmus)))

    (print "no successive perfect consonances (cp1 to cp2)")
    (setq h-intervals-1-2 (list nil nil nil nil))
    (setf (first h-intervals-1-2) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-1)) (first (cp counterpoint-2)) (first h-intervals-1-2))
    (setf are-cp1-cp2-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first h-intervals-1-2) are-cp1-cp2-cons-arr)
    (add-no-successive-p-cons-cst are-cp1-cp2-cons-arr)

    (setq h-intervals-cp1-cf (list nil nil nil nil))
    (setf (first h-intervals-cp1-cf) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-1)) (first (cp *cantus-firmus)) (first h-intervals-cp1-cf))
    (setf are-cp1-cf-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first h-intervals-cp1-cf) are-cp1-cf-cons-arr)
    (add-no-successive-p-cons-cst are-cp1-cf-cons-arr)

    (setq h-intervals-cp2-cf (list nil nil nil nil))
    (setf (first h-intervals-cp2-cf) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-2)) (first (cp *cantus-firmus)) (first h-intervals-cp2-cf))
    (setf are-cp2-cf-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first h-intervals-cp2-cf) are-cp2-cf-cons-arr)
    (add-no-successive-p-cons-cst are-cp2-cf-cons-arr)


    ; THIS CLASHES WITH THE PENULT RULES WHEN USED ACROSS TWO DIFFERENT SPECIES
    ;(add-h-cons-cst *cf-len *cf-last-index (first h-intervals-1-2))
    ;(dotimes (i *cf-len)
        ;(if (eq i *cf-last-index)
            ;(gil::g-member *sp* PENULT_THESIS_VAR (nth i (first h-intervals-1-2)))
    ;         (gil::g-member *sp* ALL_CONS_VAR (nth i (first h-intervals-1-2)))
        ;)
    ;)
    

    (print "Last chord cannot be minor")
    ;(add-no-minor-third-in-last-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2)))) 
    (dolist (counterpoint counterpoints)
        (if (eq (species counterpoint) 4) (progn 
                (setf (h-intervals-abs counterpoint) (gil::add-int-var-array *sp* *cf-last-index -127 127))
                (setf (h-intervals-brut counterpoint) (gil::add-int-var-array *sp* *cf-last-index -127 127))
                (create-intervals (rest *cf) (third (cp counterpoint)) (h-intervals-abs counterpoint) (h-intervals-brut counterpoint))
            ) (progn
                (setf (h-intervals-abs counterpoint) (gil::add-int-var-array *sp* *cf-len -127 127))
                (setf (h-intervals-brut counterpoint) (gil::add-int-var-array *sp* *cf-len -127 127))
                (create-intervals *cf (first (cp counterpoint)) (h-intervals-abs counterpoint) (h-intervals-brut counterpoint))
            ) 
        )
    )
    (if (or (member 4 species-list) (member 5 species-list))
        nil ; debug
        (progn 
            (print "Last chord cannot include a tenth")
            (add-no-tenth-in-last-chord-cst 
                (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2))
                (h-intervals-brut counterpoint-1) (h-intervals-brut counterpoint-2)
            ) 
            
            (print "Last chord must be a perfect chord") 
            (add-last-chord-h-triad-cst (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)))

            (print "Ascending sixths sound harsh")
            (dolist (counterpoint counterpoints) 
                (add-no-ascending-sixths-cst (first (h-intervals counterpoint)) (first (cp counterpoint)))
            )
        )
    )



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
                    (5 (compute-no-direct-move-to-p-cons-costs-cst 
                        (fourth (motions counterpoint)) direct-move-to-p-cons-cost (collect-bot-array (is-p-cons-arr counterpoint) (fourth (is-3rd-species-arr counterpoint))) nil
                    ))
                    (otherwise (error "Unexpected species when computing the cost for no-direct-move-to-p-cons"))
                )
                (add-cost-to-factors direct-move-to-p-cons-cost 'direct-move-to-p-cons-cost)
            )
        )
        
        ; Cost #2: as many different notes as possible
        (print "as many different notes as possible")
        (let (
            (variety-cost (gil::add-int-var-array *sp* (* 3 (- (length (first (cp counterpoint))) 2)) 0 1))
            )
            (compute-variety-cost (first (cp counterpoint)) variety-cost)
            (add-cost-to-factors variety-cost 'variety-cost)
        )
    ))

    ; Cost #3
    (print "prefer harmonic triad") ; todo check interdependency with 1st and 2nd cost
    (if (member 4 species-list) ; the 4th species behaves differently, as the note to be considered is the note on the upbeat, and not on the downbeat as the other species
        (progn
            (setq h-triad-cost (gil::add-int-var-array-dom *sp* *cf-last-index (list 0 1)))
            (if (eq (species counterpoint-1) 4)
                (if (eq (species counterpoint-2) 4) 
                    ; both are of fourth species
                    (compute-h-triad-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) h-triad-cost)
                    ; only the first is of fourth species
                    (compute-h-triad-cost (first (h-intervals counterpoint-1)) (rest (first (h-intervals counterpoint-2))) h-triad-cost)
                )
                ; only the second is of fourth species
                (compute-h-triad-cost (rest (first (h-intervals counterpoint-1))) (first (h-intervals counterpoint-2)) h-triad-cost)
            )
        )
        (progn
            (setq h-triad-cost (gil::add-int-var-array-dom *sp* *cf-len (list 0 1)))
            (compute-h-triad-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) h-triad-cost)
        )
    )
    (add-cost-to-factors h-triad-cost 'h-triad-cost)

    ; Cost #4, only for 3rd species: if harmonic triad isn't achieved on the downbeat, it shall be on the other beats
    (dotimes (i *N-VOICES) 
        (if (eq (species (nth i counterpoints)) 3) (let
            (
                (h-triad-3rd-species-cost (gil::add-int-var-array-dom *sp* (* *cf-last-index 3) (list 0 1)))
            )
            (dotimes (j 3) (progn 
               (compute-h-triad-cost 
                    (nth (+ j 1) (h-intervals (nth i counterpoints))) ; this is the jth beat
                    (first (h-intervals (nth (logxor i 1) counterpoints))) ; these are the intervals of the OTHER counterpoint
                    (subseq h-triad-3rd-species-cost (* j *cf-last-index) (* (+ j 1) *cf-last-index))) ; these are the costs corresponding to the jth beat
            ))
            (add-cost-to-factors h-triad-3rd-species-cost 'h-triad-3rd-species-cost)
        ))
    )

    (dolist (counterpoint counterpoints)
       ; (case (species counterpoint) ((1 2 3) (add-p-cons-cost-cst (h-intervals-to-bass counterpoint))))
        (case (species counterpoint) ((4) (add-p-cons-cost-cst (h-intervals-to-bass counterpoint) t)))
    )
    
    
    ; TO DELETE LINE
    (setq *h-intervals-1-2 h-intervals-1-2)
    

    ;================================================================================;
    ;                                    RETURN                                      ;
    ;================================================================================;
    (append (fux-search-engine solution-array species-list) (list species-list))
    
)