(in-package :fuxcp)

; Author: Anton Lamotte
; Date: October 2023
; This file contains the function that adds all the necessary constraints to the five species for three voices.

;;===================================#
;; Three voices counterpoint handler #
;;===================================#
(defun fux-cp-3v (species-list parts)
    (print "########## SIXTH SPECIES ##########")
    (setf cantus-firmus (first parts))
    (setf counterpoint-1 (second parts))
    (setf counterpoint-2 (third parts))
    (print (list "species list = " species-list))

    (dotimes (i *N-PARTS)
        (case (species (nth i parts))
            (0 (fux-cp-cf (nth i parts)))
            (1 (fux-cp-1st (nth i parts) 6))
            (2 (fux-cp-2nd (nth i parts) 7))
            (3 (fux-cp-3rd (nth i parts) 8))
            (4 (fux-cp-4th (nth i parts) 9))
            (5 (fux-cp-5th (nth i parts) 10))
            (otherwise (error "Unexpected value in the species list, when calling fux-cp-3v."))
        )
    )
    
    (setf solution-array (append (solution-array counterpoint-1) (solution-array counterpoint-2))) ; the final array with both counterpoints

    (dotimes (i *N-VOICES)
        (create-h-intervals (first (notes (nth i *upper))) (first (notes *lowest)) (first (h-intervals (nth i *upper))))
        (setf (h-intervals-abs (nth i *upper)) (gil::add-int-var-array *sp* *cf-len -127 127))
        (setf (h-intervals-brut (nth i *upper)) (gil::add-int-var-array *sp* *cf-len -127 127))
        (create-intervals (first (notes *lowest)) (first (notes (nth i *upper))) (h-intervals-abs (nth i *upper)) (h-intervals-brut (nth i *upper)))
    )

    ;================================================================================;
    ;                                CONSTRAINTS                                     ;
    ;================================================================================;
    (print "no unison between cp1 and cp2")
    
    ;todo debug this doesn't work
    (loop for v1 in parts
        for i from 0
        do (loop for v2 in (nthcdr (1+ i) parts) 
        do (progn ; take each possible pair or parts    
            (case (species v1)
                (4 (case (species v2)
                    (4 (add-no-unison-at-all-cst (third (notes v1)) (third (notes v2))))
                    (otherwise (add-no-unison-at-all-cst (third (notes v1)) (first (notes v2))))
                ))
                (otherwise (case (species v2)
                    (4 (add-no-unison-at-all-cst (first (notes v1)) (third (notes v2))))
                    (otherwise (add-no-unison-at-all-cst (first (notes v1)) (first (notes v2))))
                ))
            )
        )
    ))
    

    (print "all voices can't go in the same direction")
    (add-no-together-move-cst (first (motions counterpoint-1)) (first (motions counterpoint-2)))

    
    (print "no successive perfect consonances (cp1 to cp2)")
    (setq h-intervals-1-2 (list nil nil nil nil))
    (setf (first h-intervals-1-2) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (notes counterpoint-1)) (first (notes counterpoint-2)) (first h-intervals-1-2))
    (setf are-cp1-cp2-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first h-intervals-1-2) are-cp1-cp2-cons-arr)
    (add-no-successive-p-cons-cst are-cp1-cp2-cons-arr) 

    (print "Last chord cannot be minor")
    
    ; next line covered by creating the harmonic arrays
    ;(add-no-minor-third-in-last-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2)))) 
    
    (print "Last chord cannot include a tenth")
    (dotimes (i *N-VOICES)
        (add-no-tenth-in-last-chord-cst (first (h-intervals (nth i *upper))) (h-intervals-brut (nth i *upper)))
    )

    (print "Last chord must be a harmonic triad") 
    (add-last-chord-h-triad-cst (first (h-intervals (first *upper))) (first (h-intervals (second *upper))))

    (dotimes (i *N-PARTS)
        (if (eq (species (nth i parts)) 2) 
           ;(add-arsis-consonance-with-thesis-from-other-voices-cst parts (- i 1))
           nil
        )
    )

    (if (equal species-list '(5 5))
        (let (
                (is-same-species (gil::add-bool-var-array *sp* (solution-len (second parts)) 0 1))
                (is-same-species-int (gil::add-int-var-array *sp* (solution-len (second parts)) 0 1))
                (percentage-same-species (gil::add-int-var *sp* 0 (solution-len (second parts))))
            )
            (dotimes (i (solution-len (second parts)))
                (gil::g-rel-reify *sp* (nth i (species-arr (second parts))) gil::IRT_EQ (nth i (species-arr (third parts))) (nth i is-same-species))
                (gil::g-rel-reify *sp* (nth i is-same-species-int) gil::IRT_EQ 1 (nth i is-same-species))
            )
            (gil::g-sum *sp* percentage-same-species is-same-species-int)
            (gil::g-rel *sp* percentage-same-species gil::IRT_LE (floor (/ (solution-len (second parts)) 2)))
        )
    )

    ;================================================================================;
    ;                                    COSTS                                       ;
    ;================================================================================;
    (dolist (part parts) (progn
        (print "as few direct motion to reach a perfect consonance as possible")
        ; Cost #1: as few direct motion to reach a perfect consonance as possible
        (if (or (eq (species part) 4) (eq (species part) 0))
            nil ; pass, this cost doesn't apply to 4th species nor to the cantus firmus
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
        (if (eq (species part) 0)
            nil ; this cost has no sense for the cantus firmus
            (let (
                (variety-cost (gil::add-int-var-array *sp* (* 3 (- (length (first (notes counterpoint))) 2)) 0 1))
                )
                (compute-variety-cost (first (notes counterpoint)) variety-cost)
                (add-cost-to-factors variety-cost 'variety-cost)
            )
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

    #| TO DO REWRITE THIS PART    (dotimes (i *N-PARTS)
        ; Cost #4, only for 3rd species: if harmonic triad isn't achieved on the downbeat, it shall be on the second or third one
        (if (eq (species (nth i parts)) 3) (let
            (
                (h-triad-3rd-species-cost (gil::add-int-var-array-dom *sp* (* *cf-last-index 2) (list 0 1)))
            )
            (dotimes (j 2) (progn 
               (compute-h-triad-cost 
                    (nth (+ j 1) (h-intervals (nth i parts))) ; this is the jth beat
                    (first (h-intervals (nth (logxor i 1) parts))) ; these are the intervals of the OTHER counterpoint
                    (subseq h-triad-3rd-species-cost (* j *cf-last-index) (* (+ j 1) *cf-last-index))) ; these are the costs corresponding to the jth beat
            ))
            (add-cost-to-factors h-triad-3rd-species-cost 'h-triad-3rd-species-cost)
        ))
    )
     |#

    ; TO DELETE LINE
    (setq *h-intervals-1-2 h-intervals-1-2)
    
    ;================================================================================;
    ;                                    RETURN                                      ;
    ;================================================================================;
    (append (fux-search-engine solution-array species-list) (list species-list))
    
)