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
            (0 (fux-cp-cf  (nth i parts)))
            (1 (fux-cp-1st (nth i parts) 6))
            (2 (fux-cp-2nd (nth i parts) 7))
            (3 (fux-cp-3rd (nth i parts) 8))
            (4 (fux-cp-4th (nth i parts) 9))
            (5 (fux-cp-5th (nth i parts) 10))
            (otherwise (error "Unexpected value in the species list, when calling fux-cp-3v."))
        )
    )
    
    (setf solution-array (append (solution-array counterpoint-1) (solution-array counterpoint-2))) ; the final array with both counterpoints

    (dotimes (i *N-COUNTERPOINTS)
        (create-h-intervals (first (notes (nth i *upper))) (first (notes *lowest)) (first (h-intervals (nth i *upper))))
        (setf (h-intervals-abs (nth i *upper)) (gil::add-int-var-array *sp* *cf-len -127 127))
        (setf (h-intervals-brut (nth i *upper)) (gil::add-int-var-array *sp* *cf-len -127 127))
        (create-intervals (first (notes *lowest)) (first (notes (nth i *upper))) (h-intervals-abs (nth i *upper)) (h-intervals-brut (nth i *upper)))
    )

    ;================================================================================;
    ;                                CONSTRAINTS                                     ;
    ;================================================================================;
    ; it is not allowed to have two direct motions
    ; WARNING: this implementation works only for three voices
    (print "No together move")
    (add-no-together-move-cst (list (first (motions counterpoint-1)) (first (motions counterpoint-2)) (first (motions cantus-firmus))))


    (print "Last chord cannot be minor")
    ; next line covered by creating the harmonic arrays
    ;(add-no-minor-third-in-last-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2)))) 
    (dotimes (i (- *N-PARTS 1))
        (add-no-minor-third-cst (lastone (first (h-intervals (nth i *upper)))))
    )
    
    (print "Last chord cannot include a tenth")
    (dotimes (i *N-COUNTERPOINTS)
        (add-no-tenth-in-last-chord-cst (first (h-intervals (nth i *upper))) (h-intervals-brut (nth i *upper)))
    )

    (print "Last chord must be a harmonic triad") 
    (add-last-chord-h-triad-cst (first (h-intervals (first *upper))) (first (h-intervals (second *upper))))

    ; fifth species only
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
    ; Cost #1 : no successive perfect consonances
    (setf succ-p-cons-cost (gil::add-int-var-array-dom *sp* (* 3 *cf-last-index) *succ-p-cons-domain*))
    (setf succ-p-cons-cost-index 0)
    (loop 
        ; for each possible pair or parts
        ; for example if we have (cf, cp1 and c2), take (cf and cp1), (cf and cp2) and (cp1 and cp2)
        for v1 in parts 
        for i from 0 
        do (loop for v2 in (nthcdr (1+ i) parts) 
        do (progn 
            ; no unison between the voices
            (print "No unison between the voices")
            (dotimes (i 4) (if (eq i 0)
                ; first beat can be the same on first and last measure
                (add-no-unison-cst (nth i (notes v1)) (nth i (notes v2)))
                ; other beats must always be different
                (add-no-unison-at-all-cst (nth i (notes v1)) (nth i (notes v2)))
            ))
            ; (add-no-unison-cst (first (notes v1)) (first (notes v2)))
            (print "No successive perfect consonances")
            (let (
                (h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
                (is-cons-arr-1-2 (gil::add-bool-var-array *sp* *cf-len 0 1))
                (current-cost (subseq succ-p-cons-cost succ-p-cons-cost-index))
                )
                (incf succ-p-cons-cost-index *cf-last-index)
                
                (if (member 4 (list (species v1) (species v2)))
                    ; if one voice is of the fourth species the last chord was not created yet, due to the delaying of the fourth specues
                    (create-h-intervals (last (first (notes v1))) (last (first (notes v2))) (last h-intervals-1-2)) 
                )

                (create-h-intervals (first (notes v1)) (first (notes v2)) h-intervals-1-2)
                (create-is-p-cons-arr h-intervals-1-2 is-cons-arr-1-2)
                (cond 
                    ((and (/= 2 (species v1)) (/= 2 (species v2)) (/= 4 (species v1)) (/= 4 (species v2))) ; if both voices are not from the 2nd nor from the 4th species
                        (add-no-successive-p-cons-cst is-cons-arr-1-2 current-cost) ; for all species except the fourth and the second, successive perfect consonances are prohibited
                    )
                    ((= 2 (species v1))
                        (add-no-successive-p-cons-2nd-species-cst is-cons-arr-1-2 h-intervals-1-2 (first (m-succ-intervals v1)) current-cost) ; for the second species, successive fifths are allowed if there is a third in between
                    )
                    ((= 2 (species v2))
                        (add-no-successive-p-cons-2nd-species-cst is-cons-arr-1-2 h-intervals-1-2 (first (m-succ-intervals v2)) current-cost) ; for the second species, successive fifths are allowed if there is a third in between
                    )
                    ((or (eq 4 (species v1)) (eq 4 (species v2)))
                        (add-no-successive-p-cons-4th-species-cst is-cons-arr-1-2 h-intervals-1-2 current-cost) ; for the fourth species, successive fifths are allowed, but no other successive perfect consonances
                    ) 
                )
                (if (and (eq (species v1) 4) (eq (species v2) 4)) (setq *h-intervals-1-2 h-intervals-1-2))
            )
             
        )
    ))
    (add-cost-to-factors succ-p-cons-cost 'succ-p-cons-cost)

    (dolist (part parts) (progn
        (print "as few direct motion to reach a perfect consonance as possible")
        ; Cost #2: as few direct motion to reach a perfect consonance as possible
        (if (eq (species part) 4)
            nil ; pass, this cost doesn't apply to 4th species
            (let ((direct-move-to-p-cons-cost (gil::add-int-var-array-dom *sp* *cf-last-index (list 0 8))))
                (print (list "species = " (species part)))
                (case (species part)
                    ((0 1) (compute-no-direct-move-to-p-cons-costs-cst (first (motions part)) direct-move-to-p-cons-cost (is-p-cons-arr part)))
                    (2 (compute-no-direct-move-to-p-cons-costs-cst (real-motions part) direct-move-to-p-cons-cost (is-p-cons-arr part)))
                    (3 (compute-no-direct-move-to-p-cons-costs-cst (fourth (motions part)) direct-move-to-p-cons-cost (is-p-cons-arr part)))
                    (5 (compute-no-direct-move-to-p-cons-costs-cst 
                        (fourth (motions part)) direct-move-to-p-cons-cost (collect-bot-array (is-p-cons-arr part) (fourth (is-3rd-species-arr part))) nil
                    ))
                    (otherwise (error "Unexpected species when computing the cost for no-direct-move-to-p-cons"))
                )
                (add-cost-to-factors direct-move-to-p-cons-cost 'direct-move-to-p-cons-cost)
            )
        )
        
        ; Cost #3: as many different notes as possible
        (print "as many different notes as possible")
        (if (eq (species part) 0)
            nil ; this cost has no sense for the cantus firmus
            (let (
                (variety-cost (gil::add-int-var-array-dom *sp* (* 3 (- (length (first (notes part))) 2)) (append '(0)(getparam-val 'variety-cost))))
                )
                (compute-variety-cost (first (notes part)) variety-cost)
                (add-cost-to-factors variety-cost 'variety-cost)
            )
        )
    ))

    ; Cost #4
    (print "prefer harmonic triad") ; todo check interdependency with 1st and 2nd cost
    (if (member 4 species-list) ; the 4th species behaves differently, as the note to be considered is the note on the upbeat, and not on the downbeat as the other species
        (progn
            (setq h-triad-cost (gil::add-int-var-array-dom *sp* *cf-last-index (append '(0) (getparam-val 'h-triad-cost))))
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
            (setq h-triad-cost (gil::add-int-var-array-dom *sp* *cf-len (append '(0) (getparam-val 'h-triad-cost))))
            (compute-h-triad-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) h-triad-cost)
        )
    )
    (add-cost-to-factors h-triad-cost 'h-triad-cost)

    (dotimes (i *N-PARTS)
        ; Cost #5, only for 3rd species: if harmonic triad isn't achieved on the downbeat, it shall be on the second or third one
        (if (or (eq (species (nth i parts)) 3) (eq (species (nth i parts)) 5))  (let
            (
                (h-triad-3rd-species-cost (gil::add-int-var-array-dom *sp* (* *cf-last-index 2) (list 0 1)))
            )
            (dotimes (j 2) (progn 
               (compute-h-triad-cost-3rd-species 
                    (nth (+ j 1) (h-intervals (nth i parts))) ; this is the jth beat
                    (subseq h-triad-3rd-species-cost (* j *cf-last-index) (* (+ j 1) *cf-last-index))) ; these are the costs corresponding to the jth beat
            ))
            (add-cost-to-factors h-triad-3rd-species-cost 'h-triad-3rd-species-cost)
        ))
    )    

    ;================================================================================;
    ;                                    RETURN                                      ;
    ;================================================================================;
    (append (fux-search-engine solution-array species-list) (list species-list))
    
)