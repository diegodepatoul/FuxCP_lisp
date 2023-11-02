(in-package :fuxcp)
(defun backup-cp2-values ()
    ; Backuping all the second counterpoint variables 
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
)

(defun create-2v-cp (cp1 cp2 total-cp)
    (setf total-cp-len (* 2 *cf-len))
    (setf total-cp (gil::add-int-var-array *sp* total-cp-len 0 127)) 
    ;(merge-cp-same-len (list (first *cp) (first *cp2)) *total-cp) ; merge the two counterpoint arrays into one
    ;(append-cp (list (first *cp) (first *cp2)) *total-cp) ; merge the two counterpoint arrays into one
    (setf total-cp (append (first cp1) (first cp2)))
)