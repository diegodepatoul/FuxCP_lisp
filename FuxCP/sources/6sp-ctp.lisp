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
    (print *cp)
    (fux-cp-1st 6)
    (print *cp)
    (setf *cp2 (copy-list *cp))
    (setf *h-intervals2 (copy-list *h-intervals))
    (fux-cp-1st 6)
    (print *cp)
    (print *cp2)

    (setq *total-cp-len (* 2 *cf-len))
    (setq *total-cp (gil::add-int-var-array *sp* *total-cp-len 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (append-cp (list (first *cp) (first *cp2)) *total-cp) ; merge the two counterpoint arrays into one

    (print "no unisson between cp1 and cp2")
    (add-no-unisson-cst (first *cp) (first *cp2))



    ; RETURN
    (if (eq species 6)
        ; then create the search engine
        ;(append (fux-search-engine (first *cp) 6) (list species))
        (append (fux-search-engine *total-cp 6) (list species))
        ; else
        nil
    )
)