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
(defun fux-cp-7th (&optional (species 7))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"
    ;; ADD FIRST SPECIES CONSTRAINTS
    (fux-cp-6th 7)
    (fux-cp-2nd 2)
    ; Joining both counterpoints in a single array
    (setq *total-cp-len (* 2 *cf-len))
    (setq *total-cp (gil::add-int-var-array *sp* *total-cp-len 0 127)) 
    (merge-cp-same-len (list (first *cp) (first *cp2)) *total-cp) ; merge the two counterpoint arrays into one


    (print "########## SEVENTH SPECIES ##########")

    ; RETURN
    (if (eq species 7)
        ; then create the search engine
        (append (fux-search-engine *total-cp 7) (list species))
        ; else
        nil
    )
)