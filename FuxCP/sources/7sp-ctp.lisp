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


    #|
    (setf h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-1)) (first (cp counterpoint-2)) h-intervals-1-2)
    (add-h-cons-cst *cf-len *cf-penult-index h-intervals-1-2 PENULT_THESIS_VAR)
 |#
 
    ; RETURN
    (if (eq species 7)
        ; then create the search engine
        (append (fux-search-engine solution-array 7) (list species))
        ; else
        nil
    )
)