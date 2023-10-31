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
    ;; compute the second counterpoint (1st species)
    (fux-cp-1st 6)

    ; saving values of cp2 and now working with cp1
    (backup-cp2-values)

    ; compute the first counterpoint (2nd species)
    (fux-cp-2nd 2)
    ; Joining both counterpoints in a single array
    (create-2v-cp)


    (print "########## SEVENTH SPECIES ##########")

    ; RETURN
    (if (eq species 7)
        ; then create the search engine
        (append (fux-search-engine *total-cp 7) (list species))
        ; else
        nil
    )
)