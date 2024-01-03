(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains all the cp-params interface.
; That is to say the interface blocks, as well as the global variables updated via the interface.

;;;====================
;;;= cp-params OBJECT =
;;;====================

(print "Loading cp-params object...")

(om::defclass! cp-params ()
;attributes
(
    ; ---------- Input cantus firmus ----------
    (cf-voice :accessor cf-voice :initarg :cf-voice :initform nil :documentation "")
    ; ---------- Melodic parameters ----------
    (m-step-cost-param :accessor m-step-cost-param :initform "No cost" :type string :documentation "")
    (m-third-cost-param :accessor m-third-cost-param :initform "Low cost" :type string :documentation "")
    (m-fourth-cost-param :accessor m-fourth-cost-param :initform "Low cost" :type string :documentation "")
    (m-tritone-cost-param :accessor m-tritone-cost-param :initform "Forbidden" :type string :documentation "")
    (m-fifth-cost-param :accessor m-fifth-cost-param :initform "Medium cost" :type string :documentation "")
    (m-sixth-cost-param :accessor m-sixth-cost-param :initform "Medium cost" :type string :documentation "")
    (m-seventh-cost-param :accessor m-seventh-cost-param :initform "Medium cost" :type string :documentation "")
    (m-octave-cost-param :accessor m-octave-cost-param :initform "Low cost" :type string :documentation "")
    ; ---------- Global parameters (species 1) ----------
    (borrow-mode-param :accessor borrow-mode-param :initform "Major" :type string :documentation "")
    (borrow-cost-param :accessor borrow-cost-param :initform "High cost" :type string :documentation "")
    (h-fifth-cost-param :accessor h-fifth-cost-param :initform "Low cost" :type string :documentation "")
    (h-octave-cost-param :accessor h-octave-cost-param :initform "Low cost" :type string :documentation "")
    (con-motion-cost-param :accessor con-motion-cost-param :initform "No cost" :type string :documentation "")
    (obl-motion-cost-param :accessor obl-motion-cost-param :initform "Low cost" :type string :documentation "")
    (dir-motion-cost-param :accessor dir-motion-cost-param :initform "Medium cost" :type string :documentation "")
    (penult-rule-check-param :accessor penult-rule-check-param :initform t :type boolean :documentation "")
    (succ-p-cons-cost-param :accessor succ-p-cons-cost-param :initform "Medium cost" :type string :documentation "")
    ; ---------- Species parameters ----------
    ; Species 2
    (penult-sixth-cost-param :accessor penult-sixth-cost-param :initform "Last resort" :type string :documentation "")
    ; Species 3
    (non-cambiata-cost-param :accessor non-cambiata-cost-param :initform "High cost" :type string :documentation "")
    (two-beats-apart-cost-param :accessor two-beats-apart-cost-param :initform "Low cost" :type string :documentation "")
    (con-m-after-skip-check-param :accessor con-m-after-skip-check-param :initform nil :type boolean :documentation "")
    ; Species 4
    (two-bars-apart-cost-param :accessor two-bars-apart-cost-param :initform "High cost" :type string :documentation "")
    (no-syncopation-cost-param :accessor no-syncopation-cost-param :initform "Last resort" :type string :documentation "")
    ; Species 5
    (pref-species-slider-param :accessor pref-species-slider-param :initform 50 :type integer :documentation "")
    ; ---------- Solver parameters ----------
    (species-param :accessor species-param :initform (list "1st" "1st") :type string :documentation "")
    (voice-type-param :accessor voice-type-param :initform (list "Really far above" "Above") :type string :documentation "")
    (min-skips-slider-param :accessor min-skips-slider-param :initform 0 :type integer :documentation "")
    ; ---------- Output & Stop ----------
    (current-csp :accessor current-csp :initform nil :documentation "")
    (result-voice :accessor result-voice :initarg :result-voice :initform nil :documentation "")
    ; ---------- Cost order --------------
    (no-syncope-order-param :accessor no-syncope-order-param :initform "1" :type integer :documentation "")
    (h-triad-order-param :accessor h-triad-order-param :initform "3" :type integer :documentation "")
    (h-triad-3rd-species-order-param :accessor h-triad-3rd-species-order-param :initform "4" :type integer :documentation "")
    (fifths-order-param :accessor fifths-order-param :initform "7" :type integer :documentation "")
    (octaves-order-param :accessor octaves-order-param :initform "5" :type integer :documentation "")
    (motions-order-param :accessor motions-order-param :initform "12" :type integer :documentation "")
    (direct-move-to-p-cons-order-param :accessor direct-move-to-p-cons-order-param :initform "14" :type integer :documentation "")
    (off-key-order-param :accessor off-key-order-param :initform "8" :type integer :documentation "")
    (m-degrees-order-param :accessor m-degrees-order-param :initform "13" :type integer :documentation "")
    (not-cambiata-order-param :accessor not-cambiata-order-param :initform "11" :type integer :documentation "")
    (m2-eq-zero-order-param :accessor m2-eq-zero-order-param :initform "10" :type integer :documentation "")
    (variety-order-param :accessor variety-order-param :initform "7" :type integer :documentation "")
    (penult-fifth-order-param :accessor penult-fifth-order-param :initform "6" :type integer :documentation "")
    (succ-p-cons-order-param :accessor succ-p-cons-order-param :initform "2" :type integer :documentation "")
    (linear-combination :accessor linear-combination :initform "Linear combination" :type string :documentation "")

)
    (:icon 225)
    (:documentation "This class implements FuxCP.
    FuxCP is a constraints programming based tool aiming to generate counterpoints based on cantus firmus.")
)

; the editor for the object
(defclass params-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self cp-params)) t)
(defmethod om::get-editor-class ((self cp-params)) 'params-editor)

(defmethod om::om-draw-contents ((view params-editor))
    (let* ((object (om::object view)))
        (om::om-with-focused-view view)
    )
)

; this function creates the elements for the main panel
(defun make-main-view (editor)
    ; background colour
    (om::om-set-bg-color editor om::*om-light-gray-color*)
)

; To access the melodizer object, (om::object self)
(defmethod initialize-instance ((self params-editor) &rest args)
    ;;; do what needs to be done by default
    (call-next-method) ; start the search by default?
    (make-interface self)
)

(defun make-interface (editor)
    (let* (
        (melodic-subcosts '(
            (:name "Steps" :value "No cost" :cannot-be-forbidden t :param m-step-cost)
            (:name "Third skips" :value "Low cost"  :param m-third-cost)
            (:name "Fourth leaps" :value "Low cost" :param m-fourth-cost)
            (:name "Tritone leaps" :value "Forbidden" :param m-tritone-cost)
            (:name "Fifth leaps" :value "Medium cost" :param m-fifth-cost)
            (:name "Sixth leaps" :value "Medium cost" :param m-sixth-cost)
            (:name "Seventh leaps" :value "Medium cost" :param m-seventh-cost)
            (:name "Octave leaps" :value "Low cost" :param m-octave-cost)
        ))

        (melodic-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "Melodic Preferences" :name "Melodic cost" :display nil :importance "4" :value nil :subcosts ,melodic-subcosts)
            ;; Add more cost data as needed
        ))

        (motion-subcosts '(
            (:name "Direct motion" :value "Medium cost" :cannot-be-forbidden t :param dir-motion-cost)
            (:name "Oblique motion" :value "Low cost" :param obl-motion-cost)
            (:name "Contrary motion" :value "No cost" :cannot-be-forbidden t :cost con-motion-cost)
        ))

        (general-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "General preferences" :name "Borrowed notes" :display nil :importance "4" :value "High cost" :param borrow-cost)
            (:section "General preferences" :name "Harmonic fifths on the downbeat" :display nil :importance "4" :value "High cost" :param h-fifth-cost)
            (:section "General preferences" :name "Harmonic octaves on the downbeat" :display nil :importance "4" :value "High cost" :param h-octave-cost)
            (:section "General preferences" :name "Successive perfect consonances" :display nil :importance "4" :value "High cost" :param succ-p-cons-cost)
            (:section "General preferences" :name "Repeating notes" :display nil :importance "4" :value "High cost" :param variety-cost) ; TODO VARIETY COST IS NOT IMPLEMENTED
            (:section "General preferences" :name "Not having a harmonic triad" :display nil :importance "4" :value "High cost" :param h-triad-cost) ; TODO HTRIAD IS NOT IMPLEMENTED
            (:section "General preferences" :name "Motion cost" :display nil :importance "4" :value nil :subcosts ,motion-subcosts :param motions-cost)
            (:section "General preferences" :name "Apply specific penultimate note rules" :value "Yes" :special-range ("Yes" "No") :param penult-rule-check)
            
            ;; Add more cost data as needed
        ))

        (specific-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "Second species specific pref." :name "Penultimate downbeat note is a fifth" :importance "1" :value "High cost")
            (:section "Third species specific pref." :name "Use of cambiatas" :importance "2" :value "High cost")
            (:section "Third species specific pref." :name "Force joint contrary melody after skip" :value "No" :special-range ("Yes" "No"))
            (:section "Third and fourth species specific pref." :name "Same note in downbeat and upbeat" :importance "2" :value "High cost")
            (:section "Fourth species specific pref." :name "No ligatures" :importance "3" :value "High cost")
            (:section "Fifth species specific pref." :name "Many quarters (left) or many syncopations (right)" :value 50 :make-slider t)
            ;; Add more cost data as needed
        ))
        )   
        ;; Add the cost table to the main view
        (om::om-add-subviews editor (make-cost-panel editor general-preferences  #|x-offset:|# 0    #|y-offset:|# 0   #|size:|# 500 #|colour:|# om::*azulote*))
        (om::om-add-subviews editor (make-cost-panel editor melodic-preferences  #|x-offset:|# 526  #|y-offset:|# 0   #|size:|# 500 #|colour:|# om::*azulito*))
        (om::om-add-subviews editor (make-cost-panel editor specific-preferences #|x-offset:|# 1052 #|y-offset:|# 0   #|size:|# 500 #|colour:|# (om::make-color-255 230 190 165)))
        (om::om-add-subviews editor (make-explanation-panel editor              #|x-offset:|# 0    #|y-offset:|# 501 #|size:|# 500 #|colour:|# (om::make-color-255 255 240 120)))
        (om::om-add-subviews editor (make-search-params-panel editor      #|x-offset:|# 526  #|y-offset:|# 501 #|size:|# 500 #|colour:|# om::*maq-color*))
        (om::om-add-subviews editor (make-search-buttons      editor      #|x-offset:|# 1052 #|y-offset:|# 501 #|size:|# 500 #|colour:|# om::*workspace-color* melodic-subcosts melodic-preferences motion-subcosts general-preferences  ))

    )
    
    ;; ... (existing code)
    

    editor ; Return the editor
)

(defun make-explanation-panel (editor panel-x-offset panel-y-offset size colour)
    (let* (
        ;; ... (existing code)

        ;; Explanation text
        (explanation-text "This is an explanation of the panel.")

        ;; Create a view for the explanation panel
        (explanation-panel (om::om-make-view 'om::om-view
                                :size (om::om-make-point size 100)
                                :position (om::om-make-point panel-x-offset panel-y-offset)
                                :bg-color colour))
        
        ;; Create a text element for the explanation
        (explanation-label (om::om-make-dialog-item 'om::om-static-text
                                (om::om-make-point 10 10) (om::om-make-point 580 80)
                                explanation-text
                                :font om::*om-default-font1b*))
        )

    ;; Add the text element to the explanation panel
    (om::om-add-subviews explanation-panel explanation-label)

    ;; Add the explanation panel to the main view
    explanation-panel
    )
)

(defun make-cost-panel (editor cost-data panel-x-offset panel-y-offset y-size colour)
  (let* (      
         (cost-table (om::om-make-view 'om::om-view
                       :size (om::om-make-point 525 y-size)
                       :position (om::om-make-point panel-x-offset panel-y-offset)
                       :bg-color colour))
         
         (importance-column (om::om-make-dialog-item 'om::om-static-text
                               (om::om-make-point 275 0) (om::om-make-point 150 20) "Importance"
                               :font om::*om-default-font2b*
         ))
         
         (value-column (om::om-make-dialog-item 'om::om-static-text
                           (om::om-make-point 400 0) (om::om-make-point 150 20) "Value"
                           :font om::*om-default-font2b*
         ))
        )

    ;; Add header columns to the cost table
    (om::om-add-subviews cost-table importance-column value-column)
    
    ;; Populate the cost data dynamically
    (let (
        (current-section nil)
        (y-offset 0)
        )
        (loop for index from 0 below (length cost-data)
            do
            (let* ((cost (nth index cost-data))
                    (section (getf cost :section))
                    (y-position (+ y-offset (* 45 index)))
                    (name (if (getf cost :display) (getf cost :display) (getf cost :name)))
                    (importance (getf cost :importance))
                    (value (getf cost :value))
                    (is-new-section (not (string= current-section section)))
                    )

              ;; Add subsection header if it's a new section
                (when is-new-section
                    (let ((section-label (om::om-make-dialog-item 'om::om-static-text
                                        (om::om-make-point 15 y-position) (om::om-make-point 250 20) section
                                        :font om::*om-default-font2b*)))
                    (om::om-add-subviews cost-table section-label))
                    (setf current-section section)
                    (incf y-offset 35)
                    (incf y-position 35)
                )
                ;; Add the row to the cost table
                (let* (
                    (name-label (om::om-make-dialog-item 'om::om-static-text
                                (om::om-make-point 25 y-position) (om::om-make-point 500 20) name))
                    (importance-popup (om::om-make-dialog-item 'om::pop-up-menu
                                        (om::om-make-point 275 (- y-position 7)) (om::om-make-point 50 20)
                                        (format nil "~A" importance)
                                        :value importance
                                        :range (importance-range)
                                        :di-action #'(lambda (x)
                                            (setf (getf cost :importance) (nth (om::om-get-selected-item-index x) (om::om-get-item-list x)))
                                            (print (getf cost :importance))
                                    )
                                      )
                    )
                    (value-popup (if (getf cost :make-slider)
                        (make-slider cost y-position)
                        (om::om-make-dialog-item 'om::pop-up-menu
                                    (om::om-make-point 345 (- y-position 7)) (om::om-make-point 150 20)
                                    (format nil "~A" value)
                                    :value value
                                    :range (if (getf cost :special-range)
                                        (getf cost :special-range)
                                        (value-range (getf cost :cannot-be-forbidden))
                                    )
                                    :di-action #'(lambda (x)
                                        (setf (getf cost :value) (nth (om::om-get-selected-item-index x) (om::om-get-item-list x)))
                                        (print (getf cost :value))
                                    )
                                 )
                    ))
                )
                    (cond 
                        ((and value importance) (om::om-add-subviews cost-table name-label importance-popup value-popup))
                        (value (om::om-add-subviews cost-table name-label value-popup))
                        (importance (om::om-add-subviews cost-table name-label importance-popup))
                    )
                ) ; end of row
                (print (getf cost :subcosts))
                (print (length (getf cost :subcosts)))
                (if (getf cost :subcosts)
                    (loop for index from 0 below (length (getf cost :subcosts))
                        do
                        (let*  ((cost (nth index (getf cost :subcosts)))
                                (y-position (+ y-position (* 35 (+ 1 index))))
                                (name (concatenate 'string "|---" (getf cost :name)))
                                (value (getf cost :value))
                                )
                                (incf y-offset 35)
                            ;; Add the row to the cost table
                             (let* (
                                (name-label (om::om-make-dialog-item 'om::om-static-text
                                            (om::om-make-point 100 y-position) (om::om-make-point 250 20) name))
                                (value-popup (om::om-make-dialog-item 'om::pop-up-menu
                                                (om::om-make-point 345 (- y-position 7)) (om::om-make-point 150 20)
                                                (format nil "~A" value)
                                                :value value
                                                :range (value-range (getf cost :cannot-be-forbidden))
                                                :di-action #'(lambda (x)
                                                    (setf (getf cost :value) (nth (om::om-get-selected-item-index x) (om::om-get-item-list x)))
                                                    (print (getf cost :value))
                                                )
                                            )
                                )
                                )
                                (om::om-add-subviews cost-table name-label value-popup)
                            ) ; end of subcost row 
                        ) ; end of subcost
                    ) ; end of subcost loop
                )
            ) ; end of cost
        ) ; end of loop
    )
    cost-table
))

(defun make-search-params-panel (editor panel-x-offset panel-y-offset y-size colour)
    (let* (      
        (search-params-panel (om::om-make-view 'om::om-view
                        :size (om::om-make-point 525 y-size)
                        :position (om::om-make-point panel-x-offset panel-y-offset)
                        :bg-color colour))
    )
        (om::om-add-subviews
            search-params-panel
            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 140 2)
            (om::om-make-point 200 20)
            "Solver Configuration"
            :font om::*om-default-font2b*
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 15 50)
            (om::om-make-point 150 20)
            "First voice species"
            :font om::*om-default-font1b*
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 170 50)
            (om::om-make-point 200 20)
            "First voice species"
            :range (list "1st" "2nd" "3rd" "4th" "5th")
            :value (first (species-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (first (species-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 15 100)
            (om::om-make-point 150 20)
            "First voice range"
            :font om::*om-default-font1b*
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 170 100)
            (om::om-make-point 200 20)
            "Voice range"
            :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
            :value (first (voice-type-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (first (voice-type-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 15 150)
            (om::om-make-point 150 20)
            "Second voice species"
            :font om::*om-default-font1b*
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 170 150)
            (om::om-make-point 200 20)
            "Second voice species"
            :range (list "None" "1st" "2nd" "3rd" "4th" "5th")
            :value (second (species-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (second (species-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 15 200)
            (om::om-make-point 150 20)
            "Second voice range"
            :font om::*om-default-font1b*
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 170 200)
            (om::om-make-point 200 20)
            "Second voice range"
            :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
            :value (second (voice-type-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (second (voice-type-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            ) 

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 15 250)
            (om::om-make-point 150 20)
            "Borrowing mode"
            :font om::*om-default-font1b*
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 170 250)
            (om::om-make-point 200 20)
            "Borrowing mode"
            :range (list "None" "Major" "Minor")
            :value (borrow-mode-param (om::object editor))
            :di-action #'(lambda (cost)
                (setf (borrow-mode-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 15 300)
            (om::om-make-point 150 20)
            "Minimum % of skips"
            :font om::*om-default-font1b*
            )

            (om::om-make-dialog-item
            'om::om-slider
            (om::om-make-point 170 300)
            (om::om-make-point 200 20)
            "Minimum % of skips"
            :range '(0 100)
            :increment 1
            :value (min-skips-slider-param (om::object editor))
            :di-action #'(lambda (s)
                (setf (min-skips-slider-param (om::object editor)) (om::om-slider-value s))
            )
            )
        )
        search-params-panel
    )
)

(defun make-search-buttons (editor panel-x-offset panel-y-offset y-size colour melodic-subcosts)
    (let* (      
        (search-buttons (om::om-make-view 'om::om-view
                        :size (om::om-make-point 525 y-size)
                        :position (om::om-make-point panel-x-offset panel-y-offset)
                        :bg-color colour))
        )
        (om::om-add-subviews
            search-buttons
            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 140 5)
            (om::om-make-point 150 20)
            "Solver Launcher"
            :font om::*om-default-font3b*
            )

            (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 55 30) ; position (horizontal, vertical)
            (om::om-make-point 160 20) ; size (horizontal, vertical)
            "Save Config"
            :di-action #'(lambda (b)
                (if (null (cf-voice (om::object editor))); if the problem is not initialized
                    (error "No voice has been given to the solver. Please set a cantus firmus into the second input and try again.")
                )

                (set-global-cf-variables
                    (cf-voice (om::object editor))
                    (borrow-mode-param (om::object editor))
                )
                (defparameter *params* (make-hash-table))
                ;; set melodic parameters
                (dolist (subcost melodic-subcosts)
                    (setparam-cost (getf subcost :param) (getf subcost :value))
                )

                (dolist (cost general-costs)
                    (if (= (getf cost :param) 'motions-cost)
                        (error cost) ;nil ; it is tread by the subcosts
                        (if (= (get cost :param 'penult-rule-check))
                            (error cost) ; todo
                            (setparam-cost (getf subcost :param) (getf subcost :value))
                        )
                    )
                )


                ;; set species specific parameters
                (setparam-cost 'penult-sixth-cost (penult-sixth-cost-param (om::object editor)))
                (setparam-cost 'non-cambiata-cost (non-cambiata-cost-param (om::object editor)))
                (setparam-cost 'two-beats-apart-cost (two-beats-apart-cost-param (om::object editor)))
                (setparam-yes-no 'con-m-after-skip-check (con-m-after-skip-check-param (om::object editor)))
                (setparam-cost 'two-bars-apart-cost (two-bars-apart-cost-param (om::object editor)))
                (setparam-cost 'no-syncopation-cost (no-syncopation-cost-param (om::object editor)))
                (setparam-slider 'pref-species-slider (pref-species-slider-param (om::object editor)))

                ;; set search parameters
                (setparam-slider 'min-skips-slider (min-skips-slider-param (om::object editor)))
                (setparam 'borrow-mode (borrow-mode-param (om::object editor)))


                ;; preferences for the cost order
                (defparameter *cost-preferences* (make-hash-table))
                (setf (gethash 'no-syncope-cost *cost-preferences*)            (no-syncope-order-param (om::object editor)))
                (setf (gethash 'h-triad-cost *cost-preferences*)               (h-triad-order-param (om::object editor)))
                (setf (gethash 'h-triad-3rd-species-cost *cost-preferences*)   (h-triad-3rd-species-order-param (om::object editor)))
                (setf (gethash 'fifth-cost *cost-preferences*)                 (fifths-order-param (om::object editor)))
                (setf (gethash 'octave-cost *cost-preferences*)                (octaves-order-param (om::object editor)))
                (setf (gethash 'motions-cost *cost-preferences*)               (motions-order-param (om::object editor)))
                (setf (gethash 'direct-move-to-p-cons-cost *cost-preferences*) (direct-move-to-p-cons-order-param (om::object editor)))
                (setf (gethash 'off-key-cost *cost-preferences*)               (off-key-order-param (om::object editor)))
                (setf (gethash 'm-degrees-cost *cost-preferences*)             (m-degrees-order-param (om::object editor)))
                (setf (gethash 'not-cambiata-cost *cost-preferences*)          (not-cambiata-order-param (om::object editor)))
                (setf (gethash 'm2-eq-zero-cost *cost-preferences*)            (m2-eq-zero-order-param (om::object editor)))
                (setf (gethash 'variety-cost *cost-preferences*)               (variety-order-param (om::object editor)))
                (setf (gethash 'penult-thesis-cost *cost-preferences*)         (penult-fifth-order-param (om::object editor)))
                (setf (gethash 'succ-p-cons-cost *cost-preferences*)           (succ-p-cons-order-param (om::object editor)))
                (if (string= "Linear combination" (linear-combination (om::object editor))) 
                    (setf *linear-combination t)
                    (setf *linear-combination nil)
                )

                
                (setf species-integer-list (convert-to-species-integer-list (species-param (om::object editor))))
                (setf *voices-types (convert-to-voice-integer-list (voice-type-param (om::object editor))))
                (setf (current-csp (om::object editor)) (fux-cp species-integer-list))
            )
            )

            (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 55 70) ; position
            (om::om-make-point 160 20) ; size
            "Next Solution"
            :di-action #'(lambda (b)
                (if (typep (current-csp (om::object editor)) 'null); if the problem is not initialized
                    (error "The problem has not been initialized. Please set the input and press Start.")
                )
                (print "Searching for the next solution")
                ;reset the boolean because we want to continue the search
                (setparam 'is-stopped nil)
                ;get the next solution
                (mp:process-run-function ; start a new thread for the execution of the next method
                    "solver-thread" ; name of the thread, not necessary but useful for debugging
                    nil ; process initialization keywords, not needed here
                    (lambda () ; function to call
                        (setf
                            (result-voice (om::object editor))
                            (search-next-fux-cp (current-csp (om::object editor)))
                        )
                        (om::openeditorframe ; open a voice window displaying the solution
                            (om::omNG-make-new-instance (result-voice (om::object editor)) "Current solution")
                        )
                    )
                )
            )
            )

            (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 215 70) ; position
            (om::om-make-point 160 20) ; size
            "Best Solution"
            :di-action #'(lambda (b)
                (if (typep (current-csp (om::object editor)) 'null); if the problem is not initialized
                    (error "The problem has not been initialized. Please set the input and press Start.")
                )
                (print "Searching for the best solution")
                ;reset the boolean because we want to continue the search
                (setparam 'is-stopped nil)
                ;get the next solution
                (mp:process-run-function ; start a new thread for the execution of the next method
                    "solver-thread" ; name of the thread, not necessary but useful for debugging
                    nil ; process initialization keywords, not needed here
                    (lambda () ; function to call
                        (let ((check 1) (result nil))
                            (loop while check do
                                (setf result (search-next-fux-cp (current-csp (om::object editor))))
                                (if result (setf (result-voice (om::object editor)) result) (setf check nil))
                            )
                        )
                        ;(om::openeditorframe ; open a voice window displaying the solution
                        ;    (om::omNG-make-new-instance (result-voice (om::object editor)) "Current solution")
                        ;)
                    )
                )
            )
            )

            (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 215 30) ; position (horizontal, vertical)
            (om::om-make-point 160 20) ; size (horizontal, vertical)
            "Stop"
            :di-action #'(lambda (b)
                (setparam 'is-stopped t)
            )
            )
        )
        search-buttons
    )
)

(defun make-slider (cost y-position)
    (om::om-make-dialog-item
    'om::om-slider
    (om::om-make-point 350 (- y-position 3))
    (om::om-make-point 150 20)
    "5th: Preference to a lot of quarters [left] OR a lot of syncopations [right]"
    :range '(0 100)
    :increment 1
    :value (getf cost :value)
    :di-action #'(lambda (s)
        (setf (getf cost :value) (om::om-slider-value s))
                                        (print (getf cost :value))
    )
    )
)

; return the list of available costs for the preferences
; @is-required: if true, "Forbidden" is removed
(defun value-range (&optional (is-required nil))
    (let (
        (costs (list "No cost" "Low cost" "Medium cost" "High cost" "Last resort" "Cost prop. to length" "Forbidden"))
    )
        (if is-required
            (butlast costs)
            costs
        )
    )
)

(defun importance-range ()
    (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
)

; set the value @v in the hash table @h with key @k
(defun seth (h k v)
    (setf (gethash k h) v)
)

; set the value @v in the parameters with key @k
(defun setparam (k v)
    (seth *params* k v)
)

(defun setparam-yes-no (k v)
    (let ((converted (if (string= "Yes" v)
                    t
                    nil)))
        (setparam k converted)
    )
)

; set the cost-converted value @of v in the parameters with key @k
(defun setparam-cost (k v)
    (setparam k (convert-to-cost-integer v))
)

; set the species-converted value @of v in the parameters with key @k
(defun setparam-species (k v)
    (setparam k (convert-to-species-integer v))
)

; set the slider-converted value @of v in the parameters with key @k
(defun setparam-slider (k v)
    (setparam k (convert-to-percent v))
)

; convert a cost to an integer
(defun convert-to-cost-integer (param)
    (cond
    ((equal param "No cost") 0)
    ((equal param "Low cost") 1)
    ((equal param "Medium cost") 2)
    ((equal param "High cost") 4)
    ((equal param "Last resort") 8)
    ((equal param "Cost prop. to length") (* 2 *cf-len))
    ((equal param "Forbidden") (* 64 *cf-len))
    )
)

; convert a species to an integer
(defun convert-to-species-integer-list (param-list)
    (let (
        (species-list '())
        )
        (dolist (param param-list) 
        (progn 
            (cond
                ((equal param "1st") (setf species-list (append species-list '(1))))
                ((equal param "2nd") (setf species-list (append species-list '(2))))
                ((equal param "3rd") (setf species-list (append species-list '(3))))
                ((equal param "4th") (setf species-list (append species-list '(4))))
                ((equal param "5th") (setf species-list (append species-list '(5))))
                ((equal param "None") nil)
            )
        ))
        (setq *N-COUNTERPOINTS (length species-list))
        (setq *N-PARTS (+ 1 (length species-list)))
        species-list
    )
)

;; convert the string for the voice type to an integer
;; belong to {"Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below"}
;; convert to {-3 -2 -1 0 1 2 3}
(defun convert-to-voice-integer-list (params)
    (let ((integer-list (make-list *N-COUNTERPOINTS :initial-element nil))) (loop for i from 0 below *N-COUNTERPOINTS do
        (cond
            ((equal (nth i params) "Really far above") (setf (nth i integer-list) 3))
            ((equal (nth i params) "Far above") (setf (nth i integer-list) 2))
            ((equal (nth i params) "Above") (setf (nth i integer-list) 1))
            ((equal (nth i params) "Same range") (setf (nth i integer-list) 0))
            ((equal (nth i params) "Below") (setf (nth i integer-list) -1))
            ((equal (nth i params) "Far below") (setf (nth i integer-list) -2))
            ((equal (nth i params) "Really far below") (setf (nth i integer-list) -3))
        )
    )
    integer-list
    )
)

; convert a slider value to a percentage
(defun convert-to-percent (param)
    (float (/ param 100))
)

; convert a mode to an integer
(defun convert-to-mode-integer (param tone)
    (cond
    ((equal param "Major") (mod tone 12))
    ((equal param "Minor") (mod (+ tone 3) 12))
    ((equal param "None") nil)
    )
)

; define all the global variables
(defun set-global-cf-variables (cantus-firmus borrow-mode)
    (defparameter *prev-sol-check nil)
    (defparameter rythmic+pitches nil)
    (defparameter rythmic-om nil)
    (defparameter pitches-om nil)
    ; get the tonalite of the cantus firmus
    (defparameter *tonalite-offset (get-tone-offset cantus-firmus))
    ; get the *scale of the cantus firmus
    (defparameter *scale (build-scaleset (get-scale) *tonalite-offset))
    ; *chromatic *scale
    (defparameter *chromatic-scale (build-scaleset (get-scale "chromatic") *tonalite-offset))
    ; get the first note of each chord of the cantus firmus
    (defparameter *cf (mapcar #'first (to-pitch-list (om::chords cantus-firmus))))
    ; get the tempo of the cantus firmus
    (defparameter *cf-tempo (om::tempo cantus-firmus))
    ; get the first note of the cantus firmus ;; just used for the moment
    (defparameter *tone-pitch-cf (first *cf))
    ; get the borrowed scale of the cantus firmus, i.e. some notes borrowed from the natural scale of the tone (useful for modes)
    (setq mode-param (convert-to-mode-integer borrow-mode *tone-pitch-cf))
    (if mode-param
        (defparameter *borrowed-scale (build-scaleset (get-scale "borrowed") mode-param))
        (defparameter *borrowed-scale (list))
    )
    ; get notes that are not in the natural scale of the tone
    (defparameter *off-scale (set-difference *chromatic-scale *scale))
    ; length of the cantus firmus
    (defparameter *cf-len (length *cf))
    ; *cf-last-index is the number of melodic intervals in the cantus firmus
    (defparameter *cf-last-index (- *cf-len 1))
    ; *cf-penult-index is the number of larger (n -> n+2) melodic intervals in the cantus firmus
    (defparameter *cf-penult-index (- *cf-len 2))
    ; COST_UB is the upper bound of the cost function
    (defparameter COST_UB (* *cf-len 20))
    ; *N-COUNTERPOINTS is the number of counterpoints in the counterpoint
    (defparameter *N-COUNTERPOINTS -1) ; will be defined when parsing the input
)