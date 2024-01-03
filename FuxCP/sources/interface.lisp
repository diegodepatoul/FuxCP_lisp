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
            (:name "Steps" :value "No cost" :cannot-be-forbidden t)
            (:name "Third skips" :value "Low cost")
            (:name "Fourth leaps" :value "Low cost")
            (:name "Tritons leaps" :value "Forbidden")
            (:name "Fifths leaps" :value "Medium cost")
            (:name "Sixths leaps" :value "Medium cost")
            (:name "Sevenths leaps" :value "Medium cost")
            (:name "Octaves leaps" :value "Low cost")
        ))

        (melodic-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "Melodic Preferences" :name "Melodic cost" :display nil :importance "4" :value nil :subcosts ,melodic-subcosts)
            ;; Add more cost data as needed
        ))

        (motion-subcosts '(
            (:name "Direct motion" :value "Medium cost" :cannot-be-forbidden t)
            (:name "Oblique motion" :value "Low cost")
            (:name "Contrary motion" :value "No cost" :cannot-be-forbidden t)
        ))

        (general-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "General preferences" :name "Borrowed notes" :display nil :importance "4" :value "High cost")
            (:section "General preferences" :name "Harmonic fifths on the downbeat" :display nil :importance "4" :value "High cost")
            (:section "General preferences" :name "Harmonic octaves on the downbeat" :display nil :importance "4" :value "High cost")
            (:section "General preferences" :name "Successive perfect consonances" :display nil :importance "4" :value "High cost")
            (:section "General preferences" :name "Repeating notes" :display nil :importance "4" :value "High cost")
            (:section "General preferences" :name "Not having a harmonic triad" :display nil :importance "4" :value "High cost")
            (:section "General preferences" :name "Motion cost" :display nil :importance "4" :value nil :subcosts ,motion-subcosts)
            (:section "General preferences" :name "Apply specific penultimate note rules" :value "Yes" :special-range ("Yes" "No"))
            
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
    (om::om-add-subviews editor (make-cost-panel general-preferences  #|x-offset:|# 0    #|y-offset:|# 0   #|size:|# 500 #|colour:|# om::*azulote*))
    (om::om-add-subviews editor (make-cost-panel melodic-preferences  #|x-offset:|# 526  #|y-offset:|# 0   #|size:|# 500 #|colour:|# om::*azulito*))
    (om::om-add-subviews editor (make-cost-panel specific-preferences #|x-offset:|# 1052 #|y-offset:|# 0   #|size:|# 500 #|colour:|# (om::make-color-255 230 190 165)))
    (om::om-add-subviews editor (make-explanation-panel               #|x-offset:|# 0    #|y-offset:|# 501 #|size:|# 500 #|colour:|# (om::make-color-255 230 190 165)))
    )
    
    ;; ... (existing code)
    

    editor ; Return the editor
)

(defun make-explanation-panel (panel-x-offset panel-y-offset size colour)
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

(defun make-cost-panel (cost-data panel-x-offset panel-y-offset y-size colour)
  (let* (
         ;; ... (existing code)

         ;; Sample data for demonstration purposes

         
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