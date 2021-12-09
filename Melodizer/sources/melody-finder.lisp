(in-package :mldz)

;;;====================
;;;= MELODIZER OBJECT =
;;;====================

(om::defclass! melodizer () 
  ;attributes
  ((input-chords :accessor input-chords :initarg :input-chords :initform (make-instance 'voice) :documentation "The input chords on top of which the melody will be played in the form of a voice object.")
    (input-rhythm :accessor input-rhythm :initarg :input-rhythm :initform (make-instance 'voice) :documentation "The rhythm of the melody in the form of a voice object. ")
    (key :accessor key :initarg :key :initform 60 :documentation "The key the melody is in (default : C).")
    (mode :accessor mode :initarg :mode :initform "ionian (major)" :documentation "The mode the melody is in (default : major).")
    (tool-mode :accessor tool-mode :initarg :tool-mode :initform "Melody-Finder" :documentation "The mode of the tool, e.g given Melody-Finder if we want to find a melody, Accompagnement-Finder if we want to find an accompagnement, Ornement if we want to complexify the melody,...")
    (variety :accessor variety :initarg :variety :initform 1 :documentation "The minimal variety we want for the solution, expressed as a number of notes.")
    (global-interval :accessor global-interval :initarg :global-interval :initform "1" :documentation "global interval that the produced melody should cover")
    (optional-constraints :accessor optional-constraints :initarg :optional-constraints :initform (list) :documentation "a list of booleans telling if the optional constraint should be added to the problem")
    (result :accessor result :initarg :result :initform (list) :documentation "A temporary list holder to store the result of the call to melody-finder, shouldn't be touched.")
    (stop-search :accessor stop-search :initarg :stop-search :initform nil :documentation "A boolean variable to tell if the user wishes to stop the search or not.")
    (solution :accessor solution :initarg :solution :initform nil :documentation "The current solution of the CSP in the form of a voice object.")
    (solutions-list :accessor solutions-list :initarg :solution-list :initform '() :documentation "The list of all the solutions saved by the user.")
    (motives-list :accessor motives-list :initarg :motives-list :initform '() :documentation "The list of motives created by the user")
    (phrases-list :accessor phrases-list :initarg :phrases-list :initform '() :documentation "The list of phrases created by the user")
    (periods-list :accessor periods-list :initarg :periods-list :initform '() :documentation "The list of periodes created by the user")
    (output-solution :accessor output-solution :initarg :output-solution :initform nil :documentation "The selected solution.")
    (output-motif :accessor output-motif :initarg :output-motif :initform nil :documentation "The selected motif")
    (output-phrase :accessor output-phrase :initarg :output-phrase :initform nil :documentation "The selected phrase")
    (output-period :accessor output-period :initarg :output-period :initform nil :documentation "The selected period")
    (melody :accessor melody :initarg :melody :initform nil :documentation "The final melody produced by the object")
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2")
  )
  (:icon 1)
  (:doc "This class implements melodizer.
        UPDATE THIS to a complete description of the tool")
)


; the editor for the object
(defclass melodizer-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self melodizer)) t)
(defmethod om::get-editor-class ((self melodizer)) 'melodizer-editor)

(defmethod om::om-draw-contents ((view melodizer-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view 
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

; To access the melodizer object, (object self)

(defmethod initialize-instance ((self melodizer-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?, calculate the list of fundamentals, seconds,...
  (make-my-interface self)
)

; function to create the tool's interface
(defmethod make-my-interface ((self melodizer-editor))
  
  ; create the main view of the object
  (make-main-view self)

  ;;;;;;;;;;;;;;;;;
  ;;; sub views ;;;
  ;;;;;;;;;;;;;;;;;

  (let* 
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ; The coordinates here are coordinates in the main view
      (input-panel (om::om-make-view 'om::om-view ; part of the display for everything that has to do with input
        :size (om::om-make-point 450 300)
        :position (om::om-make-point 5 30)
        :bg-color om::*azulito*) 
      )
      (search-panel (om::om-make-view 'om::om-view ; part of the display for everything that has to do with the search for solutions
        :size (om::om-make-point 450 300)
        :position (om::om-make-point 460 30)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view ; part of the display for everything that has to do with adding new constraints to the problem
        :size (om::om-make-point 905 400)
        :position (om::om-make-point 5 335)
        :bg-color om::*azulito*)
      )
      (solution-assembly-panel (om::om-make-view 'om::om-view ; part of the display to put different solutions together
        :size (om::om-make-point 450 705)
        :position (om::om-make-point 915 30)
        :bg-color om::*azulito*)
      )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different sub-panels for additional constraints ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (general-constraints-panel
        (om::om-make-view
          'om::om-view
          :size (om::om-make-point 220 375)
          :position (om::om-make-point 5 20)
          :bg-color (om::om-make-color 0 0.6 0.3))
      )
      (motif-maker-constraints-panel
        (om::om-make-view
          'om::om-view
          :size (om::om-make-point 220 375)
          :position (om::om-make-point 230 20)
          :bg-color (om::om-make-color 0 0.6 0.3))
      )
      (phrase-maker-constraints-panel
        (om::om-make-view
          'om::om-view
          :size (om::om-make-point 220 375)
          :position (om::om-make-point 455 20)
          :bg-color (om::om-make-color 0 0.6 0.3))
      )
      (period-maker-constraints-panel
        (om::om-make-view
          'om::om-view
          :size (om::om-make-point 220 375)
          :position (om::om-make-point 680 20)
          :bg-color (om::om-make-color 0 0.6 0.3))
      )
    )

    ; create the input panel
    (setf elements-input-panel (make-input-panel self input-panel))

    ; create the search panel
    (setf elements-search-panel (make-search-panel self search-panel solution-assembly-panel))

    ; create the constraints panel
    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))
    ;create the general constraints panel   
    (setf elements-general-constraints-panel (make-general-constraints-panel self general-constraints-panel))
    ; create the motif maker constraints panel
    (setf elements-motif-maker-constraints-panel (make-motif-maker-constraints-panel self motif-maker-constraints-panel))
    ; create the phrase maker constraints panel
    (setf elements-phrase-maker-constraints-panel (make-phrase-maker-constraints-panel self phrase-maker-constraints-panel))
    ; create the period maker constraints panel
    (setf elements-period-maker-constraints-panel (make-period-maker-constraints-panel self period-maker-constraints-panel))
    
    ; create the solution assembly panel
    (setf elements-solution-assembly-panel (make-solution-assembly-panel self solution-assembly-panel))

    ;;; add new panel here


    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      input-panel
      search-panel
      constraints-panel
      solution-assembly-panel
    )
    ; add subviews to the constraints panel
    (om::om-add-subviews 
      constraints-panel
      general-constraints-panel
      motif-maker-constraints-panel
      phrase-maker-constraints-panel
      period-maker-constraints-panel
    )
  )
  ; return the editor
  self
)



    ;;;;;;;;;;;;;;;;;
    ;;; main view ;;;
    ;;;;;;;;;;;;;;;;;

; this function creates the elements for the main panel
(defun make-main-view (editor)
  ; background colour
  (om::om-set-bg-color editor om::*om-light-gray-color*) ;;pour changer le bg color. om peut fabriquer sa propre couleur: (om-make-color r g b)

  ; title
  (om::om-add-subviews
    editor
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 650 2) 
      (om::om-make-point 120 20) 
      "Melodizer"
      :font om::*om-default-font3b*
    )
  )
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the input panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this function creates all the elements of the input-panel (buttons, pop-up-menus,...)
; the coordinates are local to the input panel
(defun make-input-panel (editor input-panel)
  (om::om-add-subviews
    input-panel

    ; title
    (om::om-make-dialog-item 
       'om::om-static-text 
      (om::om-make-point 140 2) 
      (om::om-make-point 120 20) 
      "Parameters"
      :font om::*om-default-font1b*
    )

    ;pop-up list to select the mode of the tool (melodizer, accompagnement finder, ...)
    (om::om-make-dialog-item 
      'om::pop-up-menu 
      (om::om-make-point 5 25) 
      (om::om-make-point 200 20) 
      "Tool Mode selection"
      :value(tool-mode (om::object editor))
      :range '("Melody-Finder" "Motif Maker" "Phrase Maker" "Period Maker")
      :di-action #'(lambda (m) ; TODO reset all additional constraints that are not general
                    ;(print (nth (om-get-selected-item-index m) (om-get-item-list m))); display the selected option
                    (setf (tool-mode (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))) ; set the tool-mode according to the choice of the user
      ) 
    )

    ;pop-up list to select the key of the melody
    (om::om-make-dialog-item 
      'om::pop-up-menu 
      (om::om-make-point 5 60) 
      (om::om-make-point 200 20) 
      "Key selection"
      :range '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (note-value-to-name (key (om::object editor)))
      :di-action #'(lambda (m)
        (setf (key (om::object editor)) (name-to-note-value (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))) ; set the key according to the choice of the user
      )
    )

    ;pop-up list to select the mode of the melody
    (om::om-make-dialog-item 
      'om::pop-up-menu 
      (om::om-make-point 5 85) 
      (om::om-make-point 200 20) 
      "Mode selection"
      :range '("ionian (major)" "dorian" "phrygian" "lydian" "mixolydian" "aeolian (natural minor)" "locrian" "pentatonic" "harmonic minor" "chromatic")
      :value (mode (om::object editor))
      :di-action #'(lambda (m)
        (setf (mode (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))) ; set the mode according to the choice of the user
      )
    )

    ;button to edit the input chords
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 200 60)
      (om::om-make-point 200 20)
      "Edit input chords"
      :di-action #'(lambda (b)
        (om::openeditorframe ; open a voice window displaying the input chords
          (om::omNG-make-new-instance 
            (input-chords (om::object editor))
            "input chords" ; name of the window
          ); changes are saved when closed
        )
      )
    )

    ;button to edit the input rhythm
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 200 85)
      (om::om-make-point 200 20)
      "Edit melody rhythm"
      :di-action #'(lambda (b)
        (om::openeditorframe ; open a voice window displaying the input rhythm
          (om::omNG-make-new-instance 
            (input-rhythm (om::object editor))
            "input rhythm" ; name of the window
          )
        )
      )
    )

    ;text for the slider
    (om::om-make-dialog-item
      'om::om-static-text 
      (om::om-make-point 10 115) 
      (om::om-make-point 200 20) 
      "Variety of the solutions"
      :font om::*om-default-font1*
    )

    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 5 135) ; position
      (om::om-make-point 200 20) ; size
      "Slider"
      :range '(1 100)
      :increment 1
      :value (* 100 (/ (variety (om::object editor)) (n-pulses (input-rhythm (om::object editor)))))
      :di-action #'(lambda (s)
                    ; set the value of variety to  the value of the pointer times n-values / 100, rounded down
                    (setf 
                      (variety (om::object editor))
                      (floor ;division rounding down
                        (*
                          (om::om-slider-value s)
                          (om::n-pulses (input-rhythm (om::object editor)))
                        )
                        100
                      )
                    )
                    ;(print (variety (om::object editor)))
      )
    )

    ; button to reset the input 
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 200 25)
      (om::om-make-point 200 20)
      "Reset input"
      :di-action #'(lambda (b)
        (setf (input-chords (om::object editor)) (make-instance 'voice))
        (setf (input-rhythm (om::object editor)) (make-instance 'voice))
        (setf (key (om::object editor)) 60)
        (setf (mode (om::object editor)) "ionian (major)")
        (setf (tool-mode (om::object editor)) "Melody-Finder") 
        (setf (variety (om::object editor)) 0)
      )
    )
  )
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the search panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this function creates all the elements of the search-panel (buttons, pop-up-menus,...)
; coordinates here are local to search-panel
(defun make-search-panel (editor search-panel solution-assembly-panel)
  (om::om-add-subviews
    search-panel

    ; title
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 190 2) 
      (om::om-make-point 120 20) 
      "Search"
      :font om::*om-default-font1b*
    )

    ;pop-up list to select the desired solution
    ;this is only for the start, as a new pop-up menu is created with every new solution
    ;/!\ if you move this, you also have to move the new ones that are generating every time the list is modified! see update-pop-up function
    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 5 130)
      (om::om-make-point 320 20)
      "Solution selection"
      :range (solutions-list (om::object editor))
      :di-action #'(lambda (m)
        (setf (output-solution (om::object editor)) (nth (om::om-get-selected-item-index m) (solutions-list (om::object editor)))); set the output to the selected solution
      )
    )

    ; button to see the selected solution from the ones we keep with the input chords

    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 330 130)
      (om::om-make-point 100 20)
      "See with chords"
      :di-action #'(lambda (b)
                    (om::openeditorframe ; open the editor of the selected solution
                      (om::omNG-make-new-instance 
                        (make-instance 
                          'poly ; the selected voice object a poly with the solution and the input chords
                          :voices (list 
                            (output-solution (om::object editor))
                            (input-chords (om::object editor))
                          )
                        )
                        (format nil "solution"); name of the window
                      )
                    )
      )
    )

    ; button to start or restart the search
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 5 50) ; position (horizontal, vertical)
      (om::om-make-point 100 20) ; size (horizontal, vertical)
      "Start"
      :di-action #'(lambda (b) 
                    ;(dolist (e (chords (input-chords (object editor))))
                    ;  (print (lmidic e))
                    ;)
                    ; reset the solutions for the new search
                    (setf (solutions-list (om::object editor)) '())
                    (setf (solution (om::object editor)) nil)
                    (progn
                      (update-pop-up editor search-panel (solutions-list (om::object editor)) (om::om-make-point 5 130) (om::om-make-point 320 20) "output-solution")
                      (oa::om-invalidate-view editor)
                    )
                    ; reset the boolean that tells wether we want to stop the search or not
                    (setf (stop-search (om::object editor)) nil)
                    (cond
                      ((string-equal (tool-mode (om::object editor)) "Melody-Finder"); melody finder mode, where the user gives as input a voice with chords
                        (let init; list to take the result of the call to melody-finder
                          (setq init (melody-finder (input-chords (om::object editor)) (input-rhythm (om::object editor)) (optional-constraints (om::object editor)) (global-interval (om::object editor)) (key (om::object editor)) (mode (om::object editor)))); get the search engine and the first solution of the CSP
                          ; update the fields of the object to their new value
                          (setf (result (om::object editor)) init); store the result of the call to melody finder
                        )
                      )
                      ((string-equal (tool-mode (om::object editor)) "Motif Maker")
                        (print "This mode is not supported yet")
                      )
                      ((string-equal (tool-mode (om::object editor)) "Phrase Maker")
                        (print "This mode is not supported yet")
                      )
                      ((string-equal (tool-mode (om::object editor)) "Period Maker")
                        (print "This mode is not supported yet")
                      )
                    )
      )
    )

    ; button to find the next solution
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 115 50) ; position
      (om::om-make-point 100 20) ; size
      "Next"
      :di-action #'(lambda (b)
                    (print "Searching for the next solution")
                    ;reset the boolean because we want to continue the search
                    (setf (stop-search (om::object editor)) nil)
                    ;get the next solution  
                    (mp:process-run-function ; start a new thread for the execution of the next method
                      "next thread" ; name of the thread, not necessary but useful for debugging
                      nil ; process initialization keywords, not needed here
                      (lambda () ; function to call
                        (setf (solution (om::object editor)) (search-next-melody-finder (result (om::object editor)) (om::tree (input-rhythm (om::object editor))) (om::object editor)))
                        (setf (om::tempo (solution (om::object editor))) (om::tempo (input-rhythm (om::object editor)))); set the tempo of the new voice object to be the same as the input
                        (om::openeditorframe ; open a voice window displaying the solution
                          (om::omNG-make-new-instance 
                          (solution (om::object editor)); the new solution
                          "current solution" ; name of the window
                          )
                        )
                      )
                      ; arguments if necessary
                    )
      )
    )

    ; button to stop the search if the user wishes to
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 225 50)
      (om::om-make-point 100 20)
      "Stop"
      :di-action #'(lambda (b)
        (setf (stop-search (om::object editor)) t) ; set the boolean to true so when the timestop object tells the search engine it stopped, it can check and see the user stopped the search
      )
    )

    ; button to open the voice object editor of the current solution
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 90); position
      (om::om-make-point 160 20); size
      "See solution"
      :di-action #'(lambda (b)
                      ;(print (solution (om::object editor)))
                      (om::openeditorframe ; open a voice window displaying the selected solution
                        (om::omNG-make-new-instance 
                          (solution (om::object editor)); the last solution
                          "current solution" ; name of the window
                        )
                      )
      )  
    )

    ;button to add the solution to the list of solutions (if we find it interesting and want to keep it)
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 165 90) ; position
      (om::om-make-point 160 20) ; size
      "Keep Solution"
      :di-action #'(lambda (b)
                    (if (typep (solution (om::object editor)) 'null); if there is no solution to add
                      (error "There is no solution to keep.")
                    )
                    ;add the element to the list
                    (if (typep (solutions-list (om::object editor)) 'null); if it's the first solution
                      (setf (solutions-list (om::object editor)) (list (solution (om::object editor)))); initialize the list
                      (nconc (solutions-list (om::object editor)) (list (solution (om::object editor)))); add it to the end
                    )   
                    (progn
                      (update-pop-up editor search-panel (solutions-list (om::object editor)) (om::om-make-point 5 130) (om::om-make-point 320 20) "output-solution"); update the pop-up menu with the list of the solutions selected by the user
                      (oa::om-invalidate-view editor)
                      ;(print "updated solutions")
                    )
      )
    )

    ; button add the selected solution to the list of motives
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 5 170) ; position (horizontal, vertical)
      (om::om-make-point 100 20) ; size (horizontal, vertical)
      "Add to motives"
      :di-action #'(lambda (b) 
                    (if (typep (output-solution (om::object editor)) 'null); if there is no solution to add
                      (error "There is no motif to keep.")
                    )
                    (if (typep (motives-list (om::object editor)) 'null); if it's the first motif
                      (setf (motives-list (om::object editor)) 
                        (list 
                          (make-instance 'poly 
                            :voices (list (output-solution (om::object editor)) (input-chords (om::object editor)))
                          )
                        ); initialize the list
                      )
                      (nconc (motives-list (om::object editor)) 
                        (list 
                          (make-instance 'poly 
                            :voices (list (output-solution (om::object editor)) (input-chords (om::object editor)))
                          )
                        ); initialize the list
                      ); add it to the end
                    )
                    (progn
                      (update-pop-up editor solution-assembly-panel (motives-list (om::object editor)) (om::om-make-point 5 130) (om::om-make-point 320 20) "output-motif"); update the pop-up menu
                      (oa::om-invalidate-view editor)
                      ;(print "updated solutions")
                    )
      )
    )

    ; button to add the selected solution to the list of phrases
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 115 170) ; position
      (om::om-make-point 100 20) ; size
      "Add to phrases"
      :di-action #'(lambda (b)
                    (if (typep (output-solution (om::object editor)) 'null); if there is no solution to add
                      (error "There is no phrase to keep.")
                    )
                    (if (typep (phrases-list (om::object editor)) 'null); if it's the first phrase
                      (setf (phrases-list (om::object editor)) 
                        (list 
                          (make-instance 'poly 
                            :voices (list (output-solution (om::object editor)) (input-chords (om::object editor)))
                          )
                        ); initialize the list
                      )                      
                      (nconc (phrases-list (om::object editor)) 
                        (list 
                          (make-instance 'poly 
                            :voices (list (output-solution (om::object editor)) (input-chords (om::object editor)))
                          )
                        ); initialize the list
                      ); add it to the end                    
                    )
                    (progn
                      (update-pop-up editor solution-assembly-panel (phrases-list (om::object editor)) (om::om-make-point 5 230) (om::om-make-point 320 20) "output-phrase"); update the pop-up menu
                      (oa::om-invalidate-view editor)
                      ;(print "updated solutions")
                    )
      )
    )

    ; button to add the selected solution to the list of periods
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 225 170)
      (om::om-make-point 100 20)
      "Add to periods"
      :di-action #'(lambda (b)
                    (if (typep (output-solution (om::object editor)) 'null); if there is no solution to add
                      (error "There is no period to keep.")
                    )
                    (if (typep (periods-list (om::object editor)) 'null); if it's the first phrase
                      (setf (periods-list (om::object editor)) 
                        (list 
                          (make-instance 'poly 
                            :voices (list (output-solution (om::object editor)) (input-chords (om::object editor)))
                          )
                        ); initialize the list
                      )    
                      (nconc (periods-list (om::object editor)) 
                        (list 
                          (make-instance 'poly 
                            :voices (list (output-solution (om::object editor)) (input-chords (om::object editor)))
                          )
                        ); initialize the list
                      ); add it to the end 
                    )
                    (progn
                      (update-pop-up editor solution-assembly-panel (periods-list (om::object editor)) (om::om-make-point 5 330) (om::om-make-point 320 20) "output-period"); update the pop-up menu
                      (oa::om-invalidate-view editor)
                      ;(print "updated solutions")
                    )
      )
    )
  )
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the constraints panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this function creates the elements of the main additional constraints panel
; coordinates here are local to constraint-panel
(defun make-constraints-panel (editor constraints-panel)
  (om::om-add-subviews
    constraints-panel

    ; title
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 350 2) 
      (om::om-make-point 200 20) 
      "Additional constraints"
      :font om::*om-default-font1b*
    )
  )
)

; this function creates the elements of the general constraints panel
; coordinates here are local to general-constraints-panel
(defun make-general-constraints-panel (editor general-constraints-panel)
  (om::om-add-subviews
    general-constraints-panel

    ; title
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 50 2) 
      (om::om-make-point 200 20) 
      "General constraints"
      :font om::*om-default-font1b*
    )

    ;checkbox for all-different constraint
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 30) ; position
      (om::om-make-point 20 20) ; size
      "All different notes"
      :checked-p (find "all-different-notes" (optional-constraints (om::object editor)) :test #'equal)
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (push "all-different-notes" (optional-constraints (om::object editor)))
                      (setf (optional-constraints (om::object editor)) (remove "all-different-notes" (optional-constraints (om::object editor)) :test #'equal))
                    )
                    (print (optional-constraints (om::object editor)))
      )
    )

    ; name for all-different constraint
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 30 30) 
      (om::om-make-point 200 20) 
      "All different notes"
      :font om::*om-default-font1*
    )
  )
)

; this function creates the elements of the motif-maker constraints panel
; coordinates here are local to motif-maker-constraints-panel
(defun make-motif-maker-constraints-panel (editor motif-maker-constraints-panel)
  (om::om-add-subviews
    motif-maker-constraints-panel

    ; title
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 50 2) 
      (om::om-make-point 200 20) 
      "Motif Maker constraints"
      :font om::*om-default-font1b*
    )

    ;checkbox for strictly-increasing-pitch constraint
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 30) ; position
      (om::om-make-point 20 20) ; size
      "Strictly increasing pitch"
      :checked-p (find "strictly-increasing-pitch" (optional-constraints (om::object editor)) :test #'equal)
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (push "strictly-increasing-pitch" (optional-constraints (om::object editor)))
                      (setf (optional-constraints (om::object editor)) (remove "strictly-increasing-pitch" (optional-constraints (om::object editor)) :test #'equal))
                    )
                    (print (optional-constraints (om::object editor)))
      )
    )

    ; name for strictly-increasing-pitch constraint
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 30 30) 
      (om::om-make-point 150 20) 
      "Strictly increasing pitch"
      :font om::*om-default-font1*
    )

    ;checkbox for strictly-decreasing-pitch constraint
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 50) ; position
      (om::om-make-point 20 20) ; size
      "Strictly decreasing pitch"
      :checked-p (find "strictly-decreasing-pitch" (optional-constraints (om::object editor)) :test #'equal)
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (push "strictly-decreasing-pitch" (optional-constraints (om::object editor)))
                      (setf (optional-constraints (om::object editor)) (remove "strictly-decreasing-pitch" (optional-constraints (om::object editor)) :test #'equal))
                    )
                    (print (optional-constraints (om::object editor)))
      )
    )

    ; name for strictly-decreasing-pitch constraint
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 30 50) 
      (om::om-make-point 200 20) 
      "Strictly decreasing pitch"
      :font om::*om-default-font1*
    )

    ;checkbox for increasing-pitch constraint
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 70) ; position
      (om::om-make-point 20 20) ; size
      "Increasing pitch"
      :checked-p (find "increasing-pitch" (optional-constraints (om::object editor)) :test #'equal)
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (push "increasing-pitch" (optional-constraints (om::object editor)))
                      (setf (optional-constraints (om::object editor)) (remove "increasing-pitch" (optional-constraints (om::object editor)) :test #'equal))
                    )
                    (print (optional-constraints (om::object editor)))
      )
    )

    ; name for increasing-pitch constraint
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 30 70) 
      (om::om-make-point 150 20) 
      "Increasing pitch"
      :font om::*om-default-font1*
    )

    ;checkbox for decreasing-pitch constraint
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 90) ; position
      (om::om-make-point 20 20) ; size
      "Decreasing pitch"
      :checked-p (find "decreasing-pitch" (optional-constraints (om::object editor)) :test #'equal)
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (push "decreasing-pitch" (optional-constraints (om::object editor)))
                      (setf (optional-constraints (om::object editor)) (remove "decreasing-pitch" (optional-constraints (om::object editor)) :test #'equal))
                    )
                    (print (optional-constraints (om::object editor)))
      )
    )

    ; name for decreasing-pitch constraint
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 30 90) 
      (om::om-make-point 200 20) 
      "Decreasing pitch"
      :font om::*om-default-font1*
    )

    ;checkbox for mostly-increasing-pitch constraint
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 110) ; position
      (om::om-make-point 20 20) ; size
      "Mostly increasing pitch"
      :checked-p (find "mostly-increasing-pitch" (optional-constraints (om::object editor)) :test #'equal)
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (push "mostly-increasing-pitch" (optional-constraints (om::object editor)))
                      (setf (optional-constraints (om::object editor)) (remove "mostly-increasing-pitch" (optional-constraints (om::object editor)) :test #'equal))
                    )
                    (print (optional-constraints (om::object editor)))
      )
    )

    ; name for mostly-increasing-pitch constraint
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 30 110) 
      (om::om-make-point 150 20) 
      "Mostly increasing pitch"
      :font om::*om-default-font1*
    )

    ;checkbox for mostly-decreasing-pitch constraint
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 130) ; position
      (om::om-make-point 20 20) ; size
      "Mostly decreasing pitch"
      :checked-p (find "mostly-decreasing-pitch" (optional-constraints (om::object editor)) :test #'equal)
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (push "mostly-decreasing-pitch" (optional-constraints (om::object editor)))
                      (setf (optional-constraints (om::object editor)) (remove "mostly-decreasing-pitch" (optional-constraints (om::object editor)) :test #'equal))
                    )
                    (print (optional-constraints (om::object editor)))
      )
    )

    ; name for mostly-decreasing-pitch constraint
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 30 130) 
      (om::om-make-point 150 20) 
      "Mostly decreasing pitch"
      :font om::*om-default-font1*
    )

    ; name for the pop-up menu allowing to select the global interval for both mostly increasing/decreasing
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 10 150) 
      (om::om-make-point 200 20) 
      "Total interval (in semitones)"
      :font om::*om-default-font1*
    )

    ; pop-up menu for the selection of the global interval that the melody should go to for both
    (om::om-make-dialog-item 
      'om::pop-up-menu 
      (om::om-make-point 10 170) 
      (om::om-make-point 180 20) 
      "Key selection"
      :range (loop :for n :from 1 :below 25 :by 1 collect (write-to-string n))
      :value (global-interval (om::object editor))
      :di-action #'(lambda (m)
        (setf (global-interval (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )
  )
)

; this function creates the elements of the motif-maker constraints panel
; coordinates here are local to motif-maker-constraints-panel
(defun make-phrase-maker-constraints-panel (editor phrase-maker-constraints-panel)
  (om::om-add-subviews
    phrase-maker-constraints-panel

    ; title
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 50 2) 
      (om::om-make-point 200 20) 
      "Phrase Maker constraints"
      :font om::*om-default-font1b*
    )
  )
)

; this function creates the elements of the period-maker constraints panel
; coordinates here are local to period-maker-constraints-panel
(defun make-period-maker-constraints-panel (editor period-maker-constraints-panel)
  (om::om-add-subviews
    period-maker-constraints-panel

    ; title
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 50 2) 
      (om::om-make-point 200 20) 
      "Period Maker constraints"
      :font om::*om-default-font1b*
    )
  )
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the solution assembly panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this function creates the elements of the solution-assembly panel
; coordinates here are local to solution-assembly-panel
(defun make-solution-assembly-panel (editor solution-assembly-panel)
  (om::om-add-subviews
    solution-assembly-panel

    ; title
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 190 2) 
      (om::om-make-point 120 20) 
      "Solution assembly"
      :font om::*om-default-font1b*
    )

    ;pop-up list to select the desired motif
    ;this is only for the start, as a new pop-up menu is created with every new solution
    ;/!\ if you move this, you also have to move the new ones that are generating every time the list is modified! see update-pop-up function
    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 5 130)
      (om::om-make-point 320 20)
      "Motif selection"
      :range (motives-list (om::object editor))
      :di-action #'(lambda (m)
                    (setf (output-motif (om::object editor)) (nth (om::om-get-selected-item-index m) (motives-list (om::object editor)))); set the output to the selected solution
                    (let ((indx (om::om-get-selected-item-index m)))
                        (om::openeditorframe ; open the editor of the selected solution
                            (om::omNG-make-new-instance 
                                (output-motif (om::object editor))
                                (format nil "motif ~D" (1+ indx)); name of the window
                            )
                        )
                    )
        )
    )

    ; name for the pop-up list
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 5 110) 
      (om::om-make-point 200 20) 
      "Motives"
      :font om::*om-default-font1*
    )

    ; button to add a motif before the current melody
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 150)
      (om::om-make-point 150 20)
      "Add before current melody"
      :di-action #'(lambda (b)
                    (if (typep (melody (om::object editor)) 'null); if there is no melody yet
                      (setf (melody (om::object editor)) (output-motif (om::object editor)))
                      (setf (melody (om::object editor)) (om::concat (output-motif (om::object editor)) (melody (om::object editor))))
                    )
      )
    )

    ; button to add a motif after the current melody
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 155 150)
      (om::om-make-point 150 20)
      "Add after current melody"
      :di-action #'(lambda (b)
        (if (typep (melody (om::object editor)) 'null); if there is no melody yet
                      (setf (melody (om::object editor)) (output-motif (om::object editor)))
                      (setf (melody (om::object editor)) (om::concat (melody (om::object editor)) (output-motif (om::object editor))))
                    )
      )
    )

    ;button to reset the list of motives
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 325 130)
      (om::om-make-point 100 20)
      "Reset"
      :di-action #'(lambda (b)
                    (setf (motives-list (om::object editor)) nil)
                    (setf (output-motif (om::object editor)) nil)
                    (update-pop-up editor solution-assembly-panel (motives-list (om::object editor)) (om::om-make-point 5 130) (om::om-make-point 320 20) "output-motif"); update the pop-up menu
      )
    )

    ; pop-up list to select the desired phrase
    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 5 230)
      (om::om-make-point 320 20)
      "Phrase selection"
      :range (phrases-list (om::object editor))
      :di-action #'(lambda (m)
                    (setf (output-phrase (om::object editor)) (nth (om::om-get-selected-item-index m) (phrases-list (om::object editor)))); set the output to the selected solution
                    (let ((indx (om::om-get-selected-item-index m)))
                      (om::openeditorframe
                          (om::omNG-make-new-instance
                              (output-phrase (om::object editor))
                              (format nil "phrase ~D" (1+ indx)); name of the window
                          )
                      )
                    )
      )
    )

    ; name for the pop-up list
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 5 210) 
      (om::om-make-point 200 20) 
      "Phrases"
      :font om::*om-default-font1*
    )

    ;button to add a phrase before the current melody
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 250)
      (om::om-make-point 150 20)
      "Add before current melody"
      :di-action #'(lambda (b)
                    (if (typep (melody (om::object editor)) 'null); if there is no melody yet
                      (setf (melody (om::object editor)) (output-phrase (om::object editor)))
                      (setf (melody (om::object editor)) (om::concat (output-phrase (om::object editor)) (melody (om::object editor))))
                    )
      )
    )

    ;button to add  phrase after the current melody
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 155 250)
      (om::om-make-point 150 20)
      "Add after current melody"
      :di-action #'(lambda (b)
                    (if (typep (melody (om::object editor)) 'null); if there is no melody yet
                      (setf (melody (om::object editor)) (output-phrase (om::object editor)))
                      (setf (melody (om::object editor)) (om::concat (melody (om::object editor)) (output-phrase (om::object editor))))
                    )
      )
    )

    ;button to reset the list of phrases
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 325 230)
      (om::om-make-point 100 20)
      "Reset"
      :di-action #'(lambda (b)
                    (setf (phrases-list (om::object editor)) nil)
                    (setf (output-phrase (om::object editor)) nil)
                    (update-pop-up editor solution-assembly-panel (phrases-list (om::object editor)) (om::om-make-point 5 230) (om::om-make-point 320 20) "output-motif"); update the pop-up menu
      )
    )

    ; pop-up list to select the desired period
    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 5 330)
      (om::om-make-point 320 20)
      "Period selection"
      :range (periods-list (om::object editor))
      :di-action #'(lambda (m)
                    (setf (output-period (om::object editor)) (nth (om::om-get-selected-item-index m) (periods-list (om::object editor)))); set the output to the selected solution
                    (let ((indx (om::om-get-selected-item-index m)))
                      (om::openeditorframe
                          (om::omNG-make-new-instance
                              (output-period (om::object editor))
                              (format nil "period ~D" (1+ indx))
                          )
                      )
                    )
      )
    )

    ; name for the pop-up list
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 5 310) 
      (om::om-make-point 200 20) 
      "Periods"
      :font om::*om-default-font1*
    )

    ;button to reset the list of periods
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 325 330)
      (om::om-make-point 100 20)
      "Reset"
      :di-action #'(lambda (b)
                    (setf (periods-list (om::object editor)) nil)
                    (setf (output-period (om::object editor)) nil)
                    (update-pop-up editor solution-assembly-panel (periods-list (om::object editor)) (om::om-make-point 5 330) (om::om-make-point 320 20) "output-motif"); update the pop-up menu

      )
    )

    ;button to show the melody
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 430)
      (om::om-make-point 400 20)
      "Show melody"
      :di-action #'(lambda (b)
                    (if (typep (melody (om::object editor)) 'null); if there is no melody yet
                      (error "There is no melody currently.")
                    )
                    (om::openeditorframe ; open a voice window displaying the input chords
                      (om::omNG-make-new-instance 
                        (melody (om::object editor))
                        "melody" ; name of the window
                      )
                    )
      )
    )

    ;button to save the melody as a phrase
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 455)
      (om::om-make-point 197 20)
      "Save as phrase"
      :di-action #'(lambda (b)
                    (if (typep (melody (om::object editor)) 'null); if there is no melody to add
                      (error "There is no melody to keep.")
                    )
                    (if (typep (phrases-list (om::object editor)) 'null); if it's the first phrase
                      (setf (phrases-list (om::object editor)) (list (melody (om::object editor)))); initialize the list
                      (nconc (phrases-list (om::object editor)) (list (melody (om::object editor)))); add it to the end
                    )
                    (progn
                      (update-pop-up editor solution-assembly-panel (phrases-list (om::object editor)) (om::om-make-point 5 230) (om::om-make-point 320 20) "output-phrase"); update the pop-up menu
                      (oa::om-invalidate-view editor)
                    )
      )
    )

    ; button to reset the melody
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 480)
      (om::om-make-point 400 20)
      "Reset melody"
      :di-action #'(lambda (b)
        (setf (melody (om::object editor)) nil)
      )
    )

  )
)




  ;; ;;; text boxes (they can be edited!)
;; (om::om-make-dialog-item
;;   'om::text-box
;;   (om::om-make-point 660 50) 
;;   (om::om-make-point 180 20) 
;;   "Variety of the solutions" 
;;   :font om::*om-default-font1* 
;; )
