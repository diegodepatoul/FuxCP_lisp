(cl:defpackage "fuxcp"
  (:nicknames "FUXCP")
  (:use common-lisp om :cl-user :cl :cffi))

(in-package :fuxcp)

(print "Loading gecode-wrapper...")

(defparameter DFS 0)
(defparameter BAB 1)

(defparameter MAJOR 0)
(defparameter MINOR 5)
; corresponds to enum values in gecode_problem.h, but can be used graphically in om
(defun bab ()
    BAB
)
(defun dfs ()
    DFS
)

(defun major ()
    MAJOR
)
(defun minor ()
    MINOR
)

;;;;;;;;;;;;;;;;;;;;;
;; Problem methods ;;
;;;;;;;;;;;;;;;;;;;;;


(defun setup-counterpoint (cf species scale chromatic-scale tone-pitch-cf mode-param borrowed-scale off-scale)
    (print "starting setup-counterpoint")
    (print (list scale chromatic-scale tone-pitch-cf mode-param borrowed-scale off-scale))
    (let (
        (a (cffi::foreign-alloc :int :initial-contents cf))
        (b (cffi::foreign-alloc :int :initial-contents scale))
        (c (cffi::foreign-alloc :int :initial-contents chromatic-scale))
        (d (cffi::foreign-alloc :int :initial-contents borrowed-scale))
        (e (cffi::foreign-alloc :int :initial-contents off-scale))
        )
        (setup-cp (length cf) a species b c tone-pitch-cf mode-param d e)
    )    
)

(cffi::defcfun ("set_up_counterpoint" setup-cp) :pointer
    (size :int)
    (cf :pointer :int)
    (species :int)
    (scale :pointer :int)
    (chromatic_scale :pointer :int)
    (tone_pitch-cf :int)
    (mode_param :int)
    (borrowed_scale :pointer :int)
    (off_scale :pointer :int)
)

(cffi::defcfun ("get_counterpoint" compute-counterpoint) :pointer 
    (solver :pointer :void)
)


(defun new-counterpoint (cf)
    (print "Starting new-counterpoint")
    (let (
        (x (cffi::foreign-alloc :int :initial-contents cf))
        )
        (print "Calling new-problem")
        (new-problem (length cf) x)
    )
)

(cffi::defcfun ("create_new_problem" new-problem) :pointer
    "Creates a new instance of the problem. Returns a void* cast of a Problem*."
    (size :int)
    (cf :pointer :int)
    #|
    (size :int) ; an integer representing the size
    (key :int) ; the key of the tonality
    (mode :int) ; the mode of the tonality
    (chord-degrees :pointer :int) ; a void* cast of a int* that are the chord degrees
    (chord-states :pointer :int)  ; a void* cast of a int* that are the chord states
    |#
)

(cffi::defcfun ("get_size" get-size) :int
    "Returns the size of the space."
    (sp :pointer) ; a void* cast of a Problem*
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search engine methods ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi::defcfun ("create_solver" create-solver) :pointer
    "Creates a DFS<Problem> object. Returns a void* cast of a DFS<Problem> object."
    (sp :pointer) ; a void* cast of a Problem*
    (solver-type :int); an integer representing the type of the solver (see above)
)

(cffi::defcfun ("return_next_solution_space" return-next-solution-space) :pointer
    "Returns a pointer to the next solution of the problem. Returns a void* cast of a Problem*."
    (solver :pointer) ; a void* cast of a Base<Problem>* pointer
)

(cffi::defcfun ("return_best_solution_space" return-best-solution-space) :pointer
    "Returns a pointer to the best solution of the problem. Returns a void* cast of a Problem*. Must take a BAB solver as argument"
    (solver :pointer) ; a void* cast of a BAB<Problem>* pointer
)

;;;;;;;;;;;;;;;;;;;;;;;
;; Solution handling ;;
;;;;;;;;;;;;;;;;;;;;;;;

(cffi::defcfun ("return_solution" return-solution) :pointer
    "Returns a int* that are the values of a solution."
        (sp :pointer) ; a void* cast of a Problem object that is a solution of the problem.
)

(defun solution-to-int-array (sp)
    "Returns the values the variables have taken in the solution as a list of integers. Casts a int* into a list of numbers."
    "sp is a void* cast of a Problem* that is a solution to the problem. Calling this funciton on a non-solution 
    will result in an error."
    (if (cffi::null-pointer-p sp) ; TODO check
        (error "No (more) solutions.")
    )
    (print "Entering solution-to-int-array")
    (let* (
            (size (get-size sp))
            (ptr (return-solution sp))
        )
        (loop for i from 0 below (* size 1)
            ;(print "Trying to collect the solutions")
            collect (cffi::mem-aref ptr :int i)
        )
    )
)

; turns a list into a list of 4 element lists to make chords.
; for example: (1 2 3 4 5 6 7 8) -> ((1 2 3 4) (5 6 7 8))
(defun solution-to-chord-list (raw-solution)
    (if (<= (length raw-solution) 4)
        (list raw-solution)
        (cons (subseq raw-solution 0 4) (solution-to-chord-list (subseq raw-solution 4)))
    )
)