; Music structure data
(defparameter *simple-music*
  '((Verse -> (Measure Measure))
    (Chorus -> (Measure Measure))
    (Measure -> (Note Note Note Note))
    (Note -> MajorNote MinorNote)
    (MajorNote -> A B C D E F G)
    (MinorNote -> Am Bm Cm Dm Em Fm Gm))
   "Rules for making the most simple form of music")
  

(defvar *music* *simple-music*
  "Easy way to swap out music for other types")


; Functions for dealing with rules
(defun rule-lhs (rule)
  (first rule))


(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (name)
  (rule-rhs (assoc name *music*))) 


; Functions for selecting items from lists
(defun one-of (set)
  "Pick a random element from a set and make a list of it so we can use append"
  (list (random-elt set)))

(defun random-elt (choices)
    "Return a random element from the choices"
    (elt choices (random (length choices))))


; General Uitlitie

;; A function to call fn on each element of the-list and return the append
;; of the results (note that mapcar returns the list of the results).
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))


(defun generate (phrase)
  "Generate a random phrase for our music"
  (cond ((listp phrase)
          (mappend #'generate phrase))
         ((rewrites phrase)
          (generate (random-elt (rewrites phrase))))
         (t (list phrase))))


