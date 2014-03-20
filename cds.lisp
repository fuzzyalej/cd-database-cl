;; Database variable
(defvar *db* nil)


;; Utils
(defun print-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


;; Model
(defun make-cd (title artist rating ripped)
  (list :title title
        :artist artist
        :rating rating
        :ripped ripped))


;; Database operations
(defun add-cd (cd)
  (push cd *db*))

(defun interactive-add-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped: ")))

(defun add-cds ()
  (loop (add-cd (interactive-add-cd))
        (if (not (y-or-n-p "Another?: ")) (return))))
