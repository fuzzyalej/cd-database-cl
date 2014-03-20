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


;; Persistence
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


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

;; Queries
(defun select-by-artist (artist)
  "Bad because we will have several functions all the same"
  (remove-if-not
    #'(lambda (cd) (equal (getf cd :artist) artist))
    *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  "Bad too, because we will have several functions all the same!"
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun where (&key title artist rating (ripped nil ripped-p))
  "First version of where, cheeky"
  #'(lambda (cd)
      (and
        (if title (equal (getf cd :title) title) t)
        (if artist (equal (getf cd :artist) artist) t)
        (if rating (equal (getf cd :rating) rating) t)
        (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  "First version of update"
  (setf *db*
        (mapcar
          #'(lambda (cd)
              (when (funcall selector-fn cd)
                (if title (setf (getf cd :title) title))
                (if artist (setf (getf cd :artist) artist))
                (if rating (setf (getf cd :rating) rating))
                (if ripped-p (setf (getf cd :ripped) ripped)))
              cd) *db*)))

(defun delete (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
