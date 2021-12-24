(defcustom enime-storage-file "/tmp/data.db"
  "File to storage followed animes"
  :group 'enime
  :type 'string)

(defclass Anime-Storage (eieio-persistent)
  ((data :initarg :menagerie :initform nil)
   (file :initform "")))

(defun enime--follow-anime (anime-id current-episode)
  "Store new anime to follow"
  (let* ((store
	  (condition-case nil
	      (eieio-persistent-read enime-storage-file Anime-Storage)
	    (error nil)))
	 (animes
	  (when store
	    (oref store data)))
	 (element `(,anime-id
		    (:current-episode
		     ,current-episode
		     :opening-skip
		     0
		     :consider-finished-left
		     0
		     :current-episode-duration
		     -1))))
    (if (and animes (assoc anime-id animes))
	(message "You're already following this anime")
      (progn
	(when (not store)
	  (setf store (Anime-Storage)))
	(oset store data
	      (append animes
		      (list element)))
	(eieio-persistent-save store enime-storage-file)))))

