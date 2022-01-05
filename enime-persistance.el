(defcustom enime-storage-file "/tmp/data.db"
  "File to storage followed animes"
  :group 'enime
  :type 'string)

(defvar enime--followed-anime-alist-cache nil
  "Holds the currently saved followed anime alist, the idea is to reduce disk access, this variable should be updated every time the contents of the db file change")

(defclass Anime-Storage (eieio-persistent)
  ((data :initarg :menagerie :initform nil)
   (file :initform "")))

(defun enime--get-followed-anime-alist ()
  "Returns the currently saved followed anime alist"
  (if enime--followed-anime-alist-cache
      enime--followed-anime-alist-cache
    (let* ((store
	    (condition-case nil
		(eieio-persistent-read enime-storage-file Anime-Storage)
	      (error nil))))
      (when store
	(setq enime--followed-anime-alist-cache
	      (oref store data))
	enime--followed-anime-alist-cache))))

(defun enime--update-db (new-alist)
  "Updates the db file with new alist, also takes care of cache"
  
  (let* ((store
	  (condition-case nil
	      (eieio-persistent-read enime-storage-file Anime-Storage)
	    (error nil))))
    (when (not store)
      (setf store (Anime-Storage)))
    (oset store data
	  new-alist)
    (eieio-persistent-save store enime-storage-file)
    (setq enime--followed-anime-alist-cache new-alist)))

(defun enime--follow-anime (anime-id
			    current-episode
			    description
			    img-src)
  "Store new anime to follow"
  (let* (
	 (animes (enime--get-followed-anime-alist))
	 (element `(,anime-id
		    (:id
		     ,anime-id
		     :description
		     ,description
		     :img-src
		     ,img-src
		     :current-episode
		     ,current-episode
		     :time-elapsed
		     0
		     :opening-skip
		     0
		     :consider-finished-left
		     0
		     :current-episode-duration
		     -1))))
    (if (and animes (assoc anime-id animes))
	(message "You're already following this anime")
      (progn
	(enime--update-db (append
			   animes
			   (list element)))
	(message "Following anime")))))

(defun enime--unfollow-anime (anime-id)
  "Unfollow an anime, that is, delete it from db"
  (enime--update-db (assoc-delete-all
		     anime-id
		     (enime--get-followed-anime-alist))))


(defun enime--is-anime-followed-p (anime-id)
  "Predicate to determine if an anime is being followed"
  (when (assoc anime-id
	       (enime--get-followed-anime-alist))
    t))

