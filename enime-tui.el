;;; -*- lexical-binding: t -*-


(defvar enime--current-anime-search-results-alist nil
  "Holds the value of the currently searched anime")

(defvar enime-episode-number 1
  "Selected episode number")

(defvar enime-current-anime-key nil
  "Holds last selected key from an anime search")

(defvar enime-desired-quality "1080"
  "Desired episode quiality, may not be availabe")

(defvar enime-current-anime-id nil
  "Holds last selected id from an anime search")

(defun enime--get-anime-alist-from-key (key)
  "Returns the alist elements of an anime from key"
  (transient-plist-to-alist (car (cdr (assoc key enime--current-anime-search-results-alist)))))

(defun enime--get-anime-description-from-key (key)
  "Returns the anime description from an
enime--current-anime-search-results-alist key"
  (cdr (assoc 'description (enime--get-anime-alist-from-key key))))

(defun enime--get-anime-id-from-key (key)
  "Returns the anime description from an
enime--current-anime-search-results-alist key"
  (cdr (assoc 'id (enime--get-anime-alist-from-key key))))


(defun enime--generate-keys (length)
  "Returns a list of length containing strings from a..z A..Z
suports up to 104 keys, if more they are discarded"
  (-slice (mapcar (lambda (val) val)
		  (append
		   (mapcar
		    (lambda (num)
		      (char-to-string num))
		    (number-sequence ?a ?z))
		   (mapcar
		    (lambda (num)
		      (char-to-string num))
		    (number-sequence ?A ?Z))
		   (mapcar
		    (lambda (num)
		      (concat "-" (char-to-string num)))
		    (number-sequence ?a ?z))
		   (mapcar
		    (lambda (num)
		      (concat "-" (char-to-string num)))
		    (number-sequence ?A ?Z))))
	  0 length))

(defun enime--generate-anime-result-alist (search-string)
  "Returns an alist of animes found from search-string"
  (let* ((results (enime-search-anime search-string))
	 (keys (enime--generate-keys (length results))))
    (-zip-with (lambda (anime key)
		 `(,key (:key ,key :id ,(car anime)
			:description ,(second anime)
			:img-src ,(third anime))))
	       results keys)))

(defun enime--search-for-anime ()
  "Searches an anime for tansient menu"
  (interactive)
  (setq enime--current-anime-search-results-alist
	(enime--generate-anime-result-alist
	 (read-string "Anime: ")))
  (if enime--current-anime-search-results-alist
       (enime-select-anime-transient)
    (message "No results found")))

(transient-define-prefix enime-main-transient ()
  "Transient prefix with main menu"
  ["Commands"
   :class transient-row
   ("s" "Anime search" enime--search-for-anime :transient t)
   ("q" "Quit" transient-quit-one)]
  (interactive)
  (transient-setup 'enime-main-transient))

(transient-define-infix enime--set-anime-episode ()
  "Sets the episode to watch"
  :class 'transient-lisp-variable
  :variable 'enime-episode-number
  :key "-e"
  :description "Episode to watch"
  :reader (lambda (&rest _)
	    (let ((ep-range
		   (enime-episodes-range
		    enime-current-anime-id)))
	      (read-number (format
			    "Episode (%s-%s available): "
			    (car ep-range)
			    (second ep-range))
			   enime-episode-number))))


(transient-define-infix enime--set-desired-quality ()
  "Sets desired quality of episode, may not be available"
  :class 'transient-lisp-variable
  :variable 'enime-desired-quality
  :description "Desired video quiality"
  :key "-q"
  :reader (lambda (&rest _)
	    (second (read-multiple-choice "Choose quality: "
					  '((?a "360") (?b "480") (?c " 720") (?d "1080"))))))

(transient-define-prefix enime-anime-transient ()
  "Transient prefix for an anime"
  [:class transient-row
   :description
   (lambda () (enime--get-anime-description-from-key enime-current-anime-key))
   (enime--set-anime-episode)
   (enime--set-desired-quality)
   ])

(defun enime--set-select-anime-children (_)
  "Returns dinamically created suffixes acording with anime results
hold in enime--current-anime-search-results-alist"
  (cl-mapcan (lambda (anime-result)
               (let-alist (transient-plist-to-alist
			   (car (cdr anime-result)))
		 (and .key
                      (transient--parse-child
                       'enime-select-anime-transient
                       (list .key
                             .description
                             (lambda ()
                               (interactive)
			       (setq enime-current-anime-key .key)
			       (setq enime-current-anime-id
				     (enime--get-anime-id-from-key
				      enime-current-anime-key))
                               (enime-anime-transient)))))))
             enime--current-anime-search-results-alist))

(transient-define-prefix enime-select-anime-transient ()
  ["Select an anime"
   :setup-children enime--set-select-anime-children])

