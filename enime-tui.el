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

(defvar enime-skip-opening-time 0
  "Holds the value for skip opening time")

(defvar enime-finished-at-seconds-left 0
  "Holds the value in seconds for the time remaining to consider that an episode has finished, this is useful for going to the next episode with the continue action when the episode is at the ending for example")

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

(defun enime--get-anime-img-url-from-key (key)
  "Returns the anime description from an
enime--current-anime-search-results-alist key"
  (cdr (assoc 'img-src (enime--get-anime-alist-from-key key))))

(defun enime--get-anime-current-episode (key)
  "Returns the anime description from an
enime--current-anime-search-results-alist key"
  (let
      ((episode
	(cdr (assoc
	      'current-episode
	      (enime--get-anime-alist-from-key key)))))
    (if episode  episode 1)))

(defun enime--get-opening-skip (key)
  "Returns the corresponding value of opening skip"
  (cdr (assoc 'opening-skip (enime--get-anime-alist-from-key key))))

(defun enime--get-consider-finished-left (key)
  "Returns the corresponding value of consider-finished-left"
  (cdr (assoc 'consider-finished-left
	      (enime--get-anime-alist-from-key key))))

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

(defun enime--search-for-followed-animes ()
  "Select followed anime"
  (interactive)
  (enime-select-followed-anime-transient))

(transient-define-prefix enime-main-transient ()
  "Transient prefix with main menu"
  ["Commands"
   :class transient-row
   ("s" "Anime search" enime--search-for-anime :transient t)
   ("f" "Animes followed" enime--search-for-followed-animes :transient t)
   ("q" "Quit" transient-quit-all)]
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

(defun enime--play-episode-action ()
  "Action for playing an episode"
  (interactive)
  (message "Retrieving video, please wait...")
  (if
      (enime-play-episode enime-current-anime-id (number-to-string enime-episode-number) enime-desired-quality)
      (progn
	(enime--update-anime-property-db enime-current-anime-id :current-episode enime-episode-number)
	(transient-quit-all))
    (message "The episode cannot be retrieved")))

(defun enime--follow-action ()
  "Action for start following an anime"
  (interactive)
  (setq enime-skip-opening-time 0)
  (setq enime-finished-at-seconds-left 0)
  (enime--follow-anime enime-current-anime-id
		       enime-episode-number
		       (enime--get-anime-description-from-key
			enime-current-anime-key)
		       (enime--get-anime-img-url-from-key
			enime-current-anime-key))
  (enime-anime-transient))

(defun enime--unfollow-action ()
  "Action for unfollowing an anime"
  (interactive)
  (when (yes-or-no-p "Are you sure?")
    (enime--unfollow-anime enime-current-anime-id))
  (enime-anime-transient))

(defun enime--show-details-action ()
  "Action for displaying anime details"
  (interactive)
  (let ((img-path
	 (format "%s/%s.jpg"
		 enime-tmp-dir
		 enime-current-anime-id)))
    (enime--download-cover-art
     (enime--get-anime-img-url-from-key enime-current-anime-key)
     img-path)
    (enime--display-anime-details
     img-path
     (enime--get-anime-details enime-current-anime-id))))


(transient-define-infix enime--set-skip-opening-time ()
  "Stablishes skip opening time"
  :class 'transient-lisp-variable
  :variable 'enime-skip-opening-time
  :key "-k"
  :description "Skip opening at second"
  :reader (lambda (&rest _)
	    (let ((val
		   (read-number 
		    "Skip time in seconds: "
		    enime-skip-opening-time)))
	      (enime--update-anime-property-db
	       enime-current-anime-id
	       :opening-skip
	       val)
	      val)))


(transient-define-infix enime--set-finished-at-seconds-left ()
  "Stablishes skip opening time"
  :class 'transient-lisp-variable
  :variable 'enime-finished-at-seconds-left
  :key "-t"
  :description "Consider episode finished at seconds left"
  :reader (lambda (&rest _)
	    (let ((val
		   (read-number 
		    "Episode finished at seconds left: "
		    enime-finished-at-seconds-left)))
	      (enime--update-anime-property-db
	       enime-current-anime-id
	       :consider-finished-left
	       val)
	      val)))


(transient-define-prefix enime-anime-transient ()
  "Transient prefix for an anime"
  [:class transient-row
	  :description
	  (lambda () (enime--get-anime-description-from-key enime-current-anime-key))
	  (enime--set-anime-episode)
	  (enime--set-desired-quality)
	  ]
  ["Actions"
   :class transient-row
   ("f" "Follow anime" enime--follow-action
    :if (lambda ()
	  (not (enime--is-anime-followed-p
		enime-current-anime-id))))
   ("p" "Play epidose" enime--play-episode-action)
   ("d" "Show anime details" enime--show-details-action)
   ("m" "Return to main menu" enime-main-transient)
   ("s" "Return to anime selection" enime-select-anime-transient
    :if (lambda ()
	  (not (enime--is-anime-followed-p
		enime-current-anime-id))))
   ("q" "Quit" transient-quit-all)]
  ["Anime configuration"
   :class transient-row
   :if (lambda ()
	 (enime--is-anime-followed-p
	  enime-current-anime-id))
   ("u" "Unfollow anime" enime--unfollow-action)
   ("c" "Continue watching" enime--show-details-action)
   (enime--set-skip-opening-time)
   (enime--set-finished-at-seconds-left)
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
			       (setq enime-episode-number
				     1)
                               (enime-anime-transient)))))))
             enime--current-anime-search-results-alist))

(defun enime--generate-followed-anime-alist ()
  "Generates an alist for prefix selection based on enime--followed-anime-alist-cache"
  (let* ((keys (enime--generate-keys
		(length
		 enime--followed-anime-alist-cache))))
    (-zip-with (lambda (anime key)
		 `(,key ,(append (list :key key)
				 (car (cdr anime)))))
	       (sort (copy-list
		      enime--followed-anime-alist-cache)
		     (lambda (el1 el2) (string< (car el1) (car el2))))
	       keys)))

(defun enime--set-select-followed-anime-children (_)
  "Returns dinamically created suffixes acording with followed animes"
  (let* ((anime-alist-prefixes
	  (enime--generate-followed-anime-alist)))
    (setq
     enime--current-anime-search-results-alist
     anime-alist-prefixes)
    (cl-mapcan (lambda (anime-followed)
		 (let-alist (transient-plist-to-alist
			     (car (cdr anime-followed)))
		   (and .key
			(transient--parse-child
			 'enime-select-followed-anime-transient
			 (list .key
                               .description
                               (lambda ()
				 (interactive)
				 (setq enime-current-anime-key .key)
				 (setq enime-current-anime-id
				       (enime--get-anime-id-from-key
					enime-current-anime-key))
				 (setq enime-episode-number
				       (enime--get-anime-current-episode
					enime-current-anime-key))
				 (setq enime-skip-opening-time
				       (enime--get-anime-property
					enime-current-anime-id
					:opening-skip))
				 (setq enime-finished-at-seconds-left
				       (enime--get-consider-finished-left
					enime-current-anime-key))
				 (enime-anime-transient)))))))
	       anime-alist-prefixes)))

(transient-define-prefix enime-select-anime-transient ()
  ["Select an anime"
   :setup-children enime--set-select-anime-children])

(transient-define-prefix enime-select-followed-anime-transient ()
  ["Select a followwd anime"
   :setup-children enime--set-select-followed-anime-children])

(defun enime--restore-anime-transient-after-details ()
  "After quitting a details buffer, return to previous anime transient"
  (interactive)
  (kill-current-buffer)
  (enime-anime-transient))

(defun enime--display-anime-details (img-file-path details)
  "Opens a special buffer showing an anime image and text details"
  (condition-case nil
      (kill-buffer "enime-anime-details")
    (error nil))
  (let ((buffer
	 (get-buffer-create "enime-anime-details")))
    (switch-to-buffer buffer)
    (with-current-buffer "enime-anime-details"
      (set-window-margins nil 30 30)
      (insert img-file-path)
      (insert (concat "\n\nDETAILS\n\n" details))
      (special-mode)
      (enime--details-mode)
      (call-interactively 'iimage-mode)
      (beginning-of-buffer))))

(defun enime--download-cover-art (img-url out-path)
  "Downloads and saves a cover art image from img-url"
  (shell-command (format "curl -o %s %s &> /dev/null" out-path img-url)))


(define-minor-mode enime--details-mode
  "Minor mode for keybinding specific to detail bufers"
  nil
  :lighter "anime details")

(defvar enime--details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'enime--restore-anime-transient-after-details)
    map)
  "Keymap for enime details mode.")

(provide 'enime--details-mode)
