;;; -*- lexical-binding: t -*-

(defvar enime--current-searched-anime-id nil

  "Holds the id value of the currently searched anime")

(defvar enime--current-anime-search-results-alist nil
  "Holds the value of the currently searched anime")

(defun enime--generate-keys (length)
  "Returns a list of length containing strings from a..z A..Z
suports up to 56 keys, if more they are discarded"
  (-slice (mapcar (lambda (num) (char-to-string num))
		       (append (number-sequence ?a ?z)
			       (number-sequence ?A ?Z)))
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

(defun enime--set-selet-anime-children (_)
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
                               (message .key)))))))
             enime--current-anime-search-results-alist))

(transient-define-prefix enime-select-anime-transient ()
  ["Select an anime"
   :setup-children enime--set-selet-anime-children])


