;;; enime.el --- Watch anime with emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022 xl666

;; Author: xl666 <xavi004@gmail.com>
;; URL: https://github.com/xl666/enime
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

;;; General

(require 'cl-lib)
(require 'eieio-base)
(require 'mpv)
(require 'transient)
(require 'dash)
(require 'request)
(require 'esxml-query)

(defgroup enime nil
  "Watch anime using Emacs."
  :group 'apps)


(defcustom enime-base-url (s-trim (shell-command-to-string
				   "curl -s -L -o /dev/null -w \"%{url_effective}\n\" https://gogoanime.cm"))
  "Gogo anime base url."
  :group 'enime
  :type 'string)

(defcustom enime-tmp-dir "/tmp"
  "Directory to save temporal data like anime cover art."
  :group 'enime
  :type 'string)

(defcustom enime-storage-file "/tmp/enime.db"
  "File to storage followed animes."
  :group 'enime
  :type 'string)

;;; scrapping

(defun enime-return-parsing-tree-from-request (url params)
  "Make the request, using html parsing, only works for get.
Argument URL url to construct parsing tree.
Argument PARAMS GET parameters."
  (let ((result nil))
    (request url
      :params params
      :sync t
      :parser
      (lambda () (libxml-parse-html-region (point) (point-max)))
      :success
      (cl-function (lambda (&key data &allow-other-keys)
                     (setf result data)))
      :error (setf result nil))
    result))

(defun enime-return-raw-text-from-request (url &optional headers params)
  "Make a request and return the obtained string without parsing.
Argument URL url to return html from
Optional argument HEADERS http headers to consider.
Optional argument PARAMS GET variables."
  (let ((result nil))
    (request url
      :params params
      :headers headers
      :sync t
      :parser
      'buffer-string
      :success
      (cl-function (lambda (&key data &allow-other-keys)
                     (setf result data)))
      :error (setf result nil))
    result))

(defun enime-get-anime-title-from-node (node)
  "Return the anime title from a NODE."
  (xml-get-attribute node 'title))

(defun enime-get-anime-id-from-node (node)
  "Return the anime id from NODE."
  (let ((href (xml-get-attribute node 'href)))
    (car (last (split-string href "/")))))

(defun enime-get-anime-img-src-from-node (node)
  "Return the image src value for an anime.
Argument NODE node from a parsing tree."
  (xml-get-attribute (esxml-query "img" node) 'src))

(defun enime--extract-text-description-rec (&optional current-list)
  "Return a string containing the text of a node element.
Optional argument CURRENT-LIST to control recursion."
  (if (not current-list)  ""
    (if (stringp (car current-list))
	(concat (s-trim (car current-list))
		(enime--extract-text-description-rec
		 (cdr current-list)))
      (if (and (listp (car current-list))
	       (stringp (car (last (car current-list)))))
	  (concat (s-trim (car (last (car current-list))))
		  (enime--extract-text-description-rec
		   (cdr current-list)))
	(enime--extract-text-description-rec
	 (cdr current-list))))))

(defun enime--get-anime-details (anime-id)
  "Returs anime description from an anime id.
Argument ANIME-ID anime to be processed."
  (let* ((uri (concat "/category/" anime-id))
	 (url (concat enime-base-url uri))
	 (tree
	  (enime-return-parsing-tree-from-request url nil))
	 (nodes (esxml-query-all "div.anime_info_body_bg>p.type" tree)))
    (cl-reduce (lambda (str1 str2)
		 (concat str1 "\n\n" str2))
	       (mapcar (lambda (node)
			 (enime--extract-text-description-rec node))
		       (butlast nodes)))))

(defun enime-normalize-search-string (string)
  "Gets ride of spaces and joins with -.
Argument STRING is the search string."
  (let ((parts (split-string string)))
    (cl-reduce (lambda (str1 str2)
		 (concat str1 "-" str2))
	       parts)))


(defun enime--pages-search-anime (tree)
  "Return the number of pages found in an anime search.
Argument TREE parsing tree."
  (let ((pags (esxml-query-all ".pagination-list li" tree)))
    (length pags)))


(defun enime--process-candites-search-anime-page (tree)
  "Return candidates found in a parse TREE."
  (mapcar (lambda (node)
	    `(,(enime-get-anime-id-from-node node)
	      ,(enime-get-anime-title-from-node node)
	      ,(enime-get-anime-img-src-from-node node)))
	  (esxml-query-all "div>a[href^=\"/category/\"]" tree)))

(defun enime--collect-candidates-search-anime-pages (base-url pages name)
  "Concatenates result candidates from all PAGES of a search anime.
Argument BASE-URL url for scrapping."
  (cl-reduce #'append
	     (mapcar
	      (lambda (num)
		(let ((tree
		       (enime-return-parsing-tree-from-request base-url `(("keyword" . ,name) ("page" . ,(number-to-string num))))))
		  (enime--process-candites-search-anime-page tree)))
	      (number-sequence 2 pages))))

(defun enime-search-anime (anime-name)
  "Search for posible anime candidates from ANIME-NAME.
Returns a list of candidates
a candidate is a list of id title img-src"
  (let* ((name (enime-normalize-search-string anime-name))
	 (uri "/search.html")
	 (url (concat enime-base-url uri))
	 (tree
	  (enime-return-parsing-tree-from-request url `(("keyword" . ,name))))
	 (pages (enime--pages-search-anime tree))
	 (candidates-fist-page (enime--process-candites-search-anime-page tree)))
    (if (= 0 pages)
	candidates-fist-page
      (append candidates-fist-page (enime--collect-candidates-search-anime-pages url pages name)))))

(defun enime-get-min-episode (tree)
  "Return the first episode found from a parse treee.
Argument TREE parsing tree."
  (xml-get-attribute (car (esxml-query-all "#episode_page li a" tree)) 'ep_start))

(defun enime-get-max-episode (tree)
  "Return the last episode found from a parse treee.
Argument TREE parsing tree."
  (xml-get-attribute (car (last (esxml-query-all "#episode_page li a" tree))) 'ep_end))

(defun enime-episodes-range (anime-id)
  "Return a numeric range (inclusive in both sides) of first and last episodes.
Argument ANIME-ID anime of interest."
  (let* ((uri (concat "/category/" anime-id))
	 (url (concat enime-base-url uri))
	 (tree
	  (enime-return-parsing-tree-from-request url nil))
	 (first-ep (enime-get-min-episode tree))
	 (last-ep (enime-get-max-episode tree)))
    (if (= (string-to-number first-ep) 0)
	`("1" ,last-ep)
      `(,first-ep ,last-ep))))


(defun enime--404-url-p (url)
  "Check if a URL return a 404 error."
  (let* ((text (enime-return-raw-text-from-request url)))
    (s-contains? "404" text)))

(defun enime-get-embedded-video-link (anime-id episode &optional dub)
  "Return the url of video asociated to anime EPISODE.
Optional parameter if the anime supports dub version
Argument ANIME-ID anime of interest."
  (let* ((uri1 (if dub (concat "/" anime-id "-dub" "-" episode)
		 (concat "/" anime-id "-" episode)))
	 (uri2 (if dub (concat "/" anime-id "-dub" "-episode-" episode)
		 (concat "/" anime-id "-episode-" episode)))
	 (url1 (concat enime-base-url uri1))
	 (url2 (concat enime-base-url uri2))
	 (url (if (enime--404-url-p url1) url2 url1))
	 (tree (enime-return-parsing-tree-from-request url nil)))
    (xml-get-attribute (esxml-query "a[rel=\"13\"]" tree) 'data-video)))


(defun enime-get-video-url (embedded-url)
  "Return video url from embedded url.
Argument EMBEDDED-URL url to take as a bassis for scrapping."
  (let* (
	 (prev (string-match "http[^?]+\\?\\(id=.*\\)" embedded-url))
	 (beginning (match-beginning 1))
	 (end (match-end 1)))
    (concat "https://gogoplay1.com/download?" (substring embedded-url beginning end))))


(defun enime--get-video-links (video-url)
  "Return a list of available video urls with different qualities.
Argument VIDEO-URL scrappend video url."
  (let ((text (enime-return-raw-text-from-request
	       video-url)))
    (mapcar (lambda (intern) (car intern))
	    (s-match-strings-all "http.+\\.com\\/cdn.+expiry=[0-9]+" text))))

(defun enime--filter-downloads-qualities-tree (tree)
  "Return an alist of available download urls per quality.
Argument TREE parsing tree."
  (let* ((elements
	  (esxml-query-all "div.dowload>a" tree)))
    (mapcar (lambda (element)
	      (let* ((prev (string-match
			    "\\([0-9][0-9][0-9][0-9]?\\)"
			    (cl-third element)))
		     (beginning (match-beginning 1))
		     (end (match-end 1)))
		`(,(substring (cl-third element) beginning end)
		  ,(xml-get-attribute element 'href))))
	    (cl-remove-if
	     (lambda (val)
	       (not (s-starts-with? "Download\n"
				    (cl-third val))))
	     elements))))

(defun enime-get-available-qualities (embedded-url)
  "Return an alist of available video qualities asociated with their url.
Argument EMBEDDED-URL url for scrapping."
  (when embedded-url
    (let ((tree
	   (enime-return-parsing-tree-from-request
	    embedded-url nil)))
      (enime--filter-downloads-qualities-tree tree))))



(defun enime-set-quality (alist-links desired-quality)
  "Cheks if the desired quality is available in alist.
If not it returns the higuer quality, i.e.; the last element in alist
Argument ALIST-LINKS alist of previously found video links.
Argument DESIRED-QUALITY quality desired by user."
  (if (member desired-quality
	      (mapcar (lambda (element)
			(car element))
		      alist-links))
      desired-quality
    (caar (last alist-links))))


(defun enime--clean-url (url)
  "Deletes amp; from URL."
  (cl-reduce #'concat
	     (split-string url "amp;")))

(defun enime-get-links (embedded-url desired-quality)
  "Return a video url with quality, try to get video with desired quality.
If quality not available gets the higher quality.
Argument EMBEDDED-URL base url for scrapping.
Argument DESIRED-QUALITY quality selected by user."
  (let* ((video-url (enime-get-video-url embedded-url))
	 (links (enime--get-video-links video-url))
	 (alist-links (enime-get-available-qualities embedded-url))
	 (quality (enime-set-quality alist-links desired-quality)))
    (if quality
	(enime--clean-url (car (cdr (assoc quality alist-links))))
      (enime--clean-url video-url)) ;; some videos do not have quality info, not sure if still useful
    ))

(defun enime--good-video-url-p (video-url)
  "T if the video url is valid, nil in other case.
Argument VIDEO-URL url to check."
  (if (< (length video-url) 10)
      nil
    (string= "http" (substring video-url 0 4))))


;;; video scrapping
;; this is ugly code, I don't want to but backporting every change to make
;; video playback working again after so often changes in anime CDN
;; is becoming a chore, sorry

(defconst enime--base-path (file-name-directory (or load-file-name buffer-file-name)))
(defconst enime--dub-prefix "-dub")

(defun enime--scrap-embbeded-and-video (anime-id episode desired-quality dub-prefix)
  "Run a script to get values for video playback.
Argument ANIME-ID anime of interest.
Argument EPISODE episode number.
Argument DESIRED-QUALITY quality selected by user."
  (let* ((scrapped-urls (shell-command-to-string (format "%s/%s %s %s %s %s" enime--base-path
							 "video_scrapping.sh " anime-id episode
							 desired-quality
							 dub-prefix)))
	 (partes (split-string scrapped-urls ";;;")))
    partes))

;;; persistence

(defvar enime--followed-anime-alist-cache nil
  "Holds the currently saved followed anime alist, the idea is to reduce disk access, this variable should be updated every time the contents of the db file change.")

(defclass Anime-Storage (eieio-persistent)
  ((data :initarg :menagerie :initform nil)
   (file :initform "")))

(defun enime--get-followed-anime-alist ()
  "Return the currently saved followed anime alist."
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
  "Update the db file with new alist, also takes care of cache.
Argument NEW-ALIST alist that substitute current db."

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
			    img-src
			    dub
			    desired-quality)
  "Store new anime to follow.
Argument ANIME-ID anime of interest
Argument CURRENT-EPISODE number of episode to save in db.
Argument DESCRIPTION scrapped anime description.
Argument IMG-SRC scrapped cover art url."
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
		     -1
		     :dub
		     ,dub
		     :desired-quality
		     ,desired-quality))))
    (if (and animes (assoc anime-id animes))
	(message "You're already following this anime")
      (progn
	(enime--update-db (append
			   animes
			   (list element)))
	(message "Following anime")))))

(defun enime--unfollow-anime (anime-id)
  "Unfollow an anime, that is, delete it from db.
Argument ANIME-ID anime of interest."
  (enime--update-db (assoc-delete-all
		     anime-id
		     (enime--get-followed-anime-alist))))


(defun enime--is-anime-followed-p (anime-id)
  "Predicate to determine if an anime is being followed.
Argument ANIME-ID anime of interest."
  (when (assoc anime-id
	       (enime--get-followed-anime-alist))
    t))

(defun enime--update-anime-property-db (anime-id property new-value)
  "Change the value of PROPERTY in alist of plists saved in db.
Argument ANIME-ID anime of interest."
  (let* ((animes (enime--get-followed-anime-alist))
	 (anime-plist
	  (car (cdr
		(assoc anime-id
		       animes))))
	 (new-plist (plist-put
		     anime-plist
		     property
		     new-value)))
    (setf (cdr (assoc anime-id animes))
	  `(,new-plist))
    (enime--update-db animes)))


(defun enime--get-anime-property (anime-id property)
  "Return the value of a given PROPERTY for the given anime.
Argument ANIME-ID anime of interest."
  (let* ((animes (enime--get-followed-anime-alist))
	 (anime-plist
	  (car (cdr
		(assoc anime-id
		       animes)))))
    (plist-get anime-plist property)))


;;; TUI

(defvar enime--current-anime-search-results-alist nil
  "Holds the value of the currently searched anime.")

(defvar enime-episode-number 1
  "Selected episode number.")

(defvar enime-dub nil
  "Prefer dubbed version if available")

(defvar enime-current-anime-key nil
  "Holds last selected key from an anime search.")

(defvar enime-desired-quality "1080"
  "Desired episode quiality, may not be availabe.")

(defvar enime-current-anime-id nil
  "Holds last selected id from an anime search.")

(defvar enime-skip-opening-time 0
  "Holds the value for skip opening time.")

(defvar enime-finished-at-seconds-left 0
  "Holds the value in seconds for the time remaining to consider that an episode has finished, this is useful for going to the next episode with the continue action when the episode is at the ending for example.")

(defun enime--get-anime-alist-from-key (key)
  "Return the alist elements of an anime from KEY."
  (transient-plist-to-alist (car (cdr (assoc key enime--current-anime-search-results-alist)))))

(defun enime--get-anime-description-from-key (key)
  "Return the anime description from an `enime--current-anime-search-results-alist' KEY."
  (cdr (assoc 'description (enime--get-anime-alist-from-key key))))

(defun enime--get-anime-id-from-key (key)
  "Return the anime description from an `enime--current-anime-search-results-alist' KEY."
  (cdr (assoc 'id (enime--get-anime-alist-from-key key))))

(defun enime--get-anime-img-url-from-key (key)
  "Return the anime description from an `enime--current-anime-search-results-alist' KEY."
  (cdr (assoc 'img-src (enime--get-anime-alist-from-key key))))

(defun enime--get-anime-current-episode (anime-id)
  "Only used for animes followd.
Returns the current anime episode, check if the current episode has finished.
 Make the appropiate changes in the database if necessary
Argument ANIME-ID anime of interest."
  (let*
      ((current-episode
	(enime--get-anime-property
	 anime-id
	 :current-episode))
       (time-elapsed
	(enime--get-anime-property
	 anime-id
	 :time-elapsed))
       (consider-finished-left-temp
	(enime--get-anime-property
	 anime-id
	 :consider-finished-left))
       (consider-finished-left ;; at least 10 to sync with timers
	(if
	    (< consider-finished-left-temp 10)
	    10
	  consider-finished-left-temp))
       (current-episode-duration
	(enime--get-anime-property
	 anime-id
	 :current-episode-duration))
       (max-episode
	(string-to-number (cl-second (enime-episodes-range anime-id)))))
    (if (>= current-episode max-episode)
	max-episode
      (if (and (> time-elapsed 0)
	       (> current-episode-duration 0)
	       (<=
		(- current-episode-duration time-elapsed)
		consider-finished-left))
	  (progn
	    (enime--update-anime-property-db
	     anime-id
	     :current-episode
	     (+ current-episode 1))
	    (enime--update-anime-property-db
	     anime-id
	     :time-elapsed
	     0)
	    (enime--update-anime-property-db
	     anime-id
	     :current-episode-duration
	     0)
	    (+ current-episode 1))
	current-episode))))

(defun enime--get-opening-skip (key)
  "Return the corresponding value of opening skip.
Argument KEY alist key."
  (cdr (assoc 'opening-skip (enime--get-anime-alist-from-key key))))

(defun enime--get-consider-finished-left (key)
  "Return the corresponding value of consider-finished-left.
Argument KEY alist key."
  (cdr (assoc 'consider-finished-left
	      (enime--get-anime-alist-from-key key))))

(defun enime--generate-keys (length)
  "Return a list of LENGTH containing strings from a..z A..Z suports up to 104 keys, if more they are discarded."
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
  "Return an alist of animes found from SEARCH-STRING."
  (let* ((results (enime-search-anime search-string))
	 (keys (enime--generate-keys (length results))))
    (-zip-with (lambda (anime key)
		 `(,key (:key ,key :id ,(car anime)
			      :description ,(cl-second anime)
			      :img-src ,(cl-third anime))))
	       results keys)))

(defun enime--search-for-anime ()
  "Search an anime for tansient menu."
  (interactive)
  (setq enime--current-anime-search-results-alist
	(enime--generate-anime-result-alist
	 (read-string "Anime: ")))
  (if enime--current-anime-search-results-alist
      (enime-select-anime-transient)
    (message "No results found")))

(defun enime--search-for-followed-animes ()
  "Select followed anime."
  (interactive)
  (if (enime--get-followed-anime-alist)
      (enime-select-followed-anime-transient)
    (message "No animes followed")))

(transient-define-prefix enime-main-transient ()
  "Transient prefix with main menu."
  ["Commands"
   :class transient-row
   ("s" "Anime search" enime--search-for-anime :transient t)
   ("f" "Animes followed" enime--search-for-followed-animes :transient t)
   ("q" "Quit" transient-quit-all)]
  (interactive)
  (transient-setup 'enime-main-transient))

(transient-define-infix enime--set-anime-episode ()
  "Sets the episode to watch."
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
			    (cl-second ep-range))
			   enime-episode-number))))


(transient-define-infix enime--set-desired-quality ()
  "Sets desired quality of episode, may not be available." 
  :class 'transient-lisp-variable 
  :variable 'enime-desired-quality 
  :description "Desired video quiality" 
  :key "-q" 
  :reader (lambda (&rest _) 
	    (let ((val (cl-second (read-multiple-choice "Choose quality: " '((?a "360") 
									     (?b "480") 
									     (?c " 720") 
									     (?d "1080")))))) 
	      (when (enime--is-anime-followed-p enime-current-anime-id) 
		(enime--update-anime-property-db enime-current-anime-id 
						 :desired-quality val))
	      val)))


(transient-define-infix enime--set-dub ()
  "Sets if the dub version is prefered."
  :class 'transient-lisp-variable
  :variable 'enime-dub
  :key "-d"
  :description "Prefer dubbed version"
  :reader (lambda (&rest _)
	    (let ((val (yes-or-no-p "Prefer dubbed version if available?")))
	      (when (enime--is-anime-followed-p enime-current-anime-id)
		(enime--update-anime-property-db enime-current-anime-id
						 :dub val))
	      val)))

(defun enime--try-play-episode (anime-id episode
					 desired-quality
					 dub
					 &optional skip-to)
  "Function for trying to play an EPISODE with posible playback skip.
Argument ANIME-ID anime of interest."
  (message "Retrieving video, please wait...")
  (if
      (enime-play-episode anime-id
			  episode
			  desired-quality
			  dub
			  skip-to)
      (progn
	(when (enime--is-anime-followed-p enime-current-anime-id)
	  (enime--update-anime-property-db
	   enime-current-anime-id
	   :current-episode
	   enime-episode-number))
	(transient-quit-all))
    (message "The episode cannot be retrieved")))

(defun enime--play-episode-action ()
  "Action for playing an episode."
  (interactive)
  (enime--try-play-episode
   enime-current-anime-id
   (number-to-string enime-episode-number)
   enime-desired-quality
   enime-dub))

(defun enime--follow-action ()
  "Action for start following an anime."
  (interactive)
  (setq enime-skip-opening-time 0)
  (setq enime-finished-at-seconds-left 0)
  (enime--follow-anime enime-current-anime-id
		       enime-episode-number
		       (enime--get-anime-description-from-key
			enime-current-anime-key)
		       (enime--get-anime-img-url-from-key
			enime-current-anime-key)
		       enime-dub
		       enime-desired-quality)
  (enime-anime-transient))



(defun enime--continue-action ()
  "Continue playback were left."
  (interactive)
  (if (not (enime--can-continue-playing?
	    enime-current-anime-id))
      (progn
	(message "No more content to play")
	(enime-anime-transient))
    (progn
      (enime--try-play-episode
       enime-current-anime-id
       (number-to-string enime-episode-number)
       enime-desired-quality
       enime-dub
       (enime--get-anime-property
	enime-current-anime-id
	:time-elapsed)))))

(defun enime--unfollow-action ()
  "Action for unfollowing an anime."
  (interactive)
  (when (yes-or-no-p "Are you sure?")
    (enime--unfollow-anime enime-current-anime-id))
  (enime-anime-transient))

(defun enime--show-details-action ()
  "Action for displaying anime details."
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
  "Stablishes skip opening time."
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
  "Stablishes skip opening time."
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
  "Transient prefix for an anime."
  [:class transient-row
	  :description
	  (lambda () (enime--get-anime-description-from-key enime-current-anime-key))
	  (enime--set-anime-episode)
	  (enime--set-desired-quality)
	  (enime--set-dub)
	  ]
  ["Actions"
   :class transient-row
   ("f" "Follow anime" enime--follow-action
    :if (lambda ()
	  (not (enime--is-anime-followed-p
		enime-current-anime-id))))
   ("c" "Continue watching" enime--continue-action
    :if (lambda ()
	  (enime--is-anime-followed-p
	   enime-current-anime-id)))
   ("p" "Play epidose from start" enime--play-episode-action)
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
   
   (enime--set-skip-opening-time)
   (enime--set-finished-at-seconds-left)
   ])

(defun enime--set-select-anime-children (_)
  "Return dinamically created suffixes acording with anime results hold in `enime--current-anime-search-results-alist'."
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
			       (setq enime-dub
				     nil)
			       (setq enime-desired-quality
				     "1080")
                               (enime-anime-transient)))))))
             enime--current-anime-search-results-alist))

(defun enime--generate-followed-anime-alist ()
  "Generate an alist for prefix selection based on `enime--followed-anime-alist-cache'."
  (let* ((keys (enime--generate-keys
		(length
		 (enime--get-followed-anime-alist)))))
    (-zip-with (lambda (anime key)
		 `(,key ,(append (list :key key)
				 (car (cdr anime)))))
	       (sort (cl-copy-list
		      (enime--get-followed-anime-alist))
		     (lambda (el1 el2) (string< (car el1) (car el2))))
	       keys)))

(defun enime--set-select-followed-anime-children (_)
  "Return dinamically created suffixes acording with followed animes."
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
					enime-current-anime-id))
				 (setq enime-skip-opening-time
				       (enime--get-anime-property
					enime-current-anime-id
					:opening-skip))
				 (setq enime-finished-at-seconds-left
				       (enime--get-anime-property
					enime-current-anime-id
					:consider-finished-left))
				 (setq enime-dub
				       (enime--get-anime-property
					enime-current-anime-id
					:dub))
				 (setq enime-desired-quality
				       (enime--get-anime-property
					enime-current-anime-id
					:desired-quality))
				 (enime-anime-transient)))))))
	       anime-alist-prefixes)))

(transient-define-prefix enime-select-anime-transient ()
  ["Select an anime"
   :setup-children enime--set-select-anime-children])

(transient-define-prefix enime-select-followed-anime-transient ()
  ["Select a followwd anime"
   :setup-children enime--set-select-followed-anime-children])

(defun enime--restore-anime-transient-after-details ()
  "After quitting a details buffer, return to previous anime transient."
  (interactive)
  (kill-current-buffer)
  (enime-anime-transient))

(defun enime--display-anime-details (img-file-path details)
  "Opens a special buffer showing an anime image and text DETAILS.
Argument IMG-FILE-PATH path of downloaded covert art file."
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
  "Downloads and save a cover art image from IMG-URL.
Argument OUT-PATH directory path for saving downloaded cover art."
  (shell-command (format "curl -o %s %s &> /dev/null" out-path img-url)))


(define-minor-mode enime--details-mode
  "Minor mode for keybinding specific to detail bufers."
  nil
  :lighter "anime details")

(defvar enime--details-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'enime--restore-anime-transient-after-details)
    map)
  "Keymap for enime details mode.")

(provide 'enime--details-mode)

;;; playback

(defun enime--can-continue-playing? (anime-id)
  "T if the anime can continue playing.
Argument ANIME-ID anime of interest."
  (interactive)
  (let* ((last-episode
	  (string-to-number
	   (cl-second
	    (enime-episodes-range anime-id))))
	 (current-episode (enime--get-anime-property
			   anime-id
			   :current-episode))
	 (time-elapsed (enime--get-anime-property
			anime-id
			:time-elapsed))
	 (current-episode-duration
	  (enime--get-anime-property
	   anime-id
	   :current-episode-duration))
	 (consider-finished-left-temp
	  (enime--get-anime-property
	   anime-id
	   :consider-finished-left))
	 (consider-finished-left ;; at least 10 to sync with timers
	  (if
	      (< consider-finished-left-temp 10)
	      10
	    consider-finished-left-temp)))
    (if (> current-episode last-episode)
	nil
      (if (< current-episode last-episode)
	  t
	(if (<= time-elapsed 0)
	    t
	  (when
	      (>
	       (- current-episode-duration time-elapsed)
	       consider-finished-left)
	    t))))))

(defun enime--start-mpv-playback (embedded video-url &optional skip-to)
  "Start playing and tracking video playback.
Argument EMBEDDED url for referencing.
Argument VIDEO-URL video url to play.
Optional argument SKIP-TO seconds to go forward in video."
  (mpv-start
   (concat "--http-header-fields=referer: "
	   embedded)
   video-url)
  (enime--wait-for-playback-start enime-current-anime-id skip-to))

(defun enime--wait-for-playback-start (anime-id &optional skip-to)
  "Keep telling the user that the video is still loading.
Argument ANIME-ID anime of interest.
Optional argument SKIP-TO seconds to skip forward."
  (setq enime--loading-timer
	(run-with-timer
	 2 5
	 (lambda ()
	   (message "Video still loading (you can abort with mpv-kill)")
	   (when (not (mpv-live-p))
	     (progn (cancel-timer enime--loading-timer)
		    (message "Cannot play video")))
	   (when
	       (condition-case nil
		   (mpv-get-playback-position)
		 (error nil))
	     (progn (cancel-timer enime--loading-timer)
		    (enime--try-to-skip anime-id skip-to)
		    (message "Enjoy!!!")
		    (when (enime--is-anime-followed-p anime-id)
		      (enime--update-anime-property-db
		       anime-id
		       :current-episode-duration
		       (mpv-get-duration))
		      (enime--update-anime-data-while-playing
		       anime-id))))))))

(defun enime--try-to-skip (anime-id &optional skip-to)
  "Skips opening or skips to continue playing.
Argument ANIME-ID anime of interest.
Optional argument SKIP-TO seconds to skip forward."
  (if (and skip-to
	   (> skip-to 0))
      (mpv-seek skip-to)
    (let ((skip-time
	   (enime--get-anime-property anime-id :opening-skip)))
      (when (and skip-time
		 (> skip-time 0))
	(mpv-seek skip-time)))))

(defun enime--update-anime-data-while-playing (anime-id)
  "Update current playback time in db.
Argument ANIME-ID anime of interest."
  (setq enime--playing-timer
	(run-with-timer
	 5 10
	 (lambda ()
	   (when (not (mpv-live-p))
	     (cancel-timer enime--playing-timer))
	   (let ((playback-time
		  (condition-case nil
		      (mpv-get-playback-position)
		    (error nil))))
	     (when playback-time
	       (enime--update-anime-property-db
		anime-id
		:time-elapsed
		playback-time)))))))


(defun enime-play-episode (anime-id episode
				    desired-quality dub &optional skip-to)
  "Opens an anime EPISODE in mpv.
Argument ANIME-ID anime of interest."
  (let ((episodes-range (enime-episodes-range anime-id)))
    (when (and (>= (string-to-number episode) (string-to-number (car episodes-range)))
	       (<= (string-to-number episode) (string-to-number (cl-second episodes-range))))
      (let* ((dub-prefix (if dub  enime--dub-prefix ""))
	     (scrapped-urls (enime--scrap-embbeded-and-video anime-id episode desired-quality dub-prefix))
	     (embedded (car scrapped-urls))
	     (video-url (cl-second scrapped-urls)))
	(if (enime--good-video-url-p video-url)
	    (progn
	      (enime--start-mpv-playback embedded video-url skip-to)
	      t)
	  nil)))))

(provide 'enime)

;;; enime.el ends here
