(load "esxml-query.el") ;; dependency 

(defgroup enime nil
  "Yet another Pomodoro timer implementation."
  :group 'apps)


(defcustom enime-base-url "https://www1.gogoanime.cm/"
  "Gogo anime base url"
  :group 'enime
  :type 'string)

(defcustom enime-tmp-dir "/tmp"
  "Directory to save temporal data like anime cover art"
  :group 'enime
  :type 'string)

(defun enime-return-parsing-tree-from-request (url params)
  "makes the request, using html parsing, only works for get"
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
  "makes a request and returns the obtained string without parsing,
useful for regexp"
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
  "returns the anime title from a node"
  (xml-get-attribute node 'title))

(defun enime-get-anime-id-from-node (node)
  "returns the nime id from node"
  (let ((href (xml-get-attribute node 'href)))
    (car (last (split-string href "/")))))

(defun enime-get-anime-img-src-from-node (node)
  "returns the image src value for an anime"
  (xml-get-attribute (esxml-query "img" node) 'src))

(defun enime--extract-text-description-rec (&optional current-list)
  "Returns a string containing the text of a node element"
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
  "Returs anime description from an anime id"
  (let* ((uri (concat "/category/" anime-id))
	 (url (concat enime-base-url uri))
	 (tree
	  (enime-return-parsing-tree-from-request url nil))
	 (nodes (esxml-query-all "div.anime_info_body_bg>p.type" tree)))
    (reduce (lambda (str1 str2)
	      (concat str1 "\n\n" str2))
	    (mapcar (lambda (node)
		      (enime--extract-text-description-rec node))
		    (butlast nodes)))))

(defun enime-normalize-search-string (string)
  "gets ride of spaces and joins with -"
  (let ((parts (split-string string)))
    (reduce (lambda (str1 str2)
	      (concat str1 "-" str2))
	    parts)))


(defun enime--pages-search-anime (tree)
  "Returns the number of pages found in an anime search"
  (let ((pags (esxml-query-all ".pagination-list li" tree)))
    (length pags)))


(defun enime--process-candites-search-anime-page (tree)
  "Returns candidates found in a parse tree"
  (mapcar (lambda (node)
	    `(,(enime-get-anime-id-from-node node)
	      ,(enime-get-anime-title-from-node node)
	      ,(enime-get-anime-img-src-from-node node)))
	  (esxml-query-all "div>a[href^=\"/category/\"]" tree)))

(defun enime--collect-candidates-search-anime-pages (base-url pages)
  "Concatenates result candidates from all pages of a search anime"
  (reduce #'append
	  (mapcar
	   (lambda (num)
	     (let ((tree
		    (enime-return-parsing-tree-from-request base-url `(("keyword" . ,name) ("page" . ,(number-to-string num))))))
	       (enime--process-candites-search-anime-page tree)))
	   (number-sequence 2 pages))))

(defun enime-search-anime (anime-name)
  "searches for posible anime candidates from anime-name, returns a list of candidates
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
      (append candidates-fist-page (enime--collect-candidates-search-anime-pages url pages)))))

(defun enime-get-min-episode (tree)
  "returns the first episode found from a parse treee"
  (xml-get-attribute (car (esxml-query-all "#episode_page li a" tree)) 'ep_start))

(defun enime-get-max-episode (tree)
  "returns the last episode found from a parse treee"
  (xml-get-attribute (car (last (esxml-query-all "#episode_page li a" tree))) 'ep_end))

(defun enime-episodes-range (anime-id)
  "returns a numeric range (inclusive in both sides) of the first
and last episode availabe"
  (let* ((uri (concat "/category/" anime-id))
	 (url (concat enime-base-url uri))
	 (tree
	  (enime-return-parsing-tree-from-request url nil))
	 (first-ep (enime-get-min-episode tree))
	 (last-ep (enime-get-max-episode tree)))
    (if (= (string-to-number first-ep) 0)
	`("1" ,last-ep)
      `(,first-ep ,last-ep))))

(defun enime-get-embedded-video-link (anime-id episode &optional dub)
  "returns the url of video asociated to anime episode
optional parameter if the anime supports dub version"
  (let* ((uri (if dub  (concat "/" anime-id "-dub" "-episode-" episode)
		(concat "/" anime-id "-episode-" episode)))
	 (url (concat enime-base-url uri))
	 (tree
	  (enime-return-parsing-tree-from-request url nil))	 
	 )
    (concat "https:"
	    (xml-get-attribute
	     (car (esxml-query-all "a[rel=\"100\"]" tree)) 'data-video))))


(defun enime-get-video-url (embedded-url)
  "returns video url from embedded url"
  (let* (
	 (prev (string-match "http[^?]+\\?\\(id=.*\\)" embedded-url))
	 (beginning (match-beginning 1))
	 (end (match-end 1)))
    (concat "https://gogoplay1.com/download?" (substring embedded-url beginning end))))


(defun enime--get-video-links (video-url)
  "Returns a list of available video urls with different qualities"
  (let ((text (enime-return-raw-text-from-request
	       video-url)))
    (mapcar (lambda (intern) (car intern))
	    (s-match-strings-all "http.+\\.com\\/cdn.+expiry=[0-9]+" text))))

(defun enime-get-available-qualities (video-links)
  "returns an alist of available video qualities asociated with their url"
  (when video-links
    (mapcar (lambda (link)
	      (let* ((prev (string-match
			    "http.+\\.com\\/cdn.+\\.\\([0-9]+\\)p\\."
			    link))
		     (beginning (match-beginning 1))
		     (end (match-end 1)))
		`(,(substring link beginning end) ,link)))
	    video-links)))



(defun enime-set-quality (alist-links desired-quality)
  "cheks if the desired quality is available in alist, if not it returns the
higuer quality, i.e.; the last element in alist"
  (if (member desired-quality
	      (mapcar (lambda (element)
			(car element))
		      alist-links))
      desired-quality
    (caar (last alist-links))))


(defun enime--clean-url (url)
  "Deletes amp; from url"
  (reduce #'concat
	  (split-string url "amp;")))

(defun enime-get-links (embedded-url desired-quality)
  "returns a video url with quality, tries to get video with
desired quality, if not available gets the higher quality"
  (let* ((video-url (enime-get-video-url embedded-url))
	 (links (enime--get-video-links video-url))
	 (alist-links (enime-get-available-qualities links))
	 (quality (enime-set-quality alist-links desired-quality))
					; maybe get the tmp url?
	 )
    (if quality
	(enime--clean-url (car (cdr (assoc quality alist-links))))
      (enime--clean-url video-url)) ;; some videos do not have quality info, not sure if still useful
    ))

(defun enime--good-video-url-p (video-url)
  "t if the video url is valid, nil in other case"
  (if (< (length video-url) 10)
      nil
    (string= "http" (substring video-url 0 4))))

(defun enime-play-episode (anime-id episode desired-quality)
  "opens an anime episode in mpv, it also checks if the episode is
in the range of available episodes "
  (let ((episodes-range (enime-episodes-range anime-id)))
    (when (and (>= (string-to-number episode) (string-to-number (car episodes-range)))
	       (<= (string-to-number episode) (string-to-number (second episodes-range))))
      (let* ((embedded (enime-get-embedded-video-link anime-id episode))
	     (video-url (enime-get-links embedded desired-quality)))
	(setq uurl video-url)
	(if (enime--good-video-url-p video-url)
	    (make-process
	     :name "mpv-enime"
	     :command `("mpv" 
			,video-url))
	  nil)))))

