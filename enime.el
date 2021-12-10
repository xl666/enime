(load "esxml-query.el") ;; dependency 

(setq enime-base-url "https://www1.gogoanime.cm/")

(defun enime-return-parsing-tree-from-request (url params)
  "Makes the request, using html parsing, only works for GET"
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
  "Makes a request and returns the obtained string without parsing,
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
  "Returns the anime title from a node"
  (xml-get-attribute node 'title))

(defun enime-get-anime-id-from-node (node)
  "Returns the nime ID from node"
  (let ((href (xml-get-attribute node 'href)))
    (car (last (split-string href "/")))))

(defun enime-get-anime-img-src-from-node (node)
  "Returns the image src value for an anime"
  (xml-get-attribute (esxml-query "img" node) 'src))

(defun enime-normalize-search-string (string)
  "gets ride of spaces and joins with -"
  (let ((parts (split-string string)))
    (reduce (lambda (str1 str2)
	      (concat str1 "-" str2))
	    parts)))

(defun enime-search-anime (anime-name)
  "Searches for posible anime candidates from anime-name, returns a list of candidates
A candidate is a list of id title img-src"
  (let* ((name (enime-normalize-search-string anime-name))
	 (uri "/search.html")
	 (url (concat enime-base-url uri))
	 (tree
	  (enime-return-parsing-tree-from-request url `(("keyword" . ,name)))))
    (mapcar (lambda (node)
	      `(,(enime-get-anime-id-from-node node)
		,(enime-get-anime-title-from-node node)
		,(enime-get-anime-img-src-from-node node)))
	    (esxml-query-all "div>a[href^=\"/category/\"]" tree))))

(defun enime-get-min-episode (tree)
  "Returns the first episode found from a parse treee"
  (xml-get-attribute (car (esxml-query-all "#episode_page li a" tree)) 'ep_start))

(defun enime-get-max-episode (tree)
  "Returns the last episode found from a parse treee"
  (xml-get-attribute (car (last (esxml-query-all "#episode_page li a" tree))) 'ep_end))

(defun enime-episodes-range (anime-id)
  "Returns a numeric range (inclusive in both sides) of the first
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
  "Returns the url of video asociated to anime episode
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
  "Returns video url from embedded url"
  (let* ((text (enime-return-raw-text-from-request embedded-url))
	 (prev (string-match "sources:\\[{file: '\\([^']+\\)" text))
	 (beginning (match-beginning 1))
	 (end (match-end 1)))
    (substring text beginning end)))

(defun enime-get-video-file-details (embedded-url video-url)
  "Gets the actual video file"
  (let ((request-curl-options `(,(format "--referer %s" embedded-url))))
    (enime-return-raw-text-from-request
     video-url)))

(defun enime-get-available-qualities (video-file)
  "Returns a list of available video qualities from video file details"
  (let* ((positions (s-matched-positions-all "Name=\"\\([0-9]+\\)" video-file)))
    (mapcar (lambda (pair)
	      (substring video-file (+ 6 (car pair)) (cdr pair))) ;; +6 to get ride of NAME=\"
	    positions))
  )

(defun enime-set-quality (video-file desired-quality)
  "cheks if the desired quality is available, if not it returns the
higuer quality"
  (let* ((qualities (enime-get-available-qualities video-file)))
    (if (member desired-quality qualities)
	desired-quality
      (car (last qualities)))))

(defun enime-get-links (embedded-url desired-quality)
  "Returns a video url with cuality, tries to get video with
desired quality, if not available gets the higher quality"
  (let* ((video-url (enime-get-video-url embedded-url))
	 (video-file (enime-get-video-file-details embedded-url video-url))
	 (quality (enime-set-quality video-file desired-quality))
					; maybe get the tmp url?
	 )
    (if quality
	(concat (substring video-url 0 -4) quality ".m3u8")
      video-url) ;; some videos do not have quality info
    ))

(let* ((embedded (enime-get-embedded-video-link "dragon-quest-dai-no-daibouken-2020" "60"))
       (video-url (enime-get-links embedded "1080")))
  (make-process
   :name "mpv-enime"
   :command `("mpv" ,(concat "--http-header-fields=Referer: " embedded)
	      ,video-url))
					;(format "mpv --http-header-fields=\"Referer: %s\" \"%s\"" embedded video-url)

  )

