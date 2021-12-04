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
