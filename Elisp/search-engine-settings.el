;; Google Search

(setq search-engines
      '(
        (("google" "g") "https://google.com/search?q=%s")
        (("duckduckgo" "d" "ddg") "https://duckduckgo.com/lite/?q=%s")
        (("rfc" "r") "https://www.rfc-editor.org/rfc/rfc%s.txt")
        (("rfc-kw" "rk") "https://www.rfc-editor.org/search/rfc_search_detail.php?title=%s")
        ))

(setq search-engine-default "duckduckgo")

(defun search-get-engine (engine-name engine-list)
  (cond
   ((null engine-list) nil)
   ((member engine-name (caar engine-list)) (cadar engine-list))
   (t (search-get-engine engine-name (cdr engine-list)))))

(defun search-engine (engine-name term)
  "Search for a term using an engine."
  (interactive "MEngine: \nMTerm: ")
  (let* ((url (search-get-engine engine-name search-engines)))
    (if (equal url nil)
        (message "Error: search engine \"%s\" unknown." engine-name)
      (eww (format url (url-hexify-string term))))))

(defun search-web (term)
  "Search the web using google or a specified engine."
  (interactive "MQuery: ")
  (let ((idx (position ?: term)))
    (if (equal idx nil)
        (search-engine search-engine-default term)
      (search-engine (subseq term 0 idx)
                     (subseq term (+ 1 idx))))))

(global-set-key (kbd "C-c w") 'search-web)

(setq eww-search-prefix "https://google.com/?q=")
