;; Project Settings

;; (setq org-publish-project-alist
;;       '(("org-notes-mathql"
;;          :base-directory "~/mathql/org/"
;;          :base-extension "org"
;;          :publishing-directory "~/mathql/www_/"
;;          :publishing-function org-html-publish-to-html
;;          :exclude "PrivatePage.org" ;; regexp
;;          :headline-levels 3
;;          :section-numbers nil
;;          :with-toc nil
;; 	 :html-doctype "html5"
;; 	 :html-link-home "/index.html"
;; 	 :html-link-up "index.html"
;; 	 :html-head-include-default-style nil
;; 	 :html-head-include-scripts t
;; 	 :html5-fancy t
;; 	 :recursive t
;;          :html-head " <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
;; <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
;; <link rel=\"stylesheet\" type=\"text/css\" href=\"/assets/css/style.css\" />"
;;          :html-preamble t
;; 	 :html-preamble-format (("en" "<div id=\"logo\"><a href=\"/index.html\">MathQL Theorem Solver</a></div>"))
;; 	 :html-postamble-format (("en" "<p class=\"Date\">Last updated: %d</p>"))
;; 	 :html-postamble t)

;; 	("org-static-mathql"
;;        :base-directory "~/mathql/assets/"
;;        :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;;        :publishing-directory "~/mathql/www_/assets/"
;;        :recursive t
;;        :publishing-function org-publish-attachment)      
        
;;         ("mathql" :components ("org-notes-mathql" "org-static-mathql"))))
