(defun top-10-buffers ()
  (interactive)
  (lexical-let ((buffer-list (buffer-list)))
    (pop-to-buffer-same-window "My Buffer List")
    (erase-buffer)
    (insert "Top 10 Buffers \n")
    (insert "-------------- \n")
    (loop for item in buffer-list
	  for index from 0 to 9
	  do (insert (format "    [%s]  %s~\n" index item)))
    (special-mode)
    (use-local-map (copy-keymap special-mode-map))
    (loop for item in buffer-list
	  for index from 0 to 9
	  do (local-set-key (format "%s" index) `(lambda ()
						   (interactive)
						   (switch-to-buffer ',item))))
    (local-set-key (format "%s" index) #'(lambda ()
					   (interactive)
					   (switch-to-buffer (nth 0 buffer-list))))))
