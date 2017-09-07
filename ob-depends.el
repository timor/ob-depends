;;; Support dependencies between source blocks for execution
;;; Implements a new header arg :depends, which lists names of source
;;; blocks or names of subtrees or the symbol :serial.  When the block
;;; is executed, the dependent source blocks are executed before that.

;;; Circular dependencies result in an endless loop, so be careful.

;;; NOTE: dependencies into subtrees do not nest.

(require 'ob-core)

(defun org-babel-depends-find-prev ()
  "Find location of previous org src block in same subtree, return nil of none was found."
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (condition-case nil
	  (progn
	    (org-babel-previous-src-block)
	    (point))
	(user-error nil)))))

(defun org-babel-execute-src-block-with-dependencies (&optional ignore-serial)
  "Execute all :depends arguments, then execute src block."
  (interactive)
  (let* ((params (nth 2 (org-babel-get-src-block-info)))
	 (dep (cdr (assoc :depends params))))
    (when dep
      (let* ((deps (split-string dep))
	     (blocks (loop for d in deps collect
			   (if (and (not ignore-serial) (string-equal d "serial"))
			       (let ((loc (org-babel-depends-find-prev)))
				 (when loc
				   (list :block loc)))
			     (let* ((s (split-string d ":"))
				    (key (first s))
				    (val (second s)))
			       (pcase key
				 ;; block:blockname
				 ("block" (let ((loc (org-babel-find-named-block val)))
					    (if loc
						(list :block loc)
					      (user-error "cannot depend on unknown block %s" val))))
				 ;; id:subtree-id
				 ("id" (let ((loc (org-find-entry-with-id val)))
					 (if loc
					     (list :subtree loc)
					   (user-error "cannot depend on unknown entry %s" val))))))))))
	(loop for spec in blocks when spec do
	      (let ((loc (second spec))
		    (type (first spec)))
		(save-excursion
		  (goto-char loc)
		  (case type
		    (:block (org-babel-execute-src-block-with-dependencies))
		    (:subtree (org-babel-execute-subtree))))))))
    (org-babel-execute-src-block)))

(defun org-babel-depends-ctrl-c-ctrl-c-hook ()
  (org-babel-when-in-src-block
   (org-babel-execute-src-block-with-dependencies)
   :exec-depend))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-depends-ctrl-c-ctrl-c-hook)

(provide 'ob-depends)
