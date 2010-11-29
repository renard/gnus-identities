;;; gnus-identities.el --- Change identity when composing a message.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: gnus, identities
;; Created: 2010-11-29
;; Last changed: 2010-11-29 16:37:09
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 
;; (see gnus-identities-change for details)

;;; Code:

(defcustom gnus-identities-preserve-headers '("To" "Subject" "Gcc")
  "List of header to preserve when changing identity using
`gnus-identities-change'.
Please note that all other headers would be removed.

This shouldn't be changed unless you know what you are doing."
  :group 'gnus-message
  :type 'list)


(defun gnus-identities-list ()
  "List all identities defined in `gnus-posting-styles'.
An identity is defined by 'identity symbol."
  (let (r)
    (mapcar
     '(lambda (x)
	(mapcar
	 '(lambda (y)
	    (when (and (listp y) (eq 'x-identity (car y)))
	      (add-to-list 'r (cadr y))))
	 x))
     gnus-posting-styles)
    r))


(defun gnus-identities-get-id (id)
  "Returns configuration defined by ID in `gnus-posting-styles'."
  (let (r)
    (mapcar
     '(lambda (x)
	(mapcar
	 '(lambda (y)
	    (when (and (listp y) (eq 'x-identity (car y))
		       (stringp (cadr y))
		       (string= id (cadr y)))
	      (setq r (cdr x))))
	 x))
     gnus-posting-styles)
    r))


(defun gnus-identities-message-list-headers ()
  "List all headers from a message."
  (save-excursion
    (save-restriction
      (beginning-of-buffer)
      (let ((ret nil))
	(when
	    (re-search-forward  "^\\([^:]+\\):" (point-at-eol) t)
	  (add-to-list 'ret (substring-no-properties (match-string 1))))
	(while (message-next-header)
	    (when
		(re-search-forward "^\\([^:]+\\):" (point-at-eol) t)
	      (add-to-list 'ret (substring-no-properties (match-string 1)))))
	ret))))


(defun gnus-identities-change (id)
  "Change message identity to ID.

All headers (but those defined in
`gnus-identities-preserve-headers') and signature would be
removed and replaced by the ones defined in ID section of
`gnus-posting-styles'.

In order to make `gnus-identities-change', an \"x-identity\" header
has to be defined in every item from `gnus-posting-styles' which
could be something like:

  (setq gnus-posting-styles
     '((\".*\"
        (x-identity \"default\")
        (name \"John Doe\")
	(address \"jdoe@example.com\")
	(organization \"John\"))
       (\"nnimap+somewhere:INBOX\"
        (x-identity \"jd\")
        (name \"John D.\")
	(address \"john.doe@example.com\")
	(signature \"John\"))))"
  (interactive
   (list (completing-read  "Identity: " (gnus-identities-list) nil 1 )))

  (save-excursion
    (save-restriction

      ;; remove all headers
      (mapcar '(lambda (h)
		 (unless (member h gnus-identities-preserve-headers)
		   (message-remove-header h)))
	      (gnus-identities-message-list-headers))

      ;; remove signature
      (end-of-buffer)
      (when (re-search-backward message-signature-separator nil t)
	(delete-region (1- (point)) (point-max)))

      ;; Insert headers
      (let ((gnus-posting-styles `((".*" ,@(gnus-identities-get-id id))))
	    (message-setup-hook nil))
	(mapcar '(lambda (x)
		   (when (functionp x)
		     (funcall x)))
		(gnus-configure-posting-styles gnus-newsgroup-name)))
      ;; Remove identity header
      (message-remove-header "x-identity"))))
	     
(add-hook 'message-setup-hook '(lambda () (message-remove-header "x-identity")))
(define-key message-mode-map "\C-c\C-p" 'gnus-identities-change)


(provide 'gnus-identities)
