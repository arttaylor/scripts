;;; check-url-update.el --- Check URLs for updates since last visited
;;
;; Copyright (C) 2003 Art Taylor
;;
;; Filename: uniq.el
;; Author: Art Taylor <art@arttaylor.com>
;; Version: 1.0
;; Keywords: uniq, duplicates
;;
;; [Commentary]
;;
;; I was annoyed at http://www.ChangeDetect.com, so I wrote this.  Use
;; this script, and when you get sick of it in twenty minutes, write a
;; real program, with real threading, and a real UI, and then give me
;; a copy.  Tested on XEmacs 21.4.
;; Usage: UTSL.  Hackery needed.
;;
;; [License]
;;
;; This software is provided 'as-is', without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:
;;
;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software. If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.
;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.
;; 3. This notice may not be removed or altered from any source
;;    distribution.
;;
;; Note that this license is borrowed from zlib via nullsoft.
;;
;; Written 12-Feb-2003, Washington, DC
;;

(require 'url)
(require 'md5)

(setq output-buffer-name "*url-updates*")

(defun get-new-hash (url)
  "Hash the page found at the given url."
  (md5 (let ((url-multiple-p nil))
	 (save-excursion
	   (save-window-excursion
	     (url-retrieve url)))
	 (buffer-string url-working-buffer))))

(defun save-url-alist ()
  "Save my-url-alist to the file."
  (with-temp-buffer 
    (insert "(setf my-url-alist '" )
    (prin1 my-url-alist (current-buffer))
    (insert ") ")
    (write-region (point-min) (point-max) (expand-file-name "~/.check-url.el"))))

(defun check-url-for-update (x)
  "Check the page at the url in the car of the cons cell for 
   an update, comparing its hash to that in the cdr."
  (let ((url (car x))
	(oldhash (cdr x))
	(newhash (get-new-hash (car x))))
    (if (not (string= oldhash newhash))
	(progn
	  (princ (concat (current-time-string) ":" url "\n") (get-buffer-create output-buffer-name))
	  (setf (cdr x) newhash)))))

(defun check-updates ()
  "Check all of the urls in my-url-alist for updates."
  (load-file (expand-file-name "~/.check-url.el"))
  (mapcar #'check-url-for-update my-url-alist)
  (save-url-alist))

(defun add-url (url)
  "Add the given url to my-url-alist and save to file."
  (if (not (assoc url my-url-alist))
      (progn 
	(setf my-url-alist (cons `(,url . "1") my-url-alist))
	(save-url-alist))))

(load-file (expand-file-name "~/.check-url.el"))

(add-url "http://lambda.weblogs.com")
(add-url "http://blogs.arttaylor.com")

(check-updates)

