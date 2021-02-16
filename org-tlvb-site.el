(require 'ox-html)
(require 'seq)

(org-link-set-parameters "image"
			 :follow nil
			 :export nil
			 :store  nil)

(org-link-set-parameters "gallery"
			 :follow nil
			 :export nil
			 :store  nil)

(org-export-define-derived-backend 'tlvb-site-html 'html
  :menu-entry
  '(?T tlvb-site-html
       ((?H "As HTML buffer" tlvb-site-export-as-html)
	(?h "As HTML file" tlvb-site-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (tlvb-site-export-to-html t s v b)
		(org-open-file (tlvb-site-export-to-html nil s v b)))))))
  :options-alist
  '((:thumbnail-th-gm-command nil nil tlvb-site-th-gm-command)
    (:thumbnail-th-light-gm-command nil nil tlvb-site-th-light-gm-command)
    (:thumbnail-th-dark-gm-command nil nil tlvb-site-th-dark-gm-command))
  :translate-alist '((link . tlvb-site-link)))


(defconst tlvb-site-thumbnail-types
  '("th" "th-light" "th-dark"))

(defcustom tlvb-site-th-gm-command
  (lambda (file thumb-file thumb-type)
    (format "convert %s -scale 256x256 %s\n" file thumb-file))
  "function to, when given the FILE THUMB-FILE THUMB-TYPE
construct a command string to send to GraphicsMagic in order
to create the thumbnail THUMB-FILE from FILE.
THUMB-TYPE will be ``th''"
  :group 'org-export-tlvb-site
  :version "27.1"
  :package-version '(org-tlvb "0")
  :type 'function)

(defcustom tlvb-site-th-light-gm-command
  (lambda (file thumb-file thumb-type)
    (format "convert %s -scale 256x256 -gamma 2 %s\n" file thumb-file))
  "function to, when given the FILE THUMB-FILE THUMB-TYPE
construct a command string to send to GraphicsMagic in order
to create the light-theme thumbnail THUMB-FILE from FILE.
THUMB-TYPE will be ``th-light''"
  :group 'org-export-tlvb-site
  :version "27.1"
  :package-version '(org-tlvb "0")
  :type 'function)

(defcustom tlvb-site-th-dark-gm-command
  (lambda (file thumb-file thumb-type)
    (format "convert %s -scale 256x256 -gamma 0.5 %s\n" file thumb-file))
  "function to, when given the FILE THUMB-FILE THUMB-TYPE
construct a command string to send to GraphicsMagic in order
to create the dark-theme thumbnail THUMB-FILE from FILE.
THUMB-TYPE will be ``th-dark''"
  :group 'org-export-tlvb-site
  :version "27.1"
  :package-version '(org-tlvb "0")
  :type 'function)

(defun tlvb-site-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'tlvb-site-html "*Org tlvb site html Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

(defun tlvb-site-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((file (org-export-output-file-name "html" subtreep)))
    (org-export-to-file 'tlvb-site-html file
      async subtreep visible-only body-only ext-plist (lambda () (html-mode)))))

(defun tlvb-site-link (link desc info)
  "Call own processing for image and gallery type links, fall back on org-html-link for others."
  (let ((type (org-element-property :type link))
	(target (org-element-property :path link)))
    (cond ((string= "image" type)
	   (tlvb-image-process-and-html-for target))
	  ((string= "gallery" type)
	   (tlvb-gallery-process-and-html-for target))
	  (t
	   (org-html-link link desc info)))))

(defun maybe-create-dir (dir)
  "Create dir unless it exists."
  (if (not (file-exists-p dir)) (make-directory dir) nil))

(defun concat-non-nil (seq sep)
  "concat non-nil items of SEQ separated by SEP"
  (mapconcat 'identity (seq-filter (lambda (x) x) seq) sep))

(defun thumb-file-name (thumb-dir file-base thumb-type file-ext)
  (concat thumb-dir file-base "." thumb-type file-ext))

(defun gm-process-of (file)
  "Builds string with commands for creating all thumbnails, for use with gm-batch."
  (let* ((file-dir            (or (file-name-directory file) "./"))
	 (file-base           (file-name-base file))
	 (file-ext            (file-name-extension file t))
	 (thumb-dir           (concat file-dir "thumbnails/")))
    (concat-non-nil (seq-map (lambda (thumb-type)
			       (let* ((thumb-file (thumb-file-name thumb-dir file-base thumb-type file-ext))
				      (gm-command-fun (symbol-value (intern-soft (format "tlvb-site-%s-gm-command" thumb-type)))))
				 (if (file-newer-than-file-p file thumb-file)
				     (funcall gm-command-fun file thumb-file thumb-type))))
			     tlvb-site-thumbnail-types)
		    "\n")))

(defun html-thumbnail-link (file)
  "Builds html string containing thumbnail and link tags."
  (let* ((file-dir     (or (file-name-directory file) ""))
	 (file-base    (file-name-base file))
	 (file-ext     (file-name-extension file t))
	 (thumb-dir    (concat file-dir "thumbnails/"))
	 (thumb-normal (elt tlvb-site-thumbnail-types 0))
	 (thumbs-other (seq-drop tlvb-site-thumbnail-types 1)))
    (concat (format "<a href=\"%s\" class=\"%s\" style=\"background-image: url(%s);\">" file thumb-normal (thumb-file-name thumb-dir file-base thumb-normal file-ext))
	    (mapconcat (lambda (thumb-type)
		       (format "<img class=\"%s\" src=\"%s\">" thumb-type (thumb-file-name thumb-dir file-base thumb-type file-ext)))
		       thumbs-other
		       "")
	    "</a>")))

(defun gm-batch (gm-command)
  "Run GM-COMMAND as a graphicsmagick batch job unless it is nil."
  (if gm-command
      (let ((gm-proc (make-process :name "GraphicsMagick" :command '("gm" "batch" "-prompt" "off" "-echo" "off") :connection-type 'pty)))
	(progn
	  (process-send-string gm-proc gm-command)
	  (process-send-eof gm-proc)))))


(defun image-process (file)
  "Process an image FILE, create a thumbnail dir if needed, and three thumbnails, regular and light/dark theme variants."
  (let* ((file-dir (or (file-name-directory file) "./"))
	 (thumb-dir (concat file-dir "thumbnails/"))
	 (gm-command (gm-process-of file)))
    (progn
      (maybe-create-dir thumb-dir)
      (gm-batch gm-command))))

(defun tlvb-image-process-and-html-for (file)
  "Generate thumbnails and html for an image FILE."
  (progn
    (image-process file)
    (html-thumbnail-link file)))

(defun tlvb-gallery-process-and-html-for (dir)
  "Generate thumbnails and html for a gallery DIR."
  (let* ((exts '("png" "jpg"))
	 (fileregex (mapconcat (lambda (s) (format ".*\\.%s" s)) exts "\\|"))
	 (dir-slash (concat (directory-file-name dir) "/"))
	 (files (mapcar (lambda (f) (concat dir-slash f)) (directory-files dir nil fileregex)))
	 (html-data ""))
    (progn
      (maybe-create-dir (concat dir-slash "thumbnails/"))
      (gm-batch (mapconcat (lambda (file) (gm-process-of file)) files ""))
      (concat "<div class=\"gallery\">\n"
	      (dolist (file files html-data)
		(setq html-data (concat html-data (html-thumbnail-link file) "\n")))
	      "</div>"))))

(provide 'org-tlvb-site)


;; (defun maybe-create-thumb-file-gm-command (file thumb-file)
;;   "graphicsmagick command to create a thumbnail file.
;; If THUMB-FILE exists and is newer than FILE returns nil,
;; otherwise a string usable with gm-batch"
;;   (if (file-newer-than-file-p file thumb-file)
;;       (format "convert %s -scale ^256x256 -gravity center -crop 256x256+0+0 %s\n"
;; 	      file thumb-file)
;;     nil))

;; (defun maybe-create-thumb-file-light-gm-command (file thumb-file)
;;   "Graphicsmagick command to create a light theme thumbnail file.
;; If THUMB-FILE exists and is newer than FILE returns nil,
;; otherwise a string usable with gm-batch."
;;   (if (file-newer-than-file-p file thumb-file)
;;       (let ((dark-color "222233")
;; 	    (light-color "ffddaa"))
;; 	(format (concat "convert %s -scale ^512x512 -normalize -gravity center -crop 512x512+0+0"
;; 			" -colorspace gray -rotate -45 -ordered-dither intensity 5x5 -rotate 45"
;; 			" -gravity center -crop 512x512+0+0 -scale 256x256 mpr:halftone"
;; 			"\n"
;; 			"composite -size 256x256 xc:\\#%s xc:\\#%s mpr:halftone %s"
;; 			"\n")
;; 		file light-color dark-color thumb-file))
;;     nil))

;; (defun maybe-create-thumb-file-dark-gm-command (file thumb-file)
;;   "Graphicsmagick command to create a dark theme thumbnail file.
;; If THUMB-FILE exists and is newer than FILE returns nil,
;; otherwise a string usable with gm-batch."
;;   (if (file-newer-than-file-p file thumb-file)
;;       (let ((dark-color "102000")
;; 	    (light-color "707f4f"))
;; 	(format (concat "convert %s -scale ^512x512 -normalize -gravity center -crop 512x512+0+0"
;; 			" -colorspace gray -ordered-dither intensity 3x3"
;; 			" -scale 256x256 mpr:lcd"
;; 			"\n"
;; 			"composite -size 256x256 xc:\\#%s xc:\\#%s mpr:lcd %s"
;; 			"\n")
;; 		file light-color dark-color thumb-file))
;;     nil))
