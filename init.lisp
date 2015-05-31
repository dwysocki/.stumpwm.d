;; -*-lisp-*-
;;
;; init.lisp

(in-package :stumpwm)


;; load external rc files

(defvar *load-directory*
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwm.d")))
  "A directory with initially loaded files.")

(defun load-file (filename)
  "Load a file FILENAME (without extension) from `*load-directory*'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               *load-directory*)))
    (if (probe-file file)
        (load file)
        (format *error-output* "File '~a' doesn't exist." file))))

(defun load-files (filenames)
  "Load a list of files (without extensions) from `*load-directory*'."
  (mapcar #'load-file filenames))


;; load all files
(load-files
  '("util"
    "gaps"
    "keys"
    "layout"
    "mode-line"
    "mouse"
    "system"))
