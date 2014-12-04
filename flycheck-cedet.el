(require 'dash)
(require 'flycheck)


;;;; EDE project support
(defun flycheck-cedet-get-ede-cpp-project (file)
  "Get EDE C/C++ project for FILE.
If FILE does not belong to an EDE C/C++ project, or if EDE is not present, this
function returns nil."
  (when (featurep 'ede)
    (let ((project (ede-current-project (expand-file-name file))))
      (when (ede-cpp-root-project-p project)
        project))))

(defun flycheck-cedet-get-ede-cpp-project-includes (project)
  "Get include paths from EDE C/C++ PROJECT."
  (when (ede-cpp-root-project-p project)
    (let* ((root-path (ede-project-root-directory project))
           (include-paths (oref project include-path)))
      (--map (expand-file-name it root-path) include-paths))))


;;;; Project helpers
(defun flycheck-cedet-get-cpp-includes (option-name file)
  "Construct a list of includes using OPTION-NAME for the specified FILE.
If FILE is associated with a supported project type and if it has a list of
include paths a list consisting of OPTION-NAME concatenated with each path
will be constructed."
  (--map (concat option-name it)
         (let ((ede-proj (flycheck-cedet-get-ede-cpp-project file)))
           (cond
            (ede-proj (flycheck-cedet-get-ede-cpp-project-includes ede-proj))))))

(defun flycheck-cedet-get-cpp-definitions (option-name file)
  "Construct a list of definitions using OPTION-NAME for the specified FILE.
If FILE is associated with a supported project type and if it has a list of
definitions a list consisting of OPTION-NAME concatenated with the name of the
definition followed by an optional =<value> will be constructed."
  (--map (let ((macro (car it))
               (value (cdr it)))
           (concat option-name macro
                   (unless (string= "" value)
                     (concat "=" value))))
         (let ((ede-proj (flycheck-cedet-get-ede-cpp-project file)))
           (cond
            (ede-proj (ede-preprocessor-map ede-proj))))))


(provide 'flycheck-cedet)
;;; flycheck-cedet.el ends here
