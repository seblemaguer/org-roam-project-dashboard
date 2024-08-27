;;; org-roam-project_dashboard.el --- A dashboard which shows the org-roam projects  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 24 August 2024
;;

;; Author: Sébastien Le Maguer <sebastien.lemaguer@helsinki.fi> and ChatGPT

;; Package-Requires: ((emacs "29.1") (org-roam "2.2.0") (magit-section "0.1"))
;; Keywords:
;; Homepage:

;; project_dashboard is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; project_dashboard is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with project_dashboard.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-roam)
(require 'magit-section)

(defcustom org-roam-project-dashboard-buffer-name "*Project Dashboard*"
  "Name of the buffer used for the project dashboard."
  :type 'string
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-list-tags '("research" "tools")
  "List of tags which defines the categories of projects"
  :type 'list
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-threshold-tasks 0
  "Number of tasks to display per project in the dashboard. If <=0, list all the tasks "
  :type 'integer
  :group 'org-roam-project-dashboard)

(defun org-roam-project-dashboard--get-projects (tag)
  "Find all Org-roam node IDs and titles that have the specified TAG."
  (let ((nodes (org-roam-db-query
                [:select [nodes:id nodes:title]
                         :from tags
                         :left-join nodes
                         :on (= tags:node-id nodes:id)
                         :where (and (= tags:tag $s1) (= nodes:level 0))]
                tag)))
    nodes))

(defun org-roam-project-dashboard-keep-task-predicate (task)
  "Predicate to determine if a TASK should be considered as an actual task.
The rule implemented in this function is that a valid task should not have a progression indicator."
  (let ((title (nth 1 task)))
    (not (string-match-p "\\[\\([0-9]+%\\|[0-9]+/[0-9]+\\)\\]$" title))))

(defun org-roam-project-dashboard-keep-todo-predicate (task)
  "Predicate to determine if the TASK remains to be done or not.
This predicate considers only TODO tasks to be done."
  (let ((title (nth 2 task)))
    (string-match-p "^TODO$" title)))

(defun org-roam-project-dashboard--get-project-tasks (node-id)
  "Get all tasks (TODOs) in the project with NODE-ID, including its subnodes."
  (let ((tasks (org-roam-db-query
                [:select [out_nodes:id out_nodes:title out_nodes:todo]
                         :from nodes out_nodes
                         :where (and (= file [:select in_nodes:file
                                                      :from nodes in_nodes
                                                      :where (= in_nodes:id $s1)])
                                     (> level [:select in_nodes:level
                                                       :from nodes in_nodes
                                                       :where (= in_nodes:id $s1)])
                                     (not (null out_nodes:todo))
                                     )]
                node-id)))
    (cl-remove-if-not #'org-roam-project-dashboard-keep-task-predicate tasks)))

(defun org-roam-project-dashboard--calculate-progress (node-id)
  "Calculate the completion progress of the project with NODE-ID, including its subnodes."
  (let* ((tasks (org-roam-project-dashboard--get-project-tasks node-id))
         (total (length tasks))
         (done (cl-count "DONE" tasks :key #'caddr :test #'string=)))
    (if (> total 0)
        (/ (* 100 done) total)
      0)))

(defun interpolate-color (color1 color2 percentage)
  "Interpolate between COLOR1 and COLOR2 based on PERCENTAGE.
COLOR1 and COLOR2 should be in the format '(R G B), where each value is between 0 and 255."
  (let ((r1 (nth 0 color1))
        (g1 (nth 1 color1))
        (b1 (nth 2 color1))
        (r2 (nth 0 color2))
        (g2 (nth 1 color2))
        (b2 (nth 2 color2)))
    (list
     (+ r1 (round (* (/ percentage 100.0) (- r2 r1))))
     (+ g1 (round (* (/ percentage 100.0) (- g2 g1))))
     (+ b1 (round (* (/ percentage 100.0) (- b2 b1)))))))

(defun rgb-to-hex (rgb)
  "Convert an RGB list to a hex color string."
  (format "#%02x%02x%02x" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))

(defun org-roam-project-dashboard~format-section (section-name)
  "Format the SECTION-NAME to be more readable."
  (setq section-name (replace-regexp-in-string "_" " " section-name))
  (concat (upcase (substring section-name 0 1))
          (downcase (substring section-name 1))))

(defun org-roam-project-dashboard--generate-progress-bar (percentage)
  "Generate a color gradient progress bar for PERCENTAGE."
  (let* ((bar-width 30)
         (completed (/ (* percentage bar-width) 100))
         (uncompleted (- bar-width completed))
         ;; Define the start and end colors for the gradient
         (start-color '(255 0 0))  ;; Red
         (end-color '(0 255 0))    ;; Green
         ;; Generate the gradient colors for the completed part
         (completed-bar (apply 'concat (mapcar (lambda (i)
                                                 (let ((color (interpolate-color start-color end-color (* 100.0 (/ i (float bar-width))))))
                                                   (propertize "█" 'face `(:foreground ,(rgb-to-hex color)))))
                                               (number-sequence 1 completed))))
         ;; Color the uncompleted part with a shadow
         (uncompleted-bar (propertize (make-string uncompleted ?░) 'face 'shadow)))
    (concat completed-bar uncompleted-bar (format " %d%%" percentage))))

(defun org-roam-project-dashboard--insert-projects (tag)
  "Insert the list of PROJECTS into the dashboard buffer, with magit-sections and aligned progress bars."
  (let ((projects (org-roam-project-dashboard--get-projects tag)))
    (when projects
      (let* ((longest-title-length
              (apply 'max (mapcar (lambda (project) (length (cadr project))) projects)))
             (padding 4)  ;; Additional padding between the title and progress bar
             (sorted-projects (sort projects (lambda (a b) (string< (cadr a) (cadr b))))))  ;; Optional sorting by title
        (magit-insert-section (magit-section tag)
          (magit-insert-heading (propertize (org-roam-project-dashboard~format-section tag) 'face '((:inherit outline-1 :weight ultra-bold :height 150))))
          (dolist (project sorted-projects)
            (let* ((node-id (car project))
                   (title (cadr project))
                   (progress (org-roam-project-dashboard--calculate-progress node-id))
                   (progress-bar (org-roam-project-dashboard--generate-progress-bar progress))
                   (padded-string (make-string (+ padding (- longest-title-length (length title))) ? ))
                   (tasks (cl-remove-if-not #'org-roam-project-dashboard-keep-todo-predicate
                                            (org-roam-project-dashboard--get-project-tasks node-id))))
              (magit-insert-section (magit-section node-id 'hide)
                (magit-insert-heading
                  (insert
                   (format " [[id:%s][%s]] %s%s\n" node-id title padded-string progress-bar)))
                (magit-insert-section-body
                  (dolist (task (if (> org-roam-project-dashboard-threshold-tasks 0)
                                    (seq-take tasks org-roam-project-dashboard-threshold-tasks)
                                  tasks))
                    (let* ((task-id (car task))
                           (task-title (cadr task)))
                      (insert (format "  - TODO [[id:%s][%s]]\n" task-id task-title))))
                  (insert "\n")
                  )))))))))

(defun open-org-roam-node-from-link ()
  "Open the Org-roam node corresponding to the ID stored in the text properties."
  (interactive)
  (let ((node-id (get-text-property (point) 'org-roam-id)))
    (when node-id
      (org-roam-id-open node-id nil))))

(defun advise-magit-section-show-for-org-roam (&rest _args)
  "Run `make-org-roam-links-clickable` after `magit-section-show`.
Accepts any arguments passed by `magit-section-show` but ignores them."
  (let ((inhibit-read-only t))
    (make-org-roam-links-clickable)))

(defun make-org-roam-links-clickable ()
  "Make org-roam links clickable and render them with only the title in the current buffer."
  (interactive)
  (let ((link-regex "\\(\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]\\)"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward link-regex nil t)
        (let* ((full-link (match-string 1))
               (node-id (match-string 2))
               (title (match-string 3))
               (start (match-beginning 0))
               (end (match-end 0)))

          ;; Replace the link with the title
          (delete-region start end)
          (insert  (propertize title 'face '((:inherit outline-2))))

          ;; Apply clickable properties to the title
          (add-text-properties start (+ start (length title))
                               `(mouse-face highlight
                                            help-echo ,(format "mouse-1: Visit org-roam node (id: %s / title: %s)" node-id title)
                                            follow-link t
                                            keymap ,(let ((map (make-sparse-keymap)))
                                                      (define-key map (kbd "RET") 'open-org-roam-node-from-link)
                                                      (define-key map [mouse-1]   'open-org-roam-node-from-link)
                                                      map)
                                            org-roam-id ,node-id)))))))

(define-derived-mode org-roam-project-dashboard-mode magit-section-mode "Project Dashboard"
  "Major mode for project dashboards based on magit-section-mode."
  (make-org-roam-links-clickable)
  (advice-add 'magit-section-show :after #'advise-magit-section-show-for-org-roam)
  (define-key org-roam-project-dashboard-mode-map "g" #'org-roam-project-dashboard-refresh))

(defun org-roam-project-dashboard-refresh ()
  "Refresh the content of the org roam project dashboard"
  (interactive)
  (if (eq (buffer-local-value 'major-mode (current-buffer)) 'org-roam-project-dashboard-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Project Dashboard\n")
        (insert "=================\n\n")
        (unless org-roam-project-dashboard-list-tags
          (error "The list of tags should not be empty!"))
        (magit-insert-section (magit-section "root")
          (dolist (tag org-roam-project-dashboard-list-tags)
            (org-roam-project-dashboard--insert-projects tag)
            (insert "\n"))))
    (error "This function is only useable in org-roam-project-dashboard-mode")))

;;;###autoload
(defun org-roam-project-dashboard-show ()
  "Show the project dashboard."
  (interactive)
  (with-current-buffer (get-buffer-create org-roam-project-dashboard-buffer-name)
    (org-roam-project-dashboard-mode)
    (org-roam-project-dashboard-refresh)
    (switch-to-buffer (current-buffer))))

(provide 'org-roam-project-dashboard)

;;; org-roam-project-dashboard.el ends here
