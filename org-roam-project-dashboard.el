;;; org-roam-project-dashboard.el --- A dashboard which shows the org-roam projects  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 24 August 2024
;;

;; Author: Sébastien Le Maguer <sebastien.lemaguer@helsinki.fi> and ChatGPT
;; Version: 1.3b
;; Package-Requires: ((emacs "29.1") (org-roam "2.2.0") (magit-section "0.1"))
;; Keywords: outlines, org-roam, dashboard, project, tags
;; Homepage: https://github.com/seblemaguer/org-roam-project-dashboard

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

;; This package provides a quick way to spawn a dashboard generated
;; using org-roam's data.

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
  "List of tags which defines the categories of projects."
  :type '(list string)
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-ignored-tags '("onhold")
  "List of tags which indicating that the project should be ignored."
  :type '(list string)
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-threshold-tasks 0
  "Number of tasks to display per project in the dashboard.
If <=0, list all the tasks "
  :type 'integer
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-show-all-projects nil
  "Flag to determine if all the projects should be shown (t) or
 only the ones with tasks to be done (nil)."
  :type 'boolean
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-start-color "red3"
  "The start color of the gradient for the progress bar in the dashboard."
  :type 'color
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-end-color "green3"
  "The end color of the gradient for the progress bar in the dashboard."
  :type 'color
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-fill-character "█"
  "The character used for the filled part of the progress bar."
  :type 'character
  :group 'org-roam-project-dashboard)

(defcustom org-roam-project-dashboard-background-character ?░
  "The character used for the filled part of the progress bar."
  :type 'character
  :group 'org-roam-project-dashboard)

(defface org-roam-project-dashboard-todo
  '((t :weight ultra-bold :foreground "red"))
  "TODO face for org-roam-project-dashboard"
  :group 'org-roam-project-dashboard)

(defface org-roam-project-dashboard-effort
  '((t :weight ultra-bold))
  "Effort face for org-roam-project-dashboard"
  :group 'org-roam-project-dashboard)

(defface org-roam-project-dashboard-priority
  '((t :weight ultra-bold :foreground "blue"))
  "Priority face for org-roam-project-dashboard"
  :group 'org-roam-project-dashboard)

(defface org-roam-project-dashboard-header
  '((t :inherit outline-1 :weight ultra-bold :height 150))
  "Face for the header of a section in org-roam-project-dashboard."
  :group 'org-roam-project-dashboard)

(defface org-roam-project-dashboard-project
  '((t :inherit outline-2 :weight bold))
  "Face for the name of a project in org-roam-project-dashboard."
  :group 'org-roam-project-dashboard)

(defface org-roam-project-dashboard-task
  '((t :inherit outline-4))
  "Face for the title of a task in org-roam-project-dashboard."
  :group 'org-roam-project-dashboard)

(defun org-roam-project-dashboard~get-projects (tag)
  "Find all Org-roam node IDs and titles that have the specified TAG."
  (let ((nodes (org-roam-db-query
                [:select [nodes:id nodes:title]
                         :from tags
                         :left-join nodes
                         :on (= tags:node-id nodes:id)
                         :where (and (= tags:tag $s1) (= nodes:level 0))]
                tag)))

    (mapcar (lambda (task)
              (list :id (nth 0 task)
                    :title (nth 1 task)))
            nodes)))

(defun org-roam-project-dashboard~validate-tags (node-id node-tags)
  "List all the tags for the node identified by NODE-ID which are part of
the set of NODE-TAGS."
  (when node-tags
    (cl-intersection (org-roam-node-tags (org-roam-node-from-id node-id)) node-tags :test 'string=)))

(defun org-roam-project-dashboard-keep-task-predicate (task)
  "Predicate to determine if a TASK should be considered as an actual task.
The rule implemented in this function is that a valid task should
not have a progression indicator."
  (let ((title (plist-get task :title)))
    (not (string-match-p "\\[\\([0-9]+%\\|[0-9]+/[0-9]+\\)\\]$" title))))

(defun org-roam-project-dashboard-keep-todo-predicate (task)
  "Predicate to determine if the TASK remains to be done or not.
This predicate considers only TODO tasks to be done."
  (let ((todo (plist-get task :todo)))
    (string-match-p "^TODO$" todo)))

(defun org-roam-project-dashboard~demote-nil-priority (tasks)
  "Demote the TASKS whose priority is nil"
  (sort tasks
        (lambda (task-a task-b)
          (and (plist-get task-a :priority) (not (plist-get task-b :priority))))))

(defun org-roam-project-dashboard~get-project-tasks (node-id)
  "Get all tasks (TODOs) in the project with NODE-ID, including its
subnodes."
  (let ((tasks (org-roam-db-query
                [:select [out_nodes:id out_nodes:title out_nodes:todo out_nodes:priority out_nodes:scheduled out_nodes:properties]
                         :from nodes out_nodes
                         :where (and (= file [:select in_nodes:file
                                                      :from nodes in_nodes
                                                      :where (= in_nodes:id $s1)])
                                     (> level [:select in_nodes:level
                                                       :from nodes in_nodes
                                                       :where (= in_nodes:id $s1)])
                                     (not (null out_nodes:todo)))
                         :order-by [(asc out_nodes:priority)]]
                node-id)))
    (setq tasks (mapcar (lambda (task)
                          (list :id (nth 0 task)
                                :title (nth 1 task)
                                :todo (nth 2 task)
                                :priority (nth 3 task)
                                :scheduled (nth 4 task)
                                :properties (nth 5 task)))
                        tasks))
    (org-roam-project-dashboard~demote-nil-priority
     (cl-remove-if-not #'org-roam-project-dashboard-keep-task-predicate tasks))))

(defun org-roam-project-dashboard~calculate-progress (node-id)
  "Calculate the completion progress of the project with NODE-ID,
including its subnodes."
  (let* ((tasks (org-roam-project-dashboard~get-project-tasks node-id))
         (total (length tasks))
         (done (cl-count-if
                (lambda (task) (member (plist-get task :todo) org-done-keywords-for-agenda))
                tasks)))
    (if (> total 0)
        (/ (* 100 done) total)
      100)))

(defun org-roam-project-dashboard~compute-total-effort (tasks)
  "Compute the total effort require by a project whose TASKS are provided
as a parameter."
  (let* ((total-time 0))
    (dolist (task tasks)
      (let ((effort (cdr (assoc "EFFORT" (plist-get task :properties))))
            (hours 0)
            (minutes 0))

        (when effort
          ;; Step 2: Parse the hours and minutes from the EFFORT string
          (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" effort)
            (setq hours (string-to-number (match-string 1 effort)))  ;; Extract hours
            (setq minutes (string-to-number (match-string 2 effort))))
          (setq total-time (+ total-time (* hours 60) minutes)))))
    (format "%03d:%02d:%02d" (/ total-time 1440) (/ (% total-time 1440) 60) (% total-time 60))))

(defun org-roam-project-dashboard~interpolate-color (color1 color2 percentage)
  "Interpolate between COLOR1 and COLOR2 based on PERCENTAGE. COLOR1
and COLOR2 should be in the format \\'(r g b), where each value is
between 0 and 255."
  (let* ((rgb1 (color-name-to-rgb color1))  ;; Get 16-bit RGB values
         (rgb2 (color-name-to-rgb color2))
         (r1 (* (nth 0 rgb1) 255))
         (g1 (* (nth 1 rgb1) 255))
         (b1 (* (nth 2 rgb1) 255))
         (r2 (* (nth 0 rgb2) 255))
         (g2 (* (nth 1 rgb2) 255))
         (b2 (* (nth 2 rgb2) 255)))
    (list
     (+ r1 (round (* (/ percentage 100.0) (- r2 r1))))
     (+ g1 (round (* (/ percentage 100.0) (- g2 g1))))
     (+ b1 (round (* (/ percentage 100.0) (- b2 b1)))))))

(defun org-roam-project-dashboard~rgb-to-hex (rgb)
  "Convert an RGB list to a hex color string."
  (format "#%02x%02x%02x" (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))

(defun org-roam-project-dashboard~format-section (section-name)
  "Format the SECTION-NAME to be more readable."
  (setq section-name (replace-regexp-in-string "_" " " section-name))
  (concat (upcase (substring section-name 0 1))
          (downcase (substring section-name 1))))

(defun org-roam-project-dashboard~generate-progress-bar (percentage)
  "Generate a color gradient progress bar for PERCENTAGE."
  (let* ((bar-width 30)
         (completed (/ (* percentage bar-width) 100))
         (uncompleted (- bar-width completed))
         ;; Generate the gradient colors for the completed part
         (completed-bar
          (apply 'concat
                 (mapcar (lambda (i)
                           (let ((color (org-roam-project-dashboard~interpolate-color
                                         org-roam-project-dashboard-start-color
                                         org-roam-project-dashboard-end-color
                                         (* 100.0 (/ i (float bar-width))))))
                             (propertize org-roam-project-dashboard-fill-character
                                         'face
                                         `(:foreground ,(org-roam-project-dashboard~rgb-to-hex color)))))
                         (number-sequence 1 completed))))
         ;; Color the uncompleted part with a shadow
         (uncompleted-bar (propertize (make-string uncompleted org-roam-project-dashboard-background-character) 'face 'shadow)))
    (concat completed-bar uncompleted-bar (format " %2d%%" percentage))))


(defun org-roam-project-dashboard~insert-projects (tag)
  "Insert the list of PROJECTS into the dashboard buffer, with
magit-sections and aligned progress bars."
  (let ((projects (org-roam-project-dashboard~get-projects tag)))
    (when projects
      (let* ((padding 4)
             (sorted-projects (sort projects (lambda (project-a project-b) (string< (plist-get project-a :title) (plist-get project-b :title))))))
        (magit-insert-section (magit-section tag)
          (magit-insert-heading
            (propertize (org-roam-project-dashboard~format-section tag)
                        'face
                        'org-roam-project-dashboard-header))

          (dolist (project sorted-projects)
            (unless (org-roam-project-dashboard~validate-tags (plist-get project :id) org-roam-project-dashboard-ignored-tags)
              (let* ((node-id (plist-get project :id))
                     (title (plist-get project :title))
                     (progress (org-roam-project-dashboard~calculate-progress node-id))
                     (progress-bar (org-roam-project-dashboard~generate-progress-bar progress))
                     (tasks (cl-remove-if-not #'org-roam-project-dashboard-keep-todo-predicate
                                              (org-roam-project-dashboard~get-project-tasks node-id)))
                     (total-effort (org-roam-project-dashboard~compute-total-effort tasks))
                     (padded-string (make-string (+ padding (- (window-body-width) (length title) 10 (length total-effort) (length progress-bar))) ? )))
                (when (or (< progress 100) org-roam-project-dashboard-show-all-projects)
                  (magit-insert-section (magit-section node-id 'hide)
                    (magit-insert-heading
                      (insert " ")
                      (if total-effort
                          (progn
                            (insert (propertize (format "[%s]" total-effort)
                                                'face 'org-roam-project-dashboard-effort)))
                        (insert "       "))
                      (insert " ")
                      (insert (propertize (format "[[id:%s][%s]]" node-id title)
                                          'face 'org-roam-project-dashboard-project))
                      (insert (format " %s%s"  padded-string progress-bar))
                      (insert "\n"))
                    (magit-insert-section-body
                      (dolist (task (if (> org-roam-project-dashboard-threshold-tasks 0)
                                        (seq-take tasks org-roam-project-dashboard-threshold-tasks)
                                      tasks))
                        (let* ((task-id (plist-get task :id))
                               (task-title (plist-get task :title))
                               (task-todo (plist-get task :todo))
                               (task-priority (plist-get task :priority))
                               (task-is-scheduled (plist-get task :scheduled))
                               (effort (cdr (assoc "EFFORT" (plist-get task :properties)))))
                          (insert "  - ")
                          (if task-is-scheduled
                              (insert "✓ ")
                            (insert "  "))

                          (if effort
                              (progn
                                (insert (propertize (format "[%s]" effort)
                                                    'face 'org-roam-project-dashboard-effort)))
                            (insert "      "))

                          (insert " ")
                          (insert (propertize (format " %s " task-todo)
                                              'face 'org-roam-project-dashboard-todo))
                          (insert " ")
                          (when task-priority
                            (insert (propertize (format " %c " task-priority)
                                                'face 'org-roam-project-dashboard-priority))
                            (insert " "))
                          (insert (propertize (format "[[id:%s][%s]]" task-id task-title)
                                              'face 'org-roam-project-dashboard-task))
                          (insert "\n")
                          ))
                      (insert "\n"))))))))))))

(defun org-roam-project-dashboard-open-node-from-link ()
  "Open the Org-roam node corresponding to the ID stored in the text properties."
  (interactive)
  (let ((node-id (get-text-property (point) 'org-roam-id)))
    (when node-id
      (org-roam-id-open node-id nil))))

(defun org-roam-project-dashboard~advice-magit-section-show (&rest _args)
  "Run `make-org-roam-links-clickable` after `magit-section-show`.
Accepts any arguments passed by `magit-section-show` but ignores them."
  (let ((inhibit-read-only t))
    (org-roam-project-dashboard~make-links-clickable)))

(defun org-roam-project-dashboard~make-links-clickable ()
  "Make org-roam links clickable and render them with only the title
in the current buffer."
  (interactive)
  (let ((link-regex "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward link-regex nil t)
        (let* ((node-id (match-string 1))
               (title (match-string 2))
               (start (match-beginning 0)))

          ;; Replace the link with the title
          (delete-region start (+ start (length node-id) 7))
          (delete-region (+ start (length title)) (+ start (length title) 2))

          ;; Apply clickable properties to the title
          (add-text-properties (line-beginning-position) (line-end-position)
                               `(mouse-face highlight
                                            help-echo ,(format "mouse-1: Visit org-roam node (id: %s / title: %s)" node-id title)
                                            follow-link t
                                            keymap ,(let ((map (make-sparse-keymap)))
                                                      (define-key map (kbd "RET") 'org-roam-project-dashboard-open-node-from-link)
                                                      (define-key map [mouse-1]   'org-roam-project-dashboard-open-node-from-link)
                                                      map)
                                            org-roam-id ,node-id)))))))


(defun org-roam-project-dashboard~insert-header ()
  "Insert the header."
  (let* ((width (window-width))
         (text "Project Dashboard")
         (text-len (length text))
         (padding 5) ;; spaces inside box on each side
         (box-width (+ text-len (* 2 padding)))
         (left-pad (/ (- width box-width) 2))
         (left-pad (max 0 left-pad)) ;; don't go negative
         (spaces (make-string left-pad ?\s))
         (line-top (concat spaces "╔" (make-string box-width ?═) "╗\n"))
         ;; center the text inside the box with padding spaces:
         (middle (concat spaces
                         "║"
                         (make-string padding ?\s)
                         text
                         (make-string padding ?\s)
                         "║\n"))
         (line-bottom (concat spaces "╚" (make-string box-width ?═) "╝\n")))
    (insert line-top middle line-bottom)
    (dotimes (_ 5) (insert "\n"))))


(define-derived-mode org-roam-project-dashboard-mode magit-section-mode "Project Dashboard"
  "Major mode for project dashboards based on magit-section-mode."
  (advice-add 'magit-section-show :after #'org-roam-project-dashboard~advice-magit-section-show)
  (define-key org-roam-project-dashboard-mode-map "g" #'org-roam-project-dashboard-refresh))

(defun org-roam-project-dashboard-refresh ()
  "Refresh the content of the org roam project dashboard"
  (interactive)
  (if (eq (buffer-local-value 'major-mode (current-buffer)) 'org-roam-project-dashboard-mode)
      (let ((inhibit-read-only t)
            (cur-pos (point)))
        (erase-buffer)
        (org-roam-project-dashboard~insert-header)
        (unless org-roam-project-dashboard-list-tags
          (error "The list of tags should not be empty!"))
        (magit-insert-section (magit-section "root")
          (dolist (tag org-roam-project-dashboard-list-tags)
            (org-roam-project-dashboard~insert-projects tag)
            (insert "\n")))
        (org-roam-project-dashboard~make-links-clickable)
        (goto-char cur-pos))
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
