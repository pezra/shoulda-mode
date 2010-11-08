;;
;; Shoulda (minor) mode
;; ==================
;;
;; This minor mode provides some enhancements to ruby-mode in
;; the contexts of Shoulda specifications.  Namely, it provides the
;; following capabilities:
;;
;;  * toggle back and forth between a spec and it's target (bound to
;;    `\C-c ,t`)
;;
;;  * verify the spec file associated with the current buffer (bound to `\C-c ,v`)
;;  
;;  * verify the spec defined in the current buffer if it is a spec
;;    file (bound to `\C-c ,v`)
;;
;;  * verify the example defined at the point of the current buffer (bound to `\C-c ,s`)
;;
;;  * re-run the last verification process (bound to `\C-c ,r`)
;;
;;  * toggle the pendingness of the example at the point (bound to
;;    `\C-c ,d`)
;;
;;  * disable the example at the point by making it pending
;;
;;  * reenable the disabled example at the point
;;
;;  * run "spec" rake task for project (bound to `\C-c ,a`)
;;
;;
;; Dependencies
;; ------------
;;
;; This minor mode depends on `mode-compile`.  The expectations depend
;; `on el-expectataions.el`.
;; 
;;
;; (c) 2008 Peter Williams <http://pezra.barelyenough.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

(require 'ruby-mode)

(defconst shoulda-mode-abbrev-table (make-abbrev-table))

(defconst shoulda-mode-keymap (make-sparse-keymap) "Keymap used in shoulda mode")

(define-key shoulda-mode-keymap (kbd "C-c ,v") 'shoulda-verify)
(define-key shoulda-mode-keymap (kbd "C-c ,s") 'shoulda-verify-single)
(define-key shoulda-mode-keymap (kbd "C-c ,a") 'shoulda-verify-all)
(define-key shoulda-mode-keymap (kbd "C-c ,d") 'shoulda-toggle-example-pendingness)
(define-key shoulda-mode-keymap (kbd "C-c ,t") 'shoulda-toggle-spec-and-target)

(define-minor-mode shoulda-mode
  "Minor mode for shoulda files"
  :lighter " shoulda"
  :keymap  shoulda-mode-keymap)

(defcustom shoulda-command "ruby \"%f\" %o"
  "The command to run when verifying should specs. \"%f\" will be replaced by the filename being tested. \"%o\" will be replaced by the options to test unit'"
  :type 'string
  :group 'rspec-mode)

;; Snippets
(if (require 'snippet nil t)
    (snippet-with-abbrev-table
     'shoulda-mode-abbrev-table
     ("helper" . "require 'pathname'\nrequire Pathname(__FILE__).dirname + '../spec_helper'\n\n$.")
     ("desc"   . "describe $${ClassName} do\n  $.\nend ")
     ("descm"  . "describe $${ClassName}, \"$${modifier}\" do\n  $.\nend ")
     ("it"     . "it \"should $${what exactly?}\" do\n  $.\n  end ")
     ("bef"    . "before do\n  $.\n  end"))
  )


(defun shoulda-beginning-of-example ()
  "Moves point to the beginning of the example in which the point current is."
  (interactive)
  (let ((start (point)))
    (goto-char 
     (save-excursion
       (end-of-line)
       (unless (and (search-backward-regexp "^[[:space:]]*should[[:space:]]*(?[\"']" nil t)
                    (save-excursion (ruby-end-of-block) (< start (point))))
         (error "Unable to find an example"))
       (point)))))

(defun shoulda-example-pending-p ()
  "True if the example under point is pending. Otherwise false"
  (interactive)
  (save-excursion
    (shoulda-beginning-of-example)
    (re-search-forward "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)) t)))


(defun shoulda-toggle-example-pendingness ()
  "Disables active examples and enables pending examples."
  (interactive)
  (if (shoulda-example-pending-p)
      (shoulda-enable-example)
    (shoulda-disable-example)))

(defun shoulda-disable-example ()
  "Disable the example in which the point is located"
  (interactive)
  (when (not (shoulda-example-pending-p))   
    (save-excursion
      (shoulda-beginning-of-example)
      (end-of-line)
      (insert "\npending")
      (indent-for-tab-command))))

(defun shoulda-enable-example ()
  "Enable the example in which the point is located"
  (interactive)
  (when (shoulda-example-pending-p)
    (save-excursion
      (shoulda-beginning-of-example)
      (search-forward-regexp "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)))
      (beginning-of-line)
      (delete-region (save-excursion (beginning-of-line) (point)) 
                     (save-excursion (forward-line 1) (point))))))
  
(defun shoulda-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (shoulda-run-single-file (shoulda-spec-file-for (buffer-file-name))))

(defun shoulda-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (shoulda-run-single-file (buffer-file-name) "-n" (shoulda-regexp-for-example (shoulda-example-name-at-point))))

(defun shoulda-regexp-for-example (example-name)
  "Converts example name into a regexp that matched the example name, escaping all regexp special characters"
  (concat "\"/" (replace-regexp-in-string "[]\\[/\\(\\)+?.]" (lambda (m) (concat "\\\\\\\\\\\\\\\\" m)) example-name) "/\""))

(defun shoulda-verify-all ()
  "Runs the 'spec' rake task for the project of the current file."
  (interactive)
  (let ((default-directory (or (shoulda-project-root) default-directory)))
    (shoulda-run)))

(defun shoulda-toggle-spec-and-target ()
  "Switches to the spec for the current buffer if it is a
   non-spec file, or switch to the target of the current buffer
   if the current is a spec"
  (interactive)
  (find-file
   (if (shoulda-buffer-is-spec-p)
       (shoulda-target-file-for (buffer-file-name))
     (shoulda-spec-file-for (buffer-file-name)))))

(defun shoulda-spec-file-for (a-file-name)
  "Find spec for the specified file"
  (if (shoulda-spec-file-p a-file-name)
      a-file-name
    (shoulda-specize-file-name (expand-file-name (replace-regexp-in-string "^\\.\\./[^/]+/" "" (file-relative-name a-file-name (shoulda-spec-directory a-file-name))) 
                                               (shoulda-spec-directory a-file-name)))))

(defun shoulda-target-file-for (a-spec-file-name)
  "Find the target for a-spec-file-name"
  (first 
   (file-expand-wildcards 
    (replace-regexp-in-string "/test/" "/*/" (shoulda-targetize-file-name a-spec-file-name)))))

(defun shoulda-specize-file-name (a-file-name)
  "Returns a-file-name but converted in to a spec file name"
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string "\\(\\.rb\\)?$" "_test.rb" (file-name-nondirectory a-file-name))))

(defun shoulda-targetize-file-name (a-file-name)
  "Returns a-file-name but converted into a non-spec file name"
     (concat (file-name-directory a-file-name)
             (shoulda-file-name-with-default-extension 
              (replace-regexp-in-string "_test\\.rb" "" (file-name-nondirectory a-file-name)))))
  
(defun shoulda-file-name-with-default-extension (a-file-name)
  "Adds .rb file extension to a-file-name if it does not already have an extension"
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".rb")))
        
(defun shoulda-directory-subdirectories (directory)
  "Returns list of subdirectories"
  (remove-if 
   (lambda (dir) (or (string-match "^\\.\\.?$" (file-name-nondirectory dir)) 
                     (not (file-directory-p dir))))
   (directory-files directory t)))

(defun shoulda-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun shoulda-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (shoulda-parent-directory a-directory)))
   
(defun shoulda-spec-directory (a-file)
  "Returns the nearest spec directory that could contain specs for a-file"
  (if (file-directory-p a-file)
      (or
       (first (directory-files a-file t "^spec$"))
       (if (shoulda-root-directory-p a-file)
           nil
         (shoulda-spec-directory (shoulda-parent-directory a-file))))
    (shoulda-spec-directory (shoulda-parent-directory a-file))))

(defun shoulda-spec-file-p (a-file-name)
  "Returns true if the specified file is a spec"
  (string-match "\\(_\\|-\\)test\\.rb$" a-file-name))

(defun shoulda-buffer-is-spec-p ()
  "Returns true if the current buffer is a spec"
  (and (buffer-file-name)
       (shoulda-spec-file-p (buffer-file-name))))

(defun shoulda-example-name-at-point ()
  "Returns the name of the example in which the point is currently positioned; or nil if it is outside of and example"
  (save-excursion 
    (end-of-line)
    (re-search-backward "\\(\\(should\\|context\\)[[:space:](]+['\"]\\|def[[:space:]]+test_\\|should_[^[:space:](]+[[:space:](]['\"]\\)\\(.*\\)$")
    (replace-regexp-in-string "\\(\\(['\"][)[:space:]]*\\(do\\|DO\\|Do\\|{\\)\\)\\|()\\)[[:space:]]*$" "" (match-string 3))))
                    
(defun shoulda-register-verify-redo (redoer)
  "Register a bit of code that will repeat a verification process"
  (let ((redoer-cmd (eval (append '(lambda () (interactive)) (list redoer)))))
    (global-set-key (kbd "C-c ,r") redoer-cmd)))

(defun shoulda-run (&rest opts)
  "Runs spec with the specified options"
  (shoulda-register-verify-redo (cons 'shoulda-run opts))
  (compile (concat "rake test TEST_OPTS=\'" (mapconcat (lambda (x) x) opts " ") "\'"))
  (end-of-buffer-other-window 0))

(defun shoulda-run-single-file (spec-file &rest opts)
  "Runs spec with the specified options"
  (shoulda-register-verify-redo (cons 'shoulda-run-single-file (cons spec-file opts)))
  (compile (shoulda-inject-spec-file-name (shoulda-inject-options shoulda-command opts) spec-file))
  (end-of-buffer-other-window 0))

(defun shoulda-inject-options (cmd-pattern opts-list)
  "Replaces '%o' with options string"
  (replace-regexp-in-string "%o" (mapconcat (lambda (x) x) opts " ") cmd-pattern))

(defun shoulda-inject-spec-file-name (cmd-pattern spec-file)
  "Replaces '%f' with file name"
  (replace-regexp-in-string "%f" spec-file cmd-pattern))

(defun shoulda-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((shoulda-root-directory-p directory) nil)
          ((file-exists-p (concat directory "Rakefile")) directory)
          (t (shoulda-project-root (file-name-directory (directory-file-name directory)))))))

;; Makes sure that shoulda buffers are given the shoulda minor mode by default
(add-hook 'ruby-mode-hook
          (lambda ()
            (when (shoulda-buffer-is-spec-p)
              (shoulda-mode))))

;; Add verify related spec keybinding to ruby ruby modes
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ,v") 'shoulda-verify)
            (local-set-key (kbd "C-c ,a") 'shoulda-verify-all)
            (local-set-key (kbd "C-c ,t") 'shoulda-toggle-spec-and-target)))

;; Add verify related spec keybinding to ruby ruby modes
(add-hook 'rails-minor-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ,v") 'shoulda-verify)
            (local-set-key (kbd "C-c ,a") 'shoulda-verify-all)
            (local-set-key (kbd "C-c ,t") 'shoulda-toggle-spec-and-target)))

;; This hook makes any abbreviation that are defined in
;; shoulda-mode-abbrev-table available in shoulda buffers
(add-hook 'shoulda-mode-hook
          (lambda ()
            (merge-abbrev-tables shoulda-mode-abbrev-table
                                 local-abbrev-table)))

;; abbrev
;; from http://www.opensource.apple.com/darwinsource/Current/emacs-59/emacs/lisp/derived.el
(defun merge-abbrev-tables (old new)
  "Merge an old abbrev table into a new one.
This function requires internal knowledge of how abbrev tables work,
presuming that they are obarrays with the abbrev as the symbol, the expansion
as the value of the symbol, and the hook as the function definition."
  (when old
    (mapatoms
     (lambda(it)
       (or (intern-soft (symbol-name it) new)
           (define-abbrev new
             (symbol-name it)
             (symbol-value it)
             (symbol-function it)
             nil
             t)))
     old)))

(add-to-list 'compilation-error-regexp-alist-alist 
	     '(shoulda "\\([0-9A-Za-z_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2))
(add-to-list 'compilation-error-regexp-alist 'shoulda)


(provide 'shoulda-mode)

