(require 'subr-x)
(require 'tramp nil t)

(defgroup emacs-vterm nil
  "Customization group for the homebrew Emacs vterm helpers."
  :group 'term)

(defcustom emacs-vterm-reserved-buffer-names '("*vterm*" "vterm")
  "Buffer names that should never be auto-renamed by emacs-vterm."
  :type '(repeat string)
  :group 'emacs-vterm)

(defvar-local emacs-vterm--last-buffer-name nil)

(defun emacs-vterm--rename-eligible-p (&optional buffer)
  "Return non-nil when BUFFER should be auto-renamed."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'vterm-mode)
         (not (member (buffer-name) emacs-vterm-reserved-buffer-names)))))

(defun emacs-vterm--current-directory (&optional buffer)
  "Return the best effort directory string for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((and (fboundp 'vterm--get-pwd) (bound-and-true-p vterm--term))
      (or (ignore-errors (vterm--get-pwd)) default-directory))
     (t default-directory))))

(defun emacs-vterm--abbreviate-path (path remote)
  "Return a readable variant of PATH. REMOTE indicates remote paths."
  (let ((clean (directory-file-name (or path ""))))
    (cond
     ((string-empty-p clean) "/")
     ((and remote clean) clean)
     ((fboundp 'abbreviate-file-name)
      (abbreviate-file-name clean))
     (t clean))))

(defun emacs-vterm--short-hostname (host)
  "Return HOST without domain segments."
  (when host
    (car (split-string host "\\."))))

(defun emacs-vterm--format-host (host &optional port)
  "Format HOST (optionally PORT) for display while stripping domain noise."
  (let* ((name (or (emacs-vterm--short-hostname host) host))
         (port-str (cond
                    ((integerp port) (number-to-string port))
                    ((and (stringp port)
                          (string-match-p "\\`[0-9]+\\'" port))
                     port)
                    (t nil))))
    (if (and port-str (not (string-empty-p port-str)))
        (format "%s#%s" name port-str)
      name)))

(defun emacs-vterm--format-buffer-name (&optional buffer)
  "Build the desired buffer name for BUFFER based on directory/host."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dir (emacs-vterm--current-directory))
           (remote-host (and dir (file-remote-p dir 'host)))
           (remote-port (and dir (file-remote-p dir 'port)))
           (remote-path (and dir (file-remote-p dir 'localname)))
           (path (emacs-vterm--abbreviate-path (or remote-path dir) remote-host))
           (host (or (and remote-host (emacs-vterm--format-host remote-host remote-port))
                     (emacs-vterm--format-host (and (fboundp 'system-name) (system-name)))
                     "localhost")))
      (when (and path host)
        (format "vterm: %s@%s" path host)))))

(defun emacs-vterm--generate-buffer-name ()
  "Generate a new buffer name using our dynamic formatter."
  (generate-new-buffer-name
   (or (emacs-vterm--format-buffer-name) "vterm")))

(defun emacs-vterm--refresh-buffer-name (&rest _)
  "Rename the current vterm buffer unless it is reserved."
  (when (emacs-vterm--rename-eligible-p)
    (when-let ((new-name (emacs-vterm--format-buffer-name)))
      (unless (equal new-name emacs-vterm--last-buffer-name)
        (setq emacs-vterm--last-buffer-name new-name)
        (rename-buffer new-name t)))))

(defun emacs-vterm--setup-buffer ()
  "Initialize dynamic buffer naming for the current vterm."
  (setq emacs-vterm--last-buffer-name nil)
  (emacs-vterm--refresh-buffer-name))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook #'emacs-vterm--setup-buffer)
  (advice-add 'vterm--set-directory :after #'emacs-vterm--refresh-buffer-name))

;; Auto-start vterm for GUI, no-args launches, while respecting user config
(when (and (display-graphic-p)
           (not noninteractive)
           (not (daemonp))
           (null command-line-args-left)
           (not (bound-and-true-p initial-buffer-choice)))
  (setq initial-buffer-choice
        (lambda ()
          (require 'vterm nil t)
          (if (fboundp 'vterm)
              (let ((buf (emacs-vterm--generate-buffer-name)))
                (vterm buf))
            (progn
              (message "emacs-vterm: vterm not found. Install with M-x package-install RET vterm RET")
              (get-buffer-create "*scratch*"))))))

;; Also open vterm on newly created GUI frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (and (display-graphic-p frame)
                       (not noninteractive)
                       (not (daemonp)))
              (with-selected-frame frame
                (require 'vterm nil t)
                (when (fboundp 'vterm)
                  (let ((buf (emacs-vterm--generate-buffer-name)))
                    (vterm buf)))))))
