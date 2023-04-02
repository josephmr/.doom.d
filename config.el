;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Joseph Rollins"
      user-mail-address "rollins.joseph@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono" :size 14))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq scroll-margin 2
      evil-want-fine-undo 't)

(after! vterm
  (evil-set-initial-state 'vterm-mode 'emacs))

(after! git-link
  (setq git-link-default-branch "master"))

(setq browse-at-remote-add-line-number-if-no-region-selected t)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq-default tab-width 2
              indent-tabs-mode nil)

(after! typescript-mode
  :config
  (setq typescript-indent-level 2))

(after! web-mode
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-block-padding 2))

(defun org-copy-attachment-image ()
  "Copy the contents of the attachment file at point to the clipboard for macOS."
  (interactive)
  (let* ((link-info (org-element-context))
         (file-name (org-element-property :path link-info))
         (file-id (org-entry-get-with-inheritance  "ID"))
         (file-id-prefix (substring file-id 0 2))
         (file-id-suffix (substring file-id 2))
         (base-dir (expand-file-name (concat "~/org/.attach/" file-id-prefix "/" file-id-suffix))))
    (when (and file-name (file-exists-p (concat base-dir "/" file-name)))
      (let ((script (format "tell app \"Finder\" to set the clipboard to ( POSIX file \"%s\" )"
                            (concat base-dir "/" file-name))))
        (start-process "osascript" nil "osascript" "-e" script)
        (message "Attachment contents copied to clipboard.")))))

(map! :leader
      :map org-mode-map
      "m a y" #'org-copy-attachment-image)

(let* ((host-config (concat (system-name) ".el"))
       (host-config-abs (expand-file-name host-config doom-user-dir)))
  (when (file-readable-p host-config-abs)
    (load-file host-config-abs)))

;; Convert restclient response to json-mode when 200
;;
;;Could use restclient-content-type-modes to set to json-mode, but this stopped
;; `:results pure` from removing the headers and did not pretty-print the json
;; by default.
;;
;; TODO add a way to disable
(defun restclient-my-json-when-ok ()
  (when (equal major-mode 'js-mode)
    (save-excursion
      (goto-char (point-max))
      (when (and (search-backward "200 OK" nil t)
                 (search-backward "}" nil t))
        (forward-char 1)
        (delete-region (point) (point-max))
        (newline)
        (json-mode)
        (goto-char (point-min))
        (pulse-momentary-highlight-region (point-min) (point-max)
                                          )))))

(after! restclient
  (add-hook 'restclient-response-loaded-hook #'restclient-my-json-when-ok))
