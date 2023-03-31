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

(after! vterm
  (evil-set-initial-state 'vterm-mode 'emacs))

(after! git-link
  (setq git-link-default-branch "master"))

(setq browse-at-remote-add-line-number-if-no-region-selected t)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(defun my-import-js ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol 'no-properties)))
    (save-excursion
      (with-no-warnings (goto-line 1))
      (insert! ("import %s from '%s';\n" symbol symbol)))))

(defun my-swap-styles ()
  (interactive)
  (let* ((f (buffer-file-name))
         (is-native (s-contains? "native_v2" f))
         (is-comp (s-contains? ".jsx" f))
         (style (if is-native ".style.js" ".lazy.scss"))
         (needle (if is-comp ".jsx" style))
         (swap (if is-comp style ".jsx")))
    (switch-to-buffer
     (find-file-noselect
      (string-replace needle swap f)))))

(defun my-find-component-usages ()
  (interactive)
  (+vertico/project-search nil (format "<%s" (thing-at-point 'symbol 'no-properties))))

(after! web-mode
  :config
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

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

(defun work-project-hook ()
  "Configuration specifically for Guilded"
  (when (and (projectile-project-p)
             (string= (projectile-project-name) "guilded"))
    (setq +format-with-lsp nil)
    (map! :leader
          :map (rjsx-mode-map scss-mode-map)
          "m s" #'my-swap-styles)

    (map! :leader
          :map rjsx-mode-map
          "m i" #'my-import-js)

    (map! :leader
          :map rjsx-mode-map
          "m c r" #'my-find-component-usages)

    (map! :leader
          "p *" #'projectile-find-file-dwim)
    ))

(add-hook 'projectile-after-switch-project-hook #'work-project-hook)

;; Fix a weird issue where ts-ls complains it "Could not find source file" when
;; opening files. Found out that doing `:e!` fixed the issue, turned on lsp-logs
;; to see that it sent a didChange notification. This just immediately sends a
;; didChange notification which resolve the issue...gross.
;;
;; Restricted to just guilded project since its probably some weird guilded
;; related thing.
(defun my-lsp-hack-fix ()
  "Send a `textDocument/didChange` LSP notification for the current buffer if the project is named 'guilded'."
  (when (and (lsp-session)
             buffer-file-name
             (string= "guilded" (projectile-project-name)))
    (lsp--send-notification
     (lsp--make-notification "textDocument/didChange"
                             `(:textDocument (:uri ,(buffer-file-name))
                                             :contentChanges
                                             [(:text ,(buffer-substring-no-properties (point-min) (point-max)))])))))

(add-hook 'lsp-after-open-hook #'my-lsp-hack-fix)
