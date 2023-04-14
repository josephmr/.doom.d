;;; guilded.el -*- lexical-binding: t; -*-

(setq +format-with-lsp nil)

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

(map! :map (rjsx-mode-map scss-mode-map)
      :leader
      "m s" #'my-swap-styles)

(map! :map rjsx-mode-map
      :leader
      "m i" #'my-import-js)

(map! :map rjsx-mode-map
      :leader
      "m c r" #'my-find-component-usages)

(map! :leader
      "p *" #'projectile-find-file-dwim)

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
