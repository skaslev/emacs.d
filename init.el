(defmacro if-mac (true &optional false)
  (if (eq 'darwin system-type) true false))

(defmacro if-linux (true &optional false)
  (if (eq 'gnu/linux system-type) true false))

(defmacro if-windows (true &optional false)
  (if (eq 'windows-nt system-type) true false))

(setq load-path
      (append load-path
              '("~/.emacs.d"
                "~/.emacs.d/progmodes"
                "~/.emacs.d/progmodes/haskell-mode-2.4"
                "~/.emacs.d/color-theme-6.6.0")))

(if-mac
 (defun maximize-frame ()
   (interactive)
   (set-frame-position (selected-frame) 0 0)
   (set-frame-size (selected-frame) 1000 1000))
 (require 'maxframe))

(defun setup-frame (frame)
  (select-frame frame)
  (set-frame-parameter (selected-frame) 'alpha 90)
  (if-mac (set-frame-font "Monaco 14"))
  (if-linux (set-frame-font "Inconsolata 19"))
  (if-windows (set-frame-font "-*-Consolas-normal-r-*-*-17-*-*-*-c-*-*-iso8859-1"))
  (maximize-frame))

(add-hook 'after-make-frame-functions 'setup-frame)
(add-hook 'window-setup-hook (lambda () (setup-frame (selected-frame))))
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(ido-mode 1)
(menu-bar-mode (if-mac 1 0))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(which-func-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(dynamic-completion-mode 1)
(auto-compression-mode 1)
(global-font-lock-mode 1)
(global-auto-revert-mode 1)
(windmove-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq bell-volume 0)
(setq scroll-step 1)
(setq make-backup-files nil)
(setq windmove-wrap-around t)
(setq parens-require-spaces nil)
(setq tab-always-indent 'complete)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
(setq ido-enable-flex-matching t)
(setq shell-file-name "bash")
(setq explicit-shell-file-name "bash")

(setq default-major-mode 'text-mode)
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)
(setq-default ispell-program-name "aspell")
(setq current-language-environment "Bulgarian")
(setq default-input-method "bulgarian-phonetic")

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c n") 'linum-mode)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t (let* ((w1 (first (window-list)))
                  (w2 (second (window-list)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1))))
  (other-window 1))

(global-set-key (kbd "C-c s") 'swap-windows)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'pager)
(global-set-key (kbd "C-v") 'pager-page-down)
(global-set-key (kbd "M-v") 'pager-page-up)
(global-set-key (kbd "M-p") 'pager-row-up)
(global-set-key (kbd "M-n") 'pager-row-down)

(require 'redo)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-?") 'redo)

(require 'color-theme)
(color-theme-initialize)

(require 'zenburn)
(color-theme-zenburn)

(require 'htmlize)

(load "haskell-site-file")
(autoload 'go-mode  "go-mode"  "Go editing mode." t)
(autoload 'cg-mode  "cg-mode"  "Cg editing mode." t)
(autoload 'mel-mode "mel-mode" "Mel editting mode." t)
(autoload 'lua-mode "lua-mode" "Lua editting mode." t)
(autoload 'mma-mode "mma.el"   "Mathematica package file mode" t)
(autoload 'rsl-mode "rsl-mode" "RenderMan Shading Language editing mode" t)
(autoload 'rib-mode "rib-mode" "RenderMan Interface Bytestream editing mode" t)

(setq auto-mode-alist
      (append '(("\\.cl$"   . c-mode)
                ("\\.go$"   . go-mode)
                ("\\.cs$"   . c++-mode)
                ("\\.cg$"   . cg-mode)
                ("\\.hlsl$" . cg-mode)
                ("\\.f[xh]$" . cg-mode)
                ("\\.o?sl$" . rsl-mode)
                ("\\.rib$"  . rib-mode)
                ("\\.ma$"   . mel-mode)
                ("\\.mel$"  . mel-mode)
                ("\\.lua$"  . lua-mode)
                ("\\.m$"    . mma-mode)
                ("\\.org$"  . org-mode))
              auto-mode-alist))

(defadvice python-send-buffer (after advice-switch-to-python)
  "Switch to *Python* after C-c C-c"
  (python-switch-to-python t))

(add-hook
 'text-mode-hook
 (lambda ()
   (flyspell-mode 1)
   (auto-fill-mode 1)
   (setq fill-column 80)))

(add-hook
 'shell-mode-hook
 (lambda ()
   (ansi-color-for-comint-mode-on)))

(add-hook
 'c-mode-hook
 (lambda ()
   (c-set-style "linux")
   (setq tab-width 8)
   (setq indent-tabs-mode t)))

(add-hook
 'c++-mode-hook
 (lambda ()
   (setq tab-width 4)
   (c-add-style
    "my-c++-style"
    '((c-basic-offset . 4)
      (c-comment-only-line-offset . 0)
      (c-offsets-alist . ((statement-block-intro . +)
                          (knr-argdecl-intro . +)
                          (substatement-open . 0)
                          (substatement-label . 0)
                          (label . 0)
                          (statement-cont . +)
                          (inline-open . 0)
                          (inexpr-class . 0)
                          (innamespace . 0)
                          (comment-intro . 0)
                          (arglist-intro . +)
                          (arglist-cont . /)
                          (arglist-close . 0)
                          (inher-intro . *)
                          (inher-cont . 0)
                          (member-init-intro . *)
                          (member-init-cont . /)))))
   (c-set-style "my-c++-style")))

(add-hook
 'haskell-mode-hook
 (lambda ()
   (setq tab-width 4)
   (turn-on-haskell-indent)
   (turn-on-haskell-doc-mode)
   (turn-on-haskell-decl-scan)))

(add-hook
 'mel-mode-hook
 (lambda ()
   (require 'etom)
   (setq etom-default-host "localhost")
   (setq etom-default-port 2323)
   (local-set-key (kbd "C-c C-r") 'etom-send-region)
   (local-set-key (kbd "C-c C-c") 'etom-send-buffer)
   (local-set-key (kbd "C-c C-l") 'etom-send-buffer)
   (local-set-key (kbd "C-c C-z") 'etom-show-buffer)))

(add-hook
 'lua-mode-hook
 (lambda ()
   (setq tab-width 4)
   (setq indent-tabs-mode t)
   (setq lua-indent-level 4)))

(if-windows
 (progn
   ;; Suppress the '~/.emacs.d/server is unsafe' error
   (when (= emacs-major-version 23)
     (require 'server)
     (defun server-ensure-safe-dir (dir) "Noop" t))
   ;; Cygwin setup
   (require 'cygwin-mount)
   (cygwin-mount-activate)
   (add-hook 'comint-output-filter-functions
             'shell-strip-ctrl-m nil t)
   (add-hook 'comint-output-filter-functions
             'comint-watch-for-password-prompt nil t)
   (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
     "Use cygwin's /dev/null as the null-device."
     (let ((null-device "/dev/null"))
       ad-do-it))
   (ad-activate 'grep-compute-defaults)))

(server-start)

(message "init.el loaded successfully.")
