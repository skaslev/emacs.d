;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RenderMan Shader Mode
;; c-mode + RenderMan compile/render functionality
;; steve may, accad, 4/27/95
;;   initial version
;; steve may, accad, 4/27/96
;;   - added capability to set RIB filename and included directories
;;   - added highlighting capability
;;
;; Edited by Rudy Cortes based on Jens Beier's vfl-mode
;;  - added support for Win PRman.
;;  - added support for 3Delight.
;;  - added suport for aqsis
;;  - updated syntax highlighting
;;----------------------------------------
;;
;; To make this mode enabled whenever a file ending in ".sl" is loaded,
;; add the following to your .emacs file.
;;
;;
;;----------------------------------------
;;
;;

(require 'font-lock)
(require 'cc-mode)

(defvar rsl-mode-hook nil)

(defun rsl-mode ()
  "Major mode for editing RenderMan shaders.
This is actually just C mode with commands for compiling and 
rendering shaders.

C-c C-c    save buffer & compile shader for PhotoRealistic RenderMan
C-c C      save buffer & compile shader for BMRT
C-c 3      save buffer & compile shader for 3Delight
C-C a      save buffer & compile shader for aqsis
C-c C-r    call render with the current RIB file (PRman Unix)
C-c M-r    call render with the current RIB file (PRman Win32)
C-c R      call rendrib with the current RIB filename (BMRT)
C-c M-3    call render with the current RIB file (3delight)
C-C M-a    call render with the current RIB file (aqsis)
C-c C-s    set the current RIB filename (default is rman.rib)
C-c C-i    set the current include directories as LISP list of strings;
           each string denoting one directory. For example (at the prompt):
           (\"/usr/local/shaders\" \"/usr/shaders\").

To get info on C mode, select 'Describe Function...' from the 'Help'
menu and enter 'c-mode' at the prompt.
"
  (interactive)
  (c-mode)
  (local-set-key "\C-c\C-c" 'rsl-compile-prman)
  (local-set-key "\C-cC" 'rsl-compile-bmrt)
  (local-set-key "\C-c3" 'rsl-compile-3delight)
  (local-set-key "\C-ca" 'rsl-compile-aqsis)
  (local-set-key "\C-c\C-r" 'rsl-render-prman)
  (local-set-key "\C-c\M-r" 'rsl-render-prmanwin)
  (local-set-key "\C-cR" 'rsl-render-bmrt)
  (local-set-key "\C-c\M-3" 'rsl-render-3delight)
  (local-set-key "\C-c\M-a" 'rsl-render-aqsis)
  (local-set-key "\C-c\C-s" 'rsl-set-ribfile)
  (local-set-key "\C-c\C-i" 'rsl-set-inc-dirs)
  (setq major-mode 'rsl-mode)
  (setq mode-name "RenderMan Shader")
  (make-variable-buffer-local 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist 
        (append compilation-error-regexp-alist 
                rsl-compilation-regexp-alist))
  (setq c-conditional-key "\\<\\(for\\|if\\|do\\|else\\|while\\|switch\\|forpoints\\)\\>[^_]")
  (setq c-font-lock-extra-types
        (append '("vector" "normal" "matrix" "point" "string")
                c-font-lock-extra-types))
  (setq font-lock-defaults '(rsl-font-lock-keywords nil nil ((?_ . "w"))))  
  (run-hooks 'rsl-mode-hook))

;; rsl error message
;;"WoodPlanks.vfl" line 73 ERROR (1010) Undefined instruction "nax"
(setq rsl-compilation-regexp-alist
      '(("\"\\(.+\\)\" line \\([0-9]+\\) .+" 1 2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlighting for font-lock functionality
;; adherent to the RI spec 3.2
;;
(defvar rsl-font-lock-keywords-3
    '(
      ;; Types
      ("\\<\\(float\\|string\\|point\\|vector\\|normal\\|matrix\\|array\\|color\\)\\>" . font-lock-constant-face)
      ;;globals
      ("\\<\\(Cs\\|Os\\|P\\|I\\|E\\|s\\|t\\|dPdu\\|dPdv\\|N\\|Ng\\|Cl\\|Ol\\|L\\|u\\|v\\|du\\|dv\\|Ci\\|Oi\\)\\>" . font-lock-builtin-face)
      ;; built in geometric functions
      ("\\<\\(xcomp\\|ycomp\\|zcomp\\|setxcomp\\|setycomp\\|setzcomp\\|length\\|normalize\\|distance\\|ptlined\\|rotate\\|area\\|faceforward\\|reflect\\|refract\\|fresnel\\|transform\\|vtransform\\|ntransform\\|depth\\|calculatenormal\\)\\>" . font-lock-builtin-face)
      ;; built in math functions
      ("\\<\\(PI\\|radians\\|degrees\\|sin\\|asin\\|cos\\|acos\\|tan\\|atan\\|pow\\|exp\\|sqrt\\|inversesqrt\\|log\\|mod\\|abs\\|sign\\|min\\|max\\|clamp\\|mix\\|floor\\|ceil\\|round\\|step\\|smoothstep\\|filterstep\\|spline\\|Du\\|Dv\\|Deriv\\|random\\|noise\\|pnoise\\|cellnoise\\)\\>" . font-lock-keyword-face)
      ;; built in color functions
      ("\\<\\(comp\\|set_comp\\|ctransform\\)\\>" . font-lock-keyword-face)
      ;; built in Matrix functions
      ("\\<\\(determinant\\|translate\\|scale\\)\\>" . font-lock-keyword-face)
      ;; built in string functions
      ("\\<\\(concat\\|printf\\|format\\|match\\)\\>" . font-lock-keyword-face)
      ;; built in shading and lighting functions
      ("\\<\\(ambient\\|diffuse\\|specular\\|specularbrdf\\phong\\|trace\\)\\>" . font-lock-reference-face)
      ;;built in Texture mapping functions
      ("\\<\\(texture\\|environment\\|shadow\\|textureinfo\\)\\>" . font-lock-keyword-face)
      ;; built in Message passing functions
      ("\\<\\(atmosphere\\|incident\\|opposite\\|attribute\\|option\\|rendererinfo\\|shadername\\)\\>" . font-lock-keyword-face)
      ;; 3Delight and BMRT specific functions
      ("\\<\\(fulltrace\\|rayhittest\\|visibility\\|isshadowray\\|raylevel\\)\\>" . font-lock-builtin-face)
      ;; PRman 11.x functions
      ("\\<\\(occlusion\\|gather\\)\\>" . font-lock-builtin-face)
      )
"Default words to highlight in rsl mode.")



(setq rsl-font-lock-keywords
  (append rsl-font-lock-keywords-3
          c-font-lock-keywords-2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init Variables
;;
(defvar rsl-mode-inc-list nil "List of shader compiler include directories.")

(defvar rsl-mode-rib-file "rman.rib" "Name of RIB file used for shader tests.")

(defun rsl-exp-inc-list (alist)
  (cond
   ((null alist) "")
   (t (concat " -I" (expand-file-name (car alist)) 
	      (rsl-exp-inc-list (cdr alist))))))

(defun rsl-exp-inc-list-aqsis (alist)
  (cond
   ((null alist) "")
   (t (concat " -i" (expand-file-name (car alist)) 
	      (rsl-exp-inc-list (cdr alist))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set default rib file and include directories
;;
(defun rsl-set-ribfile (name)
  "Set the name of the rib file used for render tests."
  (interactive "sName and path of RIB file: ")
  (setq rsl-mode-rib-file name))

(defun rsl-set-inc-dirs (dirs)
  "Set the current include directories as a LISP list of strings - each string denoting one directory."
  (interactive "xList of directories to include: ")
  (setq rsl-mode-inc-list dirs))



;;;;;;;;;;;;;;;;;
;; Shader Compiling Commands
;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using shader (PRMan)
;;
(defun rsl-compile-prman (args)
  "Save the RenderMan shader in the current buffer and compile it for PhotoRealistic RenderMan."
  (interactive "P")
  (save-buffer)
  (let* ((rsl-source-file (buffer-name)))
    (compile (concat "shader" (rsl-exp-inc-list rsl-mode-inc-list) 
		     " " rsl-source-file)))
  (message "Type C-x ` to go to the next error or C-x 1 to remove compilation window."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using slc (BMRT)
;;
(defun rsl-compile-bmrt (args)
  "Save the RenderMan shader in the current buffer and compile it for BMRT."
  (interactive "P")
  (save-buffer)
  (let* ((rsl-source-file (buffer-name)))
    (compile (concat "slc" (rsl-exp-inc-list rsl-mode-inc-list) 
		     " " rsl-source-file)))
  (message "Type C-x 1 to remove compilation window."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using shaderdl (3Delight)
;;
(defun rsl-compile-3delight (args)
  "Save the RenderMan shader in the current buffer and compile it for 3Delight."
  (interactive "P")
  (save-buffer)
  (let* ((rsl-source-file (buffer-name)))
    (compile (concat "shaderdl" (rsl-exp-inc-list rsl-mode-inc-list) 
		     " " rsl-source-file)))
  (message "Type C-x 1 to remove compilation window."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save & compile current buffer using aqsl (aqsis)
;;
(defun rsl-compile-aqsis (args)
  "Save the RenderMan shader in the current buffer and compile it for 3Delight."
  (interactive "P")
  (save-buffer)
  (let* ((rsl-source-file (buffer-name)))
    (compile (concat "aqsl" (rsl-exp-inc-list-aqsis rsl-mode-inc-list) 
		     " " rsl-source-file)))
  (message "Type C-x 1 to remove compilation window."))


;;;;;;;;;;;;;;;;;
;; Rendering Commands
;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute shell command "prman rman.rib" (windows version)
;;
(defun rsl-render-prmanwin (args)
  "Render the current RIB file."
  (interactive "P")
  (let ((cmd (concat "prman " rsl-mode-rib-file)))
    (message cmd)
    (shell-command cmd '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute shell command "render rman.rib" (unix version)
;;
(defun rsl-render-prman (args)
  "Render the current RIB file."
  (interactive "P")
  (let ((cmd (concat "render " rsl-mode-rib-file)))
    (message cmd)
    (shell-command cmd '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute shell command "rendrib rman.rib" BMRT
;;
(defun rsl-render-bmrt (args)
  "Render the RIB file rman.rib in the current directory."
  (interactive "P")
  (let ((cmd (concat "rendrib " rsl-mode-rib-file)))
    (message cmd)
    (shell-command cmd '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute shell command "renderdl rman.rib" 3delight
;;
(defun rsl-render-3delight (args)
  "Render the RIB file rman.rib in the current directory."
  (interactive "P")
  (let ((cmd (concat "renderdl -d " rsl-mode-rib-file)))
    (message cmd)
    (shell-command cmd '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute shell command "aqsis rman.rib" Aqsis
;;
(defun rsl-render-aqsis (args)
  "Render the RIB file rman.rib in the current directory."
  (interactive "P")
  (let ((cmd (concat "aqsis -d " rsl-mode-rib-file)))
    (message cmd)
    (shell-command cmd '())))



(setq auto-mode-alist (append '(("\\.sl$" . rsl-mode)) auto-mode-alist))

(provide 'rsl-mode)
