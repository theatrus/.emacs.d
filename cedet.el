
;;;;
(semantic-mode 1)
(require 'semantic/ia)
;(require 'semantic/bovine/gcc)
(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-g" 'semantic-ia-fast-jump))
(add-hook 'c-mode-common-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
 (local-set-key "." 'semantic-complete-self-insert)
 (local-set-key ">" 'semantic-complete-self-insert))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

(global-ede-mode t)

(global-semantic-mru-bookmark-mode t)

;(ede-cpp-root-project "statsite" :file "/home/atrus/repos/statsite/Makefile"
;     :include-path '( "/deps/murmurhash" "/deps/inih" "/src" "/deps/libev" )
;     :system-include-path '( "/usr/include/c++/3.2.2/" )
;     :spp-table '( ("MOOSE" . "")
;                   ("CONST" . "const") )
;     :spp-files '( "include/config.h" )
;     )
