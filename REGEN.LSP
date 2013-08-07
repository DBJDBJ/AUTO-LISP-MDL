;
; 20.10.92 DBJ for HEVACOMP
; regen + ^c crashes acad12 so this is temp solution :
; put it at the top of acad.lsp
;
        (command ".undefine" "regen")
(defun c:regen ()
(setvar "cmdecho" 0)
        (command ".regen")
        (command ".undefine" "regen")
(setvar "cmdecho" 1)
        (princ)
)
