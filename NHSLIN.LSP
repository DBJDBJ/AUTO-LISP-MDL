;
; nhslin.lsp 12.10.92 dbj  created
;
(setq function
(lambda ( / blknme rtd p1 p2 a1 a2 )
  
  (defun rtd (a) (/ (* a 180.0) pi))    ; radians to degrees
  
  (setvar "cmdecho" 0)
  (setq *error* runerr
        blknme "NHSLT1"
        p1 (getpoint "\nFirst point :")
        p2 (if p1 (getpoint p1 "\rNext  point :"))
        a1 (if p2 (angle p1 p2))
  )
(while p2 
  (if a2 (progn
  (prompt (strcat "\nPrevious angle: " (rtos (rtd a1)) " degrees "))
  (prompt (strcat "\nThis     angle: " (rtos (rtd a2)) " degrees "))
  (prompt (strcat "\nIncluded angle: " (rtos (rtd (- a2 a1))) " degrees "))
  (setq a1 a2)
  ))
  (command ".insert" blknme "xscale" p1 p2 p1 p2)
  (setq p1 p2
        p2 (getpoint p1 "\nNext  point :")
        a2 (if p2 (angle p1 p2))
  )  
) ; while
  (setvar "cmdecho" 1)
  ( princ )
);end
);setq function
