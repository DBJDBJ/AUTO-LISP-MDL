;.......................................................................
; New elevation for MLD placement 
; return elevated start point
(defun mldelev ( spt / askelev p1 p2 point)
(defun askelev ( spt / cmd msg )
 (setq cmd T )
 (while cmd
   (setq msg 
     (strcat "\nCurrent elevation is " (rtos (getvar "elevation"))
             ", Up or Down or <Point new Z>: ") 
   ) ; msg set
 (prompt "\r                                                                  ")
   (initget "Up Down" ) (setq cmd (getpoint spt msg))
   (cond 
         ((= cmd "Up") (initget 7); no nul,zero or negativ
	  (setq cmd (getint "\nUp= "))
	  (setvar "elevation" (+ (getvar "elevation") cmd))
	  (setq spt (subst (getvar "elevation")(last spt) spt)) 
	 )
         ((= cmd "Down")(initget 7) ; no nul or zero
	  (setq cmd (getint "\nDown= "))
	  (setvar "elevation" (- (getvar "elevation") cmd))
	  (setq spt (subst (getvar "elevation")(last spt) spt)) 
	 )
	 ((= nil cmd) ; do nothing, go back
	 )
         (T ; otherwise point is picked 
	  (setvar "elevation" (last cmd))
	  (setq spt (subst (getvar "elevation")(last spt) spt)) 
         ) ; T 
   ) ; cond
 ) ; while cmd
 spt
);askelev
; mldelev begin
(setvar "pdmode" 0)
(setq p1 spt spt (askelev spt))
(if (and spt (/= p1 spt))
    (progn
     (command ".point" p1) (setq point (entlast))
     (lineucs p1 spt)
     (setq   p1 (trans  p1  (dxf -1 point) 1) p2 (trans  spt (dxf -1 point) 1))
     (mldline p1 p2 mldef)
    );progn
);if
(command ".erase" point "")
(command ".ucs" "p")
spt ; return
);mldelev
; DBJ oct 92
; ucs org on p1 , x end on p3 and y end on p2
(defun lineucs ( p1 p2 / p3 ) 
 (setq p3 (polar p1 (- (angle p1 p2) (dtr 90)) 1.0 ))
 (command ".ucs" "3" p1 p3 p2)
)
