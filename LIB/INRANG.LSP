;
; return T if angle is inner otherwise nil
; if p3 is right form p1
; looking from p1 towards p2 angle is considered inner

(defun inrang ( p1 p2 p3 / oldfollow)
(setq oldfollow (getvar "ucsfollow"))
(setvar "ucsfollow" 0) ; not to go to plan automaticaly
(command ".ucs" "w")
  (lineucs p1 p2) (setq p3 (trans p3 0 1)) 
(command ".ucs" "p")
(command ".ucs" "p") ; once more if it was'nt wcs
(setvar "ucsfollow" oldfollow) ; not to go to plan automaticaly
(> (car p3) 0.0)  ; X positive if inner
)
;
; align ucs to vector p1 p2 with Y axis in p1 p2 direction
; ucs org on p1 , x end on p3 and y end on p2
(defun lineucs ( p1 p2 / p3 ) 
 (setq p3 (polar p1 (- (angle p1 p2) (dtr 90)) 1.0 ))
 (command ".ucs" "3" p1 p3 p2)
)
;..........................................................................
; Return T if p3 is right of vector p1 p2
(defun rightside (p1 p2 p3 / p4 ang12 ndist normdist)
(defun normdist ( p1 p2 p3 / p4 )
    (setq    p4 (polar p3 (- (angle p1 p2) (/ pi 2.0)) 1.0)
             p4 (inters p1 p2 p3 p4 nil))
p4
)
    (setq ang12 (angle p1 p2)
             p4 (normdist p1 p2 p3)
          ndist (distance p3 p4)
             p4 (polar p4 (+ ang12 (/ pi 2.0)) ndist)
    )
    (grdraw p1 p2 1)
    (grdraw p2 p3 2)
    (grdraw p2 p4 3)
    (= p3 p4)
)
 (defun c:test ()
  (princ "\nTest is : ")
  (print 
  (rightside 
      (getpoint "\np1: ")
      (getpoint "\np2: ")
      (getpoint "\np3: "))))
;..........................................................................
;
(defun dtr (a) (* pi (/ a 180.0)))    ; degrees to radians
(defun rtd (a) (/ (* a 180.0) pi))    ; radians to degrees
;
(defun dxf (v ename) (cdr (assoc v (entget ename))))
