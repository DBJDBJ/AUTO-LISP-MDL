;
; arcomm.lsp DBJ created
; ARC related functions for MLD application
;
;..........................................................................
(load (strcat envpath "lib/arc"))
;..........................................................................
; Draw the MLD Arcs and return a list of their enames
;
(defun mldarcshow (cpt from to / c ang1 ang2 return lineucs)
 (setq c 1  return '() ang1 (angle cpt from) ang2 (angle cpt   to))
    (repeat (length (cadr mldef))
       (mldlayon (mldlyr mldef c) nil ) ; layer set 
       (command ".linetype" "set" (mldltp mldef c) "") ; ltype set
       (command ".arc" from "c" cpt to) 
       (setq return (append return (list (entlast))))
       (setq    c  (1+ c))
       (if (/= nil (mldoff mldef c))
       (setq from  (polar from ang1 (mldoff mldef c))
               to  (polar   to ang2 (mldoff mldef c))) ;setq
       ) ; if
    ) ; repeat
return ; list of enames
) ; mldarcshow
;
; arrange offseted arcs by current mldefinition 
; and by 'leader' temp arc
; return their enames in a list
;
;..........................................................................
;..........................................................................
;..........................................................................
(defun mldarc ( ename from / to an1 an2 cpt return 
                    oldlyr oldltp oldosm rad arcoff ccw)
;..........................................................................
(defun arcoff ( arcname n / rad ang cpt pt off nextarc )
(setq cpt (trans (dxf 10 arcname)(dxf -1 arcname) 1)
      ang (arc1a arcname)
      off (mldoff mldef n)
      rad (if ccw 
               (- (dxf 40 arcname) off)
               (+ (dxf 40 arcname) off)
          );if
      off (abs off)
       pt (polar cpt ang rad)
);setq
  (mldlayon (mldlyr mldef n) nil )
  (if (/= 0.0 off)
  (progn
  (command ".offset" off (list arcname (arc1p arcname)) pt "")
     (setq nextarc (entlast))
  );progn
     (setq nextarc arcname)
  );if
     (command ".chprop" nextarc "" "LT" (mldltp mldef n) "")
     (command ".chprop" nextarc "" "LA" (mldlyr mldef n) "")
     (if (mldoff mldef (1+ n)) (arcoff nextarc (1+ n)))
(setq return (append return (list nextarc)))
);arcoff
;
; mldarc begin
;
 (setq oldltp (getvar "celtype") oldosm (getvar "osmode") return '()
       oldlyr (getvar "clayer") ) (setvar "osmode" 0) 

    (setq return '())
    (cond 
     ((equal from (arc1p ename) 0.1)
        ;(prompt "\nFROM = ARC1P")
	(setq ccw nil)
     )
     ((not (equal from (arc1p ename) 0.1))
        ;(prompt "\nFROM /= ARC1P")
	(setq ccw T)
     )
     (T (prompt "\nSomething mystical in mldarc !"))
    )
    ; (setq ccw (if (= from (arc1p ename)) 1.0 -1.0));ccw
    (arcoff ename 1)
    (if (and (/= "RIGHT" (mldjust mldef)) (/= "LEFT"  (mldjust mldef)))
    (command ".erase" ename "") ; the leader 
    )

 (command ".linetype" "set" oldltp "") (command ".layer" "set" oldlyr "")
 (setvar "osmode" oldosm)
(reverse return) ; becouse it is filled recursively Graeme !
) ; mldarc
;..........................................................................
;..........................................................................
;..........................................................................
; do the arc command , draw it and return to point 
(defun arcomm ( from  / to )
  (initget "Center End")
  (setq to (getpoint from "\nARC: Center/End/<Second point>: "))
   (cond 
      ((= to "Center") (arcenter from)
      )
      ((= to "End")    (arcend from)
      )
      ((= 'LIST (type to))
        (command ".arc" from to ) (prompt "\nARC: End point: ")
        (command pause)
      )
      (T nil)
   ) ; cond
   (if to
   ; arc is in (entlast) in this momment
   (if (equal from (arc1p (entlast)) 0.1) 
            (arc2p (entlast))
	    (arc1p (entlast))
   );if
   );if
) ;arcomm
;;
;;; Endpoint option in the arc option was selected.
;;;
(defun arcend ( strtpt / cpt  strtpt nextpt temp rad )
  (initget 1)
  (setq cpt (getpoint strtpt "\nARC: Endpoint: "))
  ;; Start the ARC command so that we can get visual dragging.
  (command "arc" strtpt "e" cpt)
  (initget "Angle Direction Radius Center")
  (setq nextpt (getkword "\nARC: Angle/Direction/Radius/<Center>: "))
  (cond 
    ((= nextpt "Angle")
      (prompt "\nARC: Included angle: ")
      (command "a" pause)
    )
    ((= nextpt "Direction")
      (prompt "\nARC: Tangent direction: ")
      (command "d" pause)
    )          
    ((= nextpt "Radius")
      (setq temp T)
      (while temp
        (initget 1)
        (setq rad (getdist cpt "\nARC: Radius: "))
        
        (if (or (<= rad (/ (getvar "tracewid") 2.0))
                (< rad (/ (distance strtpt cpt) 2.0)))
          (progn
            (princ "\nThe radius entered is less than 1/2 ")
            (princ "of the Dline width or is invalid")
            (princ "\nfor the selected endpoints.  ")
            (princ "Please enter a radius greater than ")
            (if (< (/ (getvar "tracewid") 2.0) 
                   (/ (distance strtpt cpt) 2.0))
              (princ (rtos (/ (distance strtpt cpt) 2.0)))
              (princ (rtos (/ (getvar "tracewid") 2.0)))
            )
            (princ ". ")
          )
          (setq temp nil)
        )
      )
      (command "r" rad)
    ) ; of radius
    (T
      (prompt "\nARC: Center: ")
      (command pause)
    )
  )
)
;;;
;;; The CEnter option for drawing arc segments was selected.
;;;
;;;
(defun arcenter ( strtpt / cpt temp nextpt)
  (setq temp T)
  (while temp
    (initget 1)
    (setq cpt (getpoint strtpt "\nARC: Center point: "))
    (if (<= (distance cpt strtpt) (abs (getvar "tracewid")) )
      (progn
        (princ 
        "\nThe radius defined by the selected center point is too small ")
        (princ "\nfor the current MLD width-> ")(print (getvar "tracewid"))
        (princ "\nPlease select another point.")
      )
      (setq temp nil)
    )
  )
  ;; Start the ARC command so that we can get visual dragging.
  (command "arc" strtpt "c" cpt)
  (initget "Angle Length Endpoint")
  (setq nextpt (getkword "\nARC: Angle/Length of chord/<Endpoint>: "))
  (cond 
    ((= nextpt "Angle")
      (prompt "\nARC: Included angle: ")
      (command "a" pause)
    )
    ((= nextpt "Length")
      (prompt "\nARC: Chord length: ")
      (command "l" pause)
    )
    (T
      (prompt "\nARC: Endpoint: ")
      (command pause)
      )
    ) ; cond
) ; arcenter
;;;
;;; Verify nextpt of (entlast)
;;; Get the point on the line or arc at the opposite 
;;; end from the start point (strtpt).
;;;
(defun chk_nextp ( strtpt / ename cpt ang ange rad nextpt )
  (setq ename (entlast))
  (if (= (dxf 0 ename) "LINE")
    (setq nextpt (if (equal strtpt (dxf 10 ename) 0.001)
                   (dxf 11 ename)
                   (dxf 10 ename)
                 )
    )
    ;; Then it must be an arc...
    (progn
      ;; get its center point
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (dxf 50 ename)     ; starting angle
            rad  (dxf 40 ename)     ; radius
      )
      (setq ange (trans '(1 0 0) (dxf -1 ename) 1)
            ange (angle '(0 0 0) ange)
            ang (+ ang ange)
      )
      (if (> ang (* 2 pi))
        (setq ang (- ang (* 2 pi)))
      )
      (setq nextpt (if (equal strtpt (polar cpt ang rad) 0.01)
                     (polar cpt (dxf 51 ename) rad)
                     (polar cpt ang rad)
                   )
      )
    )
  ) ; if
) ; of chk_nextp
;
; return the mid point of arc
(defun arcmidp ( ename / cpt an1 an2 rad mid )
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            rad  (dxf 40 ename)     ; radius
            an1  (polar cpt (dxf 50 ename) rad)    ; starting point
            an2  (polar cpt (dxf 51 ename) rad)    ; ending point
            mid  (midp an1 an2)  ; chord midpoint
            ; angle from chord midpoint opposite of arc center
            (if (rightside an1 an2 cpt)
                (seqt an1  (angle mid cpt))
                (seqt an1  (angle cpt mid))
            )
            mid  (polar cpt an1 rad) ; arc midpoint
      )
  (grdraw cpt (polar cpt (dxf 50 ename) rad) 1)
  (grdraw cpt (polar cpt (dxf 51 ename) rad) 1)
  (grdraw cpt mid 1)
mid ; return
); arcmidp
; mid point of two 3D points
(defun midp (p1 p2)
(setq p1 (mapcar '+ p1 p2)) (mapcar '/ p1 '(2.0 2.0 2.0))
);midp
;..........................................................................
;
; return end of arc futher from point p
(defun arcop ( p ename / c a1 a2 )
   (setq a1 (arc1p ename) a2 (arc2p ename))
   (if (> (distance p a1 )(distance p a2))
       a1
       a2 
   )
) ; arcop
;;;
;;; Get the point at the opposite 
;;; end from the start point (strtpt).
;;;
(defun oposite ( strtpt ename / cpt ang ange rad nextpt )
  (if (= (dxf 0 ename) "LINE")
    (setq nextpt (if (equal strtpt (dxf 10 ename) 0.001)
                   (dxf 11 ename)
                   (dxf 10 ename)
                 )
    )
    ;; Then it must be an arc...
    (progn
      ;; get its center point
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (dxf 50 ename)     ; starting angle
            rad  (dxf 40 ename)     ; radius
      )
      (setq ange (trans '(1 0 0) (dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles 
            ang (+ ang ange)
      )
      (if (> ang (* 2 pi))
        (setq ang (- ang (* 2 pi)))
      )
      (setq nextpt (if (equal strtpt (polar cpt ang rad) 0.01)
                     (polar cpt (dxf 51 ename) rad)
                     (polar cpt ang rad)
                   )
      )
    )
  ) ; if
) ; of oposite 
;;;
;;; Return ARC starting point
;;;
(defun arc1p( ename / cpt ang ange rad )
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (dxf 50 ename)     ; starting angle
            rad  (dxf 40 ename)     ; radius
      )
      (setq ange (trans '(1 0 0) (dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles Graeme
            ang (+ ang ange)
      )
      (if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
(polar cpt ang rad)
) ; of arc1p
;;;
;;; Return ARC ending point
;;;
(defun arc2p( ename / cpt ang ange rad )
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (dxf 51 ename)     ; ending angle  
            rad  (dxf 40 ename)     ; radius
      )
      (setq ange (trans '(1 0 0) (dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles Graeme
            ang (+ ang ange)
      )
      (if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
(polar cpt ang rad)
) ; of arc2p
;;;
;;; Return ARC mid point
;;;
(defun arcmp( ename / cpt an1 an2 ange rad midp dxf )
(defun dxf (v ename) (cdr (assoc v (entget ename))))
(defun midp (p1 p2) (setq p1 (mapcar '+ p1 p2)) (mapcar '/ p1 '(2.0 2.0 2.0)))
      (setq rad  (dxf 40 ename)     ; radius
            cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
             p1  (arc1p ename) 
             p2  (arc2p ename) 
             mp  (midp p1 p2)
	     mp  (polar cpt (angle cpt mp) rad)
      )
      (if (not (onright p1 (midp p1 p2) mp))
	     mp  (polar cpt (angle mp cpt) rad)
      )
      (grdraw cpt p1 1)
      (grdraw cpt p2 2)
      (grdraw cpt mp 3)
mp
) ; of arcmp
;..........................................................................
; Return T if p3 is right of vector p1 p2
(defun onright ( p1 p2 p3 / p4 ang12 ndist normdist)
	(defun normdist ( p1 p2 p3 / p4 )
 	   (setq    p4 (polar p3 (- (angle p1 p2) (/ pi 2.0)) 1.0))
  	           (inters p1 p2 p3 p4 nil)
	)
    (setq ang12 (angle p1 p2)
             p4 (normdist p1 p2 p3)
          ndist (distance p3 p4)
             p4 (polar p4 (+ ang12 (/ pi 2.0)) ndist)
    )
    (not (equal p3 p4 0.000000001))
)
;..........................................................................
(defun amip( ename / cpt an1 an2 rad amd atrans )
(defun atrans ( a ename / zero )
      (setq zero (trans '(1 0 0) (dxf -1 ename) 1) ; this is how to 'trans'
            zero (angle '(0 0 0) zero)             ; the angles
            a (+ a zero)
      )
  (if (> a (* 2 pi)) (setq a (- a (* 2 pi))))
  a ; return
) ; atrans
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            an1  (atrans (dxf 50 ename) ename)     ; angle  
            an2  (atrans (dxf 51 ename) ename)     ; ending angle  
            rad  (dxf 40 ename)     ; radius
	    amd  (+ an1 (/ (- an2 an1) 2.0))
      )
    (grdraw cpt (polar cpt amd rad) 1)
(polar cpt amd rad)
) ; of amip
