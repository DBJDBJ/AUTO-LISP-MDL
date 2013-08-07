;...........................................................
; edge and arc or circle intersection 
; 
(defun lac
 ( linename arcname
   o2 ; ref point to find closer intersection
   / r a b p q c d x1 x2 x3 x4 x5 x6 dbj
 x7 x8 x9 x10 x11 x12 x13 i j k k1 k2 k3 k4 xx1 xx2 xx3 xx4 s x y
 y1p y2p y1m y2m xp xm yp ym psel pdis xyp mdis xym )

(setq e1 (entget linename) e2 (entget arcname)
      r (cdr (assoc 40 e2))   a (cadr (assoc 10 e2))
      b (caddr (assoc 10 e2)) p (cadr (assoc 10 e1))
      q (caddr (assoc 10 e1)) c (cadr (assoc 11 e1))
      d (caddr (assoc 11 e1))
) ; setq 
      (if (/= (- p c) 0)
          (progn 
	    (setq s (/ (- q d)(- p c)) x1 (+ 1 (* s s))
	         x2 (* 2 s q) x3 (* 2 a) x4 (* 2 p s s)
		 x5 (* 2 s b) x6 (* a a) x7 (* 2 s q p)
		 x8 (* 2 s p b) x9 (* q q) x10 (* 2 q b)
                x11 (* s s p p) x12 (* b b) x13 (* r r)
		  i x1 j (- (- x2 x3)(+ x4 x5)) k1 (- x6 x7)
		 k2 (+ x8 x9) k3 (- x11 x10) k4 (- x12 x13)
		  k (+ k1 k2 k3 k4) xx1 (- 0 j) xx2 (* j j)
		xx3 (* 4 i k) xx4 (* 2 i)
	   );setq
	    ; check is there intersection at all
	    ; DBJ added 06.03.92
		(cond 
		((> (- xx2 xx3) 0) 
		 (setq xp (/ (+ xx1 (sqrt (- xx2 xx3))) xx4)
		       xm (/ (- xx1 (sqrt (- xx2 xx3))) xx4)
                      y1p (- xp p) y1m (- xm p) y2p (* s y1p)
	              y2m (* s y1m) yp (+ q y2p) ym (+ q y2m)
		      dbj T );setq
		)
		(T  (setq dbj nil) ; no intersection, sorry mate !
		)
		);cond
	 );progn
	 (progn 
	   (setq xp p 
	         xm p 
		  x (abs (- a p)) dbj T
           )
	   (if (= x 0) 
	       (setq y r) 
	       (setq y (sqrt (- (* r r)(* x x))))
	   )
	   (setq yp (+ b y) ym (- b y))
         );progn
    );if
    (cond
     ( (= dbj T) (setq psel o2 xyp (list xp yp) xym (list xm ym)
	        pdis (distance psel xyp) mdis (distance psel xym));setq
                (if (< pdis mdis)
                       xyp ; return         
                       xym
                );if
     )
     ( T nil ) ; No intersection
    );cond
) ; eofun lac 
;;; 
;;; input line arc and ref point
;;; modify them to ip or do nothing
;;; 
(defun la ( l a p / ip c fuzz closer arc1p arc2p )
;;;
;;; Is p1 or p2 closer to p3 ?
(defun closer ( p1 p2 p3 )
(if (<= (distance p1 p3)(distance p2 p3))  p1 p2));closer
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
;la begin
  (setq ip (lac l a p) fuzz 0.000000001)
  (if ip (progn  
  ; line modify
  (if (equal (dxf 11 l) (closer (dxf 10 l)(dxf 11 l) ip) fuzz )
      (setq l (subst (cons 11 ip) (assoc 11 (entget l)) (entget l)))
      (setq l (subst (cons 10 ip) (assoc 10 (entget l)) (entget l)))
  ) ; if
  ; arc modify
  (setq c (trans (dxf 10 a) (dxf -1 a) 1)) ; arc center
  (if (equal (arc1p a) (closer (arc1p a)(arc2p a) ip) fuzz)
  (setq a (subst (cons 50 (angle c ip)) (assoc 50 (entget a)) (entget a)))
  (setq a (subst (cons 51 (angle c ip)) (assoc 51 (entget a)) (entget a)))
  )
  ) ; progn
  (setq l (entget l) a (entget a)) ; else not changed
  ) ; if
  (list (entmod l) (entmod a))
);la
;
(defun dxf (v ename) (cdr (assoc v (entget ename))))
;
(defun c:la ( / l a p )
   (setq a (car (entsel "\nARC  : ")))
   (setq l (car (entsel "\nLINE : ")))
   (setq p (getpoint "\nREFP : "))
   (la l a p)
)
