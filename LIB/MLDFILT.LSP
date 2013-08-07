
;.....................................................................
; return the most inner intersection point of one filleted mld 
(defun cutbase ( mld1 / line llen minlin)
(defun llen ( line ) (distance (dxf 10 line)(dxf 11 line)) ) ; of maxd
; cutbase begin
  (setq minlin (car mld1))
  (foreach line mld1
     (if (< (llen line) (llen minlin) ) (setq minlin line)  ) ; if 
  ) ; 
  (dxf 11 minlin) ; is the result
) ; cutbase
;
; break the mld linear set with normal from point cutbase
; return the ending mld
(defun mldcut ( mldset p1 / p2 line normp return ) 
	(defun normp ( ename p3 / p1 p2 p4 )
	   (setq p1 (dxf 10 ename) p2 (dxf 11 ename))
 	   (setq    p4 (polar p3 (- (angle p1 p2) (/ pi 2.0)) 1.0))
  	           (inters p1 p2 p3 p4 nil)
	);normp
;
(setq return '())
(foreach line mldset  (setq p2 (normp line p1))
     (setq return (append return (list (list p2 (dxf 11 line)))))
;    (command ".circle" p2 100.0)
     (command ".break" (list line  (list (car p2) (cadr p2))) 
                             (list (car p2) (cadr p2))) ; break
) ; foreach
return ; the last leftover
) ; mldcut
;.....................................................................
(defun mldfillet ( mld1 mld2  / c 1ent 2ent oldrad typ )
; mldfillet begin
(setq c 0 oldrad (getvar "filletrad")
      typ (strcat (dxf 0 1ent)(dxf 0 2ent)) )
(if (/= 0.0 oldrad) (command ".fillet" "r" 0.0))
      (cond
        ((= "ARCARC" typ)
  (repeat (length mld1)  (setq 1ent (nth c mld1) 2ent (nth c mld2) c (1+ c))
  )
	)
	((= "ARCLINE" typ)
  (repeat (length mld1)  (setq 1ent (nth c mld1) 2ent (nth c mld2) c (1+ c))
         (command ".trim" 1ent 2ent "" (dxf 11 2ent) (arc1p 1ent) "")
  )
	)
	((= "LINEARC" typ)
  (repeat (length mld1)  (setq 1ent (nth c mld1) 2ent (nth c mld2) c (1+ c))
           (setq 2ent (list 2ent (oposite (dxf 11 1ent) 2ent)))
           (setq 1ent (list 1ent (dxf 10 1ent)))
         (command ".fillet" 1ent 2ent)
  )
	)
	((= "LINELINE" typ)
  (repeat (length mld1)  (setq 1ent (nth c mld1) 2ent (nth c mld2) c (1+ c))
           (if (/= (angle (dxf 10 1ent)(dxf 11 1ent))
                  (angle (dxf 10 2ent)(dxf 11 2ent)))
           (progn
           (setq 1ent (list 1ent (dxf 10 1ent)) 2ent (list 2ent (dxf 11 2ent)))
           (command ".fillet" 1ent 2ent)
	   ) ; progn
           ) ; if
  )
	) ; "LINELINE" fillet
	(T (prompt "\nWrong ENtity in MLDFILLET ..."))
      ) ; cond
  ) ; if & repeat
 (setvar "filletrad" oldrad)
) ; mldfillet
