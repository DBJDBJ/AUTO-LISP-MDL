;..........................................................................
;function to restore system on error
(defun mldcerr ( s ) 
       (setvar "cmdecho" 0)
       (if (/= s "Function cancelled")
       		(princ (strcat "\nMLDcError: " s))
       );if
       (setq *error* *olderr*)  (gc) ; garbage colection
       (if (/= "acad" (getvar "menuname"))
       (command ".menu" "acad")
       )
  (princ)
);mlderr
;..........................................................................
(defun c:mldc ( / 1mld 2mld cbase fullset)
 (setq *olderr* *error* *error* mldcerr )
 (prompt "\r(c) DBJ_LTD 1998 : MLDC -> Connect two MLD's                     ")
 (setq fullset (get2mld))
 (if fullset (progn 
             (setq 1mld (car  fullset) 2mld (cadr fullset) )
             (setq 2mld (layersort 1mld 2mld ) );setq    
             (if (> (length 1mld)(length 2mld)) 
	            (setq 1mld (lenmatch 1mld 2mld)) 
             );if
             (if (> (length 2mld)(length 1mld)) 
	            (setq 2mld (lenmatch 2mld 1mld)) 
             );if
	      (setq 1mld (mldext 2mld 1mld 11 10)) ; make 1mld 11's on ip
	      (setq 2mld (mldext 1mld 2mld 10 11)) ; make 2mld 10's on ip
  (setq cbase (?dxf 11 (ccutbase 1mld)))
  (cmldcut 1mld cbase)
  (setq cbase (?dxf 10 (ccutbase 2mld)))
  (cmldcut 2mld cbase)
   ) ; progn
 ) ; if
 (setq *error* *olderr* )
 (princ)
) ; c:mldc
;..........................................................................
(defun ?dxf (v ename) (cdr (assoc v (entget ename))))
;..........................................................................
(defun findone ( / return lll lin ang newset fuzz )
  (setq lll (car fullset) return (list (car fullset))
        fullset (cdr fullset) newset '() fuzz 0.0000000001 );setq
  (foreach lin fullset
    (if (not
        (inters (?dxf 10 lll)(?dxf 11 lll)(?dxf 10 lin)(?dxf 11 lin) nil)
    )
          (setq return (append return (list lin)))
          (setq newset (append newset (list lin)))
      ) ; if
  ) ; for
  (setq fullset newset)
return
); of findone
;..........................................................................
; find TWO sets of paralels on this ename list
(defun findmld ( fullset  / 1mld 2mld )
   (setq 1mld (findone))
   (if fullset (setq 2mld (findone)))
         (if (and 1mld 2mld)
                 (list 1mld 2mld)
                  nil
         )
);find mld
;..........................................................................
; 
(defun get2mld ( / p1 p2 fullset )
   (setq fullset (ssget "c" (setq p1 (getpoint "\nFirst corner: "))
                            (setq p2 (getcorner p1 "\nSecond corner: "))
                 ))
 (if fullset (progn
 (setq    fullset  (ssss fullset (list '(0 . "LINE"))) ; only lines 
         fullset  (ss2l fullset) ; sset to list
         fullset (findmld fullset) ; extract two mld sets or nil
   )
   (if (not fullset)
     (progn
       (prompt "\nTwo MLD sets not found in this window ...")
       (initget 1 "Yes No")
       (if (= "Yes" (getkword "\nTry again Yes/No: "))
           (setq fullset (get2mld))
           (setq fullset nil)
       )
    )) ; if
 )) ; progn & if
 (foreach p1 (car fullset) (redraw p1 3))
fullset  
) ; of get2mld
;.....................................................................
(defun llen ( line ) (distance (?dxf 10 line)(?dxf 11 line)) )
;.....................................................................
; return the most inner intersection point of one filleted linear mld 
(defun ccutbase ( mld1 / line minlin)
  (setq minlin (car mld1))
  (foreach line mld1
     (if (< (llen line) (llen minlin) ) (setq minlin line)  ) ; if 
  ) ; 
  minlin ; is the result
) ; cutbase
;
; break the mld linear set with normal from point cutbase
; return the ending mld
(defun cmldcut ( mldset p1 / p2 line normp return p ) 
	(defun normp ( ename p3 / p1 p2 p4 )
	   (setq p1 (?dxf 10 ename) p2 (?dxf 11 ename))
 	   (setq    p4 (polar p3 (- (angle p1 p2) (/ pi 2.0)) 1.0))
  	           (inters p1 p2 p3 p4 nil)
	);normp
;
(setq return '() ) (setvar "cmdecho" 0)
(foreach line mldset  (setq p2 (normp line p1))
  (if (and (/= p2 (?dxf 10 line))
           (/= p2 (?dxf 11 line))
      )
    (progn 
    (command ".break" (list line p2) p2)
     (setq return (append return (list (entlast))))
    ) ; progn
     (setq return (append return (list line)))
  )
) ; foreach
;
return ; the last leftover
) ; mldcut
;.....................................................................
; match length of two lists, first is bigger, return it smaller
(defun lenmatch ( 1l 2l / diff rl )
(setq diff (- (length 1l)(length 2l)) rl (reverse 1l))
  (cond
    ((= diff 0) l1 )
    ((< diff 0) (lenmatch 2l 1l))
    (T (reverse (member (nth diff rl) rl)))
  );cond
);lenmatch
;........................................................................
; adjust 2mld by layer
; layer of (car 1mld) to be = to layer of (car 2mld) 
(defun layersort ( 1mld 2mld / line1 line2 result)
         (foreach line1 1mld
            (foreach line2 2mld
               (if (= (?dxf 8 line2)(?dxf 8 line1))
                   (setq result (append result (list line2)))
               );if
            ) ; for line2
         ) ; for
result
) ; of layersort
;...........................................................................
; make ?dxf c1 of 2mld same as point of inters. with 1mld
(defun mldext ( 1mld 2mld c1 c2 / c 1lin 2lin result ip)
  (setq c 0)
  (repeat (length 1mld)
  (setq 1lin (nth c 1mld) 2lin (nth c 2mld) c (1+ c)) 
(setq ip 
(inters (?dxf 10 1lin)(?dxf 11 1lin) (?dxf 10 2lin)(?dxf 11 2lin) nil ));ip setq
(if ip (progn
  (if (> (distance ip (?dxf c1 2lin)) (distance ip (?dxf c2 2lin)))
      (setq 2lin (swapline 2lin)) 
  );if
      (setq 2lin (linemod 2lin c1 ip)) 
));p&if
  (redraw 2lin 3)
  ); repeat
2mld
) ; of mldext
;...........................................................................
;
(defun linemod ( ename what newp / line )
  (if (member what '(10 11))
      (setq line (entget ename)
            newp (cons what newp) 
            line (subst newp (assoc what line) line)   
            line (entmod line)
       ) ; setq 
  );if
 (entupd ename)
);linemode
;
(defun swapline ( ename / p1 p2 line ) ; swap line endings
       (setq   p1 (cons 10 (?dxf 11 ename)) 
               p2 (cons 11 (?dxf 10 ename)) 
             line (entget ename)
             line (subst p1 (assoc 10 line) line)   
             line (subst p2 (assoc 11 line) line)   
       ) ; setq 
 (entmod line)
 (entupd ename)
) ; of swapline 
;.....................................................................
; ssss.lsp Selection Set Sub Set 06.02.92 DBJ created
; syntax for using same as a (ssget "x" ....)
; example : (call '("ssss" lset (list '(0 . "LINE"))))
; to extract only ent's of type "LINE"  
; returns SSet or nil
;
(defun ssss ( ss clist / with what len pass elem sstemp cn ent )
(setq len (sslength ss) cn 0 sstemp (ssadd))
(repeat len
  (setq ent (entget (ssname ss cn)) pass T)
   (foreach elem clist
     (setq with (car elem) what (strcase (cdr elem))
           pass (and (= what (cdr (assoc with ent)) ) pass) ; and
     ) ; setq  
   ); foreach
     (if pass
       (setq sstemp (ssadd (ssname ss cn) sstemp))
     )
  (setq cn (1+ cn))
);repeat
(if (> (sslength sstemp) 0)
     sstemp ; return 
     nil    
)
);ssss
; take sset and return list of enames in it
(defun ss2l ( ss / return cn)
(setq cn 0 return '())
(repeat (sslength ss)
  (setq return (append return (list (ssname ss cn)))
            cn (1+ cn)
  )
);repeat
return
) ;ss2l
