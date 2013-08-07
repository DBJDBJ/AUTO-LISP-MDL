;
; adjust mld2 by layer
; layer of (car mld1) to be = to layer of (car mld2) 
(defun layersort ( mld1 mld2 / line1 line2 result)
         (foreach line1 mld1
            (foreach line2 mld2
               (if (= (dxf 8 line2)(dxf 8 line1))
                   (setq result (append result (list line2)))
               );if
            ) ; for line2
         ) ; for
         (if (= (length result)(length mld1))
             (progn (if run_debug (prompt "\nMLD's adjusted by layers"))
             (setq mld2 result))
         )
mld2
) ; of layersort
(defun mldorder ( mld1 mld2 / lin1 lin2 ip c)
(setq c 0)
  (repeat (length mld1)
     (setq lin1 (nth c mld1) lin2 (nth c mld2)
             ip (inters (dxf 10 lin1)(dxf 11 lin1)
                        (dxf 10 lin2)(dxf 11 lin2) nil)
     )
     (if (not (equal ip (dxf 11 lin1) 0.001 ))
         (swapline lin1) ; end of first towards ip
     );
     (if (not (equal ip (dxf 10 lin2) 0.001 ))
         (swapline lin2) ; start of second towards ip
     );
  )
 (list mld1 mld2) ; return both changed
)
; or if how = "ORDER"
; make start of mld2 closer point to inters. with mld1
(defun geomsort ( mld1 mld2 / line1 line2 result ip)
         (foreach line1 mld1
            (foreach line2 mld2
               (setq ip (inters (dxf 10 line1)(dxf 11 line1)
                                (dxf 10 line2)(dxf 11 line2) nil ));ip setq
               (if (> (distance ip (dxf 10 line2))
                      (distance ip (dxf 11 line2)))
                   (swapline line2) 
               );if
            ) ; for line2
         ) ; for
mld2
) ; of geomsort
;
(defun swapline ( ename / p1 p2 line ) ; swap line endings
       (setq   p1 (cons 10 (dxf 11 ename)) 
               p2 (cons 11 (dxf 10 ename)) 
             line (entget ename)
             line (subst p1 (assoc 10 line) line)   
             line (subst p2 (assoc 11 line) line)   
       ) ; setq 
 (entmod line)
 (entupd ename)
) ; of swapline 
(defun c:swapdemo ( / ent )
(setq ent (entsel "\nSelect solid: "))
 (while T
    (swapline (car ent)) (repeat 100000)
 )
)
