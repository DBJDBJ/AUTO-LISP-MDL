(setq function (lambda ( mld1 mld2 / connect )
(connect mld1 mld2)
)) ; lambda function
;
; do the connections of two lists of paralel lines 
; connecting end of first to intersection
; and intersection to begining of second
; if intersections lies on both of them they are changed
; and conections are nil
; returns list of a four sublists 
;
(defun connect (mld1 mld2 / return l1 l2 l3 l4 p1 p2 c tmp overlap r1 r2 r3 r4 
                          overlaping notoverlaping)
;
; lines are overlaping 'fillet' them and return new lists
(defun overlaping ( l1 l2 / ip )
     (setq ip (inters (car l1) (cadr l1) (car l2) (cadr l2) )
           l1 (list (car l1) ip) ; start to intersection
           l2 (list ip (cadr l2)) ; intersection to end
     ) ; setq 
     (list l1 l2) ; return
)
; lines are not overlaping , return list of two new lines 
; from end of first to intersection and
; from intersection to start of second
(defun notoverlaping ( l1 l2 / ip )
     (setq ip (inters (car l1) (cadr l1) (car l2) (cadr l2) nil)
           l1 (list (cadr l1) ip) ; end to intersection
           l2 (list ip (car l2)) ; intersection to start
     ) ; setq 
     (list l1 l2) ; return
)
;
; connect begin
;
  (setq c 0 )
  (repeat (length mld1)
    (setq  l1 (nth c mld1) ; first 
           l2 (nth c mld2) ; second
           l3 nil
           l4 nil
     overlap  (inters (car l1) (cadr l1) (car l2) (cadr l2) T)
     );setq
      (cond
        ( (= nil overlap)
          (setq  tmp  (notoverlaping l1 l2)  
                  l3  (car  tmp)
                  l4  (cadr tmp)
          )
        )
        ( (/= nil overlap)
          (setq tmp  (overlaping l1 l2)
                 l1  (car  tmp)
                 l2  (cadr tmp)
          )
         )
      ) ; of cond
      (setq   ; return (append return (list (list l1 l2 l3 l4)))
              r1 (append r1 (list l1))
              r2 (append r2 (list l2))
              r3 (append r3 (list l3))
              r4 (append r4 (list l4))
                  c (1+ c)
      ) ; of setq 
  ); repeat 
 (list r1 r3 r4 r2) ; return
) ; of connect
