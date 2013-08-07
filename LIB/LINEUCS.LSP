; DBJ oct 92
; align ucs to vector p1 p2 with Y axis in p1 p2 direction
; ucs org on p1 , x end on p3 and y end on p2
(defun lineucs ( p1 p2 / p3 ) 
 (setq p3 (polar p1 (- (angle p1 p2) (dtr 90)) 1.0 ))
 (command ".ucs" "3" p1 p3 p2)
)
