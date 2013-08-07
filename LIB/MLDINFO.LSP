;
; return current justification as a "Left","Center","Right" or "Number"
(defun mldjust ( mldef / wid just )
 (setq wid (mldwid mldef) just (abs (caaadr mldef)))
  (cond ((= 0.0 just) "LEFT")
        ((= wid just) "RIGHT")
        ((= (/ wid 2) just) "CENTER")
        (T (rtos just))
  )
)
;
; return definition of line N from mldef or nil if not exist
(defun getmldef ( mldef n )
  (if (< n 1) nil (nth (1- n) (cadr mldef))) ;if
)
; return linetype of line N from mldef or nil if there is no n lines
(defun mldltp ( mldef n / ldef )
 (setq ldef (getmldef mldef n)) (if   ldef (nth 1 ldef) nil)  
)
; return offset of line N from mldef or nil if there is no n lines
(defun mldoff ( mldef n / ldef )
 (setq ldef (getmldef mldef n)) (if   ldef (nth 0 ldef) nil)  
)
; return full width of current mldef set of parralels 
(defun mldwid ( mldef / ldef return )
     (setq return 0.0)   ; one line mldef has width 0 !
        (foreach ldef (cdadr mldef) ; no name and no first line 
         (setq return (+ return (car ldef)))
        )
return ; mldwid
)
; return layer of line N from mldef or nil if there is no n lines
(defun mldlyr ( mldef n / ldef )
 (setq ldef (getmldef mldef n)) (if   ldef (nth 2 ldef) nil)  
)
