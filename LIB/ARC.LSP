;;;
;;; Return ARC starting point
;;;
(defun arc1p( ename / cpt ang ange rad )
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (dxf 50 ename)     ; starting angle
            rad  (dxf 40 ename)     ; radius
      )
      (setq ange (trans '(1 0 0) (dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles 
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
            ange (angle '(0 0 0) ange)             ; the angles 
            ang (+ ang ange)
      )
      (if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
(polar cpt ang rad)
) ; of arc2p
;;;
;;; Return ARC mid point
;;;
(defun arcmp( ename / cpt ang ange rad )
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (+ (arc1a ename) (/ (- (arc2a ename)(arc1a ename)) 2.0))
            rad  (dxf 40 ename)     ; radius
      )
(polar cpt ang rad)
) ; of arcmp
;;;
;;; Return ARC starting angle
;;;
(defun arc1a( ename / cpt ang ange )
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (dxf 50 ename)     ; angle  
      )
      (setq ange (trans '(1 0 0) (dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles 
            ang (+ ang ange)
      )
  (if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
ang
) ; of arc1a
;;;
;;; Return ARC ending angle
;;;
(defun arc2a( ename / cpt ang ange )
      (setq cpt  (trans (dxf 10 ename) (dxf -1 ename) 1)
            ang  (dxf 51 ename)     ; angle  
      )
      (setq ange (trans '(1 0 0) (dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles 
            ang (+ ang ange)
      )
  (if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
ang
) ; of arc2a
