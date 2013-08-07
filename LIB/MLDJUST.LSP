;;; MLDJUST.LSP 17.10.92 DBJ CREATED
;;;
;;; TRACEWID holds the full width of current mld set !
;;; first offset of first line from mldef holds the current justificaton 
;;; initialy it is set to 0.0 which is Left justification ...
;;;
;;; Set the alignment of the "ghost" line to one of the following values:
;;;   
;;;   Left   == 0.0
;;;           
;;;   Center == -1/2 of width (Real number)
;;;          
;;;   Right  == -1/1 of width (Real number)
;;;
;;;   Other  == 0 <= (Real number) <= width
;;;
;;; All of the alignment options are taken as if you are standing at the
;;; start point of the line or arc looking toward the end point, with 
;;; left and negative values being on the left, positive on the right.
;;; 
;;; NOTE:  If you wish to allow the dragline to be positioned outside
;;;      of the two arcs or lines being created, you may set the local 
;;;      variable "bigoffset" = T, on the 4th line of the defun, which  
;;;      checks that the offset value entered is not greater or less 
;;;      than + or - TRACEWID .
;;;      
;;;      You should be aware that the results of allowing this to occur
;;;      may not be obvious or necessarily correct.  Specifically, when
;;;      drawing lines with a width of 1 and an offset of 4, if you draw
;;;      segments as follows, the lines will cross back on themselves.
;;;      
(defun mldjustify ( / curjust temp bigoffset subnewoff)
(defun subnewoff (newoff / oldline oldset newline newset)
      (setq oldline (caadr mldef)
            oldset  (cadr  mldef)
            newline (subst newoff (car oldline) oldline) 
            newset  (subst newline (car oldset) oldset )
      )
 (list (car mldef) newset)
)
  (setq curjust (caaadr mldef)) ; current justification
  (initget "Left Center Right")
  (setq temp curjust)
  (setq bigoffset nil)                   ; See note above.
  (setq curjust (getreal (strcat
    "\nSet Dragline position to Left/Center/Right/<"
    (mldjust mldef) ">: ")))
  (cond
    ((= curjust "Left")
      (setq curjust 0.0
      )
    )
    ((= curjust "Center")
      (setq curjust (- (/ (getvar "tracewid") 2.0))
      )
    )
    ((= curjust "Right")
      (setq curjust (* (getvar "tracewid") -1.0)
      )
    )
    ((= (type curjust) 'REAL)
      (if bigoffset
        (prompt "\nBe careful, no offset control !")
        (progn
          (if (or (> curjust (getvar "tracewid") (< curjust 0.0)))
            (progn
              (princ "\nValue entered is out of range.  Reset to Left ...")
              (setq curjust 0.0)) ; Left
            )
          )
        ); if
      ) ; of real typed
    (T
      (setq curjust temp)
    )
  ) ; of cond
(setq mldef (subnewoff curjust))
(princ)
); of mldjustify
