;
; mldlayon dbj created 19.10.92
; switch to requested layer , make it if it does not exist
; in default color of clr=nil or in color clr 
(defun mldlayon ( lname clr)
  (setvar "cmdecho" 0)
  (tblnext "layer" T)
  (if (= nil (tblsearch "layer" lname ))
      (command ".layer" "make" lname "")
  )
  (if (/= nil clr)
      (command ".layer" "c" clr lname "")
  )
  (command ".layer" "thaw" lname "set" lname "")
)
