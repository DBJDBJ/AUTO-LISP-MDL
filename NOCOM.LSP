
;
;function to restore system on error
;
(defun dbj_ff_error ( s / func ) 
       (setvar "cmdecho" 0)
       (if (/= s "Function cancelled")
       		(princ (strcat "\nDBJ_ff_Error: " s))
       );if
       (setq *error* *olderr*)  (gc) ; garbage colection
       (if (/= "acad" (getvar "menuname"))
	       (command ".menu" "acad")
       )
  (princ)
);dbj_ff_err
;.........................................................................
; Return what is left till comment char
(defun nocomment ( s / c ch comment )
   (setq c 0 comment ";")
   (while (and (<=  c (strlen s)) (/=  comment (substr s (1+ c) 1) ))
	  (setq c (1+ c)));while
   (substr s 1 c)
); nocomment
;.........................................................................
; Return the file line 
; but first remove comments 
(defun readll ( f / line )
  (if (setq line (read-line f))
          line 
	  nil ; EOF
  );if
) ; readll
;.........................................................................
; ffn  =  full file name
; return file name with path in front or nil  if not there
(defun DBJ_ffn ( fn )
(if (findfile fn)
    (strcat (findfile fn) fn)
    nil
) ; if
) ; dbj_ffn
;.........................................................................
; ff1  =  file filter 1
; copy file fa to file fb but with no comments ... 
(defun DBJ_ff1 ( fa fb / fpa fpb inl outl )
  (setq inl 0 outl inl)(prompt "\n")
  (if (setq fa (open fa "r"))
  (if (setq fb (open fb "w"))
   (progn
     (while (setq line (readll fa))  
      (setq inl (1+ inl))
      (if (/=  "" (setq line (nocomment line)))
         (progn
         (setq outl (1+ outl))
         (prompt (strcat "\rIN: " (itoa inl) " OUT: " (itoa outl) ))
         (write-line line fb )
	 ) ; progn
      ) ; if
     ) ; while
    (close fa)
    (close fb)
   );progn
     (getstring (strcat "\nFile " fb " could NOT be opened for WRITING..."))
  );if
     (getstring (strcat "\nFile " fa " could NOT be opened for REDING ..."))
  );if
  return ; list of names
); DBJ_ff1
;
(defun c:nocom ( / n1 n2 )
    (setq *olderr* *error* *error* dbj_ff_error)
    (setq n1 (getstring "\nSource File name :")
          n2 (getstring "\nTarget File name :")
    ); setq
     (cond  
        (( = (chr 0) n1))
        (( = (chr 0) n2))
        (( = nil n1))
        (( = nil n2))
	(T  (setq n1 (findfile n1) )
	    (cond (( = nil n1) (prompt "\nSource is not on path?"))
		  (T (dbj_ff1 n1 n2))
	    );cond
	);T
     );cond
    (setq *error* *olderr*) 
  (princ)
);nocomm
