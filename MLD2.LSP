
;
; MLD2.lsp 29.10.90 (c) DBJ for HEVACOMP SALES LTD
;
; GLOBALS :     MLDEF   = current ML Definition in a list
; ACADVAR :     TRACEWID keeps full width of MLDEF
;
; Multiple parralleles with different ltype , offset and layer 
; Mld definitions are stored in a *.mld file
;
(defun dtr (a) (* pi (/ a 180.0)))    ; degrees to radians
(defun #dxf (v ename) 
 (if (not (member v '(10 11)))
     (cdr (assoc v (entget ename)))
     (trans (cdr (assoc v (entget ename))) (#dxf -1 ename) 1)
 );if
);#dxf
;
;function to restore system on error
;
(defun mlderr ( s / func ) 
       (setvar "cmdecho" 0)
       (if (/= s "Function cancelled")
       		(princ (strcat "\nMLDError: " s))
       );if
       (setq *error* *olderr*)  (gc) ; garbage colection
       (if (/= "acad" (getvar "menuname"))
       (command ".menu" "acad")
       )
  (princ)
);mlderr
;
;
(defun mld2 ()
; / mpll getmldef mldltp mldoff mldlyr mldjust mldlayon mldwid
;                           mldline grdmld linecomm ending
;                           mldlead mldcmd refpnt from *olderr* )
;
(defun mldbegin ( / from )
;
	(setq *olderr* *error* *error* mlderr run_debug nil)
	(graphscr)
	; (prompt "\n*MLD by DBJ_LTD (c) 1998*")
	(command ".ucsicon" "off")
	(command ".ucs" "w")
	(setvar "blipmode" 0)
	; (setvar "osmode" 0)
	(setvar "aunits" 0) (setvar "auprec" 16)
	(setvar "lunits" 2) (setvar "luprec" 16)
	(setvar "expert"  3 )
;	(if (not (tblsearch "ltype" "phantomx2")) ; last from acad.lin
;	    (progn
;	    (prompt "\nPlease wait, loading acad.lin")        
;	    (command ".linetype" "load" "*" "" "")
;	    )
;	);if
	(if (setq from (getpoint "\nStarting point: "))
	  (mldcmd (p2e from)) 
	);if
	(setvar "aunits" 0) (setvar "auprec" 2)
	(setvar "lunits" 2) (setvar "luprec" 2)
	(command ".ucsicon" "on")
	(setvar "expert"  0 )
  (setq *error* *olderr*)
) ; mldbegin
(defun refpnt ()
 (setvar "lastpoint" (getpoint "reference point: ") )
 (getpoint (getvar "lastpoint") "\nrelative coordinate (with @): ") 
) ; refpnt
;
(defun midpnt ( spt / oldosn p )
  (setq oldosn (getvar "osmode"))
   (setvar "osmode" 2)
    (setq p (getpoint spt "Mid point: ") ) 
   (setvar "osmode" oldosn)
  (if (not p) spt p)
) ; midpnt
;
; set of lines by mld definition parralel to p1 p2
; return list of lines, line being list of two points
; justification value is the offset of the first line in mldef
;
(defun mpll ( p1 p2 mldef / dst next return c )
; parralell line p3-p4 to line p1-p2 on distance dst
; +dst is right -dst is left of going direction
(defun pll ( p1 p2 dst / p3 p4 )
	(setq p3 (polar p1 (- (angle p1 p2) (dtr 90)) dst )
	      p4 (polar p2 (- (angle p1 p2) (dtr 90)) dst )
	)
 (list p3 p4) ; return 
)
; mpll begin
       (setq  return '() c 1
	      dst  (mldoff mldef c) ; justify the mld set
	      next (pll p1 p2 dst)
	      return (append return (list next))
       )
  (repeat (1- (length (cadr mldef)))
       (setq  c (1+ c)
	      dst (mldoff mldef c)
	      next (pll (car next) (cadr next) dst)
	      return (append return (list next))
       )
  ) ; repeat 
 return
); mpll
;
; draw mld set on the screen 
; receive list of lines as lists of points and return list of enames
;
(defun mldline (p1 p2 mldef / c linset line oldlyr oldltp oldosm return)
 (setq oldltp (getvar "celtype") oldosm (getvar "osmode")
       oldlyr (getvar "clayer") return '() )
 (setvar "osmode" 0) 
 (setq linset (mpll p1 p2 mldef))
   (setq c 0)
   (foreach line linset
    (setq c (1+ c))
    (if (car line)(progn
       ; (mldlayon (mldlyr mldef c) nil ) ; colour default
       ; (command ".linetype" "set" (mldltp mldef c) "")
       (command ".line" (car  line) (cadr line) "")
       (setq return (append return (list (entlast))))
    ));if
   ) ; foreach
 ; (command ".linetype" "set" oldltp "")
 ; (command ".layer" "set" oldlyr "")
 (setvar "osmode" oldosm) 
return ; list of enames 
) ; mldline
;.......................................................................
; chprops of a mld line/arc set
(defun mldchp ( mld / c ename oldlyr oldltp oldosm newl newt)
 (setq oldltp (getvar "celtype") oldlyr (getvar "clayer") c 0)
   (foreach ename mld 
       (setq c (1+ c) newl (mldlyr mldef c) newt (mldltp mldef c))
       (if (= nil (tblsearch "layer" newl))
           (command ".layer" "make"  newl "") 
       ) ; if
       (if (= nil (tblsearch "ltype" newt))
           (setq newt "bylayer")
       );if
       (command ".chprop" ename "" "LA" newl "LT" newt "")
   ) ; foreach
 (command ".linetype" "set" oldltp "")
 (command ".layer" "set" oldlyr "")
) ; mldchp
;.......................................................................
; print mld command line , wait for input and return command or point
; return nil if return pressed
(defun mldcmd ( spt / cmd mldprev mldlast ang result)
 (setq cmd T mldlast '() mldprev '())
 (while cmd
 (prompt "\r                                                                 ")
   (initget "Info ALign Arc JUmp Justify Undo Elevate Ref Midp")
 (setq cmd 
  (getpoint spt "\rInfo|ALign|Arc|JUmp|Justify|Elevate|Ref|Midp <Line>: ")
 );cmd
	   (if (= 0 (getvar "worlducs")) (progn
	       (if (= 'LIST (type cmd)) (setq cmd (trans cmd 1 0)))
	       (setq spt (trans spt 1 0))
	       (command ".ucsicon" "off" )
	       (command ".ucs" "w")
	   )) ; if
   (if (= 'LIST (type cmd)) (setq cmd (p2e cmd))); drop it to current elevation
   (cond ((= cmd "Arc")  ; draw arc MLD and return new spt 
	     (command ".undo" "mark")
	     (setq cmd spt spt (arcomm spt ))
	     (if spt
               (if (equal cmd spt 0.1)
                  (getstring "\nToo close to previous point ...")
	       (progn
	         (setq mldlast (mldarc (a2e (entlast)) cmd))
		 (mldchp mldlast)
		 (setq cmd spt)
	       );progn
               );if
	     );if spt
	 ) ; arc option
		 ((= cmd "JUmp") 
		    (initget 7)
		    (setq spt (getpoint "\nJUMP to: "))
		    (setq mldprev mldlast)
		 )
		 ((= cmd "ALign")
		    (command ".ucsicon" "on" )
		    (setq ang (- (align) (ucsang)))
		    (setq spt (trans spt 1 0))
		    (command ".ucs" "z" ang)
		    (setq spt (trans spt 0 1))
		 );align
		 ((= cmd "Info")
		     (mldisply mldef)
		 );info
	 ((= cmd "Justify") (mldjustify))
	 ((= cmd "Undo") (command ".undo" "back" )
	                 (setq mldprev nil) (setq mldlast nil))
	 ((= cmd "Elevate")
	     (command ".undo" "mark")
	     (setq result (mldelev mldlast spt))
	     (setq spt (car result)
	           mldlast (cadr result) 
	           mldprev nil )
	 ) ; Elevate option
	 ((= cmd nil) (prompt "\n"))
	 ((or (= 'LIST (type cmd)) ; next point for a line
	      (= cmd "Ref")(= cmd "Midp")
          );or
	   (if (= cmd "Ref")  (setq cmd (refpnt)))
	   (if (= cmd "Midp") (setq cmd (midpnt spt)))
          (if (equal cmd spt 0.1)
             (getstring "\nToo close to previous point ...")
           (progn
	   (command ".undo" "mark")
	   (setq mldlast (mldline spt cmd mldef))
	   (mldchp mldlast)  
	   (setq spt cmd) ; next begin
	   ));progn&if
	 ) ; line option 
	 (T (prompt (strcat "\nSorry no " cmd " option..."))
	 )
   ) ; cond
   (if (or (= "Elevate" cmd)(= "Arc" cmd)(= 'LIST (type cmd)))
     (progn
       (if mldprev 
	 (setq mldlast (mldconnect mldprev mldlast ))
       );if
       (setq mldprev mldlast)
     );progn
   );if
 ) ; while cmd
);mldcmd
;........................................................................
(setvar "cmdecho" 0 )
	(if (= nil mldef) ; get mldef
	 (progn 
	   (getstring "\nMLD not yet defined, press Return...")
           (textscr)
	   (mldn "BSL")
	 );progn
	);if
   (if mldef 
     (progn
       (command ".undo" "group") (mldbegin) (command ".undo" "end")
     ))
(setvar "cmdecho" 1 )
( princ )
);mld2  end
;.....................................................................
;
; connect to arc or line mld's and return the last one changed
;
(defun mldconnect ( mldprev mldlast / oldos arc line typ base return name c)
    (setq oldos (getvar "osmode"))(setvar "osmode" 0)
    (setq typ (strcat (#dxf 0 (car mldprev))(#dxf 0 (car mldlast)))
	  return '())
	    (cond 
	     ((= typ "LINELINE")
	       (setq base (mldfillet mldprev mldlast ))
	       (setq mldprev (car base))
	       (setq mldlast (cadr base))
	       (setq base (cutbase mldprev))
	       (setq mldprev (mldcut mldprev base) )
	       (setq mldlast (mldcut mldlast base) )
	     )
	     ((= typ "LINEARC")
	       (setq c 0)
	       (foreach line mldprev 
		  (setq arc (nth c mldlast) c (1+ c))
		  (la line arc (#dxf 11 line))
	       )
	       (setq base (cutbase mldprev))
	       (setq mldprev (mldcut mldprev base) )
	       (setq mldlast mldlast)
	     );linearc
	     ((= typ "ARCLINE")
	       (setq c 0)
	       (foreach arc mldprev
		  (setq line (nth c mldlast) c (1+ c))
		  (la line arc (#dxf 10 line))
	       )
	       (setq base (10cutbase mldlast))
	       (setq mldlast (mldcut mldlast base) )
	     );arcline
	     ((= typ "ARCARC")
	     )
	     (T (prompt "\nWrong Enity type in MLDCONNECT..."))
	    ); cond
  (setvar "osmode" oldos)
mldlast ; return
) ; mldconnect
;.....................................................................
(defun llen ( line ) (distance (#dxf 10 line)(#dxf 11 line)) ) ; of maxd
;.....................................................................
; return the most inner intersection point of one filleted linear mld 
(defun 10cutbase ( mld1 / line minlin)
  (setq minlin (car mld1))
  (foreach line mld1
     (if (< (llen line) (llen minlin) ) (setq minlin line)  ) ; if 
  ) ; 
  (#dxf 10 minlin) ; is the result
) ; 10cutbase
;.....................................................................
; return the most inner intersection point of one filleted linear mld 
(defun cutbase ( mld1 / line minlin)
  (setq minlin (car mld1))
  (foreach line mld1
     (if (< (llen line) (llen minlin) ) (setq minlin line)  ) ; if 
  ) ; 
  (#dxf 11 minlin) ; is the result
) ; cutbase
;
; break the mld linear set with normal from point cutbase
; return the ending mld
(defun mldcut ( mldset p1 / p2 line return p ) 
	(defun normp ( ename p3 / p1 p2 p4 )
	   (setq p1 (#dxf 10 ename) p2 (#dxf 11 ename))
	   (setq    p4 (polar p3 (- (angle p1 p2) (/ pi 2.0)) 1.0))
		   (inters p1 p2 p3 p4 nil)
	);normp
;
(setq return '() ) (setvar "cmdecho" 0)
(foreach line mldset  (setq p2 (normp line p1))
  (if (and (/= p2 (#dxf 10 line))
	   (/= p2 (#dxf 11 line))
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
(defun mldfillet ( 1mld 2mld  / c 1ent 2ent oldrad typ )
; mldfillet begin
(setq c 0 )
(setq oldrad (getvar "filletrad"))
(setq    typ (strcat (#dxf 0 (car 1mld))(#dxf 0 (car 2mld))) )
(if (/= 0.0 oldrad) (command ".fillet" "r" 0.0))
      (cond
	((= "LINELINE" typ)
	   (repeat (length 1mld)
	      (setq 1ent (nth c 1mld) 2ent (nth c 2mld) c (1+ c))
	   (if (/= (angle (#dxf 10 1ent)(#dxf 11 1ent))
		  (angle (#dxf 10 2ent)(#dxf 11 2ent)))
	   (progn
	   (setq 1ent (list 1ent (#dxf 10 1ent)) 2ent (list 2ent (#dxf 11 2ent)))
	   (command ".fillet" 1ent 2ent)
	   ) ; progn
	   ) ; if
	   ) ; repeat 
	) ; "LINELINE" fillet
	(T (prompt "\nWrong ENtity in MLDFILLET ..."))
      ) ; cond
 (setvar "filletrad" oldrad)
 (list 1mld 2mld) ; return both sets filetted
) ; mldfillet
;........................................................................
(setq function (lambda () (mld2)));setq function
;........................................................................
;........................................................................
;........................................................................
;........................................................................
;........................................................................
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
(defun mldjustify ( / curjust temp bigoffset )
;;;
  (setq curjust (mldoff mldef 1)) ; current justification
  (initget "Left Center Right")
  (setq temp curjust)
  (setq bigoffset T)                   ; See note above.
  (initget "Left Center Right")
  (setq curjust (getreal (strcat
    "\nSet Dragline position to Left/Center/Right/<"
    (mldjust mldef) ">: ")))
  (cond
    ((= curjust nil)
      (setq curjust temp)
    )
    ((= curjust "Left")
      (setq curjust 0.0
      )
    )
    ((= curjust "Center")
      (setq curjust (- (/ (mldwid mldef) 2.0))
      )
    )
    ((= curjust "Right")
      (setq curjust (* (mldwid mldef) -1.0)
      )
    )
    ((= (type curjust) 'REAL)
      (if bigoffset
	(prompt "\nBe careful, no offset control !")
	(progn
	  (if (or (> curjust (mldwid mldef) (< curjust 0.0)))
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
	  (initget "Yes No")
	  (setq n (getkword "\nMirror this MLD Y or <N>: "))
	    (if (= "Yes" n) 
	        (setq mldef (mldmirr mldef))
	    )
(princ)
); of mldjustify
;
; return current justification as a "Left","Centre","Right" or "Number"
(defun mldjust ( mldef / wid just )
 (setq wid (mldwid mldef) just (abs (caaadr mldef)))
  (cond ((= 0.0 just) "LEFT")
	((= wid just) "RIGHT")
	((= (/ wid 2) just) "CENTER")
	(T (rtos just 2 2))
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
;
; mldlayon dbj created 19.10.92
; switch to requested layer , make it if it does not exist
; in default color of clr=nil or in color clr 
(defun mldlayon ( lname clr)
  (setvar "cmdecho" 0) (tblnext "layer" T)
  (if (= nil (tblsearch "layer" lname )) (command ".layer" "make" lname "")
  )
  (if (/= nil clr) (command ".layer" "c" clr lname "")
  )
  (command ".layer" "thaw" lname "set" lname "")
);mldlayon
; Submit justification as a offset of a first line
(defun subnewoff (newoff / oldline oldset newline newset)
      (setq oldline (caadr mldef)
            oldset  (cadr  mldef)
            newline (subst newoff (car oldline) oldline) 
            newset  (subst newline (car oldset) oldset )
      )
 (list (car mldef) newset)
);subnewoff
;
;.........................................................................
; mirror the lines of the MLDEF but not the distances 
(defun mldmirr (mldef / elem lines names elem2)
 (setq lines (cadr mldef))
   (foreach elem lines 
      (setq names (append names (cdr elem)))
   );foreach
      (setq names (reverse names)) ; (princ "\nNames: ")(print names)
   (foreach elem lines
    (setq elem2 (subst (cadr names) (cadr elem) elem) 
          elem2 (subst (car names) (last elem2) elem2) 
           names (cdr (cdr names))
          lines (subst elem2 elem lines)
    );setq
   );foreach (princ "\nNew lines: ")(print lines)
 (subst lines (cadr mldef) mldef)
) ; mldmirr
;.........................................................................
(defun nocomment ( s lower / c ch comment )
   (setq c 0 comment ";")
   (while (and (<= c (strlen s)) (/= comment (substr s (1+ c) 1) ))
	      (setq c (1+ c)));while
   (if (> c 0) (strcase (substr s 1 c) lower) (chr 0))
); nocomment
;.........................................................................
; Return the list of space delimited words in a file line 
; but first check for comments 
(defun readll ( f / line )
  (if (setq line (read-line f))
      (if (/= "" (setq line (nocomment line nil))) ; no comm. and ucase
          (read (strcat "(" line ")" )) ; something is left
	  (readll f)                    ; nothing, go on next
      ) ; if
  );if
) ; readll
;.........................................................................
; This function finds all mld names in a file
; and returns all names in a list !
(defun mldnames ( fn / line return f )
  (setq return '())
  (setq f (open fn "r"))
  (if f
  (progn
    (while (setq line (readll f))
      (if (member '* line)
          (if (= 'STR (type (cadr (member '* line))))
           (setq return (append return (list (cadr (member '* line)))))
          )
      )
    ) ; while
  (close f)
  );progn
     (getstring (strcat "\nFile " fn " NOT found ..."))
  );if
  return ; list of names
); mldnames
;
; This function displays NUMBERED mldnames from a list  
; and returns the number of a chosen name or nil
(defun mldshownames ( names / line c return allowed) 
(if names (progn
  (textscr) (setq c 0 allowed (chr 0)) 
  (prompt "\n-----------------------------------------------------------------")
  (prompt (strcat "\nAvailable Multiple Line Definitions "))
  (prompt "\n-----------------------------------------------------------------")
  (foreach line names 
  (setq c (1+ c) ) (prompt (strcat "\n" (itoa c) " . " line)))
  (prompt "\n-----------------------------------------------------------------")
  (prompt "\n")
  (setq allowed (strcat "[1.." (itoa c) "]") return (1+ c))
  (while (> return c)
    (initget 7 )
    (setq return (getint (strcat "\nChose new MLD please " allowed " ->")))
  )
  (setq return (nth (1- return) names))
)) ; progn & if
  return ; the chosen one !
); mldshownames
;
; This function finds mld by names in a file
; and returns the mldef global list !
(defun mldget ( mldname fn / f line return done )
  (setq  return '() mldname (strcase mldname))
  (setq f (open fn "r"))
  (if f
  (progn (setq done nil)
    (prompt (strcat "\nFile " fn " searching for MLD " mldname ))
    (while (and (setq line (readll f)) (not done))
      (if (member mldname line)
          (if (member '* line)
	      (progn
	      (prompt " found ->")
	      (setq line (readll f))
              (while (and line (not (member '* line)))
	             (setq return (append return (list line)))
         	 (setq line (readll f))
	      ) ; inner while
	      (prompt " finished") (setq done T)
	      ) ; progn
          ); if
      );if
    ) ; while
  (close f)(prompt "\n")
  (setq return (list mldname return))
  );progn
     (getstring (strcat "\nFile " fn " NOT found ..."))
  );if
  return ; new mldef list 
); mldget
;.........................................................................
; reding new mld from file ...
(defun mldn ( mldfile / n which )
(textpage)
(setvar "cmdecho" 0) 
(if (findfile (strcat mldfile ".mld"))
  (progn
       (setq mldfile (findfile (strcat mldfile ".mld")))
       (if (setq which (mldshownames (mldnames mldfile)))
       (if (setq mldef (mldget which mldfile)) ; GLOBAL
         (progn (setvar "TRACEWID" (mldwid mldef)) ; mld width
          (setq mldef (subnewoff 0.0)); Left just. is default
          (mldjustify)
         );progn
	);if
      );if 
  );progn 
 (prompt (strcat "\nCouldn't find :" mldfile ".mld on current path?!"))
); if
(princ)
) ; mldn end
;
; arcomm.lsp DBJ created
; ARC related functions for MLD application
;
;..........................................................................
;;;
;;; Return ARC starting point
;;;
(defun arc1p ( ename / cpt ang rad )
      (setq cpt  (#dxf 10 ename)     ; center point
            ang  (arc1a  ename)     ; starting angle
            rad  (#dxf 40 ename)     ; radius
      )
(polar cpt ang rad)
) ; of arc1p
;;;
;;; Return ARC ending point
;;;
(defun arc2p ( ename / cpt ang rad )
      (setq cpt  (#dxf 10 ename)     ; center point
            ang  (arc2a  ename)     ; ending angle  
            rad  (#dxf 40 ename)     ; radius
      )
(polar cpt ang rad)
) ; of arc2p
; 03.11.92 DBJ created
; return angle translated into ECS   
(defun a2ecs ( ang ename / ange )
      (setq ange (trans '(1 0 0) (#dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles 
            ang (+ ang ange))
  (if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
ang
);a2ecs
;;;
;;; Return ARC starting angle
;;;
(defun arc1a( ename / cpt ang ange )
      (setq ang  (#dxf 50 ename)     ; angle  
            ange (trans '(1 0 0) (#dxf -1 ename) 1) ; this is how to 'trans'
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
      (setq ang  (#dxf 51 ename)     ; angle  
            ange (trans '(1 0 0) (#dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles 
            ang (+ ang ange)
      )
  (if (> ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
ang
) ; of arc2a
;..........................................................................
; Draw the MLD Arcs and return a list of their enames
;
;
; arrange offseted arcs by current mldefinition 
; and by 'leader' temp arc
; return their enames in a list
;..........................................................................
(defun mldarc ( ename from / to an1 an2 cpt 
                    oldlyr oldltp oldosm rad )
;..........................................................................
(defun arcoff ( arcname n / rad ang cpt pt off nextarc )
(setq cpt (#dxf 10 arcname)
      ang (arc1a arcname)
      off (mldoff mldef n)
      rad (if ccw 
               (- (#dxf 40 arcname) off)
               (+ (#dxf 40 arcname) off)
          );if
      off (abs off)
       pt (polar cpt ang rad)
);setq
  (mldlayon (mldlyr mldef n) nil )
  (if (/= 0.0 off)
  (progn
  (command ".offset" off (list arcname (arc1p arcname)) pt "")
     (setq nextarc (entlast))
  );progn
     (setq nextarc arcname)
  );if
 ;    (command ".chprop" nextarc "" "LT" (mldltp mldef n) "")
 ;    (command ".chprop" nextarc "" "LA" (mldlyr mldef n) "")
     (if (mldoff mldef (1+ n)) (arcoff nextarc (1+ n)))
(setq return (append return (list nextarc)))
);arcoff
;
; mldarc begin
;
 (setq oldltp (getvar "celtype") oldosm (getvar "osmode") return '()
       oldlyr (getvar "clayer") ) (setvar "osmode" 0) 

    (setq return '())
    (cond 
     ((equal from (arc1p ename) 0.1)
        ;(prompt "\nFROM = ARC1P")
	(setq ccw nil)
     )
     ((not (equal from (arc1p ename) 0.1))
        ;(prompt "\nFROM /= ARC1P")
	(setq ccw T)
     )
     (T (prompt "\nSomething mystical in mldarc !"))
    )
  (arcoff ename 1)
 (if (and (/= "RIGHT" (mldjust mldef)) (/= "LEFT"  (mldjust mldef)))
    (command ".erase" ename "") ; the leader 
 )
; (command ".linetype" "set" oldltp "") (command ".layer" "set" oldlyr "")
 (setvar "osmode" oldosm)
(reverse return)
) ; mldarc
;..........................................................................
;..........................................................................
;..........................................................................
; do the arc command , draw it and return to point 
(defun arcomm ( from  / to )
  (initget "Center End")
  (setq to (getpoint from "\nARC: Center/End/<Second point>: "))
   (cond 
      ((= to "Center") (arcenter from)
      )
      ((= to "End")    (arcend from)
      )
      ((= 'LIST (type to))
        (command ".arc" from to ) (prompt "\nARC: End point: ")
        (command pause)
      )
      (T nil)
   ) ; cond
   (if to
   ; arc is in (entlast) in this momment
   (if (equal from (arc1p (entlast)) 0.1) 
            (arc2p (entlast))
	    (arc1p (entlast))
   );if
   );if
) ;arcomm
;;
;;; Endpoint option in the arc option was selected.
;;;
(defun arcend ( strtpt / cpt nextpt temp rad )
  (initget 1)
  (setq cpt (getpoint strtpt "\nARC: Endpoint: "))
  ;; Start the ARC command so that we can get visual dragging.
  (command "arc" strtpt "e" cpt)
  (initget "Angle Direction Radius Center")
  (setq nextpt (getkword "\nARC: Angle/Direction/Radius/<Center>: "))
  (cond 
    ((= nextpt "Angle")
      (prompt "\nARC: Included angle: ")
      (command "a" pause)
    )
    ((= nextpt "Direction")
      (prompt "\nARC: Tangent direction: ")
      (command "d" pause)
    )          
    ((= nextpt "Radius")
      (setq temp T)
      (while temp
        (initget 1)
        (setq rad (getdist cpt "\nARC: Radius: "))
        (if (or (<= rad (/ (getvar "tracewid") 2.0))
                (< rad (/ (distance strtpt cpt) 2.0)))
          (progn
            (princ "\nThe radius entered is less than 1/2 ")
            (princ "of the MLD width or is invalid")
            (princ "\nfor the selected endpoints.  ")
            (princ "Please enter a radius greater than ")
            (if (< (/ (getvar "tracewid") 2.0) 
                   (/ (distance strtpt cpt) 2.0))
              (princ (rtos (/ (distance strtpt cpt) 2.0)))
              (princ (rtos (/ (getvar "tracewid") 2.0)))
            );if
          );progn
          (setq temp nil)
        );if
      );while
      (command "r" rad)
    ) ; of radius
    (T
      (prompt "\nARC: Center: ")
      (command pause)
    );T
  );cond
);of arcend
;;;
;;; The CEnter option for drawing arc segments was selected.
;;;
;;;
(defun arcenter ( strtpt / cpt temp nextpt)
  (setq temp T)
  (while temp
    (initget 1)
    (setq cpt (getpoint strtpt "\nARC: Center point: "))
    (if (<= (distance cpt strtpt) (abs (getvar "tracewid")) )
      (progn
        (princ 
        "\nThe radius defined by the selected center point is too small ")
        (princ "\nfor the current MLD width-> ")(print (getvar "tracewid"))
        (princ "\nPlease select another point.")
      )
      (setq temp nil)
    )
  )
  ;; Start the ARC command so that we can get visual dragging.
  (command "arc" strtpt "c" cpt)
  (initget "Angle Length Endpoint")
  (setq nextpt (getkword "\nARC: Angle/Length of chord/<Endpoint>: "))
  (cond 
    ((= nextpt "Angle")
      (prompt "\nARC: Included angle: ")
      (command "a" pause)
    )
    ((= nextpt "Length")
      (prompt "\nARC: Chord length: ")
      (command "l" pause)
    )
    (T
      (prompt "\nARC: Endpoint: ")
      (command pause)
      )
    ) ; cond
) ; arcenter
; mid point of two 3D points
(defun midp (p1 p2)
(setq p1 (mapcar '+ p1 p2)) (mapcar '/ p1 '(2.0 2.0 2.0))
);midp
;..........................................................................
;
; return end of arc futher from point p
(defun arcop ( p ename / c a1 a2 )
   (setq a1 (arc1p ename) a2 (arc2p ename))
   (if (> (distance p a1 )(distance p a2))
       a1
       a2 
   )
) ; arcop
;;;
;;; Get the point at the opposite 
;;; end from the start point (strtpt).
;;;
(defun oposite ( strtpt ename / cpt ang ange rad nextpt )
  (if (= (#dxf 0 ename) "LINE")
    (setq nextpt (if (equal strtpt (#dxf 10 ename) 0.001)
                   (#dxf 11 ename)
                   (#dxf 10 ename)
                 )
    )
    ;; Then it must be an arc...
    (progn
      ;; get its center point
      (setq cpt  (trans (#dxf 10 ename) (#dxf -1 ename) 1)
            ang  (#dxf 50 ename)     ; starting angle
            rad  (#dxf 40 ename)     ; radius
      )
      (setq ange (trans '(1 0 0) (#dxf -1 ename) 1) ; this is how to 'trans'
            ange (angle '(0 0 0) ange)             ; the angles 
            ang (+ ang ange)
      )
      (if (> ang (* 2 pi))
        (setq ang (- ang (* 2 pi)))
      )
      (setq nextpt (if (equal strtpt (polar cpt ang rad) 0.01)
                     (polar cpt (#dxf 51 ename) rad)
                     (polar cpt ang rad)
                   )
      )
    )
  ) ; if
) ; of oposite 
;...........................................................
; edge and arc or circle intersection 
; 
(defun lac
 ( linename arcname
   o2 ; ref point to find closer intersection
   / e1 e2 r a b p q c d x1 x2 x3 x4 x5 x6 dbj
 x7 x8 x9 x10 x11 x12 x13 i j k k1 k2 k3 k4 xx1 xx2 xx3 xx4 s x y
 y1p y2p y1m y2m xp xm yp ym psel pdis xyp mdis xym )

(setq e1 (entget linename) e2 (entget arcname)
      r (cdr (assoc 40 e2))   a (cadr (assoc 10 e2))
      b (caddr (assoc 10 e2)) p (cadr (assoc 10 e1))
      q (caddr (assoc 10 e1)) c (cadr (assoc 11 e1))
      d (caddr (assoc 11 e1))
) ; setq 
      (if (/= (- p c) 0)
          (progn 
	    (setq s (/ (- q d)(- p c)) x1 (+ 1 (* s s))
	         x2 (* 2 s q) x3 (* 2 a) x4 (* 2 p s s)
		 x5 (* 2 s b) x6 (* a a) x7 (* 2 s q p)
		 x8 (* 2 s p b) x9 (* q q) x10 (* 2 q b)
                x11 (* s s p p) x12 (* b b) x13 (* r r)
		  i x1 j (- (- x2 x3)(+ x4 x5)) k1 (- x6 x7)
		 k2 (+ x8 x9) k3 (- x11 x10) k4 (- x12 x13)
		  k (+ k1 k2 k3 k4) xx1 (- 0 j) xx2 (* j j)
		xx3 (* 4 i k) xx4 (* 2 i)
	   );setq
	    ; check is there intersection at all
	    ; DBJ added 06.03.92
		(cond 
		((> (- xx2 xx3) 0) 
		 (setq xp (/ (+ xx1 (sqrt (- xx2 xx3))) xx4)
		       xm (/ (- xx1 (sqrt (- xx2 xx3))) xx4)
                      y1p (- xp p) y1m (- xm p) y2p (* s y1p)
	              y2m (* s y1m) yp (+ q y2p) ym (+ q y2m)
		      dbj T );setq
		)
		(T  (setq dbj nil) ; no intersection, sorry mate !
		)
		);cond
	 );progn
	 (progn 
	   (setq xp p 
	         xm p 
		  x (abs (- a p)) dbj T
           )
	   (if (= x 0) 
	       (setq y r) 
	       (setq y (sqrt (- (* r r)(* x x))))
	   )
	   (setq yp (+ b y) ym (- b y))
         );progn
    );if
    (cond
     ( (= dbj T) (setq psel o2 xyp (list xp yp) xym (list xm ym)
	        pdis (distance psel xyp) mdis (distance psel xym));setq
                (if (< pdis mdis)
                       xyp ; return         
                       xym
                );if
     )
     ( T nil ) ; No intersection
    );cond
) ; eofun lac 
;;; 
;;; input line arc and ref point
;;; modify them to ip or do nothing
;;; 
(defun la ( l a p / entip ip c fuzz )
;
; Is p1 or p2 closer to p3 ?
(defun closer ( p1 p2 p3 )
(if (<= (distance p1 p3)(distance p2 p3))  p1 p2));closer
;la begin
  (setq ip (lac l a p) fuzz 0.000000001)
  (if ip (progn  
     ; (setq ip (trans ip 0 1))
  ; line modify
  (if (equal (#dxf 11 l) (closer (#dxf 10 l)(#dxf 11 l) ip) fuzz )
      (setq l (subst (cons 11 ip) (assoc 11 (entget l)) (entget l)))
      (setq l (subst (cons 10 ip) (assoc 10 (entget l)) (entget l)))
  ) ; if
  ; arc modify
     (setq c (#dxf 10 a))
  (if (equal (arc1p a) (closer (arc1p a)(arc2p a) ip) fuzz)
    (setq a (subst (cons 50 (angle c ip) ) 
                   (assoc 50 (entget a)) (entget a))
    )
    (setq a (subst (cons 51 (angle c ip) )
                   (assoc 51 (entget a)) (entget a))
    )
  ) ; if
    (list (entmod l) (entmod a)) ; change them
  ) ; progn
    (setq l (entget l) a (entget a)) ; else not changed
  ) ; if ip
);la
 ;............................................................................
 ; display info screen abbout current MLd
 ;
 (defun mldisply ( mldef / dash el line )
 (textscr)
 (textpage)
 (setq dash "\n")(repeat 70 (setq dash (strcat dash "-")))
 (prompt dash)
 (prompt "\nInformation screen for current Multiple Line Definition")
 (prompt dash)
	(prompt (strcat "\nCurrent MLD name     : " (car mldef)))
        (prompt (strcat "\nCurrent justification: " (mldjust mldef)))
        (prompt (strcat "\nCurrent elevation    : " 
	                 (rtos (getvar "elevation"))))
 (prompt dash)
 (prompt "\nMLD structure :")
 (prompt dash)
 (foreach line (cadr mldef)
     (prompt "\n")
     (foreach el line (princ el ) (princ "     "))
 )
 (prompt dash)
 (getstring "\nPress Return key when finished please ...")
 (graphscr)
 (princ)
 ) ; mldysply
;.......................................................................
(defun askelev ( spt / cmd msg )
   (setq msg 
     (strcat "\rCurrent elevation is " (rtos (getvar "elevation") 2 2)
             ", Zero it or go Up or Down or <Point new Z>: ") 
   ) ; msg set
   (initget "Zero Up Down" ) (setq cmd (getpoint spt msg))
   (cond 
         ((= cmd "Up") (initget 7); no nul,zero or negativ
	  (setq cmd (getint "\nUp=+"))
	  (setvar "elevation" (+ (getvar "elevation") cmd))
	  (setq spt (subst (getvar "elevation")(last spt) spt)) 
	 )
         ((= cmd "Down")(initget 7) ; no nul or zero
	  (setq cmd (getint "\nDown=-"))
	  (setvar "elevation" (- (getvar "elevation") cmd))
	  (setq spt (subst (getvar "elevation")(last spt) spt)) 
	 )
         ((= cmd "Zero")
	  (setq cmd 0.0) (setvar "elevation" cmd)
	  (setq spt (subst (getvar "elevation")(last spt) spt)) 
	 )
	 ((= nil cmd) ; do nothing, go back
	  (setq spt nil)
	 )
         (T ; otherwise point is picked 
	  (setvar "elevation" (last cmd))
	  (setq spt (subst (getvar "elevation")(last spt) spt)) 
         ) ; T 
   ) ; cond
 spt
);askelev
;.......................................................................
; New elevation for MLD placement 
; return elevated start point
(defun mldelev ( mldlast spt / p1 org ang up tmp)
(setq tmp (getvar "elevation"))
(setq p1 spt spt (askelev spt))
(setq up (if (> (getvar "elevation") tmp) T nil))
(if (and (not (= (getvar "elevation") tmp)) spt)
    (progn 
           (setq org p1
	         ang  (if (= "ARC" (#dxf 0 (car mldlast)))
		      (angle (#dxf 10 (car mldlast)) p1)
		      (angle (#dxf 11 (car  mldlast)) 
		             (#dxf 11 (last mldlast)))
		      );if
           )
	   (setq  p1 (trans p1 1 0) spt (trans spt 1 0))
	   (command ".ucs" "o" org)
	   (command ".ucs" "z" (rtod ang))
	     (if up (command ".ucs" "x" "+90")
		    (command ".ucs" "x" "-90"))
	   (setq  p1 (trans p1 0 1) spt (trans spt 0 1))
           (setq mldlast (mldline p1 spt mldef))(mldchp mldlast) ;vertical bit 
	   (setq p1 spt)
           (setq org p1
	          p1 (trans p1 1 0) spt (trans spt 1 0)
	   )
	   (command ".ucs" "o" org)
	     (if up (command ".ucs" "x" "-90")
		    (command ".ucs" "x" "+90"))
	   (setq  p1 (trans p1 0 1) spt (list 0.0 (mldwid mldef) 0.0))
           (setq mldlast (mldline p1 spt mldef))(mldchp mldlast);horizontal bit 
	   (setq p1 (trans p1 1 0) spt (trans spt 1 0))
	   (setq p1 spt)
	   (command ".ucs" "w" )
  );progn
);if
(list p1 mldlast) ; return
);mldelev
; return point with Z from acadvar "elevation"
(defun p2e ( p ) (subst (getvar "elevation") (last p) p)
);p2e
; return arc with substituted Z from acadvar "elevation"
(defun a2e ( a / c al)
	(if (= "ARC" (#dxf 0 a))
		(setq c (cdr (assoc 10 (entget a))) ; stay in arc's ECS
		      c (p2e c) 
		      al (entget a)
		      al (subst (cons 10 c) (assoc 10 al) al)
		      al (entmod al)
		);setq 
	); if
	(entupd a)
);a2e 
;
;Routine to measure angle in XY plane of current UCS, given in degrees
;v1.24
(defun ucsang (/ pw)  
  (setq pw (getvar "ucsxdir"))
  (if (zerop (car pw))
;    (princ 90.0)
    90.0
    (rtod (atan (/ (cadr pw) (car pw))))
  )
)
(defun rtod (ang) (setq ang (* 180.0 (/ ang pi))))
;
;General routine to find angle of 'anything'  
;v1.24
(defun align (/ ena )
(defun bsldxf ( c e ) (cdr (assoc c e))) ; bsldxf
;Main Routine to find angle of line etc ;v1.24
;Given in WCS
(defun alignm (ena / ed rot pt1 pt2)
  (if ena 
    (progn
      (setq ed (entget (car ena)))
(if (wcmatch (bsldxf 0 ed) "LINE,POLYLINE,3DFACE,TRACE,SOLID,VERTEX") ;v1.24
        (progn
          (setq pt1 (osnap (cadr ena) "NEA"))
          (setq pt2 (osnap pt1 "MID"))
          (setq rot (rtod (angle (trans pt1 1 0) (trans pt2 1 0))))
        )
        (if (bsldxf 50 ed) (setq rot (rtod (bsldxf 50 ed)))
                        (setq rot 0.0)                    ;v1.24
        );if
      );if
    );progn
    (setq rot 0.0)
  );if
  rot
);alignm
; align begin
  (setq ena (nentsel "\nSelect entity with which to align: ")) ;v1.24
  (alignm ena)
);align
;
(defun c:mld () (mld2))
(defun c:mldn () (mldn "bsl"))
(prompt "\r(c) by DBJ_LTD 1998: Use MLD  for MLD drawing, and MLDN for MLD loading")(princ)
