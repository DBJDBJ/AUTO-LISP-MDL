;......................................................................
; pick new mld from menu pop10
; this function is called from menu ml.mnu
(defun getmld ( n )
        (cond
         ((= 1 n)
(setq mldef '( "NHS001" ( (300.0 "hidden" "nhs_pipe_nhs001_01"))
             ) ; of mldef list
) ; of setq 
          )
          ((= 2 n)
(setq mldef '( "NHS002" ( (300.0 "hidden" "nhs_pipe_nhs001_01")
                          (150.0 "center" "nhs_pipe_nhs001_04") )
             ) ; of mldef list
) ; of setq 
          )
          ((= 3 n)
(setq mldef '( "NHS003" ( (300.0 "hidden" "nhs_pipe_nhs001_01")
                          (200.0 "border" "nhs_pipe_nhs001_02")
                          (150.0 "center" "nhs_pipe_nhs001_04") )
             ) ; of mldef list
) ; of setq 
          )
          ((= 4 n)
(setq mldef '( "NHS004" ( (-100.0 "hidden" "nhs_pipe_nhs001_01")
                          (100.0 "hidden" "nhs_pipe_nhs001_02")
                          (200.0 "dashed" "nhs_pipe_nhs001_03")
                          (150.0 "center" "nhs_pipe_nhs001_04") )
             ) ; of mldef list
) ; of setq 
          )
          ((= 5 n)
(setq mldef '( "NHS005" ( (300.0 "hidden" "nhs_pipe_nhs001_01")
                          (200.0 "border" "nhs_pipe_nhs001_02")
                          (200.0 "border" "nhs_pipe_nhs001_02")
                          (100.0 "dashed" "nhs_pipe_nhs001_03") 
                          (150.0 "center" "nhs_pipe_nhs001_04") )
             ) ; of mldef list
) ; of setq 
          )
	  (T (prompt "\nWrong input into GETMLD ?!"))
        ); of cond
     ; very important !
     (setvar "TRACEWID" (mldwid mldef)) ; mld width
) ; of getmld
