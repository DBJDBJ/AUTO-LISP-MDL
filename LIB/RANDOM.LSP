; routine to produce a random number between 1 and 100%
(defun rnd (/ temp random)
  (setq temp (* 1000000.0 (getvar "DATE")))
  (/ (1+ (fix (setq random (* 100.0 (- temp (fix temp)))))) 100.0)
)
