

ACOMP - The AutoLisp Compiler for AutoCad R10-R12 DOS/Windows
-------------------------------------------------------------

*Description written by Reini Urban <rurban@sbox.tu-graz.ac.at>*  
*Parts (quoted) are from the official acomp documentation.*   
*http://xarch.tu-graz.ac.at/autocad/bi4/acomp.txt*   

New:  

1) Fixed docs for FUNCTION   
2) Beware: bug on US versions -> see at the end   
3) ACADLC was never officially supported for the US version, only for the international version!   
4) In the ZIP the required interpreter ACADLC.EXP is already renamed to ACADL.EXP.   
5) A version for AutoCAD R12 WIN is available on the ftp-site (ftp://xarch.tu-graz.ac.at/pub/autocad/bi4/acadlc_win.zip)   

The following text reflects my personal opinion on the AutoLISP compiler
ACOMP available from Autodesk for the international AutoCAD Releases R10-R12.
Some information from the manual appears on quoted form. Its basically
an short overview of the in Europe quite wellknown AutoLISP compiler.   

ACOMP, the lisp compiler produces BI4 files, which are interpreted
by a special ACADL.EXP, the lisp interpreter, which replaces the normal
ACADL.EXP, provided by AutoDesk.   

"The main practical difference between using the Compiler and normal,
interpreted AutoLISP programming  is the additional process of compilation
after you have debugged your source code. The binary file(s) produced by the
Compiler are loaded in exactly the same way as normal AUtoLISP files, either
by using the LOAD function..." with the .BI4 extension added   
        command: (load "TEST.BI4")   
or by renaming the binary file to the .LSP extension   
       >RENAME TEST.BI4 TEST.LSP   
and then loading it with   
        command: (load "TEST").   
(quoted text from the manual)   

The R10 version couldn't handle PROTECT'ed AutoLISP code:   
*"It should be noted that you can freely mix compiled and int erpreted code in
your programs, except for code compiled with the PROTECT program. If you have
such files, use the original source code or/and recompile it with the AutoLISP
compiler if you can."*  
The R12 version can load PROTECTED code.  


The compiler consists of two files: ACOMP.EXE and ACOMP.INI  
and it needs the improved ACADLC.EXP, which replaces the original ACADL.EXP.
The environment variable COMPINIT should point to ACOMP.INI if it's not in
the current directory. (eg: SET COMPINIT=C:\ACOMP\ACOMP.INI)
ACADLC.EXP is already renamed to ACADL.EXP in ACOMP.ZIP.  

Usage  
-----  
        >acomp [-e] LSPFILE(s) ... [-oBINFILE]  

-e produces extended AutoLISP files. Should be used for all versions >R10c10
   which use a DOS Extender or the the extended AutoLISP overlay (R10).
   So always use switch -e, it produces (BD4A) functions, the old compiler
   produced (BDC4) functions which are comparable.  

LSPFILE(s) are standard AutoLISP source files with the default
           extension .LSP   
BINFILE    have the extension .BI2 for standard (<= R10) compiled Lisp
           and .BI4 for extended (>R10) compiled Lisp.   

Multiple sourcefiles given on the commandline will be compiled into one large
compiled binary file.   

For larger functions you will need a lot of DOS memory (> 550 KB approx.)
In such cases the compiler prints "insufficient node space", but continues
compilation. Either release  more DOS memory, or split your functions into
smaller parts.   

ACOMP seems to have problems with large environments. So best create a batch
file which unsets large environment variables, call acomp and restore the variables.
Something like:   
        @echo off
        rem usage: makebi4 filename
        rem single file without extension
        rem acomp requires a VERY clean environment
        set path=.;c:\usr\bin;E:\WINNT\system32
        REM remove inline comments
        acomp -e %1 > %1-bi4.out
        more < %1-bi4.out   

or better, with removing inline comments:    

        @echo off
        rem usage: makebi4 filename
        rem single file without extension
        rem acomp requires a VERY clean environment
        set path=.;c:\usr\bin;E:\WINNT\system32
        set b=%1.lxx
        REM remove inline comments
        perl -Sp rmalcmt.pl %1.lsp > %b
        acomp -e %b > %1-bi4.out
        del %b
        more < %1-bi4.out

The rmalcmt.pl perl script is available at  
http://xarch.tu-graz.ac.at/autocad/stdlib/utils/  
but you could also use Vladimir Nesterowsky's   

DIFFERENCES between compiled and uncompiled code
------------------------------------------------

Local vs. Global variables
--------------------------

      example for local variables:
            (defun foo (x / y)
                (setq z (cons x y))

here x and y are local variables
and z is a global (=special) variable.

Second Pass:
"Compiled funtions run much faster using only local variables, and the
compiler assumes by default that all variables are local. If the compiler
encounters a non-local reference to a variable, it declares it as a special
variable, and all subsequent references to such variables will generate
instructions for non-local reference. Sometimes, the order in which
non-local variables occur in source code may cause the Compiler to make a
second pass through the file, in order to properly handle non-local
variables."


SPECIAL
-------
In some cases the Compiler cannot recognize if a variable should be local or
non-local. For this case a functions is provided to declare variables as
non-local.

         (special <variable-list>)

eg:      (special '(z *test*))

This function should be placed at top-level of the program. It improves the
detection of non-local variables, and will, if correctly defined, supress a
second compiler pass.

Example:

FILE1.LSP:
        (defun inita (/ a)
              (setq a 1)
           (foo)
        )

FILE2.LSP:  
       (defun foo nil
              (+ a 1)
            )
       (inita)


With the normal (interpreted) AutoLISP the result will be 2.
As compiled functions you will cause an error message "bad argument type" from
the + function, because the variable from the first file is assumed as local
variable, and therefore not known to the second function.
You will need to specify
(special '(a)) in FILE1.LSP


QUOTE LAMBDA -> FUNCTION LAMBDA  [changed July 98]
-------------------------------

In some cases
            (apply '(lambda (x) (* x x)) (list 1))

which is the same as
            (apply (quote (lambda (x) (* x x))) (list 1))

should be replaced with:
            (apply (function (lambda (x) (* x x))) (list 1))

Here FUNCTION is the same as a QUOTEd LAMBDA, but the compiler understands
better what the programmer intends. So use the special form FUNCTION instead
of QUOTE for lambda expressions with system functions only.
You may also use (FUNCTION named-function) instead of 'named-function.

Limitations:
The special function FUNCTION must only be used after internal functions
which take function arguments such as apply or mapcar, but NOT with
user supplied functions, such as (remove-if) or such.

Note:
In Common Lisp FUNCTION (or #') is used like this:
          (apply #'(lambda (x) (* x x)) (list 1))
which expands to
          (apply (function (lambda (x) (* x x))) (list 1))

Vital Lisp's internal FUNCTION also works like this besides the above 
limitation on user-functions. VL's FUNCTION may be used with user functions.
Therefore you can use the assumption:

           (defun acomp-p () (eq (type bd4a) 'SUBR))
           (defun vl-p () (not (listp '(lambda () T))))

			(if (not (acomp-p))
			  (defun special (x) nil)	; AutoLISP and VL workaround
			  (if (not vl-p)
				(setq function quote)       ; plain AutoLISP workaround
			  )
			)


SELF MODIFYING CODE
-------------------
Is forbidden within compiled functions. Use instead plain lisp code.
(simply append uncompiled code to compiled lispfiles)

Ex: COPY TEST.BI4+SELFMOD.LSP TEST.LSP


MAXIMUM NUMBER OF ARGUMENTS
---------------------------
must not exceed 32 for all user defined and the following internal functions:

+ - * / = /= < <= > >= and append bool expt list logand logoir lsh mapcar max
min or rem strcat strlen

To the follwoing functions these restrictions do not apply: (since the
interpretation of most if these functions is handled over to plain autolisp)

command cond debug defun foreach lambda progn repeat setq trace undebug
untrace while


Enhancing the internal stacksize: COMPSTACK (only for < R12)
------------------------------------------------------------
The default stacksize is 4000 for plain (extended) AutoLISP (which should
work, otherwise try first to simplify your functions).
On the error message
"compiler stack overflow"

you can enhance the stacksize for compiled code with setting the environment
variable COMPSTACK to a higher size

Ex:
		SET COMPSTACK=8000



Mixed code
----------
If you intend to use compiled and uncompiled code
the following construct would be useful:

		(if (not BD4A)
		  (defun special (x) nil)
		  (setq function quote)		; this works in AutoLISP
		)
		(special '(BDC4))



Debugger
--------
The Lisp Compiler for R10 and R11 had debugging functions included, a break
with stepper, conditional breakpoints, which are not supported anymore
with the R12 compatible release. You could still use the old (better)
Lisp interpreter, but there is no support for (wcmatch), and there is a
small bug, which prevents you from retrieving attribute data.
(cannot entnext on complex entities)


List of new reserved indentifiers
---------------------------------

*BACKTRACE*     prints out the stack frame (backtrace) on error if no *ERROR*
                function is defined, default: T (only R10)

*BREAK*         enabls break interception at breakpoints (only R10)

*FASTLINK*      you cannot trace internal compiled functions, on NIL execution
                will be slower but you will see backtrace at an error.
                default: T (only R10)

*QUIETLOAD*     when NIL, the names of loaded functions are printed on the
                screen. Default is T. (only R10)

*USER_BREAK*    if defined, this function is invoked before normal
                executing break-level (only R10)

ST, SI          Stepping options at break-level, step, step in (only R10)

BDC2, BDC4      compiled function header, the following code is binary
BD4A, BD2A      (>R11)

REVERSIP        fast destructive reverse, destroys the original list!
                (all versions)

BACK_TRACE      prints backtrace information on break-level
BREAK           break function, only after progn,while,foreach,repeat,
                        top-level
DEBUG
ERRSET
NEXTATOM
SIGNAL_ERROR
SPECIAL
UNDEBUG
C:RESET         new (mainly) debugging functions

ASUBR
CSUBR
CVMPAGE
FSUBR
PAGETB
VSUBR           some new internal node types, returned by TYPE


Reflections
-----------
"Compiled BI4 Lisps" are encrypted Lisp files, in a byte-code compiled format,
where some time critical functions like (cond), (setq), (if), (while), (and),
(or) - basically conditional, logical and assignment functions - are handled
internal and all other functions are left over to the normal Lisp
interpreter (in AutoLISP:  ACADL.EXP).
Free byte-code compilers are available, even in source code. Look for
xlisp21gbc.zip, xlisp3 or cmucl

The advantages over plain AutoLISP code are:
1) speed, compiled code loads and executes much faster, comparable to ADS
   functions
2) security, your source is encrypted and cannot be read or altered
3) error testing, the additional compilation process detects errors that
   otherwise wouldn't have been detected.


I and others used it successfully for over four years now excessivly.
With R13 the support for BI4 files was dismissed, but some of the people who
wrote the compiler, released now Vital Lisp (Basis Software), a whole
programming environment under Windows, which is very recommendable.
Vital Lisp produces .FAS files and need a special Runtime module (like ACOMP)

BTW:
The internal encryption scheme and portions of the compilation process behind
compiled functions were hacked, but never publically released (and will not
in the near future).
FAS files are secure, they are standard in the Common Lisp world
for years now.

You can reach the authors at <basis@access.digex.net>
but dont ask them about ACOMP!

Bug in the US version with ACADLC
---------------------------------
You will encounter with the domestic unlocked release of AutoCAD
the following problem:
  ACAD.LSP is not loaded at startup
Workaround:
  Load ACAD.LSP from the menufile,
  append the line (load "acad" -1) at the end of ACAD.MNL.
  If you use another menu good coding style is to include a
  (load "acad.mnl") line in the <menu>.mnl file. If your
  <menu>.mnl does not do this (it often happens, what a mess),
  load acad.lsp from there.

R12 Windows ACADLC.EXE
----------------------
For the international version of ACADWIN 12 the interpreter for
compiled lisps ACADLC.EXE was delivered on the 5th disk.
  ftp://xarch.tu-graz.ac.at/pub/autocad/bi4/acadlc_win.zip
I guess that with the US version the same bug as with the DOS version
will occur (see above).
I never tested it personally, I use Vital Lisp now. If you experience
any strange phenomenon other than expired please tell me, to keep the
docs uptodate.

----
by Reini Urban <rurban@sbox.tu-graz.ac.at>
http://xarch.tu-graz.ac.at/autocad/bi4/acomp.txt
created: 19.Sep 95, last update: 24.Jul 98