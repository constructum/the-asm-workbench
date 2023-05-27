##
##   $Source: /repository/sml/sml_tk/Makefile,v $
## 
##   Makefile for sml_tk.
##   
##   $Date: 2001/03/30 13:37:57 $
##   $Revision: 3.0 $
##   Author: stefan/cxl (Last modification by $Author: 2cxl $)
## 
##   (C) 1997, Bremen Institute for Safe Systems, Universitaet Bremen
##
##
### Configuration variables. 
##
##  The important configuration variables governing sml_tk's behaviour
##  at runtime, are the following:
## 
##  SMLTK_LIB      Path to the library (files needed at runtime) 
##  SMLTK_LOGFILE  Path to the logfile
##  SMLTK_TCL	   Path to the wish executable 
##
##  For the default of this variables if not set at compilation time,
##  see sys_conf.sml.
##
### 

##
## start of configuration section
##

SMLTK_LIB     = /home/cxl/src/sml_tk/lib
SMLTK_TCL     = /usr/local/X11/bin/wish
## SMLTK_LOGFILE not set, hence by default no logging

# Produce the following heap image:
SMLTK_BINARY   = /home/cxl/src/bin/sml_tk


# SML compiler.
## SML/NJ
COMP      = njml
COMP_BIN  = sml
COMP_NAME = SML/NJ 110
## PolyML
#COMP      = poly
#COMP_BIN  = /usr/local/PolyML/bin/polyml /home/wolff/ML_dbase   
# compiler PLUS base !
#COMP_NAME = PolyML 4.0 
## MoscowML (no binary produced-- better use "root_mosml.sml" instead.)
#COMP      = mosml
#COMP_BIN  = mosml
#COMP_NAME = Moscow ML [2.00]

## COMP can also be a path to an image produced from SML/NJ 110. For example,
## the following two load sml_tk on top of the CSP encoding into Isabelle/HOL: 
#COMP      = njml
#COMP_BIN  = /home/uniform/bin/CSP
#COMP_NAME = HOL-CSP \(CSP in Isabelle/HOL\)

##
## end of configuration section
##

# Invoke the make in src with the right parameters set. 

smltk: 
	cd src; \
	$(MAKE) "SMLTK_BINARY=$(SMLTK_BINARY)" \
	        "SMLTK_COMP=$(COMP_BIN)"  \
		"SMLTK_ROOT=root_$(COMP).sml" \
	        "SMLTK_BANNER=\"SmlTk 3.0 for $(COMP_NAME)\"" \
		"SMLTK_LIB=$(SMLTK_LIB)" \
	 	"SMLTK_TCL=$(SMLTK_TCL)" \
	        all

smltkI: 
	cd src; \
	$(MAKE) "SMLTK_BINARY=$(SMLTK_BINARY)" \
	        "SMLTK_COMP=$(COMP_BIN)"  \
		"SMLTK_ROOT=root_$(COMP).sml" \
	        "SMLTK_BANNER=\"SmlTk 3.0I for $(COMP_NAME)\"" \
		"SMLTK_LIB=$(SMLTK_LIB)" \
	 	"SMLTK_TCL=$(SMLTK_TCL)" \
	        allI


# cleaning up:

clean:
	cd src; $(MAKE) clean

realclean:
	cd src; $(MAKE) clean
	rm -f $(SMLTK_BINARY) $(SMLTK_BINARY)-heap 









