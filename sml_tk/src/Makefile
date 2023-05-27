##
##   $Source: /repository/sml/sml_tk/src/Makefile,v $
## 
##   Makefile for sml_tk. Compiles sml_tk to SMLTK_BINARY, using ML image
##   SMLTK_COMP; compiled image will display SMLTK_BANNER on startup.
##   
##   $Date: 2001/03/30 13:38:56 $
##   $Revision: 3.0 $
##   Author: stefan/cxl (Last modification by $Author: 2cxl $)
## 
##   (C) 1997, Bremen Institute for Safe Systems, Universitaet Bremen
##

all :   
	echo "use \"$(SMLTK_ROOT)\"; \
	      SmlTk.init(); \
	      SmlTk.xSmlTk {imagefile=\"$(SMLTK_BINARY)\", banner=\"$(SMLTK_BANNER)\"};" | \
	$(SMLTK_COMP)

allI :   
	echo "CM.SymVal.define(\"TK_INSIDE\", 1); \
	      CM.make (); \
	      SmlTk.init(); \
	      SmlTk.xSmlTk {imagefile=\"$(SMLTK_BINARY)\", banner=\"$(SMLTK_BANNER)\"};" | \
	$(SMLTK_COMP)

clean :
	cd toolkit; $(MAKE) clean
	cd tests+examples; $(MAKE) clean
	rm -r -f .cm .depend .x86 .sparc
