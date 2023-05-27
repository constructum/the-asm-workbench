(* ***************************************************************************

   $Source: /repository/sml/sml_tk/src/toolkit/print.sml,v $
 
   Printing Format for names.

   $Date: 2001/03/30 13:39:49 $
   $Revision: 3.0 $
   Author: bu (Last modification by $Author: 2cxl $)

   (C) 1999, Albert Ludwigs Universität Freiburg
 
  ************************************************************************** *)
 
     structure Print = 
        struct
            datatype mode   = Short | Long
            type     format ={mode       : mode,
                              printdepth : int,
                              height     : int option,
                              width      : int option}

  
        end
