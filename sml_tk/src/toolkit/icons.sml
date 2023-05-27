(* ***************************************************************************
 
   $Source: /repository/sml/sml_tk/src/toolkit/icons.sml,v $
   
   Subsidiary module for GenGUI, comprising functions to handle icons. 

   Icons are three images of the same size, the normal image, a highlighted
   image (displayed to indicate it is ready for something to be dropped on),
   and an outline, displayed while the icon is being dragged. 

   Additionally, there is a "micro-images" of this image, which is used
   for drag-drop highlighting, or compact presentation in navigation 
   trees, etc. Micro-images are expected to have a fixed size 12 * 12 pt.
 
   $Date: 2001/03/30 13:39:44 $
   $Revision: 3.0 $
   Author: cxl (Last modification by $Author: 2cxl $)

   (C) 1996, 1998, Bremen Institut for Safe Systems, Universitaet Bremen
 
  ************************************************************************** *)


signature ICON_SIG =
    sig
	type icon

	(* get an icon. The first is the directory, the second 
	 * the name of the icon. We could allow for more icons here
	 *)
	val getIcon      : string * string -> icon

	(* the undefined icon *)
	val noIcon       : icon
	val isNoIcon     : icon -> bool

	val selWidth     : icon -> int
	val selHeight    : icon -> int

        val selImage     : icon -> SmlTk.IconKind
        val selHiLite    : icon -> SmlTk.IconKind
        val selOutline   : icon -> SmlTk.IconKind
        val selMicroline : icon -> SmlTk.IconKind

        (* error exception: raised when we can't read an icon file *)
	exception ICON

    end

structure Icons : ICON_SIG = 

struct

    open SmlTk BasicUtil OS.Path
 
    exception ICON

    type icon = IconKind * IconKind * IconKind * int * int * IconKind

    fun selWidth    (_, _, _, x, _ ,_) = x
    fun selHeight   (_, _, _, _, x ,_) = x
    fun selImage    (i, _, _, _, _ ,_) = i
    fun selHiLite   (_, i, _, _, _ ,_) = i
    fun selOutline  (_, _, i, _, _ ,_) = i
    fun selMicroline(_, _, _, _, _ ,i) = i

    val noIcon = (NoIcon, NoIcon, NoIcon, 0, 0,NoIcon)
	
    fun isNoIcon i = case (selImage i) of NoIcon => true | _ => false

    fun openFile f = 
	TextIO.openIn f
	handle IO.Io _ => (Debug.error ("Can't open file "^f); 
			   raise ICON)

    (* The data file is the name as the icon file, but with the extension
     * "data" *)
    fun dataFileNm ifnm =
	joinBaseExt{base=base ifnm, ext= SOME "data"}

    (* The hilight/outlined file icons have the base name of the icon file,
     * but with "-hi"/"-out" added, plus the same extension. Thus, if 
     * the icon is called "theory.gif", the outline is called 
     * "theory-out.gif" *)
    fun outFileNm ifn =
	joinBaseExt{base= (base ifn)^"-out", ext= ext ifn}

    fun hilFileNm ifn =
	joinBaseExt{base= (base ifn)^"-hi", ext= ext ifn}

    fun microFileNm ifn =
	joinBaseExt{base= (base ifn)^"-mic", ext= ext ifn}


    fun getIconData (dir, file) = 
	let val i = openFile(joinDirFile{dir=dir,
					 file=dataFileNm file})
            val w = StringUtil.toInt (valOf (TextIO.inputLine i))
	    val h = StringUtil.toInt (valOf (TextIO.inputLine i))
	    val _ = TextIO.closeIn i
        in  (w, h)
	end

    (* utility function: create file image from file n in directory *)
    fun file_im(dir, n) = FileImage(joinDirFile{dir= dir, file= n},
	                            newImageId());	

    fun getIcon(dir, file) =
	let fun readable f = (OS.FileSys.access(joinDirFile{dir= dir, 
							    file= f},
						[OS.FileSys.A_READ]))
	    val miss = List.filter (not o readable) [file, dataFileNm file]
	    fun getfile s f = 
		file_im(dir, if readable f then f 
			     else (Debug.warning ("Can't find icon file "^f^
						   "-- substituting "^s); s))
	in 
	    if null miss
		then
		let val (w, h) = getIconData(dir, file)
		    val im     = file_im(dir, file)
	            val [iH, iO, imc] = map (getfile file) [hilFileNm file, 
							    outFileNm file, 
							    microFileNm file]
		in
		    (im, iH, iO, w, h,imc)	  
		end
	    else
		(Debug.error ("Icon file(s) missing in "^dir^":"^
			      (StringUtil.concatWith ", " miss));
		 raise ICON)
	end

	    (* this disnae work: readIconWidth/Height need the image to be
	     * displayed, whereas typically you want to know the height and
	     * width of the icon in order to display it correctly. 
	     *)
	    (* val w  = readIconWidth  im *)
	    (* val h  = readconHeight im *)


end



