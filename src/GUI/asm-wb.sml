(* ******************************************************************* *\
 *
 *   The ASM Workbench for SML/NJ 1.10
 *
 *   File:         asm-wb.sml
 *
 *   Description:  ASM Workbench: Top-level UI (command line + GUI)
 *
 *   Date:         $Date: 2001/03/23 02:10:07 $
 *   Revision:     $Revision: 1.2 $
 *
\* ******************************************************************* *)

structure ASM_WB =
struct
  fun export () =
      SMLofNJ.exportFn ( valOf (OS.Process.getEnv "ASM_WB_HOME") ^ "/bin/asm-wb",
                         ASM_WB_CL.ASM_Workbench )
end
