# The ASM Workbench

A Tool Environment for Computer-Aided Analysis and Validation of Abstract State Machine Models

---

### Introduction

The ASM Workbench is one of the early implementations of [Abstract State Machines](https://en.wikipedia.org/wiki/Abstract_state_machine) (ASMs). It was developed at Paderborn University between 1997 and 2000.

It aims to address the need for tool support in applying ASMs to practical scenarios. It offers a common tool infrastructure that allows for the implementation of different ASM tools as required. Its specification language ASM-SL includes, in addition to basic ASM rules, a static type system and constructs to define the static parts of a specification (types, functions).

The infrastructure includes reusable modules that implement representations of syntactic and semantic entities of ASMs, basic operations to manipulate them, and generic transformation schemes.

Implemented tools based on said infrastructure include:

* a type checker;

* an interpreter;

* an interface to the SMV model checker.

More information about the ASM Workbench can be found in my [thesis](https://github.com/constructum/the-asm-workbench/blob/main/doc/2000%20Del%20Castillo%20-%20The%20ASM%20Workbench.pdf).

Some documentation on how to use the ASM Workbench is in preparation.

---

### Implementation platform

The ASM Workbench

* is written in the Standard ML (SML) functional programming language;

* was developed using the [Standard ML of New Jersey](https://www.smlnj.org/) ([SML/NJ](https://www.smlnj.org/)) implementation of SML;

* has a graphical user interface (GUI) based on the [sml_tk](http://www.informatik.uni-bremen.de/~clueth/sml_tk/) library from Bremen University with the [patches](https://www.tbrk.org/software/sml_tk.html) by T. Bourke and other minor adaptations.

It works and has been tested on Linux with SML/NJ and should probably run with some adaptations on other operating systems and/or SML implementations. I did not attempt to run it on other platforms, but any ports are of course very welcome.

---

### Installation instructions

To get the ASM Workbench up and running, it is probably best to go through the following steps:

1. Install SML/NJ
    - either from the SML/NJ Web site ([https://www.smlnj.org/](https://www.smlnj.org/))
    - or using a package of your Linux distribution (e.g. Debian package *smlnj*)

2. (Optional, but recommended) - Install an appropriate SML plug-in for the editor you use, as this facilitates working with the SML read-eval-print loop:
    - for Emacs, there is this [SML mode](https://www.smlnj.org/doc/Emacs/sml-mode.html) (for installation see [here](https://elpa.gnu.org/packages/sml-mode.html))

3. Get the code from this repository into a local directory

4. Set the environment variable `ASM_WB_HOME` to the local directory containing the code.

   If you want to use the GUI, three more environment variables have to be set as well:
      - `SMLTK_LIB` must be set to `$ASM_WB_HOME/sml_tk/lib/`
      - `SMLTK_LOGFILE` must be set to a file where sml_tk can output its log
      - `SMLTK_TCL` must be set to the location of the Tcl/Tk `wish` shell<br>(to determine this location: `which wish`)

5. Start the Standard ML environment with the `sml` command

6. Load into the SML environment the parts of the ASM Workbench that you want to work with:

    a. Kernel (parser, abstract syntax trees, type checker and interpreter):
      ```
      OS.FileSys.chDir (valOf (OS.Process.getEnv "ASM_WB_HOME") ^ "/src/Kernel");
      CM.make "asm.cm";
      ```
    b. Graphical user interface (GUI):
      ```
      OS.FileSys.chDir (valOf (OS.Process.getEnv "ASM_WB_HOME") ^ "/src/GUI");
      CM.make "asm-gui.cm";
      ```
      Then you can type `ASM_GUI.start()` in the the SML environment to start the GUI.
      
    c. Interface to SMV model checker (ASM2SMV):
      ```
      OS.FileSys.chDir (valOf (OS.Process.getEnv "ASM_WB_HOME") ^ "/src/ASM2SMV");
      CM.make "asm2smv.cm";
      ```

7. It is possible to "export" an image of the SML/NJ environment in order to be able to start the ASM Workbench from the command line (rather than using its functions from within the SML read-eval-print loop). An explanation of the exporting process can be found [here](https://www.cs.cmu.edu/afs/cs/local/sml/common/smlguide/smlnj.htm#export). Note that the result is not a stand-alone executable, but needs the SML/NJ system to be executed.

   - To export the basic ASM Workbench tool, usable via GUI or command-line interface, type the following command in the SML environment (after having loaded the GUI modules as explained in 6.b above):
     ```
     ASM_WB.export();
     ```
     The ASM Workbench can then be started from the (Unix shell) command line using the following script:
     ```
     $ASM_WB_HOME/bin/asm-wb
     ```
     (use `--help` to see the available options)

   - Similarly, to export the ASM2SMV tool (after loading the ASM2SMV modules, see 6.c above):
     ```
     ASM2SMV_Export.export();
     ```
     To start the ASM2SMV tool from the (Unix shell) command line:
     ```
     $ASM_WB_HOME/bin/asm2smv
     ```
     (use `--help` to see the available options)

---

### Feedback

If you have any questions or feedback, please write to

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img style="height:1em;" src="https://github.com/constructum/the-asm-workbench/blob/main/doc/asm-workbench-email.svg" />


For bug reports or change requests, you can use the GitHub repository issue tracker.

Thanks!

