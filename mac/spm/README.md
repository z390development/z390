# HLASM compatible structured programming macros (SPM)

Z390 comes with macros that can be used to create structured assembler
programs using constructs like IF/ELSE/ENDIF, CASE, DO WHILE.

These macros are includes in the `mac` folder.

Because these macros use z390 structured programming extensions (SPE), these
macros cannot be assembled using HLASM.

This folder previously contained a version of the SPM macros that are compatible with HLASM. 

Because this version is generated, it has been removed from the source.

If you wish to regenerate the spm folder for use with HLASM, use the following command:

* `bash\bldzstrmac mac\spm` for MacOS and Linux 
* `bat\bldzstrmac mac/spm` for Windows 

