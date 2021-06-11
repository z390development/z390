# User guide

## Introduction

The z390 macro assembler, linker, and emulator toolkit provides a way to develop, test, and deploy mainframe compatible assembler programs using any computer that supports a Java runtime environment.

## Licence

z390 macro assembler, linker and emulator toolkit.

Copyright (c) 2021 Don Higgins

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

## Installation

See [Getting started](getting_started.md) for installation instructions.

## Command reference

The following is a list of commands available as part of the z390 toolkit.

### z390

Start the GUI interface with optional startup command file input.

### mac

Run mz390 macro processor to expand MLC macro source file to BAL assembler source file. 

#### Read and write text files

See regression test TESTPCH2.MLC and RTTEST.BAT for example of how mz390 can be used to read and/or write any text file.   

### asm

Run mz390 and az390 assembler to expand MLC macro assembler source file and generate relocatable OBJ relocatable object file.  

!!! Note 
    The default object file format is binary 80 byte record mainframe compatible format.  
    Use option OBJHEX for ASCII text format containing hex format for object code.  The OBJHEX format supports single CSECT’s over 16 MB and you can read the OBJ file for debugging purposes. 
    See [Z390 options](reference/options/z390_options.md) for more details.
    
### asml 

Run mz390, az390, and lz390 to expand MLC macro assembler source, assemble, and link to generate 390 load module.

###	asmlg

Run mz390, az390, lz390 and ez390 to expand MLC macro source, assemble, link, and execute 390 load module.

### link

Run lz390 linker to read one or more relocatable OBJ files and create binary relocatable 390 load module file.  

If the linker option AUTOLINK is on, the linker will search SYSLIB OBJ file directory for external references to be statically linked. See [z390 options](reference/options/z390_options.md) for more details. 

The linker includes options for AMODE and RMODE to control loading and execution modes.  
The linker also has optional input command file with suffix LKD which may contain explicit INCLUDE, ENTRY, ALIAS, and NAME commands.  

### exec

Run ez390 emulator to execute 390 load module. 

#### Interactive debugger

The ez390 emulator supports the following interactive test commands when the [TEST option](reference/options/z390_options.md) is specified:

* `addr=sdt` – set memory value  (i.e. 1r?=x'80' changes mem at (r1) 31 bit
* `reg=sdt` - set register value (i.e. 15r=8 changes reg 15 to 8)
* `A addr` – set or reset up to 100 instruction address stops with hex address or relative expression such as *+4
* `B=addr` - set base for rel addr (ie B=15r% sets base to (r15) 24 bit
* `D` – display DCB file information from TIOT
* `E` – toggle between EBCDIC and ASCII mode
* `F` – display floating point registers F0-FF
* `G` - nn/addr/opcode - exec nn instr. or until specified instruction address or opcode is found with no trace.  One instruction is always executed before next opcode break even if it’s the same instruction such as a BCT 1,*.  Addresses are distinguished from count by hex . or relative expression term such as *, +, or -.
* `H`  -  list help command summary
* `J addr` -  jump to new addr and trace instruction
* `L`  - list all regs and trace current instruction
* `L reg` - list contents of register (ie l 1r dumps register 1
* `L addr len` - list contents of memory area (ie l 10. 4 dumps cvt addr
* `M` – display total memory in MB and total allocate and free bytes 
    * `P` – display current loaded program information from CDE including name, entry and length
    * `Q` - quit execution now
    * `R` – display general purpose registers R0-RF
* `S`  - clear register, address, and memory breaks
* `S reg??sdt`  - set break on register change
* `S addr??sdt` - set break on memory change
* `T nn/addr/opcode` - trace n instr. or until specified instruction address or opcode is found.  One instruction is always executed before next opcode break even if it’s the same instruction such as a BCT 1,*.  Addresses are distinguished from count by hex . or relative expression term such as *, +, or -.  The symbol EPA may be used in place of address to refer to last program load point address.
* `V` - validate/verify
    * `V * nn` - validate nn bytes starting at PSW address
    * `V psw.subfield` - validate PSW subfield
    * `V nnr` - validate GPR nn
* `Z`  - exit test and run to end of program without trace
* `* addr` = hex.,+-hex, *+-hex, dec, nnr% (24 bit), nnr? (31 bit)
* `* reg` = nnr where nn = 0-15
* `* sdt` = self defining term (b'01',c'ab',f'1',h'2',x'ff')
* `* ??` = break compare operator (=,!=,<,<=,>,>=)

## z/OS compatable macros
macro   | description
--------|--
ABEND   | abort program with specified code
ACB     | VSAM access control block
*****ACBD    | VSAM ACB DSECT???
BLDL    | Search for one or more 390 load modules in memory or in SYS390 load library directories.
CALL    | Call subroutine using standard linkage
CHECK   | Check completion of READ/WRITE 
CLOSE   | Close DCB or ACB file
CMDPROC | Control command processing tasks 
CVTD    | DSECT of limited CVT fields supported
        | CVTDCB field set to x’9B’ indicating 31 bit OS
DCB     | Data control block for file (supports 31 bit DCBLRECLF field as extension to DCBLRECL.   See SUPERZAP utility for example of how to read or write up to 2 GB file with 1 I/O operation.)
DCBD    | DSECT of limited DCB fields
DCBE    | Define EODAD and SYNAD for DCB
*** DECBD   | DSECT of limited DECB fields
** DEFINE  | Define VSAM Cluster Definition Table
** DEQ     | Release a resource lock set by ENQ
DELETE  | Freemain 390 load module if use count 0
** ENQ     | request lock on a resource
ERASE   | VSAM delete record
ESPIE   | set program interruption handler
ESTAE   | set program abend handler
** FREEMAIN | release memory area
GENCB   | generate VSAM ACB or RPL
GET     | read record from QSAM or VSAM file
GETENV  | get value of environment variable
** GETMAIN | allocate memory area
** IHAEPIE	| DSECT for ESPIE exits
** IHASDWA	| DSECT for SDWA ESTAE exits
LINK    | load and execute 390 load module 
LOAD    | load 390 load module 
MODCB   | modify ACB or RPL control block field
OPEN    | open DCB or ACB file
POINT   | position to relative record or RBA in file
POST    | post ECB as completed
PUT     | write record to QSAM or VSAM file
READ    | read block from file at current position
RETURN  | restore saved registers and return 
RPL     | VSAM request parameter list
** RPLD    | VSAM RPL DSECT 
SAVE    | save specified registers
** SETRP   | set register options for ESTAE exits
SHOWCB  | move ACB or RPL field to user area
SNAP    | dump selected area of memory and/or dump file information from TIOT/DCB’s, general and floating point registers, program information from CDE, and memory allocation totals
STIMER	| wait specified interval of time
** STORAGE	| obtain or release main storage
TESTCB	| test current value of ACB or RPL field
** TGET    | read from GUI TN3270 interface
TIME    | get time and date in requested format
TTIMER  | test and/or cancel STIMER event
** TPUT    | write to GUI TN3270 display
** VCDTD   | VSAM Catalog Definition Table DSECT
WAIT    | wait for 1 or more ECB’s to be posted
WRITE   | write block to file at current position
WTO     | write text message to operator console
WTOR    | write to operator with reply
XCTL    | transfer control and delete prior pgm
XLATE   | translate area to/from EBCDIC/ASCII

