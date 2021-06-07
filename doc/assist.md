# ASSIST support

ASSIST stands for **A**ssembler **S**ystem for **S**tudent **I**nstruction and **S**ystems **T**eaching.

It is an IBM System/370-compatible assembler and interpreter developed in the early 1970s at Penn State 
University by Graham Campbell and John Mashey.[^1]
[^1]:See [ASSIST (computing) at Wikipedia](https://en.wikipedia.org/wiki/ASSIST_(computing)) for more details.

The ASSIST support in z390 allows you to use the I/O and debugging instructions as described in the ASSIST
User Manual.

See [ASSIST
User Manual PART II. INPUT/OUTPUT AND DEBUGGING INSTRUCTIONS](http://faculty.cs.niu.edu/~byrnes/csci360/ho/asusergd.shtml#part2) for more details.

## Using ASSIST instructions with z390

### `ASSIST` option

The ASSIST option must be provided to use the ASSIST instructions. See [z390 Options](Options/z390_options.md) for details.

### `assist` command script

z390 `assist` command script can be used to assemble, link, and execute ASSIST programs.

``` dos
assist {programname}
```

The `assist` command script enables ASSIST option and defines the following files for input and output:

* XREAD={programname}.XRD
* XPRNT={programname}.XPR
* XPNCH={programname}.XPH
* XGET={programname}.XGT
* XPUT={programname}.XPT

## File processing

* Files are optional and will be opened on first access and closed at end of execution or end of file.
* All files are ASCII with automatic conversion from or to EBCDIC. 
* ASCII file records have trailing spaces removed and end with carriage return x'0d' and line feed x'0a'.
* Input fields are padded with EBCDIC spaces x'40'.
* Non ASCII output bytes are converted to periods.

## Default ASSIST option behavior

Using the ASSIST option enables z390 options NOLOADHIGH and NOINIT for compatibility with the original ASSIST assembler
and simplifies relative address calculations.

This has the following impacts:

* Programs are loaded at X'8000' instead of high end of memory.
* Registers are initialized to X'F4'.
* Memory above the PSA is initialized to X'F5'.
* Uninitialized areas of 390 load modules are initialized to X'F6'.

You can use the options LOADHIGH and INIT to override this behavior. See [z390 Options](Options/z390_options.md) for more information.

## Error handling

A trace table is used to display the last 10 instructions prior to abnormal termination of a program when the TRACE option is off.

The PSW displayed at abnormal termination includes ILC, CC, MASK, and AMODE.

## ASSIST instructions

### XDECI - Convert decimal to binary

Format: `RX X'53rxbddd' r1.s2`

* Start scan for next decimal number at s2 skipping leading blanks. 
* Convert decimal number to binary in r1 until non decimal character found or more than nine digits.
* When ten (or more) digits are found, the instruction does not do the conversion and condition code 3 is set.
* Set register 1 to address of the last non decimal character found.
* Set condition code 0 if number converted successfully else set condition code 3.

### XDECO - Convert binary to decimal

Format: `RX X'52rxbddd' r1,s2`

Convert binary 32 bit r1 value to right justified 12 character decimal field at s2.

### XDUMP - dump registers and/or storage

Format: `RXSS X'E06xbdddbddd' s1(x1),s2`

* If no operands are specified, dump registers and the default storage area.
* If no XLIMD instruction has reset default storage dump area, dump all storage.
* if s1(x1) address of storage area and s2 length of area are specified just dump that area.

### XGET - read record from ASCII file

Format: `RXSS X'E0Axbdddbddd' s1(x1),s2`

* Read record from ASCII file DDNAME=XGET into area s1(x1) with length of s2.
* Requires that the environment variable `XGET` be set and point to desired input file.
* If a file operation is successful the condition code is set to 0.
* At end of file, condition code 1 is set.
* If a file open error occurs, program terminates with S013 abend with error message showing the file specification which failed.
* If any other error occurs such as missing length, condition code 2 is set.

### XHEXI - convert hex to binary

Format: `RX X'61' r1,s2` 

Convert hex to binary (cc3 if no hex, update field addr).

### XHEXO - convert binary to hex

Format: `RX X'62' r1,s2` 

Convert value in r1 to printable hex (8 bytes), storing value at s2.

### XLIMD - Set default dump storage area

Format: `RXSS X'E08xrbdddbddd' s1(x1),s2`

Set default XDUMP storage area address and length.

### XPNCH - Write to punch

Format: `RXSS X'E04xbdddbddd' s1(x1),s2`

* Write to DDNAME=XPNCH with length s2.
* Requires that the environment variable `XPNCH` be set and point to desired output file.
* Does not set condition code.
* If a file error occurs the program aborts with abend code S013.
* If a file open error occurs, program terminates with S013 abend with error message showing the file specification which failed.

### XPRNT - Write to print

Format: `RXSS X'E02xbdddbddd' s1(x1),s2`

* Write to DDNAME=XPRNT with length s2.
* Requires that the environment variable `XPRNT` be set and point to desired output file.
* Does not set condition code.
* If a file error occurs, the program aborts with abend code S013.
* If a file open error occurs, program terminates with S013 abend with error message showing the file specification which failed.

### XPUT - Write to XPUT

Format: `RXSS X'E0Cxbdddbddd' s1(x1),s2`

* Write to DDNAME=XPUT for length s2.
* Requires that the environment variable `XPUT` be set and point to desired output file.
* If a file operation is successful the condition code is set to 0.
* If a file open error occurs, program terminates with S013 abend with error message showing the file specification which failed.
* If any other error occurs such as missing length, condition code 2 is set.

### XREAD - Read from XREAD

Format: `RXSS X'E00xbdddbddd' s1(x1),s2` 

* Read record from DDNAME=XREAD for length s2.
* Requires that the environment variable `XREAD` be set and point to desired input file.
* If a file operation is successful the condition code is set to 0.
* At end of file, condition code 1 is set.
* If a file error occurs, the program aborts with abend code S013.
* If a file open error occurs, program terminates with S013 abend with error message showing the file specification which failed.
