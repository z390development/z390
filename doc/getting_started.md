# Get started

## Installation

Follow this guide to setup z390 on your system.

1. Get the latest z390 distribution.
2. Install the prerequisites.
3. Install z390.


### Get the latest z390 distribution

Download the latest version of the z390 distribution.

[:material-download: Download z390](){ .md-button .md-button--primary }

### Install the prerequisites

You will need a Java(tm) runtime environment version 8 or above installed on your system.

[:material-link: Get AdoptOpenJDK Java](https://adoptopenjdk.net/){ .md-button .md-button--primary }

### Install z390

Installation is as simple as unzipping the z390 distribution file to your computers file system.

## Quick starts

### Hello z390

The following is a simple *Hello world* application written in mainframe assembler language.

``` hlasm
HELLO    CSECT
         BASR  15,0
         USING *,15
         WTO   'Hello z390!'
         BR    14
         END
```

Add the contents of this to a file names `HELLO.MLC'.

Now run the following z390 command to assemble, link and run the program.

=== "Windows"
    :information_source: Assumes z390 has been installed in `c:\z390`. Substitute for your local install location.

    ``` dos
    c:\z390\bat\asmlg HELLO.MLC SYSMAC(c:\z390\mac+)
    ```

=== "MacOS/Unix"
    :information_source: Assumes z390 has been installed in `/z390`. Substitute for your local install location.
    ```
    /z390/bash/asmlg HELLO.MLC "SYSMAC(/z390/mac+)"
    ```

If you have successfully run the sample program, the output to the console will be similar to the following:

``` text
09:37:54 HELLO     MZ390 START USING z390 V1.7.03 ON J2SE 14.0.2 06/05/21
09:37:54 HELLO     MZ390 ENDED   RC= 0 SEC= 0 MEM(MB)= 47 IO=200
09:37:55 HELLO     LZ390 START USING z390 V1.7.03 ON J2SE 14.0.2 06/05/21
09:37:55 HELLO     LZ390 ENDED   RC= 0 SEC= 0 MEM(MB)=  8 IO=30
09:37:55 HELLO     EZ390 START USING z390 V1.7.03 ON J2SE 14.0.2 06/05/21
{==Hello z390!==}
09:37:55 HELLO     EZ390 ENDED   RC= 0 SEC= 0 MEM(MB)= 16 IO=21 INS=5
```

### Hello zCOBOL

The following is a simple *Hello world* application written in COBOL.

``` cobol
IDENTIFICATION DIVISION.

```

### Hello zCICS

TODO

## Next steps

* Want to learn HLASM programming? - see [Assembler learning resources]().
* Check out the [z390 User Guide]() for more details of using z390
