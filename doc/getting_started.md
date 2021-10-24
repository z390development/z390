# Get started

## Installation

Follow this guide to setup z390 on your system.

1. Install the prerequisites.
2. Get the latest z390 distribution.
3. Install z390.

### Install the prerequisites

You will need a Java(tm) runtime environment version 8 or above installed on your system.

[:material-link: Get Eclipse Adoptium Java](https://adoptium.net/){ .md-button .md-button--primary }

### Get the latest z390 distribution

Download the latest version of the z390 distribution (Version {{ version }}).

[:material-download: Download z390](){ .md-button .md-button--primary }

### Install z390

Installation is as simple as unzipping the z390 distribution file to your local file system.

You can add the distribution script folder to your system path so that you can use the z390 tools directly
on the command line.

=== "Windows"
    If z390 is installed in `c:\z390`, then add `c:\z390\bat` to your system path.

=== "MacOS/Unix"
    If z390 is installed in `/usr/local/z390`, then add `/usr/local/z390/bash` to your system path.


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

Add the contents of this to a file named 'HELLO.MLC'.

Now run the following z390 command to assemble, link and run the program.

=== "Windows"
    :information_source: Assumes z390 has been installed in `c:\z390`. Substitute for your local install location.

    ``` dos
    c:\z390\bat\asmlg HELLO.MLC SYSMAC(c:\z390\mac+)
    ```

=== "MacOS/Unix"
    :information_source: Assumes z390 has been installed in `/usr/local/z390`. Substitute for your local install location.
    ```
    /usr/local/z390/bash/asmlg HELLO.MLC "SYSMAC(/usr/local/z390/mac+)"
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
        PROGRAM-ID. HELLO.

        PROCEDURE DIVISION.
            DISPLAY "HELLO ZCOBOL".
            STOP RUN.
```

Add the contents of this to a file named 'HELLO.MLC'.

Now run the following z390 command to assemble, link and run the COBOL program.

=== "Windows"
    :information_source: Assumes z390 has been installed in `c:\z390`. Substitute for your local install location.

    ``` dos
    c:\z390\bat\cblclg HELLO
    ```

=== "MacOS/Unix"
    :information_source: Assumes z390 has been installed in `/usr/local/z390`. Substitute for your local install location.
    ```
    /usr/local/z390/bash/cblclg HELLO
    ```
If you have successfully run the sample COBOL program, the output to the console will be similar to the following:

``` text
21:13:17 hello     ZC390 START USING z390 V1.7.07 ON J2SE 16.0.1 06/27/21
21:13:17 hello     ZC390 ENDED   RC= 0 SEC= 0 MEM(MB)= 10 IO=2
21:13:18 hello     MZ390 START USING z390 V1.7.07 ON J2SE 16.0.1 06/27/21
21:13:19 hello     MZ390 ENDED   RC= 0 SEC= 1 MEM(MB)=109 IO=19951
21:13:19 hello     LZ390 START USING z390 V1.7.07 ON J2SE 16.0.1 06/27/21
21:13:19 hello     LZ390 ENDED   RC= 0 SEC= 0 MEM(MB)= 11 IO=72
21:13:19 hello     EZ390 START USING z390 V1.7.07 ON J2SE 16.0.1 06/27/21
{==HELLO ZCOBOL==}
21:13:19 hello     EZ390 ENDED   RC= 0 SEC= 0 MEM(MB)= 17 IO=185 INS=41
```
### Hello zCICS

TODO

## Next steps

* New to the mainframe, assembler and COBOL? See [Learning resources](learning.md).
* Ready to learn more about using z390. Check out the [z390 User Guide](user_guide.md)
