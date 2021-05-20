# Contributing to z390 Documentation

The following document will provide details on developing z390 documentation.

It is expected that as part of contributing to the code base, that you also update any documentation impacted by the change.

All documentation will be held in the `doc` folder and be in markdown format.
Markdown files have a `.md` extension.
Markdown uses simple text files with specific syntax for formatting. See [the markdown guide](https://www.markdownguide.org/basic-syntax/) for syntax.

You can also use functions provided by the admonition extension in mkdocs. See the [doco for admonition](https://python-markdown.github.io/extensions/admonition/)

## Conventions

### Product names

Product names should always use a small z:

* z390
* zCOBOL  - Always capitalize COBOL
* zVSAM   - Always capitalize VSAM
* zCICS   - Always capitalize CICS

###  Program names and scripts

When specifying tool and script names, always use lower case to be compatible with case sensitive environments like linux.

* mz390
* az390
* asmlg

### Units of measurement

For Bytes:

* When specifying a quantity of 1024 bytes, use {quantity}K. Bytes is assumed. Capital K is deliberate.
* the word bytes can be added if the context does not make it clear that bytes are implied. {quantity}K bytes.

For seconds:

* ms = millisecond
* &micro;s = microseconds (in markdown &micro; = `&micro;`)
* ns = nanoseconds

### Code

HLASM code should be enclosed in a code block as follows:

    ``` hlasm
    LABEL    MVC  X(10),Y
    ```

### Terminology

The following provides guidelines on the use of specific terminology. If not here, you should use the general 
principle of following conventions set in IBM mainframe manuals.

term      | usage
----------|------
word      | OK. Alternate is fullword with no space.
halfword  | OK. No space should be used.
save area | In any text, ensure space is included. The only exception is in code `savearea` without space is preferred 
ASCII     | Always in capitals
EBCDIC    | Always in capitals

## Preview the web pages

The z390 docs use the Python utility mkdocs to convert the markdown to web pages that are published to GitHub pages.

The following instructions will allow you to preview the published version on GitHub prior to committing your changes.

### Installation

You will need some version of Python, and the version should be 3.4 or above.

#### MacOS/Unix

``` sh
python -m venv docenv
source docenv/bin/activate
pip install -r doc/requirements.txt
```

#### Windows

``` dos
python -m venv docenv
docenv\Scripts\activate
pip install -r doc\requirements.txt
```

### Run the live web server

When updating the markdown content, you can run a live web server which checks for changes in the markdown files and updates the web content as you edit.
You can view the web site preview using a web browser pointed at the following address:

<http://localhost:8000>

#### MacOS/Unix
    
``` sh
source docenv/bin/activate
mkdocs serve
```

#### Windows

``` dos
docenv\Scripts\activate
mkdocs serve
```