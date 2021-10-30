# Contributing to z390 Documentation

The following document will provide details on how to contribute to the z390 documentation.

## Submitting changes

Changes to documentation can be made by submitting a pull request to the z390 project repository.

## Content

All documentation will be held in the `doc` folder and be in markdown format.
Markdown files have a `.md` extension.
Markdown uses simple text files with specific syntax for formatting. See [the markdown guide](https://www.markdownguide.org/basic-syntax/) for syntax.

You can also use functions provided by the admonition extension in mkdocs. See the [doco for admonition](https://python-markdown.github.io/extensions/admonition/)

### Structure

All the documentation is contained in the `doc` folder.

The following is the sections of the documentation and their purpose and content:

Section name    |  Folder name       | Purpose and content
----------------|--------------------|--------------------
Home            | index.md           | Landing page for the documentation and project. Designed for people unfamiliar with the project.
Getting started | getting_started.md | Provides details for a new user of z390 to install and start using z390.
Reference       | reference          | Provides details of the z390 project from a user perspective.
Contributing    | contributing       | Provides details of the z390 project from a project contributor/developer perspective.

## Conventions

### Product names

Product names should always use a small z:

* z390
* zCOBOL  - Always capitalize COBOL
* zVSAM   - Always capitalize VSAM
* zCICS   - Always capitalize CICS
* zSORT   - Always capitalize SORT

###  Program names and scripts

When specifying tool and script names, always use lower case to be compatible with case sensitive environments like linux.

* mz390
* az390
* asmlg

### Units of measurement

For bytes:

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
PSW       | Program status word - Always capitals

## Generate the web site

The z390 docs use the Python utility mkdocs to convert the markdown to web pages that are published to GitHub pages.

The following instructions will allow you to preview the published version on GitHub prior to committing your changes.

### Installation

You will need Python version 3.4 or above installed.

=== "MacOS/Unix"

    ``` sh
    python -m venv docenv
    source docenv/bin/activate
    pip install -r doc/requirements.txt
    ```

=== "Windows"
    !!! Note
        You will require the GTK runtime installed on your system.

        See <https://github.com/tschoonj/GTK-for-Windows-Runtime-Environment-Installer/releases>

    ``` dos
    python -m venv docenv
    docenv\Scripts\activate
    pip install -r doc\requirements.txt
    ```

### Run the live web server

When updating the markdown content, you can run a live web server which checks for changes in the markdown files and updates the web content as you edit.
You can view the web site preview using a web browser pointed at the following address:

<http://localhost:8000>

=== "MacOS/Unix"
        
    ``` sh
    source docenv/bin/activate
    mkdocs serve
    ```

=== "Windows"

    ``` dos
    docenv\Scripts\activate
    mkdocs serve
    ```

## Generate the PDF

The distribution will include a PDF with the documentation. 

You can use the following commands to generate the PDF.

=== "Ubuntu Linux"
       
    ``` sh
    apt update && apt install -y libsdl-pango-dev
    source docenv/bin/activate
    ENABLE_PDF_EXPORT=1 mkdocs build
    ```

=== "MacOS"



    ``` sh
    brew install pango
    source docenv/bin/activate
    ENABLE_PDF_EXPORT=1 mkdocs build
    ```


=== "Windows"

    ``` dos
    docenv\Scripts\activate
    set ENABLE_PDF_EXPORT=1 
    mkdocs build
    ```

The generated PDF document will be placed in the file `/site/pdf/z390.pdf`

