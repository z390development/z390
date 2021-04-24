# Docs HOWTO

The following document will provide details on developing z390 documentation.

All document source is in markdown format.

The z390 docs use the Python utility mkdocs to convert the markdown to web pages.

The following instructions will allow you to test the webpages locally.

## Installation

You will need some version of Python -- of course version 3.

### *nix

    python -m venv docenv
    . docenv/bin/activate
    pip install -r doc/requirements.txt

### Windows

    python -m venv docenv
    docenv\Scripts\activate
    pip install -r doc\requirements.txt

## Run the live web server

### *nix
    
    . docenv/bin/activate
    mkdocs serve

### Windows
    docenv\Scripts\activate
    mkdocs serve