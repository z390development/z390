# Contributing to z390 Documentation

The following document will provide details on developing z390 documentation.

It is expected that as part of contributing to the code base, that you also update any documentation impacted by the change.

All documentation will be held in the `doc` folder and be in markdown format.
Markdown files have a `.md` extension.
Markdown uses simple text files with specific syntax for formatting. See [the markdown guide](https://www.markdownguide.org/basic-syntax/) for syntax.

You can also use functions provided by the admonition extension in mkdocs. See the [doco for admonition](https://python-markdown.github.io/extensions/admonition/)

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