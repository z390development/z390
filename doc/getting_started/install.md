# Installation

Follow this guide to setup z390 on your system.

## Install the prerequisites

You will need a Java(tm) runtime environment version 8 or above installed on 
your system.

[:material-link: Get Eclipse Adoptium Java](https://adoptium.net/){ .md-button .md-button--primary }

## Get the latest z390 distribution

Download the latest version of the z390 distribution.

[:material-download: Download z390](https://github.com/{{repo}}/releases/latest){ .md-button .md-button--primary }

## Install z390

Installation is as simple as unzipping the z390 distribution file to your local 
file system.

You can add the distribution script folder to your system path so that you can 
use the z390 tools directly on the command line.

=== "Windows"
    If z390 is installed in `c:\z390`, then add `c:\z390\bat` to your system path.

=== "MacOS/Unix"
    If z390 is installed in `/usr/local/z390`, we recommend adding the `bash` folder
    to your PATH *after* the existing entries so that system builtins and standard
    utilities keep precedence (this avoids unintentionally overriding system commands).

    For example, add the following line to your shell startup file (use `~/.bashrc`,
    `~/.bash_profile`, or `~/.profile` for bash; use `~/.zshrc` for zsh):

    ```bash
    # append z390 bash tools to the existing PATH so system commands keep precedence
    export PATH="$PATH:/usr/local/z390/bash"
    ```

    After editing the file, reload your shell config:

    ```bash
    # for bash
    source ~/.bashrc
    # or for zsh
    source ~/.zshrc
    ```

    Notes:
    - Placing `/usr/local/z390/bash` after `$PATH` ensures your system's commands
      and builtins remain the default. If you instead want z390 scripts to take
      precedence (not generally recommended), put the directory before `$PATH`.
    - Appending the path is particularly useful when z390 includes scripts that
      share names with builtins or commonly-installed utilities.

    z390 provides a small wrapper script `z390w` (located in the same `bash` folder)
    that allows you to run z390 scripts even when a command with the same name
    would otherwise hide them because it appears earlier in your PATH. The wrapper
    intentionally executes (`exec`) the target script from the z390 `bash` folder
    so the target runs as if called directly.

    Usage examples:

    ```bash
    # run the z390 'link' command even if a system 'link' exists earlier in PATH
    z390w link [options]

    # run any z390 command by passing the command name
    z390w asm   --help
    z390w help
    ```


[:material-run-fast: Continue with Quick Starts](quickstart.md){ .md-button .md-button--primary }
