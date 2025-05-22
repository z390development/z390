# Contributing to z390

The following document will provide details on how to contribute to the z390
project. For how to contribute to the documentation, see 
[contributing to documentation](contribute_docs.md)

## Setup and build

### Install Java SDK

Make sure you have a Java SDK available to compile the application.

The project has moved to the [Eclipse Temurin Java SDK](https://adoptium.net) 
(previously AdoptOpenJDK) for distribution builds.

z390 is using java 21 to build the package but code must support Java 1.8.

To ensure alignment with other developers, use of a Java SDK 21 or greater
is recommended. At a minimum, you will require an SDK of 9 or greater 
as the build jobs use parameters not available in earlier versions.

You can check if this is available by running the following command:

    shell> javac -version
    javac 21.0.0   # you should receive a message like this

### Install git

The z390 source code is maintained in a git repository.

To install git on your system, go to https://git-scm.com/downloads
to download the appropriate version for your system.

You can find a quick reference for some frequently used git commands
the bottom of this document.

### Clone the code

Start with a clone of the main repository.

    shell> git clone https://github.com/z390development/z390.git
    shell> cd z390

Once you are ready to submit changes, you will need a fork of the z390 
repository. See <https://docs.github.com/en/get-started/quickstart/fork-a-repo>
for more details.

### Build the code

=== "Windows"

    `bat> BUILD.BAT`

=== "MacOS/Unix"
        
    `bash> ./build.sh`

If the job successfully runs, it means you are ready to start development.

This build procedure invokes the full regression testing script.
Build and regression testing both can take quite some time.
Luckily, you do not need to do this very often.

### Rebuild the JAR

You can just recompile the JAR without running the full build job by running
the bldjar script.

=== "Windows"

    `bat> bat\BLDJAR.BAT`

=== "MacOS/Unix"
        
    `bash> bash/bldjar`

## Test the jar

gradle info: https://docs.gradle.org/current/userguide/userguide.html

in z390, to access and use the gradle commands, you first have to
make the z390test subdirectory your current or working directory.

The test scripts are in subdirectory z390test\src\test\groovy\org\z390\test

### Quick reference with some gradle commands

| Action                                    | Command                          |
|-------------------------------------------|----------------------------------|
| get a list of gradlew command options     | `gradlew --help`                 |
| force a test run                          | `gradlew test --rerun`           |
| run a specific test/testset               | `gradlew test --tests 'pattern'` |
| run all zCobol tests                      | `gradlew test --tests '*cbl*'`   |
| stop test after failure                   | `gradlew test --fail-fast`       |

## Proposing new functionality

Enhancements are welcome, but be aware that you are stepping into 
an existing and well established project. 

Before you spend time on an enhancement, we __strongly__ suggest
that you first discuss your proposal with the core team and get their 
buy-in before progressing.

The best place to have these discussions is on the 
[z390 developer Google group](https://groups.google.com/g/z390development).

Once you get approval from the group, you can work on your change via the 
standard GitHub pull request model.

## Submitting changes

Changes can be submitted to the project by creating a pull request on the 
[z390 project repository](https://github.com/z390development/z390).

## Creating a new release

The z390 project uses a file-based versioning system with git tags. Version
numbers follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html) principles.

### Version management

To create a new release:

1. Create a release branch from main:
   ```bash
   git checkout -b release/v1.2.3 main
   ```

2. Update the version using the `bash/bumpver` utility:
   ```bash
   # Bump patch version (1.0.0 -> 1.0.1)
   bash/bumpver patch -m "Fix bug in xyz"

   # Bump minor version (1.0.1 -> 1.1.0)
   bash/bumpver minor -m "Add new feature xyz"

   # Bump major version (1.1.0 -> 2.0.0)
   bash/bumpver major -m "Breaking change xyz"
   ```

   The bumpver utility will:
   1. Update the version in version.txt
   2. Create a git commit with your message

3. Push your branch and create a pull request:
   ```bash
   git push -u origin release/v1.2.3
   ```

### Automated Release Process

When a pull request containing changes to version.txt is merged to main, 
the GitHub Actions workflow will automatically:

1. Build the z390 distribution
2. Run all tests 
3. Create a git tag for the version
4. Create a GitHub release using the version from version.txt
5. Attach the distribution zip file to the release

## Useful technical details

The following section is to provide some technical background for new developers.

### Project structure

The following directories are the core directories for the z390 project

Directory     | Description
--------------|------------
src           | Java source for z390 JAR
bat           | Windows bat scripts for running z390 tools
bash          | *nix bash shell scripts for running z390 tools
demo          | Demo assembler programs
tests         | Tests for z/Arch instructions
zopcheck      | Comprehensive instruction check
mac           | Primary maclib folder for z390
doc+doc_overrides | z390 Markdown documentation
.github       | Scripts and config for GitHub build actions


The following directories provide additional features using the z390 toolkit.

Directory     | Description
--------------|------------
zcobol        | zCOBOL support
cics          | zCICS source, tests and demos
structuredmacros | Alternative Structured Programming Macros from Daniel H. Snyder
sort          | Sort utility
zpar          | Generate program execution traces


The following directories provide tests and demos for various features 
available in z390.

Directory     | Description
--------------|------------
assist        | ASSIST instruction support
barcode       | 
bsam          | BSAM sequential file support
guam          | Graphic User Access method support
linklib       | ???
mfacc         | Mainframe assembler coding contest
mvs           | IBM MVS 3.8j sys1.maclib macros
perl          | (Deprecated) Scipts to run Win BAT files on *nix. Use bash 
qsam          | QSAM sequential file support
rt            | Various regression tests for z390
soa           | Service Orientated Architecture (SOA) and TCP/IP support
vsam1         | VSAM file support (version 1)
vsam2         | VSAM file support (version 2)
vse           | VSE OS support

### Compatibility macros

z390 includes a number of macros that are interface compatible with those 
provided by mainframe operating systems. 

The internal operation of the macros is different from a real mainframe.
For example, the SVC instructions used by z390 are not the same as SVC
calls used by z/OS.

This means, if you want to write programs that will work on a real mainframe
then you need to use the supplied macros to perform the OS based actions. 
You cannot write programs that use SVC commands as they are not the 
same.

### Structured macro extensions

z390 extends the standard HLASM syntax with additional keywords that allow you 
to write structured macro code.

See [Structured Macro Extensions](../user_guide/z390/structured_macro.md) for more details.

This makes writing macros easier but structured macros are NOT compatible 
with HLASM.

Structured macro extensions are used extensively in the codebase which means
moving between the mainframe and z390 has some challenges.

The project is looking at how to make this transition easier as it understands
z390 users come here because they want to write and run HLASM programs and 
macros.

### License

By contributing to the z390 project, you agree to assign all copyright
to z390 Assembler LLC. 

This allows the project to operate and change without the consultation 
of all copyright holders. This has not presented itself as an issue to
date but could be an issue in the future.

If you don't agree to this condition, you are free to fork the project
and create your own version under the GNU 2 license conditions.

The following preamble should be applied to all programs

    z390 - Mainframe assembler emulator and run-time engine
    Copyright (C) 2021 z390 Assembler LLC

    This file is part of z390.
    z390 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    z390 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, see <https://www.gnu.org/licenses/>.

#### ASM macro template

```hlasm
         MACRO
.**********************************************************************
.* z390 - Mainframe assembler emulator and run-time engine
.* Copyright (C) 2021 z390 Assembler LLC
.*
.* This file is part of z390.
.*
.* z390 is free software; you can redistribute it and/or modify
.* it under the terms of the GNU General Public License as published by
.* the Free Software Foundation; either version 2 of the License, or
.* (at your option) any later version.
.* z390 is distributed in the hope that it will be useful,
.* but WITHOUT ANY WARRANTY; without even the implied warranty of
.* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
.* GNU General Public License for more details.
.*
.* You should have received a copy of the GNU General Public License 
.* along with this program; if not, see https://www.gnu.org/licenses.
.**********************************************************************
.* dd/mm/yy change details
.*          that go over a line
.* 04/19/08 RPI 833 add SETC quotes for HLASM compatibility
.**********************************************************************
.* Macro documentation goes here
.**********************************************************************

```

#### Quick reference with some git commands

git info: https://git-scm.com
git documentation:https://git-scm.com/doc

Action                                    | Command
------------------------------------------|------------
get list of available commands            | `git help`
get syntax details for a git command      | `git help <command>`
create local clone of git repo            | `git clone <url\> <subdir>`
review status of current branch           | `git status`
get list of all defined branches          | `git branch -v --all`
prepare commit                            | `git add .`
commit a set of changes                   | `git commit -m"descriptive comments"`
push changes to your own fork             | `git push`
remove ignored and added files from repo  | `git clean -d -f -X`
graphical display of branches             | `git log --graph --oneline --decorate --all`
-- > when viewing the branches displayed  | `<Enter> to scroll 1 line, <PgDn> to scroll a page, q to quit`
go 'back in time' to a specific commit    | `git branch -f <new_branch> [<start-point>]`
                                          | `git switch <new_branch>`