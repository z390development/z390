# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v1.8.0]

### Added
- First official release hosted and built on GitHub.
- Updates to copyright and license
- Support for additional z/Architecture instructions
- Bash native scripts for use on unix based OS's (MacOS, Linux)
- Initial markdown documentation - see https://z390development.github.io/z390
- Automated build and packaging process (Windows/Linux)
- Add Large Block Interface (LBI) support for BSAM read/write access method
- Initial support for zVSAM version 2
- zCICS implementation now works under MacOS/Linux

### Changed
- z390 command scripts run from any directory
- z390 command scripts no longer pause to allow automation
- Project structure standards and reorganization

### Deprecated
- Perl scripts to support z390 on Unix based OS - use native Bash script

### Removed
- The z390 distribution no longer contains Java source. If you
want to rebuild z390, then you should clone the GitHub repository
and issue the build script.
### Fixed

* #343 Increase maxlines to 400000
* #335 Correct ACALL to support parms using created vars () (dsh33782)
* #291, #305 Fix ASSIST instruction compatibility (John Ganci)
* #321 QSAM get recfm=vt not setting LLLL correctly for LBI large block
* #318 Do not replace self defining terms with numeric values
* #337 remove perl ref in sz390.java and add CMDPROC test programs (John Ganci)
* #245 update cics directories and bat files plus new bat\readme.txt (dsh33782)
* #321 fix and update rt tests for qsam and bsam support for LBI large block interface (dsh33782)
* #230 correct vector instruction errors reported by Dan Greiner (dsh33782)
* Various fixes to get zCICS working with current release