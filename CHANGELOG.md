# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.7.8-beta.1] - 2021-11-23
### Added
- First release hosted and built on Github.
- Initial support for zVSAM version 2
- Support for additional z/Architecture instructions
- Added missing zpar and demo files
- Bash native scripts for use on unix based OS's
- Initial markdown documentation
- Automated build process (Windows/Linux)

### Changed
- z390 commands can now be run from any directory
- z390 command scripts no longer pause to allow automation
- Project structure standards and reorganization

### Deprecated
- Perl scripts to support z390 on Unix based OS - use native Bash script

### Removed
### Fixed
- #230 - correct vector instruction errors reported by Dan Greiner
- #321 - Fix/update RT tests for qsam and bsam support for Large Block Interface
- #245 - Fix CICS build jobs

