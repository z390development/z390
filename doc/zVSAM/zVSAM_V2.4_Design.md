# zVSAM V2 Design and Logic manual

## Introduction

This document describes the structure of the zVSAM component of the z390 assembler and emulator.
It consists of the following parts:

- A description of the structure of the interfaces used
- A description of the structure of the files
- A description of the logical processes that implement ACB-based requests
- A description of the logical processes that implement RPL-based requests
- Addenda

## Copyright Notice

This document Copyright 2018-2026 – z390 development team

z390 is free software; its associated documentation is equally free.
You can redistribute and/or modify both software and documentation under the terms of
the GNU General Public License as published by the Free Software Foundation;
either version 2 of the License, or (at your option) any later version.

z390 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with z390;
if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

## Acknowledgements

z390's purpose is to provide a source-level compatible assembler, linker and run-time execution engine
for IBM's High-Level Assembler environment. By extension, the zVSAM component described in this document
also aims at providing source-level compatibility. As a consequence, all source-level interfaces necessarily
mimic the IBM-provided (and IBM-copyrighted) interfaces as described by IBM in publicly accessible documents.

The logic and implementation behind these interfaces, however, was developed independently from IBM
and is the product of the joint efforts of our team of volunteer developers.

Source-level compatibility is a primary goal not only for z390 and zVSAM, but also for other z390 components
such as zCOBOL and zCICS.

All IBM publications and software we refer to in this document are copyright IBM Corporation with no exception.

The drawings in this document have been made using the draw.io software.
As part of the open source for z390 the xml and jpg documents describing these drawings are available
with every distribution of z390 that contains this document

## Terminology

The reader is assumed to have at least some familiarity with IBM VSAM,
to the extent that most of the following acronyms and terms are understood:

| Acronym | Meaning                                                             |
|---------|---------------------------------------------------------------------|
| ACB     | Access Control Block                                                |
| AIX     | Alternate IndeX                                                     |
| CBMR    | Control Block Modification Request (zVSAM)                          |
| CI      | Control Interval                                                    |
| ELIX    | Extended Level IndeX – extra index level for non-unique AIX (zVSAM) |
| ESDS    | Entry Sequenced Data Set                                            |
| IBM     | International Business Machines Corp., USA                          |
| KSDS    | Key Sequenced Data Set                                              |
| Path    | Access to a base cluster, usually through an AIX                    |
| RBA     | Relative Byte Address (See note here)                               |
| RDW     | Record Descriptor Word in IBM-defined format                        |
| RLF     | Record Length Field – (zVSAM)                                       |
| RPL     | Request Parameter List                                              |
| RRDS    | Relative Record Data Set                                            |
| RRN     | Relative Record Number                                              |
| SPX     | Segment Prefix (zVSAM)                                              |
| VSAM    | Virtual Storage Access Method                                       |
| XRBA    | Extended Relative Byte Address (See note here)                      |
| XLRA    | Extended Logical Record Address (zVSAM)                             |
| zACB    | zVSAM equivalent of the ACB                                         |
| zEXLST  | zVSAM equivalent of the EXLST                                       |
| zRPL    | zVSAM equivalent of the RPL                                         |

*Note:*
The XRBA is held in block-relative form, ie. the first record in each block is calculated as
BHDRSELF/256\*PFXBLKSZ
Each subsequent record has its length added to this value according to the RPTR sequence and not the
physical sequence (which is reversed). For variable records the length includes the RLF but not any SPX
In this document we also use the following terms. The ones that are used by IBM as well, are intended to
have the same meaning they do in IBM manuals

| Term      | Meaning                                                                 |
|-----------|-------------------------------------------------------------------------|
| Block     | zVSAM equivalent of a Control Interval                                  |
| Cluster   | a set of files that logically belong together                           |
| Component | either a data component or an index component of a cluster              |
| Element   | a primary key or XRBA in an AIX record                                  |
| File      | a single file as seen by the hosting operating system                   |
| Foxes     | a value consisting of all high-values i.e. a value of all X'FF' bytes.  |
| List      | a structure holding items that are linked together by s                 |
| Segment   | a portion of a record in a spanned dataset                              |
| Segmented | a record that has been split into segments in a spanned dataset         |
| Spanned   | an attribute of a dataset that allows records to be split into segments |
| Sphere    | a cluster and all associated AIXs                                       |
| Table     | a structure holding items that are physically adjacent                  |

## Compatibility

As this document relates to zVSAM V2, there are two type of compatibility we need to consider.
On the one hand we have designed zVSAM to be compatible with IBM VSAM.
And on the other hand we need to consider compatibility with
z390's zVSAM V1 – the prior implementation of zVSAM in the z390 environment.

### zVSAM compatibility with IBM VSAM

Our z390 implementation of zVSAM V2 is intended to be source-level compatible with IBM VSAM.
This has the following consequences:

1. IBM VSAM documentation with regards to macros and interfaces applies to zVSAM
   with the exception of parameters and options not supported by zVSAM.
   Where zVSAM differs in behaviour this is noted in this document.
   Please refer to the macro descriptions for details.
2. Control Blocks (such as ACB, RPL and some others) are not compatible.
   zVSAM has its own structures. A side effect of this may be that a program's
   assembled object code may be different in size than on your IBM operating system.
   On rare occasions you may need an additional base register when porting your program either way.
3. As a rule of thumb, a program using VSAM can be ported to z390 and should be able to assemble,
   link and run without modification – provided it uses only the VSAM features and options that zVSAM supports.
   And provided the program does not run out of addressability due to different control block lengths

### zVSAM V2 compatibility with zVSAM V1

The user of z390's zVSAM component should be aware that zVSAM V2 as described in this document
is not compatible with the pre-existing zVSAM V1. We – the development team – apologize for the
inconvenience this may cause.

We have taken the following measures to facilitate the transition from zVSAM V1 to zVSAM V2:

1. We have introduced a new z390 option: ZVSAM which indicates which version of
   zVSAM you want z390 to use. It takes the following forms:
   For maximum compatibility the default is set to ZVSAM(1).
   The default will be changed to ZVSAM(2) in a future release of z390
   1. ZVSAM(0) – zVSAM usage is disallowed
   2. ZVSAM(1) – zVSAM V1 is enabled, zVSAM V2 is disabled
   3. ZVSAM(2) – zVSAM V2 is enabled, zVSAM V1 is disabled
2. To convert your zVSAM V1 clusters to zVSAM V2 you'll have to take the following steps:
   1. unload the existing data from their clusters using REPRO \
      For details on how to use REPRO, please refer to the "z390_VSAM_User_Guide"
   2. reload your data from your unload files, using ZREPRO \
      For details on how to use zREPRO, please refer to the "z390_zVSAM_zREPRO_User_Guide"
3. For zVSAM V1 and zVSAM V2 there are distinct macro libraries, MACVSAM1 and MACVSAM2.
   To use the correct zVSAM maclib, specify the correct version in your maclib concatenation
4. If a program is to run with ZVSAM(2), then all submodules that contain an OPEN macro must be
   re-assembled using MACVSAM2, even those that only use QSAM.
