# zVSAM V2 Design and Logic manual

## Introduction

This document describes the structure of the zVSAM component of the z390 assembler and runtime engine.
It consists of the following parts:

1. This introduction.
2. [A description of the structure of the interfaces used](zVSAM_V2_Design_Interfaces.md)
3. [A description of the structure of the files](zVSAM_V2_Design_File_Structure.md)
4. [A description of the logical processes that implement ACB-based requests](zVSAM_V2_Design_ACB_Processes.md)
5. [A description of the logical processes that implement RPL-based requests](zVSAM_V2_Design_RPL_Processes.md)
6. [Addenda](zVSAM_V2_Design_Addenda.md)

This part of the document (the introductory part) contains the following main chapters:

1. This introduction
2. [Copyright Notice]#(Copyright-Notice)
3. [Acknowledgements](#Acknowledgements)
4. [Terminology](#Terminology)
5. [Compatibility](#Compatibility)

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
if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.

## Acknowledgements

z390's purpose is to provide a source-level compatible assembler, linker and run-time execution engine
for IBM's High-Level Assembler environment. By extension, the zVSAM component described in this document
also aims at providing source-level compatibility. As a consequence, all source-level interfaces necessarily
mimic the IBM-provided (and IBM-copyrighted) interfaces as described by IBM in publicly accessible documents.

The logic and implementation behind these interfaces, however, was developed independently from IBM
and is the product of the joint efforts of our team of volunteer developers.

Source-level compatibility is a primary goal not only for z390 and zVSAM, but also for other z390 components
such as zCobol and zCICS.

All IBM publications and software we refer to in this document are copyright IBM Corporation with no exception.

The drawings in this document have been made using the site draw.io.
As part of the open source for z390 the xml documents describing these drawings are available
with every distribution of z390 that contains this document.

## Terminology

The reader is assumed to have at least some familiarity with IBM VSAM,
to the extent that the following acronyms and terms are understood:

| Acronym | Meaning                                             |
|---------|-----------------------------------------------------|
| ACB     | Access Control Block                                |
| AIX     | Alternate IndeX                                     |
| CBMR    | Control Block Modification Request                  |
| CI      | Control Interval                                    |
| ESDS    | Entry Sequenced Data Set                            |
| EXLST   | Exit List                                           |
| IBM     | International Business Machines Corp., USA          |
| KSDS    | Key Sequenced Data Set                              |
| LDS     | Linear Data Set                                     |
| Path    | Access to a base cluster, usually through an AIX    |
| RBA     | Relative Byte Address                               |
| RDW     | Record Descriptor Word in IBM-defined format        |
| RLF     | Record Length Field – z390 equivalent of RDW        |
| RPL     | Request Parameter List                              |
| RRDS    | Relative Record Data Set                            |
| RRN     | Relative Record Number                              |
| SPX     | Segment Prefix                                      |
| VSAM    | Virtual Storage Access Method                       |
| XRBA    | Extended Relative Byte Address                      |
| XLRA    | Extended Logical Record Address                     |
| zACB    | z390 equivalent of the ACB                          |
| zEXLST  | z390 equivalent of the EXLST                        |
| zRPL    | z390 equivalent of the RPL                          |

In this document we also use the following terms.
The ones that are used by IBM as well, are intended to have the same meaning they do in IBM manuals.

| Term      | Meaning                                                                   |
|-----------|---------------------------------------------------------------------------|
| Area      | a section of storage with a defined layout, depending on the type of Area |
| Block     | zVSAM equivalent of a Control Interval - the unit of I/O operations       |
| Cluster   | a set of files that logically belong together                             |
| Component | either a data component or an index component of a cluster                |
| File      | a single file as seen by the hosting operating system                     |
| Foxes     | a value consisting of all high-values i.e. a value of all X'FF' bytes     |
| List      | a structure holding items that are linked together by pointers            |
| Segment   | a portion of a record in a spanned dataset                                |
| Segmented | a record that has been split into segments in a spanned dataset           |
| Spanned   | an attribute of a dataset that allows records to be split into segments   |
| Sphere    | a cluster and all associated AIXs                                         |
| Table     | a structure holding items that are physically adjacent                    |

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
   Please refer to the macro descriptions in section II for details.
2. Control Blocks (such as ACB, RPL and some others) are not compatible.
   zVSAM has its own structures. A side effect of this may be that a program's
   assembled object code may be different in size than on your IBM operating system.
   On rare occasions you may need an additional base register when porting your program either way.
3. As a rule of thumb, a program using VSAM can be ported to z390 and should be able to assemble,
   link and run without modification – provided it uses only the VSAM features and options that zVSAM supports.
   And provided the program does not run out of addressability due to different control block lengths.

### zVSAM V2 compatibility with zVSAM V1

The user of z390's zVSAM component should be aware that zVSAM V2 as described in this document
is not compatible with the pre-existing zVSAM V1. We – the development team – apologize for the
inconvenience this may cause.

We have taken the following measures to facilitate the transition from zVSAM V1 to zVSAM V2:

1. We have introduced a new z390 option: ZVSAM which indicates which version of
   zVSAM you want z390 to use. For maximum compatibility the default is set to enable zVSAM v1.
   The default will be changed to zVSAM v2 in a future release of z390.
   The parameter takes the following forms:
    1. ZVSAM(0) – zVSAM usage is disallowed
    2. ZVSAM(1) – zVSAM V1 is enabled, zVSAM V2 is disabled
    3. ZVSAM(2) – zVSAM V2 is enabled, zVSAM V1 is disabled
2. To convert your zVSAM V1 clusters to zVSAM V2 you'll have to take the following steps:
    1. unload the existing data from their clusters using REPRO. \
       For details on how to use Repro, please refer to the "z390_VSAM_User_Guide"
    2. reload your data from your unload files, using ZREPRO. \
       For details on how to use zREPRO, please refer to the "z390_zVSAM_zREPRO_User_Guide"
3. For zVSAM V1 and zVSAM v2 there are distinct macro libraries.
   To use the correct zVSAM maclib, specify the correct version in your maclib concatenation.
