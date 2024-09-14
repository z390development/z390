.* COPYRIGHT (C) 1988, PAUL A. SCOTT, ALL RIGHTS RESERVED               00010000
.*                                                                      00020000
         GBLA  &SSID               STACK SUBPOOL ID                     00030000
         GBLA  &SP                 STACK POINTER                        00040000
         GBLA  &BP                 STACK BASE POINTER                   00050000
         GBLA  &HIREG              HIGHEST REGISTER AVAILABLE           00060000
.*                                                                      00070000
         GBLB  &PSMSVA             SAVE AREA DEFINED FLAG               00080000
         GBLC  &PSMSIG             UNIQUE SIGNATURE                     00090000
.*                                                                      00100000
&SSID    SETA  127                 INITIALIZE STACK SUBPOOL ID          00110000
&SP      SETA  3                   INITIALIZE STACK POINTER             00120000
&BP      SETA  13                  INITIALIZE STACK BASE POINTER        00130000
&HIREG   SETA  15                  INITIALIZE HI-REG VALUE              00140000
.*                                                                      00150000
&PSMSIG  SETC  'CAFE'                                                   00160000
