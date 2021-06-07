GUI4S    DS    0H                 START OF MAP DEFINITION
GUI4I    DS    0H                 START OF MAP INPUT DEFINITION
GUI4O    DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
INPUTI   DS    CL3                INPUT FIELD
         ORG   INPUTI
         DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
REPLYI   DS    CL3                INPUT FIELD
         ORG   REPLYI
REPLYO   DS    CL3                OUTPUT FIELD
*
GUI4E    EQU   *
GUI4L    EQU   GUI4E-GUI4S
*
* * * END OF MAP DEFINITION     * * *
*
GUI6S    DS    0H                 START OF MAP DEFINITION
GUI6I    DS    0H                 START OF MAP INPUT DEFINITION
GUI6O    DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
NAMEL    DS    AL2                INPUT FIELD LENGTH
NAMEF    DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
NAMEI    DS    CL40               INPUT FIELD
         ORG   NAMEI
         DS    CL40               OUTPUT FIELD
*
ADDRL    DS    AL2                INPUT FIELD LENGTH
ADDRF    DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
ADDRI    DS    CL40               INPUT FIELD
         ORG   ADDRI
         DS    CL40               OUTPUT FIELD
*
ZIPL     DS    AL2                INPUT FIELD LENGTH
ZIPF     DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
ZIPI     DS    CL5                INPUT FIELD
         ORG   ZIPI
         DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
NAMEHDI  DS    CL4                INPUT FIELD
         ORG   NAMEHDI
NAMEHDO  DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
NEWNAMEI DS    CL40               INPUT FIELD
         ORG   NEWNAMEI
NEWNAMEO DS    CL40               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
ADDRHDI  DS    CL4                INPUT FIELD
         ORG   ADDRHDI
ADDRHDO  DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
NEWADDRI DS    CL40               INPUT FIELD
         ORG   NEWADDRI
NEWADDRO DS    CL40               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
ZIPHDI   DS    CL4                INPUT FIELD
         ORG   ZIPHDI
ZIPHDO   DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
NEWZIPI  DS    CL5                INPUT FIELD
         ORG   NEWZIPI
NEWZIPO  DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
ERRMSGI  DS    CL34               INPUT FIELD
         ORG   ERRMSGI
ERRMSGO  DS    CL34               OUTPUT FIELD
*
TESTOCCD DS    0C                 I/O FIELD FIRST OCCURRING
         DS    AL2                INPUT FIELD LENGTH
TESTOCCF DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
TESTOCCI DS    CL5                INPUT FIELD
         ORG   TESTOCCI
TESTOCCO DS    CL5                OUTPUT FIELD
TESTOCCN DS    0C                 I/O FIELD NEXT OCCURRING
         ORG   TESTOCCD+5*(TESTOCCN-TESTOCCD)
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
OCCSUMI  DS    CL10               INPUT FIELD
         ORG   OCCSUMI
OCCSUMO  DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT GROUP FIELD LENGTH
DATEF    DS    0X                 INPUT GROUP FIELD FLAG
         DS    X                  OUTPUT GROUP FIELD ATTRIBUTE
DATEI    DS    0C                 INPUT GROUP FIELD ORIGIN
DATEO    DS    0C                 OUTPUT GROUP FIELD ORIGIN
MOI      DS    CL2                INPUT GROUP FIELD
         ORG   MOI
MOO      DS    CL2                OUTPUT GROUP FIELD
SEP1I    DS    CL1                INPUT GROUP FIELD
         ORG   SEP1I
         DS    CL1                OUTPUT GROUP FIELD
DAYI     DS    CL2                INPUT GROUP FIELD
         ORG   DAYI
DAYO     DS    CL2                OUTPUT GROUP FIELD
SEP2I    DS    CL1                INPUT GROUP FIELD
         ORG   SEP2I
         DS    CL1                OUTPUT GROUP FIELD
YRI      DS    CL2                INPUT GROUP FIELD
         ORG   YRI
YRO      DS    CL2                OUTPUT GROUP FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
SHOWMDYI DS    CL18               INPUT FIELD
         ORG   SHOWMDYI
SHOWMDYO DS    CL18               OUTPUT FIELD
*
TESTPCOL DS    AL2                INPUT FIELD LENGTH
TESTPCOF DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
TESTPCOI DS    CL12               INPUT FIELD
         ORG   TESTPCOI
TESTPCOO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
PAYMEI   DS    CL14               INPUT FIELD
         ORG   PAYMEI
PAYMEO   DS    CL14               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
PAYME1I  DS    CL12               INPUT FIELD
         ORG   PAYME1I
PAYME1O  DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
CURSI    DS    CL9                INPUT FIELD
         ORG   CURSI
CURSO    DS    CL9                OUTPUT FIELD
*
GUI6E    EQU   *
GUI6L    EQU   GUI6E-GUI6S
*
* * * END OF MAP DEFINITION     * * *
*
GUI6HS   DS    0H                 START OF MAP DEFINITION
GUI6HO   DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
GUI6HE   EQU   *
GUI6HL   EQU   GUI6HE-GUI6HS
*
* * * END OF MAP DEFINITION     * * *
*
* * * END OF MAP SET DEFINITION * * *
