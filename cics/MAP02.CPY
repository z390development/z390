BMS2S    DS    0H                 START OF MAP DEFINITION
BMS2I    DS    0H                 START OF MAP INPUT DEFINITION
BMS2O    DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
FWRAPI   DS    CL83               INPUT FIELD
         ORG   FWRAPI
         DS    CL83               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
SWRAPI   DS    CL8                INPUT FIELD
         ORG   SWRAPI
         DS    CL8                OUTPUT FIELD
*
BMS2E    EQU   *
BMS2L    EQU   BMS2E-BMS2S
*
* * * END OF MAP DEFINITION     * * *
*
* * * END OF MAP SET DEFINITION * * *
