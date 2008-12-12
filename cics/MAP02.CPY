BMS2S    DS    0H                 START OF MAP DEFINITION
BMS2I    DS    0H                 START OF MAP INPUT DEFINITION
BMS2O    DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
FWRAPL   DS    AL2                INPUT FIELD LENGTH
FWRAPF   DS    0X                 INPUT FIELD FLAG
FWRAPA   DS    X                  OUTPUT ATTRIBUTE
FWRAPI   DS    CL83               INPUT FIELD
         ORG   FWRAPI
FWRAPO   DS    CL83               OUTPUT FIELD
*
SWRAPL   DS    AL2                INPUT FIELD LENGTH
SWRAPF   DS    0X                 INPUT FIELD FLAG
SWRAPA   DS    X                  OUTPUT ATTRIBUTE
SWRAPI   DS    CL8                INPUT FIELD
         ORG   SWRAPI
SWRAPO   DS    CL8                OUTPUT FIELD
*
BMS2E    EQU   *
BMS2L    EQU   BMS2E-BMS2S
*
* * * END OF MAP DEFINITION     * * *
*
* * * END OF MAP SET DEFINITION * * *
