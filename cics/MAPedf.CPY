EDF01S   DS    0H                 START OF MAP DEFINITION
EDF01I   DS    0H                 START OF MAP INPUT DEFINITION
EDF01O   DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01TRANI DS    CL4                INPUT FIELD
         ORG   E01TRANI
E01TRANO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01PROGI DS    CL8                INPUT FIELD
         ORG   E01PROGI
E01PROGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01TSKNI DS    CL7                INPUT FIELD
         ORG   E01TSKNI
E01TSKNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01MODEI DS    CL6                INPUT FIELD
         ORG   E01MODEI
E01MODEO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01SCRNI DS    CL3                INPUT FIELD
         ORG   E01SCRNI
E01SCRNO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBTMI DS    CL7                INPUT FIELD
         ORG   E01EBTMI
E01EBTMO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBDTI DS    CL7                INPUT FIELD
         ORG   E01EBDTI
E01EBDTO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBTDI DS    CL4                INPUT FIELD
         ORG   E01EBTDI
E01EBTDO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBCSI DS    CL7                INPUT FIELD
         ORG   E01EBCSI
E01EBCSO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBCLI DS    CL5                INPUT FIELD
         ORG   E01EBCLI
E01EBCLO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBADI DS    CL5                INPUT FIELD
         ORG   E01EBADI
E01EBADO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBAXI DS    CL5                INPUT FIELD
         ORG   E01EBAXI
E01EBAXO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBFNI DS    CL7                INPUT FIELD
         ORG   E01EBFNI
E01EBFNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBFXI DS    CL10               INPUT FIELD
         ORG   E01EBFXI
E01EBFXO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBRCI DS    CL15               INPUT FIELD
         ORG   E01EBRCI
E01EBRCO DS    CL15               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBRQI DS    CL19               INPUT FIELD
         ORG   E01EBRQI
E01EBRQO DS    CL19               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBDSI DS    CL8                INPUT FIELD
         ORG   E01EBDSI
E01EBDSO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBRSI DS    CL19               INPUT FIELD
         ORG   E01EBRSI
E01EBRSO DS    CL19               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBRPI DS    CL12               INPUT FIELD
         ORG   E01EBRPI
E01EBRPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBR2I DS    CL12               INPUT FIELD
         ORG   E01EBR2I
E01EBR2O DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBPGI DS    CL8                INPUT FIELD
         ORG   E01EBPGI
E01EBPGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E01EBDFI DS    CL1                INPUT FIELD
         ORG   E01EBDFI
E01EBDFO DS    CL1                OUTPUT FIELD
*
EDF01E   EQU   *
EDF01L   EQU   EDF01E-EDF01S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0202S DS    0H                 START OF MAP DEFINITION
EDF0202I DS    0H                 START OF MAP INPUT DEFINITION
EDF0202O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202TSI DS    CL4                INPUT FIELD
         ORG   E0202TSI
E0202TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202PGI DS    CL8                INPUT FIELD
         ORG   E0202PGI
E0202PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202TNI DS    CL7                INPUT FIELD
         ORG   E0202TNI
E0202TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202MDI DS    CL6                INPUT FIELD
         ORG   E0202MDI
E0202MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202SCI DS    CL3                INPUT FIELD
         ORG   E0202SCI
E0202SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202STI DS    CL26               INPUT FIELD
         ORG   E0202STI
E0202STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202CHI DS    CL39               INPUT FIELD
         ORG   E0202CHI
E0202CHO DS    CL39               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202CMI DS    CL69               INPUT FIELD
         ORG   E0202CMI
E0202CMO DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202WHI DS    CL39               INPUT FIELD
         ORG   E0202WHI
E0202WHO DS    CL39               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202WMI DS    CL69               INPUT FIELD
         ORG   E0202WMI
E0202WMO DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202EHI DS    CL39               INPUT FIELD
         ORG   E0202EHI
E0202EHO DS    CL39               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202EMI DS    CL69               INPUT FIELD
         ORG   E0202EMI
E0202EMO DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202PRI DS    CL8                INPUT FIELD
         ORG   E0202PRI
E0202PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202OFI DS    CL9                INPUT FIELD
         ORG   E0202OFI
E0202OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E02021RA DS    X                  OUTPUT ATTRIBUTE
E02021RI DS    CL9                INPUT FIELD
         ORG   E02021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202RPI DS    CL12               INPUT FIELD
         ORG   E0202RPI
E0202RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E02022RA DS    X                  OUTPUT ATTRIBUTE
E02022RI DS    CL9                INPUT FIELD
         ORG   E02022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202R2I DS    CL3                INPUT FIELD
         ORG   E0202R2I
E0202R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0202RCI DS    CL25               INPUT FIELD
         ORG   E0202RCI
E0202RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0202P2A DS    X                  OUTPUT ATTRIBUTE
E0202P2I DS    CL21               INPUT FIELD
         ORG   E0202P2I
         DS    CL21               OUTPUT FIELD
*
EDF0202E EQU   *
EDF0202L EQU   EDF0202E-EDF0202S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0204S DS    0H                 START OF MAP DEFINITION
EDF0204I DS    0H                 START OF MAP INPUT DEFINITION
EDF0204O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204TSI DS    CL4                INPUT FIELD
         ORG   E0204TSI
E0204TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204PGI DS    CL8                INPUT FIELD
         ORG   E0204PGI
E0204PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204TNI DS    CL7                INPUT FIELD
         ORG   E0204TNI
E0204TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204SCI DS    CL3                INPUT FIELD
         ORG   E0204SCI
E0204SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204STI DS    CL26               INPUT FIELD
         ORG   E0204STI
E0204STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C1I DS    CL12               INPUT FIELD
         ORG   E0204C1I
E0204C1O DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X1I DS    CL25               INPUT FIELD
         ORG   E0204X1I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C2I DS    CL12               INPUT FIELD
         ORG   E0204C2I
E0204C2O DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X2I DS    CL25               INPUT FIELD
         ORG   E0204X2I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C3I DS    CL12               INPUT FIELD
         ORG   E0204C3I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X3I DS    CL25               INPUT FIELD
         ORG   E0204X3I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C4I DS    CL12               INPUT FIELD
         ORG   E0204C4I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X4I DS    CL25               INPUT FIELD
         ORG   E0204X4I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C5I DS    CL12               INPUT FIELD
         ORG   E0204C5I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X5I DS    CL25               INPUT FIELD
         ORG   E0204X5I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C6I DS    CL12               INPUT FIELD
         ORG   E0204C6I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X6I DS    CL25               INPUT FIELD
         ORG   E0204X6I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C7I DS    CL12               INPUT FIELD
         ORG   E0204C7I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X7I DS    CL25               INPUT FIELD
         ORG   E0204X7I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C8I DS    CL12               INPUT FIELD
         ORG   E0204C8I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X8I DS    CL25               INPUT FIELD
         ORG   E0204X8I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204C9I DS    CL12               INPUT FIELD
         ORG   E0204C9I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204X9I DS    CL25               INPUT FIELD
         ORG   E0204X9I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204CAI DS    CL12               INPUT FIELD
         ORG   E0204CAI
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204XAI DS    CL25               INPUT FIELD
         ORG   E0204XAI
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204CBI DS    CL12               INPUT FIELD
         ORG   E0204CBI
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204XBI DS    CL25               INPUT FIELD
         ORG   E0204XBI
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204CCI DS    CL12               INPUT FIELD
         ORG   E0204CCI
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204XCI DS    CL25               INPUT FIELD
         ORG   E0204XCI
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204PRI DS    CL8                INPUT FIELD
         ORG   E0204PRI
E0204PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204OFI DS    CL9                INPUT FIELD
         ORG   E0204OFI
E0204OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E02041RA DS    X                  OUTPUT ATTRIBUTE
E02041RI DS    CL9                INPUT FIELD
         ORG   E02041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204RPI DS    CL12               INPUT FIELD
         ORG   E0204RPI
E0204RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E02042RA DS    X                  OUTPUT ATTRIBUTE
E02042RI DS    CL9                INPUT FIELD
         ORG   E02042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204R2I DS    CL3                INPUT FIELD
         ORG   E0204R2I
E0204R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0204RCI DS    CL25               INPUT FIELD
         ORG   E0204RCI
E0204RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0204P7A DS    X                  OUTPUT ATTRIBUTE
E0204P7I DS    CL21               INPUT FIELD
         ORG   E0204P7I
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0204P8A DS    X                  OUTPUT ATTRIBUTE
E0204P8I DS    CL24               INPUT FIELD
         ORG   E0204P8I
         DS    CL24               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0204PAA DS    X                  OUTPUT ATTRIBUTE
E0204PAI DS    CL21               INPUT FIELD
         ORG   E0204PAI
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0204PBA DS    X                  OUTPUT ATTRIBUTE
E0204PBI DS    CL24               INPUT FIELD
         ORG   E0204PBI
         DS    CL24               OUTPUT FIELD
*
EDF0204E EQU   *
EDF0204L EQU   EDF0204E-EDF0204S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0206S DS    0H                 START OF MAP DEFINITION
EDF0206I DS    0H                 START OF MAP INPUT DEFINITION
EDF0206O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206TSI DS    CL4                INPUT FIELD
         ORG   E0206TSI
E0206TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206PGI DS    CL8                INPUT FIELD
         ORG   E0206PGI
E0206PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206TNI DS    CL7                INPUT FIELD
         ORG   E0206TNI
E0206TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206SCI DS    CL3                INPUT FIELD
         ORG   E0206SCI
E0206SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206STI DS    CL26               INPUT FIELD
         ORG   E0206STI
E0206STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C1I DS    CL6                INPUT FIELD
         ORG   E0206C1I
E0206C1O DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X1I DS    CL25               INPUT FIELD
         ORG   E0206X1I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C2I DS    CL6                INPUT FIELD
         ORG   E0206C2I
E0206C2O DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X2I DS    CL25               INPUT FIELD
         ORG   E0206X2I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C3I DS    CL6                INPUT FIELD
         ORG   E0206C3I
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X3I DS    CL25               INPUT FIELD
         ORG   E0206X3I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C4I DS    CL6                INPUT FIELD
         ORG   E0206C4I
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X4I DS    CL25               INPUT FIELD
         ORG   E0206X4I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C5I DS    CL6                INPUT FIELD
         ORG   E0206C5I
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X5I DS    CL25               INPUT FIELD
         ORG   E0206X5I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C6I DS    CL6                INPUT FIELD
         ORG   E0206C6I
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X6I DS    CL25               INPUT FIELD
         ORG   E0206X6I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C7I DS    CL6                INPUT FIELD
         ORG   E0206C7I
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X7I DS    CL25               INPUT FIELD
         ORG   E0206X7I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C8I DS    CL6                INPUT FIELD
         ORG   E0206C8I
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X8I DS    CL25               INPUT FIELD
         ORG   E0206X8I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206C9I DS    CL6                INPUT FIELD
         ORG   E0206C9I
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206X9I DS    CL25               INPUT FIELD
         ORG   E0206X9I
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206CAI DS    CL6                INPUT FIELD
         ORG   E0206CAI
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206XAI DS    CL25               INPUT FIELD
         ORG   E0206XAI
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206CBI DS    CL6                INPUT FIELD
         ORG   E0206CBI
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206XBI DS    CL25               INPUT FIELD
         ORG   E0206XBI
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206CCI DS    CL6                INPUT FIELD
         ORG   E0206CCI
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206XCI DS    CL25               INPUT FIELD
         ORG   E0206XCI
         DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206PRI DS    CL8                INPUT FIELD
         ORG   E0206PRI
E0206PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206OFI DS    CL9                INPUT FIELD
         ORG   E0206OFI
E0206OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E02061RA DS    X                  OUTPUT ATTRIBUTE
E02061RI DS    CL9                INPUT FIELD
         ORG   E02061RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206RPI DS    CL12               INPUT FIELD
         ORG   E0206RPI
E0206RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E02062RA DS    X                  OUTPUT ATTRIBUTE
E02062RI DS    CL9                INPUT FIELD
         ORG   E02062RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206R2I DS    CL3                INPUT FIELD
         ORG   E0206R2I
E0206R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0206RCI DS    CL25               INPUT FIELD
         ORG   E0206RCI
E0206RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0206P7A DS    X                  OUTPUT ATTRIBUTE
E0206P7I DS    CL21               INPUT FIELD
         ORG   E0206P7I
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0206P8A DS    X                  OUTPUT ATTRIBUTE
E0206P8I DS    CL24               INPUT FIELD
         ORG   E0206P8I
         DS    CL24               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0206PAA DS    X                  OUTPUT ATTRIBUTE
E0206PAI DS    CL21               INPUT FIELD
         ORG   E0206PAI
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0206PBA DS    X                  OUTPUT ATTRIBUTE
E0206PBI DS    CL24               INPUT FIELD
         ORG   E0206PBI
         DS    CL24               OUTPUT FIELD
*
EDF0206E EQU   *
EDF0206L EQU   EDF0206E-EDF0206S
*
* * * END OF MAP DEFINITION     * * *
*
EDF020AS DS    0H                 START OF MAP DEFINITION
EDF020AI DS    0H                 START OF MAP INPUT DEFINITION
EDF020AO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ATSI DS    CL4                INPUT FIELD
         ORG   E020ATSI
E020ATSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020APGI DS    CL8                INPUT FIELD
         ORG   E020APGI
E020APGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ATNI DS    CL7                INPUT FIELD
         ORG   E020ATNI
E020ATNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ASCI DS    CL3                INPUT FIELD
         ORG   E020ASCI
E020ASCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ASTI DS    CL26               INPUT FIELD
         ORG   E020ASTI
E020ASTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC1I DS    CL12               INPUT FIELD
         ORG   E020AC1I
E020AC1O DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC2I DS    CL12               INPUT FIELD
         ORG   E020AC2I
E020AC2O DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC3I DS    CL12               INPUT FIELD
         ORG   E020AC3I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC4I DS    CL12               INPUT FIELD
         ORG   E020AC4I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC5I DS    CL12               INPUT FIELD
         ORG   E020AC5I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC6I DS    CL12               INPUT FIELD
         ORG   E020AC6I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC7I DS    CL12               INPUT FIELD
         ORG   E020AC7I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC8I DS    CL12               INPUT FIELD
         ORG   E020AC8I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AC9I DS    CL12               INPUT FIELD
         ORG   E020AC9I
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ACAI DS    CL12               INPUT FIELD
         ORG   E020ACAI
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ACBI DS    CL12               INPUT FIELD
         ORG   E020ACBI
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ACCI DS    CL12               INPUT FIELD
         ORG   E020ACCI
         DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020APRI DS    CL8                INPUT FIELD
         ORG   E020APRI
E020APRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AOFI DS    CL9                INPUT FIELD
         ORG   E020AOFI
E020AOFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020A1RA DS    X                  OUTPUT ATTRIBUTE
E020A1RI DS    CL9                INPUT FIELD
         ORG   E020A1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ARPI DS    CL12               INPUT FIELD
         ORG   E020ARPI
E020ARPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020A2RA DS    X                  OUTPUT ATTRIBUTE
E020A2RI DS    CL9                INPUT FIELD
         ORG   E020A2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020AR2I DS    CL3                INPUT FIELD
         ORG   E020AR2I
E020AR2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ARCI DS    CL25               INPUT FIELD
         ORG   E020ARCI
E020ARCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020AP7A DS    X                  OUTPUT ATTRIBUTE
E020AP7I DS    CL21               INPUT FIELD
         ORG   E020AP7I
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020AP8A DS    X                  OUTPUT ATTRIBUTE
E020AP8I DS    CL24               INPUT FIELD
         ORG   E020AP8I
         DS    CL24               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020APAA DS    X                  OUTPUT ATTRIBUTE
E020APAI DS    CL21               INPUT FIELD
         ORG   E020APAI
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020APBA DS    X                  OUTPUT ATTRIBUTE
E020APBI DS    CL24               INPUT FIELD
         ORG   E020APBI
         DS    CL24               OUTPUT FIELD
*
EDF020AE EQU   *
EDF020AL EQU   EDF020AE-EDF020AS
*
* * * END OF MAP DEFINITION     * * *
*
EDF020CS DS    0H                 START OF MAP DEFINITION
EDF020CI DS    0H                 START OF MAP INPUT DEFINITION
EDF020CO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CTSI DS    CL4                INPUT FIELD
         ORG   E020CTSI
E020CTSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CPGI DS    CL8                INPUT FIELD
         ORG   E020CPGI
E020CPGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CTNI DS    CL7                INPUT FIELD
         ORG   E020CTNI
E020CTNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CSCI DS    CL3                INPUT FIELD
         ORG   E020CSCI
E020CSCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CSTI DS    CL26               INPUT FIELD
         ORG   E020CSTI
E020CSTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CPRI DS    CL8                INPUT FIELD
         ORG   E020CPRI
E020CPRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020COFI DS    CL9                INPUT FIELD
         ORG   E020COFI
E020COFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020C1RA DS    X                  OUTPUT ATTRIBUTE
E020C1RI DS    CL9                INPUT FIELD
         ORG   E020C1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CRPI DS    CL12               INPUT FIELD
         ORG   E020CRPI
E020CRPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020C2RA DS    X                  OUTPUT ATTRIBUTE
E020C2RI DS    CL9                INPUT FIELD
         ORG   E020C2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CR2I DS    CL3                INPUT FIELD
         ORG   E020CR2I
E020CR2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020CRCI DS    CL25               INPUT FIELD
         ORG   E020CRCI
E020CRCO DS    CL25               OUTPUT FIELD
*
EDF020CE EQU   *
EDF020CL EQU   EDF020CE-EDF020CS
*
* * * END OF MAP DEFINITION     * * *
*
EDF020ES DS    0H                 START OF MAP DEFINITION
EDF020EI DS    0H                 START OF MAP INPUT DEFINITION
EDF020EO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ETSI DS    CL4                INPUT FIELD
         ORG   E020ETSI
E020ETSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020EPGI DS    CL8                INPUT FIELD
         ORG   E020EPGI
E020EPGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ETNI DS    CL7                INPUT FIELD
         ORG   E020ETNI
E020ETNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ESCI DS    CL3                INPUT FIELD
         ORG   E020ESCI
E020ESCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ESTI DS    CL26               INPUT FIELD
         ORG   E020ESTI
E020ESTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ENHI DS    CL8                INPUT FIELD
         ORG   E020ENHI
E020ENHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020EPRI DS    CL8                INPUT FIELD
         ORG   E020EPRI
E020EPRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020EOFI DS    CL9                INPUT FIELD
         ORG   E020EOFI
E020EOFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020E1RA DS    X                  OUTPUT ATTRIBUTE
E020E1RI DS    CL9                INPUT FIELD
         ORG   E020E1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ERPI DS    CL12               INPUT FIELD
         ORG   E020ERPI
E020ERPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E020E2RA DS    X                  OUTPUT ATTRIBUTE
E020E2RI DS    CL9                INPUT FIELD
         ORG   E020E2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ER2I DS    CL3                INPUT FIELD
         ORG   E020ER2I
E020ER2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E020ERCI DS    CL25               INPUT FIELD
         ORG   E020ERCI
E020ERCO DS    CL25               OUTPUT FIELD
*
EDF020EE EQU   *
EDF020EL EQU   EDF020EE-EDF020ES
*
* * * END OF MAP DEFINITION     * * *
*
EDF0402S DS    0H                 START OF MAP DEFINITION
EDF0402I DS    0H                 START OF MAP INPUT DEFINITION
EDF0402O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402TSI DS    CL4                INPUT FIELD
         ORG   E0402TSI
E0402TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402PGI DS    CL8                INPUT FIELD
         ORG   E0402PGI
E0402PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402TNI DS    CL7                INPUT FIELD
         ORG   E0402TNI
E0402TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402MDI DS    CL6                INPUT FIELD
         ORG   E0402MDI
E0402MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402SCI DS    CL3                INPUT FIELD
         ORG   E0402SCI
E0402SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402STI DS    CL26               INPUT FIELD
         ORG   E0402STI
E0402STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402INI DS    CL63               INPUT FIELD
         ORG   E0402INI
E0402INO DS    CL63               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402LII DS    CL7                INPUT FIELD
         ORG   E0402LII
E0402LIO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402LDI DS    CL22               INPUT FIELD
         ORG   E0402LDI
E0402LDO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402NWI DS    CL6                INPUT FIELD
         ORG   E0402NWI
E0402NWO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402NHI DS    CL8                INPUT FIELD
         ORG   E0402NHI
E0402NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402PRI DS    CL8                INPUT FIELD
         ORG   E0402PRI
E0402PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402OFI DS    CL9                INPUT FIELD
         ORG   E0402OFI
E0402OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E04021RA DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E04021RI DS    CL9                INPUT FIELD
         ORG   E04021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402RPI DS    CL12               INPUT FIELD
         ORG   E0402RPI
E0402RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E04022RA DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E04022RI DS    CL9                INPUT FIELD
         ORG   E04022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402R2I DS    CL3                INPUT FIELD
         ORG   E0402R2I
E0402R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402RCI DS    CL25               INPUT FIELD
         ORG   E0402RCI
E0402RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402ACI DS    CL40               INPUT FIELD
         ORG   E0402ACI
E0402ACO DS    CL40               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0402P2A DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
E0402P2I DS    CL21               INPUT FIELD
         ORG   E0402P2I
         DS    CL21               OUTPUT FIELD
*
EDF0402E EQU   *
EDF0402L EQU   EDF0402E-EDF0402S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0404S DS    0H                 START OF MAP DEFINITION
EDF0404I DS    0H                 START OF MAP INPUT DEFINITION
EDF0404O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404TSI DS    CL4                INPUT FIELD
         ORG   E0404TSI
E0404TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404PGI DS    CL8                INPUT FIELD
         ORG   E0404PGI
E0404PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404TNI DS    CL7                INPUT FIELD
         ORG   E0404TNI
E0404TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404MDI DS    CL6                INPUT FIELD
         ORG   E0404MDI
E0404MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404SCI DS    CL3                INPUT FIELD
         ORG   E0404SCI
E0404SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404STI DS    CL26               INPUT FIELD
         ORG   E0404STI
E0404STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404FMI DS    CL71               INPUT FIELD
         ORG   E0404FMI
E0404FMO DS    CL71               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404LNI DS    CL7                INPUT FIELD
         ORG   E0404LNI
E0404LNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404NHI DS    CL8                INPUT FIELD
         ORG   E0404NHI
E0404NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404PRI DS    CL8                INPUT FIELD
         ORG   E0404PRI
E0404PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404OFI DS    CL9                INPUT FIELD
         ORG   E0404OFI
E0404OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E04041RA DS    X                  OUTPUT ATTRIBUTE
E04041RI DS    CL9                INPUT FIELD
         ORG   E04041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404RPI DS    CL12               INPUT FIELD
         ORG   E0404RPI
E0404RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E04042RA DS    X                  OUTPUT ATTRIBUTE
E04042RI DS    CL9                INPUT FIELD
         ORG   E04042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404R2I DS    CL3                INPUT FIELD
         ORG   E0404R2I
E0404R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0404RCI DS    CL25               INPUT FIELD
         ORG   E0404RCI
E0404RCO DS    CL25               OUTPUT FIELD
*
EDF0404E EQU   *
EDF0404L EQU   EDF0404E-EDF0404S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0602S DS    0H                 START OF MAP DEFINITION
EDF0602I DS    0H                 START OF MAP INPUT DEFINITION
EDF0602O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602TSI DS    CL4                INPUT FIELD
         ORG   E0602TSI
E0602TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602PGI DS    CL8                INPUT FIELD
         ORG   E0602PGI
E0602PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602TNI DS    CL7                INPUT FIELD
         ORG   E0602TNI
E0602TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602MDI DS    CL6                INPUT FIELD
         ORG   E0602MDI
E0602MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602SCI DS    CL3                INPUT FIELD
         ORG   E0602SCI
E0602SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602STI DS    CL26               INPUT FIELD
         ORG   E0602STI
E0602STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602FLI DS    CL10               INPUT FIELD
         ORG   E0602FLI
E0602FLO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602ISI DS    CL79               INPUT FIELD
         ORG   E0602ISI
E0602ISO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602LII DS    CL28               INPUT FIELD
         ORG   E0602LII
E0602LIO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602LDI DS    CL28               INPUT FIELD
         ORG   E0602LDI
E0602LDO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602RII DS    CL63               INPUT FIELD
         ORG   E0602RII
E0602RIO DS    CL63               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602RLI DS    CL22               INPUT FIELD
         ORG   E0602RLI
E0602RLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602KLI DS    CL22               INPUT FIELD
         ORG   E0602KLI
E0602KLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602GNI DS    CL7                INPUT FIELD
         ORG   E0602GNI
E0602GNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602RBI DS    CL4                INPUT FIELD
         ORG   E0602RBI
E0602RBO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602EGI DS    CL5                INPUT FIELD
         ORG   E0602EGI
E0602EGO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602NHI DS    CL8                INPUT FIELD
         ORG   E0602NHI
E0602NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602PRI DS    CL8                INPUT FIELD
         ORG   E0602PRI
E0602PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602OFI DS    CL9                INPUT FIELD
         ORG   E0602OFI
E0602OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06021RA DS    X                  OUTPUT ATTRIBUTE
E06021RI DS    CL9                INPUT FIELD
         ORG   E06021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602RPI DS    CL12               INPUT FIELD
         ORG   E0602RPI
E0602RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06022RA DS    X                  OUTPUT ATTRIBUTE
E06022RI DS    CL9                INPUT FIELD
         ORG   E06022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602R2I DS    CL3                INPUT FIELD
         ORG   E0602R2I
E0602R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0602RCI DS    CL25               INPUT FIELD
         ORG   E0602RCI
E0602RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0602P2A DS    X                  OUTPUT ATTRIBUTE
E0602P2I DS    CL21               INPUT FIELD
         ORG   E0602P2I
         DS    CL21               OUTPUT FIELD
*
EDF0602E EQU   *
EDF0602L EQU   EDF0602E-EDF0602S
*
* * * END OF MAP DEFINITION     * * *
*
EDF060CS DS    0H                 START OF MAP DEFINITION
EDF060CI DS    0H                 START OF MAP INPUT DEFINITION
EDF060CO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CTSI DS    CL4                INPUT FIELD
         ORG   E060CTSI
E060CTSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CPGI DS    CL8                INPUT FIELD
         ORG   E060CPGI
E060CPGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CTNI DS    CL7                INPUT FIELD
         ORG   E060CTNI
E060CTNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CMDI DS    CL6                INPUT FIELD
         ORG   E060CMDI
E060CMDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CSCI DS    CL3                INPUT FIELD
         ORG   E060CSCI
E060CSCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CSTI DS    CL26               INPUT FIELD
         ORG   E060CSTI
E060CSTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CFLI DS    CL10               INPUT FIELD
         ORG   E060CFLI
E060CFLO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CRII DS    CL63               INPUT FIELD
         ORG   E060CRII
E060CRIO DS    CL63               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CRLI DS    CL22               INPUT FIELD
         ORG   E060CRLI
E060CRLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CKLI DS    CL22               INPUT FIELD
         ORG   E060CKLI
E060CKLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CGNI DS    CL7                INPUT FIELD
         ORG   E060CGNI
E060CGNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CRQI DS    CL7                INPUT FIELD
         ORG   E060CRQI
E060CRQO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CRBI DS    CL4                INPUT FIELD
         ORG   E060CRBI
E060CRBO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CEGI DS    CL5                INPUT FIELD
         ORG   E060CEGI
E060CEGO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CNHI DS    CL8                INPUT FIELD
         ORG   E060CNHI
E060CNHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CPRI DS    CL8                INPUT FIELD
         ORG   E060CPRI
E060CPRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060COFI DS    CL9                INPUT FIELD
         ORG   E060COFI
E060COFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E060C1RA DS    X                  OUTPUT ATTRIBUTE
E060C1RI DS    CL9                INPUT FIELD
         ORG   E060C1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CRPI DS    CL12               INPUT FIELD
         ORG   E060CRPI
E060CRPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E060C2RA DS    X                  OUTPUT ATTRIBUTE
E060C2RI DS    CL9                INPUT FIELD
         ORG   E060C2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CR2I DS    CL3                INPUT FIELD
         ORG   E060CR2I
E060CR2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060CRCI DS    CL25               INPUT FIELD
         ORG   E060CRCI
E060CRCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E060CP2A DS    X                  OUTPUT ATTRIBUTE
E060CP2I DS    CL21               INPUT FIELD
         ORG   E060CP2I
         DS    CL21               OUTPUT FIELD
*
EDF060CE EQU   *
EDF060CL EQU   EDF060CE-EDF060CS
*
* * * END OF MAP DEFINITION     * * *
*
EDF060ES DS    0H                 START OF MAP DEFINITION
EDF060EI DS    0H                 START OF MAP INPUT DEFINITION
EDF060EO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ETSI DS    CL4                INPUT FIELD
         ORG   E060ETSI
E060ETSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060EPGI DS    CL8                INPUT FIELD
         ORG   E060EPGI
E060EPGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ETNI DS    CL7                INPUT FIELD
         ORG   E060ETNI
E060ETNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060EMDI DS    CL6                INPUT FIELD
         ORG   E060EMDI
E060EMDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ESCI DS    CL3                INPUT FIELD
         ORG   E060ESCI
E060ESCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ESTI DS    CL26               INPUT FIELD
         ORG   E060ESTI
E060ESTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060EFLI DS    CL10               INPUT FIELD
         ORG   E060EFLI
E060EFLO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060EISI DS    CL79               INPUT FIELD
         ORG   E060EISI
E060EISO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ELII DS    CL28               INPUT FIELD
         ORG   E060ELII
E060ELIO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ELDI DS    CL28               INPUT FIELD
         ORG   E060ELDI
E060ELDO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ERII DS    CL63               INPUT FIELD
         ORG   E060ERII
E060ERIO DS    CL63               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ERLI DS    CL22               INPUT FIELD
         ORG   E060ERLI
E060ERLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060EKLI DS    CL22               INPUT FIELD
         ORG   E060EKLI
E060EKLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ERQI DS    CL7                INPUT FIELD
         ORG   E060ERQI
E060ERQO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ERBI DS    CL4                INPUT FIELD
         ORG   E060ERBI
E060ERBO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ENHI DS    CL8                INPUT FIELD
         ORG   E060ENHI
E060ENHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060EPRI DS    CL8                INPUT FIELD
         ORG   E060EPRI
E060EPRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060EOFI DS    CL9                INPUT FIELD
         ORG   E060EOFI
E060EOFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E060E1RA DS    X                  OUTPUT ATTRIBUTE
E060E1RI DS    CL9                INPUT FIELD
         ORG   E060E1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ERPI DS    CL12               INPUT FIELD
         ORG   E060ERPI
E060ERPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E060E2RA DS    X                  OUTPUT ATTRIBUTE
E060E2RI DS    CL9                INPUT FIELD
         ORG   E060E2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ER2I DS    CL3                INPUT FIELD
         ORG   E060ER2I
E060ER2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E060ERCI DS    CL25               INPUT FIELD
         ORG   E060ERCI
E060ERCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E060EP2A DS    X                  OUTPUT ATTRIBUTE
E060EP2I DS    CL21               INPUT FIELD
         ORG   E060EP2I
         DS    CL21               OUTPUT FIELD
*
EDF060EE EQU   *
EDF060EL EQU   EDF060EE-EDF060ES
*
* * * END OF MAP DEFINITION     * * *
*
EDF0610S DS    0H                 START OF MAP DEFINITION
EDF0610I DS    0H                 START OF MAP INPUT DEFINITION
EDF0610O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610TSI DS    CL4                INPUT FIELD
         ORG   E0610TSI
E0610TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610PGI DS    CL8                INPUT FIELD
         ORG   E0610PGI
E0610PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610TNI DS    CL7                INPUT FIELD
         ORG   E0610TNI
E0610TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610MDI DS    CL6                INPUT FIELD
         ORG   E0610MDI
E0610MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610SCI DS    CL3                INPUT FIELD
         ORG   E0610SCI
E0610SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610STI DS    CL26               INPUT FIELD
         ORG   E0610STI
E0610STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610FLI DS    CL10               INPUT FIELD
         ORG   E0610FLI
E0610FLO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610ISI DS    CL79               INPUT FIELD
         ORG   E0610ISI
E0610ISO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610LII DS    CL28               INPUT FIELD
         ORG   E0610LII
E0610LIO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610LDI DS    CL28               INPUT FIELD
         ORG   E0610LDI
E0610LDO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610RII DS    CL63               INPUT FIELD
         ORG   E0610RII
E0610RIO DS    CL63               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610RLI DS    CL22               INPUT FIELD
         ORG   E0610RLI
E0610RLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610KLI DS    CL22               INPUT FIELD
         ORG   E0610KLI
E0610KLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610RQI DS    CL7                INPUT FIELD
         ORG   E0610RQI
E0610RQO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610RBI DS    CL4                INPUT FIELD
         ORG   E0610RBI
E0610RBO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610NHI DS    CL8                INPUT FIELD
         ORG   E0610NHI
E0610NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610PRI DS    CL8                INPUT FIELD
         ORG   E0610PRI
E0610PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610OFI DS    CL9                INPUT FIELD
         ORG   E0610OFI
E0610OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06101RA DS    X                  OUTPUT ATTRIBUTE
E06101RI DS    CL9                INPUT FIELD
         ORG   E06101RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610RPI DS    CL12               INPUT FIELD
         ORG   E0610RPI
E0610RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06102RA DS    X                  OUTPUT ATTRIBUTE
E06102RI DS    CL9                INPUT FIELD
         ORG   E06102RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610R2I DS    CL3                INPUT FIELD
         ORG   E0610R2I
E0610R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0610RCI DS    CL25               INPUT FIELD
         ORG   E0610RCI
E0610RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0610P2A DS    X                  OUTPUT ATTRIBUTE
E0610P2I DS    CL21               INPUT FIELD
         ORG   E0610P2I
         DS    CL21               OUTPUT FIELD
*
EDF0610E EQU   *
EDF0610L EQU   EDF0610E-EDF0610S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0612S DS    0H                 START OF MAP DEFINITION
EDF0612I DS    0H                 START OF MAP INPUT DEFINITION
EDF0612O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612TSI DS    CL4                INPUT FIELD
         ORG   E0612TSI
E0612TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612PGI DS    CL8                INPUT FIELD
         ORG   E0612PGI
E0612PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612TNI DS    CL7                INPUT FIELD
         ORG   E0612TNI
E0612TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612SCI DS    CL3                INPUT FIELD
         ORG   E0612SCI
E0612SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612STI DS    CL26               INPUT FIELD
         ORG   E0612STI
E0612STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612FLI DS    CL10               INPUT FIELD
         ORG   E0612FLI
E0612FLO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612RQI DS    CL7                INPUT FIELD
         ORG   E0612RQI
E0612RQO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612NHI DS    CL8                INPUT FIELD
         ORG   E0612NHI
E0612NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612PRI DS    CL8                INPUT FIELD
         ORG   E0612PRI
E0612PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612OFI DS    CL9                INPUT FIELD
         ORG   E0612OFI
E0612OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06121RA DS    X                  OUTPUT ATTRIBUTE
E06121RI DS    CL9                INPUT FIELD
         ORG   E06121RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612RPI DS    CL12               INPUT FIELD
         ORG   E0612RPI
E0612RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06122RA DS    X                  OUTPUT ATTRIBUTE
E06122RI DS    CL9                INPUT FIELD
         ORG   E06122RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612R2I DS    CL3                INPUT FIELD
         ORG   E0612R2I
E0612R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0612RCI DS    CL25               INPUT FIELD
         ORG   E0612RCI
E0612RCO DS    CL25               OUTPUT FIELD
*
EDF0612E EQU   *
EDF0612L EQU   EDF0612E-EDF0612S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0614S DS    0H                 START OF MAP DEFINITION
EDF0614I DS    0H                 START OF MAP INPUT DEFINITION
EDF0614O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614TSI DS    CL4                INPUT FIELD
         ORG   E0614TSI
E0614TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614PGI DS    CL8                INPUT FIELD
         ORG   E0614PGI
E0614PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614TNI DS    CL7                INPUT FIELD
         ORG   E0614TNI
E0614TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614MDI DS    CL6                INPUT FIELD
         ORG   E0614MDI
E0614MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614SCI DS    CL3                INPUT FIELD
         ORG   E0614SCI
E0614SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614STI DS    CL26               INPUT FIELD
         ORG   E0614STI
E0614STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614FLI DS    CL10               INPUT FIELD
         ORG   E0614FLI
E0614FLO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614RII DS    CL63               INPUT FIELD
         ORG   E0614RII
E0614RIO DS    CL63               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614RLI DS    CL22               INPUT FIELD
         ORG   E0614RLI
E0614RLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614KLI DS    CL22               INPUT FIELD
         ORG   E0614KLI
E0614KLO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614GNI DS    CL7                INPUT FIELD
         ORG   E0614GNI
E0614GNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614RQI DS    CL7                INPUT FIELD
         ORG   E0614RQI
E0614RQO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614RBI DS    CL4                INPUT FIELD
         ORG   E0614RBI
E0614RBO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614EGI DS    CL5                INPUT FIELD
         ORG   E0614EGI
E0614EGO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614NHI DS    CL8                INPUT FIELD
         ORG   E0614NHI
E0614NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614PRI DS    CL8                INPUT FIELD
         ORG   E0614PRI
E0614PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614OFI DS    CL9                INPUT FIELD
         ORG   E0614OFI
E0614OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06141RA DS    X                  OUTPUT ATTRIBUTE
E06141RI DS    CL9                INPUT FIELD
         ORG   E06141RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614RPI DS    CL12               INPUT FIELD
         ORG   E0614RPI
E0614RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E06142RA DS    X                  OUTPUT ATTRIBUTE
E06142RI DS    CL9                INPUT FIELD
         ORG   E06142RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614R2I DS    CL3                INPUT FIELD
         ORG   E0614R2I
E0614R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0614RCI DS    CL25               INPUT FIELD
         ORG   E0614RCI
E0614RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0614P2A DS    X                  OUTPUT ATTRIBUTE
E0614P2I DS    CL21               INPUT FIELD
         ORG   E0614P2I
         DS    CL21               OUTPUT FIELD
*
EDF0614E EQU   *
EDF0614L EQU   EDF0614E-EDF0614S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0A02S DS    0H                 START OF MAP DEFINITION
EDF0A02I DS    0H                 START OF MAP INPUT DEFINITION
EDF0A02O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02TSI DS    CL4                INPUT FIELD
         ORG   E0A02TSI
E0A02TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02PGI DS    CL8                INPUT FIELD
         ORG   E0A02PGI
E0A02PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02TNI DS    CL7                INPUT FIELD
         ORG   E0A02TNI
E0A02TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02MDI DS    CL6                INPUT FIELD
         ORG   E0A02MDI
E0A02MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02SCI DS    CL3                INPUT FIELD
         ORG   E0A02SCI
E0A02SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02STI DS    CL26               INPUT FIELD
         ORG   E0A02STI
E0A02STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02QUI DS    CL37               INPUT FIELD
         ORG   E0A02QUI
E0A02QUO DS    CL37               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02FMI DS    CL69               INPUT FIELD
         ORG   E0A02FMI
E0A02FMO DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02LNI DS    CL7                INPUT FIELD
         ORG   E0A02LNI
E0A02LNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02ITI DS    CL16               INPUT FIELD
         ORG   E0A02ITI
E0A02ITO DS    CL16               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02RWI DS    CL7                INPUT FIELD
         ORG   E0A02RWI
E0A02RWO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02NII DS    CL16               INPUT FIELD
         ORG   E0A02NII
E0A02NIO DS    CL16               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02NHI DS    CL8                INPUT FIELD
         ORG   E0A02NHI
E0A02NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02PRI DS    CL8                INPUT FIELD
         ORG   E0A02PRI
E0A02PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02OFI DS    CL9                INPUT FIELD
         ORG   E0A02OFI
E0A02OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0A021RA DS    X                  OUTPUT ATTRIBUTE
E0A021RI DS    CL9                INPUT FIELD
         ORG   E0A021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02RPI DS    CL12               INPUT FIELD
         ORG   E0A02RPI
E0A02RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0A022RA DS    X                  OUTPUT ATTRIBUTE
E0A022RI DS    CL9                INPUT FIELD
         ORG   E0A022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02R2I DS    CL3                INPUT FIELD
         ORG   E0A02R2I
E0A02R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A02RCI DS    CL25               INPUT FIELD
         ORG   E0A02RCI
E0A02RCO DS    CL25               OUTPUT FIELD
*
EDF0A02E EQU   *
EDF0A02L EQU   EDF0A02E-EDF0A02S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0A04S DS    0H                 START OF MAP DEFINITION
EDF0A04I DS    0H                 START OF MAP INPUT DEFINITION
EDF0A04O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04TSI DS    CL4                INPUT FIELD
         ORG   E0A04TSI
E0A04TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04PGI DS    CL8                INPUT FIELD
         ORG   E0A04PGI
E0A04PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04TNI DS    CL7                INPUT FIELD
         ORG   E0A04TNI
E0A04TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04MDI DS    CL6                INPUT FIELD
         ORG   E0A04MDI
E0A04MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04SCI DS    CL3                INPUT FIELD
         ORG   E0A04SCI
E0A04SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04STI DS    CL26               INPUT FIELD
         ORG   E0A04STI
E0A04STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04QUI DS    CL37               INPUT FIELD
         ORG   E0A04QUI
E0A04QUO DS    CL37               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04ITI DS    CL22               INPUT FIELD
         ORG   E0A04ITI
E0A04ITO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04ISI DS    CL79               INPUT FIELD
         ORG   E0A04ISI
E0A04ISO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04LII DS    CL7                INPUT FIELD
         ORG   E0A04LII
E0A04LIO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04LDI DS    CL22               INPUT FIELD
         ORG   E0A04LDI
E0A04LDO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04NII DS    CL22               INPUT FIELD
         ORG   E0A04NII
E0A04NIO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04NHI DS    CL8                INPUT FIELD
         ORG   E0A04NHI
E0A04NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04PRI DS    CL8                INPUT FIELD
         ORG   E0A04PRI
E0A04PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04OFI DS    CL9                INPUT FIELD
         ORG   E0A04OFI
E0A04OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0A041RA DS    X                  OUTPUT ATTRIBUTE
E0A041RI DS    CL9                INPUT FIELD
         ORG   E0A041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04RPI DS    CL12               INPUT FIELD
         ORG   E0A04RPI
E0A04RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0A042RA DS    X                  OUTPUT ATTRIBUTE
E0A042RI DS    CL9                INPUT FIELD
         ORG   E0A042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04R2I DS    CL3                INPUT FIELD
         ORG   E0A04R2I
E0A04R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A04RCI DS    CL25               INPUT FIELD
         ORG   E0A04RCI
E0A04RCO DS    CL25               OUTPUT FIELD
*
EDF0A04E EQU   *
EDF0A04L EQU   EDF0A04E-EDF0A04S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0A06S DS    0H                 START OF MAP DEFINITION
EDF0A06I DS    0H                 START OF MAP INPUT DEFINITION
EDF0A06O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06TSI DS    CL4                INPUT FIELD
         ORG   E0A06TSI
E0A06TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06PGI DS    CL8                INPUT FIELD
         ORG   E0A06PGI
E0A06PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06TNI DS    CL7                INPUT FIELD
         ORG   E0A06TNI
E0A06TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06MDI DS    CL6                INPUT FIELD
         ORG   E0A06MDI
E0A06MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06SCI DS    CL3                INPUT FIELD
         ORG   E0A06SCI
E0A06SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06STI DS    CL26               INPUT FIELD
         ORG   E0A06STI
E0A06STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06QUI DS    CL37               INPUT FIELD
         ORG   E0A06QUI
E0A06QUO DS    CL37               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06NHI DS    CL8                INPUT FIELD
         ORG   E0A06NHI
E0A06NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06PRI DS    CL8                INPUT FIELD
         ORG   E0A06PRI
E0A06PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06OFI DS    CL9                INPUT FIELD
         ORG   E0A06OFI
E0A06OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0A061RA DS    X                  OUTPUT ATTRIBUTE
E0A061RI DS    CL9                INPUT FIELD
         ORG   E0A061RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06RPI DS    CL12               INPUT FIELD
         ORG   E0A06RPI
E0A06RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0A062RA DS    X                  OUTPUT ATTRIBUTE
E0A062RI DS    CL9                INPUT FIELD
         ORG   E0A062RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06R2I DS    CL3                INPUT FIELD
         ORG   E0A06R2I
E0A06R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0A06RCI DS    CL25               INPUT FIELD
         ORG   E0A06RCI
E0A06RCO DS    CL25               OUTPUT FIELD
*
EDF0A06E EQU   *
EDF0A06L EQU   EDF0A06E-EDF0A06S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0C02S DS    0H                 START OF MAP DEFINITION
EDF0C02I DS    0H                 START OF MAP INPUT DEFINITION
EDF0C02O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02TSI DS    CL4                INPUT FIELD
         ORG   E0C02TSI
E0C02TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02PGI DS    CL8                INPUT FIELD
         ORG   E0C02PGI
E0C02PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02TNI DS    CL7                INPUT FIELD
         ORG   E0C02TNI
E0C02TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02SCI DS    CL3                INPUT FIELD
         ORG   E0C02SCI
E0C02SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02STI DS    CL26               INPUT FIELD
         ORG   E0C02STI
E0C02STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02LNI DS    CL22               INPUT FIELD
         ORG   E0C02LNI
E0C02LNO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0C02IMA DS    X                  OUTPUT ATTRIBUTE
E0C02IMI DS    CL16               INPUT FIELD
         ORG   E0C02IMI
E0C02IMO DS    CL16               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0C02ADA DS    X                  OUTPUT ATTRIBUTE
E0C02ADI DS    CL22               INPUT FIELD
         ORG   E0C02ADI
E0C02ADO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02NHI DS    CL8                INPUT FIELD
         ORG   E0C02NHI
E0C02NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02PRI DS    CL8                INPUT FIELD
         ORG   E0C02PRI
E0C02PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02OFI DS    CL9                INPUT FIELD
         ORG   E0C02OFI
E0C02OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0C021RA DS    X                  OUTPUT ATTRIBUTE
E0C021RI DS    CL9                INPUT FIELD
         ORG   E0C021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02RPI DS    CL12               INPUT FIELD
         ORG   E0C02RPI
E0C02RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0C022RA DS    X                  OUTPUT ATTRIBUTE
E0C022RI DS    CL9                INPUT FIELD
         ORG   E0C022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02R2I DS    CL3                INPUT FIELD
         ORG   E0C02R2I
E0C02R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C02RCI DS    CL25               INPUT FIELD
         ORG   E0C02RCI
E0C02RCO DS    CL25               OUTPUT FIELD
*
EDF0C02E EQU   *
EDF0C02L EQU   EDF0C02E-EDF0C02S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0C04S DS    0H                 START OF MAP DEFINITION
EDF0C04I DS    0H                 START OF MAP INPUT DEFINITION
EDF0C04O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04TSI DS    CL4                INPUT FIELD
         ORG   E0C04TSI
E0C04TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04PGI DS    CL8                INPUT FIELD
         ORG   E0C04PGI
E0C04PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04TNI DS    CL7                INPUT FIELD
         ORG   E0C04TNI
E0C04TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04SCI DS    CL3                INPUT FIELD
         ORG   E0C04SCI
E0C04SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04STI DS    CL26               INPUT FIELD
         ORG   E0C04STI
E0C04STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04ADI DS    CL22               INPUT FIELD
         ORG   E0C04ADI
E0C04ADO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04NHI DS    CL8                INPUT FIELD
         ORG   E0C04NHI
E0C04NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04PRI DS    CL8                INPUT FIELD
         ORG   E0C04PRI
E0C04PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04OFI DS    CL9                INPUT FIELD
         ORG   E0C04OFI
E0C04OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0C041RA DS    X                  OUTPUT ATTRIBUTE
E0C041RI DS    CL9                INPUT FIELD
         ORG   E0C041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04RPI DS    CL12               INPUT FIELD
         ORG   E0C04RPI
E0C04RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0C042RA DS    X                  OUTPUT ATTRIBUTE
E0C042RI DS    CL9                INPUT FIELD
         ORG   E0C042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04R2I DS    CL3                INPUT FIELD
         ORG   E0C04R2I
E0C04R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0C04RCI DS    CL25               INPUT FIELD
         ORG   E0C04RCI
E0C04RCO DS    CL25               OUTPUT FIELD
*
EDF0C04E EQU   *
EDF0C04L EQU   EDF0C04E-EDF0C04S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0E02S DS    0H                 START OF MAP DEFINITION
EDF0E02I DS    0H                 START OF MAP INPUT DEFINITION
EDF0E02O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02TSI DS    CL4                INPUT FIELD
         ORG   E0E02TSI
E0E02TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02PGI DS    CL8                INPUT FIELD
         ORG   E0E02PGI
E0E02PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02TNI DS    CL7                INPUT FIELD
         ORG   E0E02TNI
E0E02TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02MDI DS    CL6                INPUT FIELD
         ORG   E0E02MDI
E0E02MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02SCI DS    CL3                INPUT FIELD
         ORG   E0E02SCI
E0E02SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02STI DS    CL26               INPUT FIELD
         ORG   E0E02STI
E0E02STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02PMI DS    CL10               INPUT FIELD
         ORG   E0E02PMI
E0E02PMO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02CMI DS    CL79               INPUT FIELD
         ORG   E0E02CMI
E0E02CMO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02LNI DS    CL16               INPUT FIELD
         ORG   E0E02LNI
E0E02LNO DS    CL16               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02NHI DS    CL8                INPUT FIELD
         ORG   E0E02NHI
E0E02NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02PRI DS    CL8                INPUT FIELD
         ORG   E0E02PRI
E0E02PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02OFI DS    CL9                INPUT FIELD
         ORG   E0E02OFI
E0E02OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E021RA DS    X                  OUTPUT ATTRIBUTE
E0E021RI DS    CL9                INPUT FIELD
         ORG   E0E021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02RPI DS    CL12               INPUT FIELD
         ORG   E0E02RPI
E0E02RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E022RA DS    X                  OUTPUT ATTRIBUTE
E0E022RI DS    CL9                INPUT FIELD
         ORG   E0E022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02R2I DS    CL3                INPUT FIELD
         ORG   E0E02R2I
E0E02R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E02RCI DS    CL25               INPUT FIELD
         ORG   E0E02RCI
E0E02RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E02P2A DS    X                  OUTPUT ATTRIBUTE
E0E02P2I DS    CL21               INPUT FIELD
         ORG   E0E02P2I
         DS    CL21               OUTPUT FIELD
*
EDF0E02E EQU   *
EDF0E02L EQU   EDF0E02E-EDF0E02S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0E04S DS    0H                 START OF MAP DEFINITION
EDF0E04I DS    0H                 START OF MAP INPUT DEFINITION
EDF0E04O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04TSI DS    CL4                INPUT FIELD
         ORG   E0E04TSI
E0E04TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04PGI DS    CL8                INPUT FIELD
         ORG   E0E04PGI
E0E04PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04TNI DS    CL7                INPUT FIELD
         ORG   E0E04TNI
E0E04TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04MDI DS    CL6                INPUT FIELD
         ORG   E0E04MDI
E0E04MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04SCI DS    CL3                INPUT FIELD
         ORG   E0E04SCI
E0E04SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04STI DS    CL26               INPUT FIELD
         ORG   E0E04STI
E0E04STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04PMI DS    CL19               INPUT FIELD
         ORG   E0E04PMI
E0E04PMO DS    CL19               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04CMI DS    CL79               INPUT FIELD
         ORG   E0E04CMI
E0E04CMO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04LNI DS    CL16               INPUT FIELD
         ORG   E0E04LNI
E0E04LNO DS    CL16               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04NHI DS    CL8                INPUT FIELD
         ORG   E0E04NHI
E0E04NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04PRI DS    CL8                INPUT FIELD
         ORG   E0E04PRI
E0E04PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04OFI DS    CL9                INPUT FIELD
         ORG   E0E04OFI
E0E04OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E041RA DS    X                  OUTPUT ATTRIBUTE
E0E041RI DS    CL9                INPUT FIELD
         ORG   E0E041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04RPI DS    CL12               INPUT FIELD
         ORG   E0E04RPI
E0E04RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E042RA DS    X                  OUTPUT ATTRIBUTE
E0E042RI DS    CL9                INPUT FIELD
         ORG   E0E042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04R2I DS    CL3                INPUT FIELD
         ORG   E0E04R2I
E0E04R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E04RCI DS    CL25               INPUT FIELD
         ORG   E0E04RCI
E0E04RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E04P2A DS    X                  OUTPUT ATTRIBUTE
E0E04P2I DS    CL21               INPUT FIELD
         ORG   E0E04P2I
         DS    CL21               OUTPUT FIELD
*
EDF0E04E EQU   *
EDF0E04L EQU   EDF0E04E-EDF0E04S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0E06S DS    0H                 START OF MAP DEFINITION
EDF0E06I DS    0H                 START OF MAP INPUT DEFINITION
EDF0E06O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06TSI DS    CL4                INPUT FIELD
         ORG   E0E06TSI
E0E06TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06PGI DS    CL8                INPUT FIELD
         ORG   E0E06PGI
E0E06PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06TNI DS    CL7                INPUT FIELD
         ORG   E0E06TNI
E0E06TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06SCI DS    CL3                INPUT FIELD
         ORG   E0E06SCI
E0E06SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06STI DS    CL26               INPUT FIELD
         ORG   E0E06STI
E0E06STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06PMI DS    CL10               INPUT FIELD
         ORG   E0E06PMI
E0E06PMO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06ETI DS    CL23               INPUT FIELD
         ORG   E0E06ETI
E0E06ETO DS    CL23               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06LNI DS    CL17               INPUT FIELD
         ORG   E0E06LNI
E0E06LNO DS    CL17               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06FLI DS    CL23               INPUT FIELD
         ORG   E0E06FLI
E0E06FLO DS    CL23               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06NHI DS    CL8                INPUT FIELD
         ORG   E0E06NHI
E0E06NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06PRI DS    CL8                INPUT FIELD
         ORG   E0E06PRI
E0E06PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06OFI DS    CL9                INPUT FIELD
         ORG   E0E06OFI
E0E06OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E061RA DS    X                  OUTPUT ATTRIBUTE
E0E061RI DS    CL9                INPUT FIELD
         ORG   E0E061RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06RPI DS    CL12               INPUT FIELD
         ORG   E0E06RPI
E0E06RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E062RA DS    X                  OUTPUT ATTRIBUTE
E0E062RI DS    CL9                INPUT FIELD
         ORG   E0E062RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06R2I DS    CL3                INPUT FIELD
         ORG   E0E06R2I
E0E06R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E06RCI DS    CL25               INPUT FIELD
         ORG   E0E06RCI
E0E06RCO DS    CL25               OUTPUT FIELD
*
EDF0E06E EQU   *
EDF0E06L EQU   EDF0E06E-EDF0E06S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0E08S DS    0H                 START OF MAP DEFINITION
EDF0E08I DS    0H                 START OF MAP INPUT DEFINITION
EDF0E08O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08TSI DS    CL4                INPUT FIELD
         ORG   E0E08TSI
E0E08TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08PGI DS    CL8                INPUT FIELD
         ORG   E0E08PGI
E0E08PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08TNI DS    CL7                INPUT FIELD
         ORG   E0E08TNI
E0E08TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08MDI DS    CL6                INPUT FIELD
         ORG   E0E08MDI
E0E08MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08SCI DS    CL3                INPUT FIELD
         ORG   E0E08SCI
E0E08SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08STI DS    CL26               INPUT FIELD
         ORG   E0E08STI
E0E08STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08TRI DS    CL15               INPUT FIELD
         ORG   E0E08TRI
E0E08TRO DS    CL15               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08CMI DS    CL79               INPUT FIELD
         ORG   E0E08CMI
E0E08CMO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08LNI DS    CL16               INPUT FIELD
         ORG   E0E08LNI
E0E08LNO DS    CL16               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08IMI DS    CL9                INPUT FIELD
         ORG   E0E08IMI
E0E08IMO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08PRI DS    CL8                INPUT FIELD
         ORG   E0E08PRI
E0E08PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E08OFI DS    CL9                INPUT FIELD
         ORG   E0E08OFI
E0E08OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E08P2A DS    X                  OUTPUT ATTRIBUTE
E0E08P2I DS    CL21               INPUT FIELD
         ORG   E0E08P2I
         DS    CL21               OUTPUT FIELD
*
EDF0E08E EQU   *
EDF0E08L EQU   EDF0E08E-EDF0E08S
*
* * * END OF MAP DEFINITION     * * *
*
EDF0E0AS DS    0H                 START OF MAP DEFINITION
EDF0E0AI DS    0H                 START OF MAP INPUT DEFINITION
EDF0E0AO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ATSI DS    CL4                INPUT FIELD
         ORG   E0E0ATSI
E0E0ATSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0APGI DS    CL8                INPUT FIELD
         ORG   E0E0APGI
E0E0APGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ATNI DS    CL7                INPUT FIELD
         ORG   E0E0ATNI
E0E0ATNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ASCI DS    CL3                INPUT FIELD
         ORG   E0E0ASCI
E0E0ASCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ASTI DS    CL26               INPUT FIELD
         ORG   E0E0ASTI
E0E0ASTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0APMI DS    CL10               INPUT FIELD
         ORG   E0E0APMI
E0E0APMO DS    CL10               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ANHI DS    CL8                INPUT FIELD
         ORG   E0E0ANHI
E0E0ANHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0APRI DS    CL8                INPUT FIELD
         ORG   E0E0APRI
E0E0APRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0AOFI DS    CL9                INPUT FIELD
         ORG   E0E0AOFI
E0E0AOFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E0A1RA DS    X                  OUTPUT ATTRIBUTE
E0E0A1RI DS    CL9                INPUT FIELD
         ORG   E0E0A1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ARPI DS    CL12               INPUT FIELD
         ORG   E0E0ARPI
E0E0ARPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E0A2RA DS    X                  OUTPUT ATTRIBUTE
E0E0A2RI DS    CL9                INPUT FIELD
         ORG   E0E0A2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0AR2I DS    CL3                INPUT FIELD
         ORG   E0E0AR2I
E0E0AR2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ARCI DS    CL25               INPUT FIELD
         ORG   E0E0ARCI
E0E0ARCO DS    CL25               OUTPUT FIELD
*
EDF0E0AE EQU   *
EDF0E0AL EQU   EDF0E0AE-EDF0E0AS
*
* * * END OF MAP DEFINITION     * * *
*
EDF0E0ES DS    0H                 START OF MAP DEFINITION
EDF0E0EI DS    0H                 START OF MAP INPUT DEFINITION
EDF0E0EO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ETSI DS    CL4                INPUT FIELD
         ORG   E0E0ETSI
E0E0ETSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0EPGI DS    CL8                INPUT FIELD
         ORG   E0E0EPGI
E0E0EPGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ETNI DS    CL7                INPUT FIELD
         ORG   E0E0ETNI
E0E0ETNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ESCI DS    CL3                INPUT FIELD
         ORG   E0E0ESCI
E0E0ESCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ESTI DS    CL26               INPUT FIELD
         ORG   E0E0ESTI
E0E0ESTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ECRI DS    CL6                INPUT FIELD
         ORG   E0E0ECRI
E0E0ECRO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0EPLI DS    CL31               INPUT FIELD
         ORG   E0E0EPLI
E0E0EPLO DS    CL31               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0EPRI DS    CL8                INPUT FIELD
         ORG   E0E0EPRI
E0E0EPRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0EOFI DS    CL9                INPUT FIELD
         ORG   E0E0EOFI
E0E0EOFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E0E1RA DS    X                  OUTPUT ATTRIBUTE
E0E0E1RI DS    CL9                INPUT FIELD
         ORG   E0E0E1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ERPI DS    CL12               INPUT FIELD
         ORG   E0E0ERPI
E0E0ERPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E0E0E2RA DS    X                  OUTPUT ATTRIBUTE
E0E0E2RI DS    CL9                INPUT FIELD
         ORG   E0E0E2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ER2I DS    CL3                INPUT FIELD
         ORG   E0E0ER2I
E0E0ER2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E0E0ERCI DS    CL25               INPUT FIELD
         ORG   E0E0ERCI
E0E0ERCO DS    CL25               OUTPUT FIELD
*
EDF0E0EE EQU   *
EDF0E0EL EQU   EDF0E0EE-EDF0E0ES
*
* * * END OF MAP DEFINITION     * * *
*
EDF1002S DS    0H                 START OF MAP DEFINITION
EDF1002I DS    0H                 START OF MAP INPUT DEFINITION
EDF1002O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002TSI DS    CL4                INPUT FIELD
         ORG   E1002TSI
E1002TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002PGI DS    CL8                INPUT FIELD
         ORG   E1002PGI
E1002PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002TNI DS    CL7                INPUT FIELD
         ORG   E1002TNI
E1002TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002SCI DS    CL3                INPUT FIELD
         ORG   E1002SCI
E1002SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002STI DS    CL26               INPUT FIELD
         ORG   E1002STI
E1002STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1002ABA DS    X                  OUTPUT ATTRIBUTE
E1002ABI DS    CL7                INPUT FIELD
         ORG   E1002ABI
         DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002ATI DS    CL17               INPUT FIELD
         ORG   E1002ATI
E1002ATO DS    CL17               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002NHI DS    CL8                INPUT FIELD
         ORG   E1002NHI
E1002NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002PRI DS    CL8                INPUT FIELD
         ORG   E1002PRI
E1002PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002OFI DS    CL9                INPUT FIELD
         ORG   E1002OFI
E1002OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002FNI DS    CL7                INPUT FIELD
         ORG   E1002FNI
E1002FNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E10021RA DS    X                  OUTPUT ATTRIBUTE
E10021RI DS    CL9                INPUT FIELD
         ORG   E10021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002RPI DS    CL12               INPUT FIELD
         ORG   E1002RPI
E1002RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E10022RA DS    X                  OUTPUT ATTRIBUTE
E10022RI DS    CL9                INPUT FIELD
         ORG   E10022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002R2I DS    CL3                INPUT FIELD
         ORG   E1002R2I
E1002R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1002RCI DS    CL25               INPUT FIELD
         ORG   E1002RCI
E1002RCO DS    CL25               OUTPUT FIELD
*
EDF1002E EQU   *
EDF1002L EQU   EDF1002E-EDF1002S
*
* * * END OF MAP DEFINITION     * * *
*
EDF1004S DS    0H                 START OF MAP DEFINITION
EDF1004I DS    0H                 START OF MAP INPUT DEFINITION
EDF1004O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004TSI DS    CL4                INPUT FIELD
         ORG   E1004TSI
E1004TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004PGI DS    CL8                INPUT FIELD
         ORG   E1004PGI
E1004PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004TNI DS    CL7                INPUT FIELD
         ORG   E1004TNI
E1004TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004MDI DS    CL6                INPUT FIELD
         ORG   E1004MDI
E1004MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004SCI DS    CL3                INPUT FIELD
         ORG   E1004SCI
E1004SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004STI DS    CL26               INPUT FIELD
         ORG   E1004STI
E1004STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004ITI DS    CL19               INPUT FIELD
         ORG   E1004ITI
E1004ITO DS    CL19               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004FUI DS    CL42               INPUT FIELD
         ORG   E1004FUI
E1004FUO DS    CL42               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004RQI DS    CL27               INPUT FIELD
         ORG   E1004RQI
E1004RQO DS    CL27               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004NHI DS    CL8                INPUT FIELD
         ORG   E1004NHI
E1004NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004PRI DS    CL8                INPUT FIELD
         ORG   E1004PRI
E1004PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004OFI DS    CL9                INPUT FIELD
         ORG   E1004OFI
E1004OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E10041RA DS    X                  OUTPUT ATTRIBUTE
E10041RI DS    CL9                INPUT FIELD
         ORG   E10041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004RPI DS    CL12               INPUT FIELD
         ORG   E1004RPI
E1004RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E10042RA DS    X                  OUTPUT ATTRIBUTE
E10042RI DS    CL9                INPUT FIELD
         ORG   E10042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004R2I DS    CL3                INPUT FIELD
         ORG   E1004R2I
E1004R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1004RCI DS    CL25               INPUT FIELD
         ORG   E1004RCI
E1004RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1004P2A DS    X                  OUTPUT ATTRIBUTE
E1004P2I DS    CL21               INPUT FIELD
         ORG   E1004P2I
         DS    CL21               OUTPUT FIELD
*
EDF1004E EQU   *
EDF1004L EQU   EDF1004E-EDF1004S
*
* * * END OF MAP DEFINITION     * * *
*
EDF1008S DS    0H                 START OF MAP DEFINITION
EDF1008I DS    0H                 START OF MAP INPUT DEFINITION
EDF1008O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008TSI DS    CL4                INPUT FIELD
         ORG   E1008TSI
E1008TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008PGI DS    CL8                INPUT FIELD
         ORG   E1008PGI
E1008PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008TNI DS    CL7                INPUT FIELD
         ORG   E1008TNI
E1008TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008MDI DS    CL6                INPUT FIELD
         ORG   E1008MDI
E1008MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008SCI DS    CL3                INPUT FIELD
         ORG   E1008SCI
E1008SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008STI DS    CL26               INPUT FIELD
         ORG   E1008STI
E1008STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008TII DS    CL6                INPUT FIELD
         ORG   E1008TII
E1008TIO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008ITI DS    CL20               INPUT FIELD
         ORG   E1008ITI
E1008ITO DS    CL20               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008FUI DS    CL42               INPUT FIELD
         ORG   E1008FUI
E1008FUO DS    CL42               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008RQI DS    CL31               INPUT FIELD
         ORG   E1008RQI
E1008RQO DS    CL31               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008FMI DS    CL79               INPUT FIELD
         ORG   E1008FMI
E1008FMO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008LNI DS    CL17               INPUT FIELD
         ORG   E1008LNI
E1008LNO DS    CL17               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008TMI DS    CL16               INPUT FIELD
         ORG   E1008TMI
E1008TMO DS    CL16               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008RTI DS    CL23               INPUT FIELD
         ORG   E1008RTI
E1008RTO DS    CL23               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008RMI DS    CL23               INPUT FIELD
         ORG   E1008RMI
E1008RMO DS    CL23               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008QUI DS    CL31               INPUT FIELD
         ORG   E1008QUI
E1008QUO DS    CL31               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008NHI DS    CL8                INPUT FIELD
         ORG   E1008NHI
E1008NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008PRI DS    CL8                INPUT FIELD
         ORG   E1008PRI
E1008PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008OFI DS    CL9                INPUT FIELD
         ORG   E1008OFI
E1008OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E10081RA DS    X                  OUTPUT ATTRIBUTE
E10081RI DS    CL9                INPUT FIELD
         ORG   E10081RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008RPI DS    CL12               INPUT FIELD
         ORG   E1008RPI
E1008RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E10082RA DS    X                  OUTPUT ATTRIBUTE
E10082RI DS    CL9                INPUT FIELD
         ORG   E10082RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008R2I DS    CL3                INPUT FIELD
         ORG   E1008R2I
E1008R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1008RCI DS    CL25               INPUT FIELD
         ORG   E1008RCI
E1008RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1008P2A DS    X                  OUTPUT ATTRIBUTE
E1008P2I DS    CL21               INPUT FIELD
         ORG   E1008P2I
         DS    CL21               OUTPUT FIELD
*
EDF1008E EQU   *
EDF1008L EQU   EDF1008E-EDF1008S
*
* * * END OF MAP DEFINITION     * * *
*
EDF100AS DS    0H                 START OF MAP DEFINITION
EDF100AI DS    0H                 START OF MAP INPUT DEFINITION
EDF100AO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ATSI DS    CL4                INPUT FIELD
         ORG   E100ATSI
E100ATSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100APGI DS    CL8                INPUT FIELD
         ORG   E100APGI
E100APGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ATNI DS    CL7                INPUT FIELD
         ORG   E100ATNI
E100ATNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100AMDI DS    CL6                INPUT FIELD
         ORG   E100AMDI
E100AMDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ASCI DS    CL3                INPUT FIELD
         ORG   E100ASCI
E100ASCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ASTI DS    CL26               INPUT FIELD
         ORG   E100ASTI
E100ASTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100AISI DS    CL79               INPUT FIELD
         ORG   E100AISI
E100AISO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ALNI DS    CL22               INPUT FIELD
         ORG   E100ALNI
E100ALNO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ALDI DS    CL22               INPUT FIELD
         ORG   E100ALDI
E100ALDO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ARTI DS    CL28               INPUT FIELD
         ORG   E100ARTI
E100ARTO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ARMI DS    CL28               INPUT FIELD
         ORG   E100ARMI
E100ARMO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100AQUI DS    CL36               INPUT FIELD
         ORG   E100AQUI
E100AQUO DS    CL36               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ANHI DS    CL8                INPUT FIELD
         ORG   E100ANHI
E100ANHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100APRI DS    CL8                INPUT FIELD
         ORG   E100APRI
E100APRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100AOFI DS    CL9                INPUT FIELD
         ORG   E100AOFI
E100AOFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E100A1RA DS    X                  OUTPUT ATTRIBUTE
E100A1RI DS    CL9                INPUT FIELD
         ORG   E100A1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ARPI DS    CL12               INPUT FIELD
         ORG   E100ARPI
E100ARPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E100A2RA DS    X                  OUTPUT ATTRIBUTE
E100A2RI DS    CL9                INPUT FIELD
         ORG   E100A2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100AR2I DS    CL3                INPUT FIELD
         ORG   E100AR2I
E100AR2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100ARCI DS    CL25               INPUT FIELD
         ORG   E100ARCI
E100ARCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E100AP2A DS    X                  OUTPUT ATTRIBUTE
E100AP2I DS    CL21               INPUT FIELD
         ORG   E100AP2I
         DS    CL21               OUTPUT FIELD
*
EDF100AE EQU   *
EDF100AL EQU   EDF100AE-EDF100AS
*
* * * END OF MAP DEFINITION     * * *
*
EDF100CS DS    0H                 START OF MAP DEFINITION
EDF100CI DS    0H                 START OF MAP INPUT DEFINITION
EDF100CO DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CTSI DS    CL4                INPUT FIELD
         ORG   E100CTSI
E100CTSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CPGI DS    CL8                INPUT FIELD
         ORG   E100CPGI
E100CPGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CTNI DS    CL7                INPUT FIELD
         ORG   E100CTNI
E100CTNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CMDI DS    CL6                INPUT FIELD
         ORG   E100CMDI
E100CMDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CSCI DS    CL3                INPUT FIELD
         ORG   E100CSCI
E100CSCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CSTI DS    CL26               INPUT FIELD
         ORG   E100CSTI
E100CSTO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CRQI DS    CL27               INPUT FIELD
         ORG   E100CRQI
E100CRQO DS    CL27               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CNHI DS    CL8                INPUT FIELD
         ORG   E100CNHI
E100CNHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CPRI DS    CL8                INPUT FIELD
         ORG   E100CPRI
E100CPRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100COFI DS    CL9                INPUT FIELD
         ORG   E100COFI
E100COFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E100C1RA DS    X                  OUTPUT ATTRIBUTE
E100C1RI DS    CL9                INPUT FIELD
         ORG   E100C1RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CRPI DS    CL12               INPUT FIELD
         ORG   E100CRPI
E100CRPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E100C2RA DS    X                  OUTPUT ATTRIBUTE
E100C2RI DS    CL9                INPUT FIELD
         ORG   E100C2RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CR2I DS    CL3                INPUT FIELD
         ORG   E100CR2I
E100CR2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E100CRCI DS    CL25               INPUT FIELD
         ORG   E100CRCI
E100CRCO DS    CL25               OUTPUT FIELD
*
EDF100CE EQU   *
EDF100CL EQU   EDF100CE-EDF100CS
*
* * * END OF MAP DEFINITION     * * *
*
EDF1204S DS    0H                 START OF MAP DEFINITION
EDF1204I DS    0H                 START OF MAP INPUT DEFINITION
EDF1204O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204TSI DS    CL4                INPUT FIELD
         ORG   E1204TSI
E1204TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204PGI DS    CL8                INPUT FIELD
         ORG   E1204PGI
E1204PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204TNI DS    CL7                INPUT FIELD
         ORG   E1204TNI
E1204TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204MDI DS    CL6                INPUT FIELD
         ORG   E1204MDI
E1204MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204SCI DS    CL3                INPUT FIELD
         ORG   E1204SCI
E1204SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204STI DS    CL26               INPUT FIELD
         ORG   E1204STI
E1204STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204RSI DS    CL69               INPUT FIELD
         ORG   E1204RSI
E1204RSO DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1204LHA DS    X                  OUTPUT ATTRIBUTE
E1204LHI DS    CL6                INPUT FIELD
         ORG   E1204LHI
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204LNI DS    CL7                INPUT FIELD
         ORG   E1204LNI
E1204LNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204NSI DS    CL9                INPUT FIELD
         ORG   E1204NSI
E1204NSO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204NHI DS    CL8                INPUT FIELD
         ORG   E1204NHI
E1204NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204PRI DS    CL8                INPUT FIELD
         ORG   E1204PRI
E1204PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204OFI DS    CL9                INPUT FIELD
         ORG   E1204OFI
E1204OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E12041RA DS    X                  OUTPUT ATTRIBUTE
E12041RI DS    CL9                INPUT FIELD
         ORG   E12041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204RPI DS    CL12               INPUT FIELD
         ORG   E1204RPI
E1204RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E12042RA DS    X                  OUTPUT ATTRIBUTE
E12042RI DS    CL9                INPUT FIELD
         ORG   E12042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204R2I DS    CL3                INPUT FIELD
         ORG   E1204R2I
E1204R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1204RCI DS    CL25               INPUT FIELD
         ORG   E1204RCI
E1204RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1204P2A DS    X                  OUTPUT ATTRIBUTE
E1204P2I DS    CL21               INPUT FIELD
         ORG   E1204P2I
         DS    CL21               OUTPUT FIELD
*
EDF1204E EQU   *
EDF1204L EQU   EDF1204E-EDF1204S
*
* * * END OF MAP DEFINITION     * * *
*
EDF1206S DS    0H                 START OF MAP DEFINITION
EDF1206I DS    0H                 START OF MAP INPUT DEFINITION
EDF1206O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206TSI DS    CL4                INPUT FIELD
         ORG   E1206TSI
E1206TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206PGI DS    CL8                INPUT FIELD
         ORG   E1206PGI
E1206PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206TNI DS    CL7                INPUT FIELD
         ORG   E1206TNI
E1206TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206MDI DS    CL6                INPUT FIELD
         ORG   E1206MDI
E1206MDO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206SCI DS    CL3                INPUT FIELD
         ORG   E1206SCI
E1206SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206STI DS    CL26               INPUT FIELD
         ORG   E1206STI
E1206STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206RSI DS    CL69               INPUT FIELD
         ORG   E1206RSI
E1206RSO DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1206LHA DS    X                  OUTPUT ATTRIBUTE
E1206LHI DS    CL6                INPUT FIELD
         ORG   E1206LHI
         DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206LNI DS    CL7                INPUT FIELD
         ORG   E1206LNI
E1206LNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206NHI DS    CL8                INPUT FIELD
         ORG   E1206NHI
E1206NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206PRI DS    CL8                INPUT FIELD
         ORG   E1206PRI
E1206PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206OFI DS    CL9                INPUT FIELD
         ORG   E1206OFI
E1206OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E12061RA DS    X                  OUTPUT ATTRIBUTE
E12061RI DS    CL9                INPUT FIELD
         ORG   E12061RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206RPI DS    CL12               INPUT FIELD
         ORG   E1206RPI
E1206RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E12062RA DS    X                  OUTPUT ATTRIBUTE
E12062RI DS    CL9                INPUT FIELD
         ORG   E12062RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206R2I DS    CL3                INPUT FIELD
         ORG   E1206R2I
E1206R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1206RCI DS    CL25               INPUT FIELD
         ORG   E1206RCI
E1206RCO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1206P2A DS    X                  OUTPUT ATTRIBUTE
E1206P2I DS    CL21               INPUT FIELD
         ORG   E1206P2I
         DS    CL21               OUTPUT FIELD
*
EDF1206E EQU   *
EDF1206L EQU   EDF1206E-EDF1206S
*
* * * END OF MAP DEFINITION     * * *
*
EDF1802S DS    0H                 START OF MAP DEFINITION
EDF1802I DS    0H                 START OF MAP INPUT DEFINITION
EDF1802O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802TSI DS    CL4                INPUT FIELD
         ORG   E1802TSI
E1802TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802PGI DS    CL8                INPUT FIELD
         ORG   E1802PGI
E1802PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802TNI DS    CL7                INPUT FIELD
         ORG   E1802TNI
E1802TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802SCI DS    CL3                INPUT FIELD
         ORG   E1802SCI
E1802SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802STI DS    CL26               INPUT FIELD
         ORG   E1802STI
E1802STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802MPI DS    CL9                INPUT FIELD
         ORG   E1802MPI
E1802MPO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802MSI DS    CL19               INPUT FIELD
         ORG   E1802MSI
E1802MSO DS    CL19               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802SRI DS    CL13               INPUT FIELD
         ORG   E1802SRI
E1802SRO DS    CL13               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802NHI DS    CL8                INPUT FIELD
         ORG   E1802NHI
E1802NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802PRI DS    CL8                INPUT FIELD
         ORG   E1802PRI
E1802PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802OFI DS    CL9                INPUT FIELD
         ORG   E1802OFI
E1802OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E18021RA DS    X                  OUTPUT ATTRIBUTE
E18021RI DS    CL9                INPUT FIELD
         ORG   E18021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802RPI DS    CL12               INPUT FIELD
         ORG   E1802RPI
E1802RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E18022RA DS    X                  OUTPUT ATTRIBUTE
E18022RI DS    CL9                INPUT FIELD
         ORG   E18022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802R2I DS    CL3                INPUT FIELD
         ORG   E1802R2I
E1802R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1802RCI DS    CL25               INPUT FIELD
         ORG   E1802RCI
E1802RCO DS    CL25               OUTPUT FIELD
*
EDF1802E EQU   *
EDF1802L EQU   EDF1802E-EDF1802S
*
* * * END OF MAP DEFINITION     * * *
*
EDF1804S DS    0H                 START OF MAP DEFINITION
EDF1804I DS    0H                 START OF MAP INPUT DEFINITION
EDF1804O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804TSI DS    CL4                INPUT FIELD
         ORG   E1804TSI
E1804TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804PGI DS    CL8                INPUT FIELD
         ORG   E1804PGI
E1804PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804TNI DS    CL7                INPUT FIELD
         ORG   E1804TNI
E1804TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804SCI DS    CL3                INPUT FIELD
         ORG   E1804SCI
E1804SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804STI DS    CL26               INPUT FIELD
         ORG   E1804STI
E1804STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804MPI DS    CL9                INPUT FIELD
         ORG   E1804MPI
E1804MPO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804MSI DS    CL19               INPUT FIELD
         ORG   E1804MSI
E1804MSO DS    CL19               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804SRI DS    CL13               INPUT FIELD
         ORG   E1804SRI
E1804SRO DS    CL13               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804DSI DS    CL28               INPUT FIELD
         ORG   E1804DSI
E1804DSO DS    CL28               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804LNI DS    CL22               INPUT FIELD
         ORG   E1804LNI
E1804LNO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804DMI DS    CL8                INPUT FIELD
         ORG   E1804DMI
E1804DMO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804CSI DS    CL6                INPUT FIELD
         ORG   E1804CSI
E1804CSO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804CPI DS    CL22               INPUT FIELD
         ORG   E1804CPI
E1804CPO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804ERI DS    CL8                INPUT FIELD
         ORG   E1804ERI
E1804ERO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804ALI DS    CL5                INPUT FIELD
         ORG   E1804ALI
E1804ALO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804FKI DS    CL6                INPUT FIELD
         ORG   E1804FKI
E1804FKO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804FRI DS    CL5                INPUT FIELD
         ORG   E1804FRI
E1804FRO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804NHI DS    CL8                INPUT FIELD
         ORG   E1804NHI
E1804NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804PRI DS    CL8                INPUT FIELD
         ORG   E1804PRI
E1804PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804OFI DS    CL9                INPUT FIELD
         ORG   E1804OFI
E1804OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E18041RA DS    X                  OUTPUT ATTRIBUTE
E18041RI DS    CL9                INPUT FIELD
         ORG   E18041RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804RPI DS    CL12               INPUT FIELD
         ORG   E1804RPI
E1804RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E18042RA DS    X                  OUTPUT ATTRIBUTE
E18042RI DS    CL9                INPUT FIELD
         ORG   E18042RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804R2I DS    CL3                INPUT FIELD
         ORG   E1804R2I
E1804R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1804RCI DS    CL25               INPUT FIELD
         ORG   E1804RCI
E1804RCO DS    CL25               OUTPUT FIELD
*
EDF1804E EQU   *
EDF1804L EQU   EDF1804E-EDF1804S
*
* * * END OF MAP DEFINITION     * * *
*
EDF1812S DS    0H                 START OF MAP DEFINITION
EDF1812I DS    0H                 START OF MAP INPUT DEFINITION
EDF1812O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812TSI DS    CL4                INPUT FIELD
         ORG   E1812TSI
E1812TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812PGI DS    CL8                INPUT FIELD
         ORG   E1812PGI
E1812PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812TNI DS    CL7                INPUT FIELD
         ORG   E1812TNI
E1812TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812SCI DS    CL3                INPUT FIELD
         ORG   E1812SCI
E1812SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812STI DS    CL26               INPUT FIELD
         ORG   E1812STI
E1812STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812CSI DS    CL6                INPUT FIELD
         ORG   E1812CSI
E1812CSO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812CPI DS    CL22               INPUT FIELD
         ORG   E1812CPI
E1812CPO DS    CL22               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812ERI DS    CL8                INPUT FIELD
         ORG   E1812ERI
E1812ERO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812ALI DS    CL5                INPUT FIELD
         ORG   E1812ALI
E1812ALO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812FKI DS    CL6                INPUT FIELD
         ORG   E1812FKI
E1812FKO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812FRI DS    CL5                INPUT FIELD
         ORG   E1812FRI
E1812FRO DS    CL5                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812NHI DS    CL8                INPUT FIELD
         ORG   E1812NHI
E1812NHO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812PRI DS    CL8                INPUT FIELD
         ORG   E1812PRI
E1812PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812OFI DS    CL9                INPUT FIELD
         ORG   E1812OFI
E1812OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E18121RA DS    X                  OUTPUT ATTRIBUTE
E18121RI DS    CL9                INPUT FIELD
         ORG   E18121RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812RPI DS    CL12               INPUT FIELD
         ORG   E1812RPI
E1812RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E18122RA DS    X                  OUTPUT ATTRIBUTE
E18122RI DS    CL9                INPUT FIELD
         ORG   E18122RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812R2I DS    CL3                INPUT FIELD
         ORG   E1812R2I
E1812R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1812RCI DS    CL25               INPUT FIELD
         ORG   E1812RCI
E1812RCO DS    CL25               OUTPUT FIELD
*
EDF1812E EQU   *
EDF1812L EQU   EDF1812E-EDF1812S
*
* * * END OF MAP DEFINITION     * * *
*
EDF1C02S DS    0H                 START OF MAP DEFINITION
EDF1C02I DS    0H                 START OF MAP INPUT DEFINITION
EDF1C02O DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02TSI DS    CL4                INPUT FIELD
         ORG   E1C02TSI
E1C02TSO DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02PGI DS    CL8                INPUT FIELD
         ORG   E1C02PGI
E1C02PGO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02TNI DS    CL7                INPUT FIELD
         ORG   E1C02TNI
E1C02TNO DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02SCI DS    CL3                INPUT FIELD
         ORG   E1C02SCI
E1C02SCO DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02STI DS    CL26               INPUT FIELD
         ORG   E1C02STI
E1C02STO DS    CL26               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02DCI DS    CL6                INPUT FIELD
         ORG   E1C02DCI
E1C02DCO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02CMI DS    CL8                INPUT FIELD
         ORG   E1C02CMI
E1C02CMO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02FMI DS    CL25               INPUT FIELD
         ORG   E1C02FMI
E1C02FMO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02LNI DS    CL25               INPUT FIELD
         ORG   E1C02LNI
E1C02LNO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02SLI DS    CL25               INPUT FIELD
         ORG   E1C02SLI
E1C02SLO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02LLI DS    CL25               INPUT FIELD
         ORG   E1C02LLI
E1C02LLO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02NSI DS    CL25               INPUT FIELD
         ORG   E1C02NSI
E1C02NSO DS    CL25               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02PRI DS    CL8                INPUT FIELD
         ORG   E1C02PRI
E1C02PRO DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02OFI DS    CL9                INPUT FIELD
         ORG   E1C02OFI
E1C02OFO DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1C021RA DS    X                  OUTPUT ATTRIBUTE
E1C021RI DS    CL9                INPUT FIELD
         ORG   E1C021RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02RPI DS    CL12               INPUT FIELD
         ORG   E1C02RPI
E1C02RPO DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
E1C022RA DS    X                  OUTPUT ATTRIBUTE
E1C022RI DS    CL9                INPUT FIELD
         ORG   E1C022RI
         DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02R2I DS    CL3                INPUT FIELD
         ORG   E1C02R2I
E1C02R2O DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
E1C02RCI DS    CL25               INPUT FIELD
         ORG   E1C02RCI
E1C02RCO DS    CL25               OUTPUT FIELD
*
EDF1C02E EQU   *
EDF1C02L EQU   EDF1C02E-EDF1C02S
*
* * * END OF MAP DEFINITION     * * *
*
EDFKCPS  DS    0H                 START OF MAP DEFINITION
EDFKCPI  DS    0H                 START OF MAP INPUT DEFINITION
EDFKCPO  DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPTSI  DS    CL4                INPUT FIELD
         ORG   EKCPTSI
EKCPTSO  DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPPGI  DS    CL8                INPUT FIELD
         ORG   EKCPPGI
EKCPPGO  DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPTNI  DS    CL7                INPUT FIELD
         ORG   EKCPTNI
EKCPTNO  DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPSCI  DS    CL3                INPUT FIELD
         ORG   EKCPSCI
EKCPSCO  DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EKCPSTC  DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPSTI  DS    CL35               INPUT FIELD
         ORG   EKCPSTI
EKCPSTO  DS    CL35               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPPRI  DS    CL8                INPUT FIELD
         ORG   EKCPPRI
EKCPPRO  DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPOFI  DS    CL9                INPUT FIELD
         ORG   EKCPOFI
EKCPOFO  DS    CL9                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPRPI  DS    CL12               INPUT FIELD
         ORG   EKCPRPI
EKCPRPO  DS    CL12               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPR2I  DS    CL3                INPUT FIELD
         ORG   EKCPR2I
EKCPR2O  DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
EKCPENA  DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPENI  DS    CL15               INPUT FIELD
         ORG   EKCPENI
         DS    CL15               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
EKCPP3A  DS    X                  OUTPUT ATTRIBUTE
         DS    X                  OUTPUT COLOUR ATTRIBUTE
EKCPP3I  DS    CL20               INPUT FIELD
         ORG   EKCPP3I
         DS    CL20               OUTPUT FIELD
*
EDFKCPE  EQU   *
EDFKCPL  EQU   EDFKCPE-EDFKCPS
*
* * * END OF MAP DEFINITION     * * *
*
EDFWKSS  DS    0H                 START OF MAP DEFINITION
EDFWKSI  DS    0H                 START OF MAP INPUT DEFINITION
EDFWKSO  DS    0H                 START OF MAP OUTPUT DEFINITION
         DS    12C                TIOA PREFIX
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSTSI  DS    CL4                INPUT FIELD
         ORG   EWKSTSI
EWKSTSO  DS    CL4                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSPGI  DS    CL8                INPUT FIELD
         ORG   EWKSPGI
EWKSPGO  DS    CL8                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSTNI  DS    CL7                INPUT FIELD
         ORG   EWKSTNI
EWKSTNO  DS    CL7                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSMDEI DS    CL6                INPUT FIELD
         ORG   EWKSMDEI
EWKSMDEO DS    CL6                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSSCI  DS    CL3                INPUT FIELD
         ORG   EWKSSCI
EWKSSCO  DS    CL3                OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSHEDI DS    CL79               INPUT FIELD
         ORG   EWKSHEDI
EWKSHEDO DS    CL79               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL01I DS    CL69               INPUT FIELD
         ORG   EWKSL01I
EWKSL01O DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL02I DS    CL69               INPUT FIELD
         ORG   EWKSL02I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL03I DS    CL69               INPUT FIELD
         ORG   EWKSL03I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL04I DS    CL69               INPUT FIELD
         ORG   EWKSL04I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL05I DS    CL69               INPUT FIELD
         ORG   EWKSL05I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL06I DS    CL69               INPUT FIELD
         ORG   EWKSL06I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL07I DS    CL69               INPUT FIELD
         ORG   EWKSL07I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL08I DS    CL69               INPUT FIELD
         ORG   EWKSL08I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL09I DS    CL69               INPUT FIELD
         ORG   EWKSL09I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL10I DS    CL69               INPUT FIELD
         ORG   EWKSL10I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL11I DS    CL69               INPUT FIELD
         ORG   EWKSL11I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL12I DS    CL69               INPUT FIELD
         ORG   EWKSL12I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL13I DS    CL69               INPUT FIELD
         ORG   EWKSL13I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL14I DS    CL69               INPUT FIELD
         ORG   EWKSL14I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL15I DS    CL69               INPUT FIELD
         ORG   EWKSL15I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
         DS    X                  OUTPUT ATTRIBUTE
EWKSL16I DS    CL69               INPUT FIELD
         ORG   EWKSL16I
         DS    CL69               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
EWKSP7A  DS    X                  OUTPUT ATTRIBUTE
EWKSP7I  DS    CL21               INPUT FIELD
         ORG   EWKSP7I
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
EWKSP8A  DS    X                  OUTPUT ATTRIBUTE
EWKSP8I  DS    CL24               INPUT FIELD
         ORG   EWKSP8I
         DS    CL24               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
EWKSPAA  DS    X                  OUTPUT ATTRIBUTE
EWKSPAI  DS    CL21               INPUT FIELD
         ORG   EWKSPAI
         DS    CL21               OUTPUT FIELD
*
         DS    AL2                INPUT FIELD LENGTH
         DS    0X                 INPUT FIELD FLAG
EWKSPBA  DS    X                  OUTPUT ATTRIBUTE
EWKSPBI  DS    CL24               INPUT FIELD
         ORG   EWKSPBI
         DS    CL24               OUTPUT FIELD
*
EDFWKSE  EQU   *
EDFWKSL  EQU   EDFWKSE-EDFWKSS
*
* * * END OF MAP DEFINITION     * * *
*
* * * END OF MAP SET DEFINITION * * *
