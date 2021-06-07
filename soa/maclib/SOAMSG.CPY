      *****************************************************************
      * SOAMSG.CPY copybook defining COBOL client SOA message layout               
      * Author - Don Higgins                                           
      * Date   - 05/22/08                                              
      *****************************************************************
       01  SOA-MSG.
           10  SOA-MSG-LEN   PIC S9(9)   COMP.
           10  SOA-MSG-TIME  PIC S9(18)  COMP.
           10  SOA-MSG-NAME  PIC X(8).
           10  SOA-MSG-RC    PIC S9(9)   COMP.
           10  SOA-MSG-PARMS PIC X(1024).      
