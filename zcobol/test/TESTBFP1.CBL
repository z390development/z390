       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTBFP1.
       AUTHOR.        DON HIGGINS.
       DATE-WRITTEN.  03/12/09.
      ***************************************************************
      * REGRESSION TEST BFP FLOATING POINT - REQUIRES FLOAT(BINARY)                   
      ***************************************************************
      * 03/23/09 RPI 1004 INITIAL CODING TO TEST BFP                           
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * THE FOLLOWING 3 TYPES ARE MAPPED TO BFP USING FLOAT(BINARY) 
       77 FLT-SRT FLOAT-SHORT.
       77 FLT-LNG FLOAT-LONG. 
       77 FLT-EXT FLOAT-EXTENDED.
      * THE FOLLOWING 9 TYPES ALWAYS MAP TO HFP, BFP, AND DFP.
       77 HFP-SRT FLOAT-HEX-7      VALUE 1234567.
       77 HFP-LNG FLOAT-HEX-15     VALUE 123456789012345.
       77 HFP-EXT FLOAT-HEX-30 
                  VALUE 123456789012345678901234567890.
       77 BFP-SRT FLOAT-BINARY-7   VALUE 1234567.
       77 BFP-LNG FLOAT-BINARY-16  VALUE 1234567890123456.
       77 BFP-EXT FLOAT-BINARY-34
                  VALUE 1234567890123456789012345678901234.
       77 DFP-SRT FLOAT-DECIMAL-7  VALUE 1234567.
       77 DFP-LNG FLOAT-DECIMAL-16 VALUE 1234567890123456.
       77 DFP-EXT FLOAT-DECIMAL-34
                  VALUE 1234567890123456789012345678901234.
      * THE FOLLOWING 4 TYPES DEFINE 16, 32, 64, AND 128 BINARY TYPES
      * NOTE 18 DIGIT DW LIMITED TO 16 DIGITS TO FIT IN BFP/DFP_LNG
      * NOTE 39 DIGIT QW LIMITED TO 34 DIGITS TO FIT IN BFP/DFP_EXT
       77 HW      COMP PIC S9(4)  VALUE 1234.
       77 FW      COMP PIC S9(9)  VALUE 123456789.
       77 DW      COMP PIC S9(18) VALUE 1234567890123456.
       77 QW      COMP PIC S9(39) 
                  VALUE 1234567890123456789012345678901234.
      * THE FOLLOWING 2 TYPES DEFINE 31 DIGIT PACKED AND ZONED DECIMAL
       77 PD      COMP-3 PIC S9(31) 
                  VALUE 1234567890123456789012345678901.
       77 ZD             PIC S9(31) 
                  VALUE 1234567890123456789012345678901.
       77 GOOD    COMP-3 PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTBFP1 STARTING'
           DISPLAY 'HFP_SRT=' HFP-SRT
           DISPLAY 'HFP_LNG=' HFP-LNG
           DISPLAY 'HFP_EXT=' HFP-EXT
           DISPLAY 'BFP_SRT=' BFP-SRT
           DISPLAY 'BFP_LNG=' BFP-LNG
           DISPLAY 'BFP_EXT=' BFP-EXT
           DISPLAY 'DFP_SRT=' DFP-SRT
           DISPLAY 'DFP_LNG=' DFP-LNG
           DISPLAY 'DFP_EXT=' DFP-EXT
           DISPLAY 'HW=' HW
           DISPLAY 'FW=' FW
           DISPLAY 'DW=' DW
           DISPLAY 'QW=' QW
           DISPLAY 'PD=' PD
           DISPLAY 'ZD=' ZD
           IF HFP-SRT = 1234567
              PERFORM GOOD ELSE PERFORM BAD.
           IF HFP-LNG = 123456789012345
              PERFORM GOOD ELSE PERFORM BAD.
           IF HFP-EXT = 123456789012345678901234567890
              PERFORM GOOD ELSE PERFORM BAD.
           IF BFP-SRT = 1234567
              PERFORM GOOD ELSE PERFORM BAD.
           IF BFP-LNG = 1234567890123456
              PERFORM GOOD ELSE PERFORM BAD.
           IF BFP-EXT = 1234567890123456789012345678901234
              PERFORM GOOD ELSE PERFORM BAD.
           IF DFP-SRT = 1234567
              PERFORM GOOD ELSE PERFORM BAD.
           IF DFP-LNG = 1234567890123456
              PERFORM GOOD ELSE PERFORM BAD.
           IF DFP-EXT = 1234567890123456789012345678901234
              PERFORM GOOD ELSE PERFORM BAD.
           IF HW      = 1234
              PERFORM GOOD ELSE PERFORM BAD.
           IF FW      = 123456789
              PERFORM GOOD ELSE PERFORM BAD.
           IF DW      = 1234567890123456
              PERFORM GOOD ELSE PERFORM BAD.
           IF QW      = 1234567890123456789012345678901234
              PERFORM GOOD ELSE PERFORM BAD.
           IF PD      = 1234567890123456789012345678901
              PERFORM GOOD ELSE PERFORM BAD.
           IF ZD      = 1234567890123456789012345678901
              PERFORM GOOD ELSE PERFORM BAD.
      * TEST ADD
           MOVE       0 TO FLT-SRT
           ADD  HFP-SRT TO FLT-SRT
           IF   FLT-SRT = HFP-SRT  
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT-LNG
           ADD  HFP-LNG TO FLT-LNG
           IF   FLT-LNG = HFP-LNG           
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT-EXT
           ADD  HFP-EXT TO FLT-EXT
           IF   FLT-EXT = HFP-EXT  
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT_SRT
           ADD  BFP-SRT TO FLT-SRT
           IF   FLT-SRT = BFP_SRT
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT_LNG
           ADD  BFP-LNG TO FLT-LNG
           IF   FLT-LNG = BFP_LNG
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT_EXT
           ADD  BFP-EXT TO FLT-EXT
           IF   FLT-EXT = BFP_EXT
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT_SRT
           ADD  DFP-SRT TO FLT-SRT
           IF   FLT-SRT = DFP_SRT
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT_LNG
           ADD  DFP-LNG TO FLT-LNG
           IF   FLT-LNG = DFP_LNG
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT_EXT
           ADD  DFP-EXT TO FLT-EXT
           IF   FLT-EXT = DFP_EXT
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE       0 TO FLT-SRT
           ADD  HW      TO FLT-SRT
           IF   FLT-SRT = 1234
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-LNG
           ADD  HW      TO FLT-LNG
           IF   FLT-LNG = 1234
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-EXT
           ADD  HW      TO FLT-EXT
           IF   FLT-EXT = 1234      
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-LNG
           ADD  FW      TO FLT-LNG
           IF   FLT-LNG = 123456789
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-EXT
           ADD  FW      TO FLT-EXT
           IF   FLT-EXT = 123456789
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-LNG
           ADD  DW      TO FLT-LNG
           IF   FLT-LNG = 1234567890123456
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-EXT
           ADD  DW      TO FLT-EXT
           IF   FLT-EXT = 1234567890123456
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-EXT
           ADD  QW      TO FLT-EXT
           IF   FLT-EXT = QW         
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-EXT
           ADD  PD      TO FLT-EXT
           IF   FLT-EXT = PD                                
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE 0       TO FLT-EXT
           ADD  ZD      TO FLT-EXT
           IF   FLT-EXT = ZD                               
                PERFORM GOOD ELSE PERFORM BAD.
      * TEST SUBTRACT
           MOVE      HFP-SRT TO   FLT-SRT
           SUBTRACT  HFP-SRT FROM FLT-SRT
           IF   FLT-SRT = ZERO     
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      HFP-LNG TO   FLT-LNG
           SUBTRACT  HFP-LNG FROM FLT-LNG
           IF   FLT-LNG = ZERO                   
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      HFP-EXT TO   FLT-EXT
           SUBTRACT  HFP-EXT FROM FLT-EXT
           IF   FLT-EXT = ZERO                              
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      BFP-SRT TO   FLT-SRT
           SUBTRACT  BFP-SRT FROM FLT-SRT
           IF   FLT-SRT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      BFP-LNG TO   FLT-LNG
           SUBTRACT  BFP-LNG FROM FLT-LNG
           IF   FLT-LNG = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      BFP-EXT TO   FLT-EXT
           SUBTRACT  BFP-EXT FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      DFP-SRT TO   FLT-SRT
           SUBTRACT  DFP-SRT FROM FLT-SRT
           IF   FLT-SRT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      DFP-LNG TO   FLT-LNG
           SUBTRACT  DFP-LNG FROM FLT-LNG
           IF   FLT-LNG = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      DFP-EXT TO   FLT-EXT
           SUBTRACT  DFP-EXT FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      HW      TO   FLT-SRT
           SUBTRACT  HW      FROM FLT-SRT
           IF   FLT-SRT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      HW      TO   FLT-LNG
           SUBTRACT  HW      FROM FLT-LNG
           IF   FLT-LNG = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      HW      TO   FLT-EXT
           SUBTRACT  HW      FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      FW      TO   FLT-LNG
           SUBTRACT  FW      FROM FLT-LNG
           IF   FLT-LNG = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      FW      TO   FLT-EXT
           SUBTRACT  FW      FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      DW      TO   FLT-LNG
           SUBTRACT  DW      FROM FLT-LNG
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      DW      TO   FLT-EXT
           SUBTRACT  DW      FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      QW      TO   FLT-EXT
           SUBTRACT  QW      FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      PD      TO   FLT-EXT
           SUBTRACT  PD      FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      ZD      TO   FLT-EXT
           SUBTRACT  ZD      FROM FLT-EXT
           IF   FLT-EXT = ZERO                                            
                PERFORM GOOD ELSE PERFORM BAD.
      * TEST MULTIPLY
           MOVE      10      TO   FLT-SRT
           MOVE      123456  TO   HFP-SRT
           MULTIPLY  HFP-SRT BY   FLT-SRT
           IF   FLT-SRT = 1234560
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10              TO   FLT-LNG
           MOVE      123456789012345 TO   HFP-LNG
           MULTIPLY  HFP-LNG         BY   FLT-LNG
           IF   FLT-LNG = 1234567890123450                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                            TO FLT-EXT
           MOVE      12345678901234567890123456789 TO HFP-EXT
           MULTIPLY  HFP-EXT                       BY FLT-EXT
           IF   FLT-EXT = 123456789012345678901234567890                                              
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10       TO   FLT-SRT
           MOVE      123456   TO   BFP-SRT
           MULTIPLY  BFP-SRT  BY   FLT-SRT
           IF   FLT-SRT = 1234560                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                TO   FLT-LNG
           MOVE      123456789012345   TO   BFP-LNG
           MULTIPLY  BFP-LNG           BY   FLT-LNG
           IF   FLT-LNG = 1234567890123450                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                TO FLT-EXT
           MOVE      123456789012345678901234567890123 TO BFP-EXT
           MULTIPLY  BFP-EXT                           BY FLT-EXT
           IF   FLT-EXT = 1234567890123456789012345678901230                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10      TO   FLT-SRT
           MOVE      123456   TO  DFP-SRT
           MULTIPLY  DFP-SRT BY   FLT-SRT
           IF   FLT-SRT = 1234560                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                TO   FLT-LNG
           MOVE      123456789012345   TO   DFP-LNG
           MULTIPLY  DFP-LNG           BY   FLT-LNG
           IF   FLT-LNG = 1234567890123450                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                TO FLT-EXT
           MOVE      123456789012345678901234567890123 TO DFP-EXT
           MULTIPLY  DFP-EXT                           BY FLT-EXT
           IF   FLT-EXT = 1234567890123456789012345678901230                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10      TO   FLT-SRT
           MOVE      12345   TO   HW      
           MULTIPLY  HW      BY   FLT-SRT
           IF   FLT-SRT = 123450                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10          TO   FLT-LNG
           MOVE      123456789   TO   FW      
           MULTIPLY  FW          BY   FLT-LNG
           IF   FLT-LNG = 1234567890                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                TO   FLT-LNG
           MOVE      123456789012345   TO   DW      
           MULTIPLY  DW                BY   FLT-LNG
           IF   FLT-LNG = 1234567890123450                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                TO FLT-EXT
           MOVE      123456789012345678901234567890123 TO QW      
           MULTIPLY  QW                                BY FLT-EXT
           IF   FLT-EXT = 1234567890123456789012345678901230                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                               TO   FLT-EXT
           MOVE      123456789012345678901234567890   TO   PD      
           MULTIPLY  PD                               BY   FLT-EXT
           IF   FLT-EXT = 1234567890123456789012345678900                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                               TO   FLT-EXT
           MOVE      123456789012345678901234567890   TO   ZD      
           MULTIPLY  ZD                               BY   FLT-EXT
           IF   FLT-EXT = 1234567890123456789012345678900                                          
                PERFORM GOOD ELSE PERFORM BAD.
      * TEST DIVIDE
           MOVE      10      TO   HFP-SRT
           MOVE      1234560 TO   FLT-SRT
           DIVIDE    HFP-SRT INTO FLT-SRT
           IF   FLT-SRT = 123456
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10               TO   HFP-LNG
           MOVE      1234567890123450 TO   FLT-LNG
           DIVIDE    HFP-LNG          INTO FLT-LNG
           IF   FLT-LNG = 123456789012345                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                 TO   HFP-EXT
           MOVE      1234567890123456789012345678901230 TO   FLT-EXT
           DIVIDE    HFP-EXT                            INTO FLT-EXT
           IF   FLT-EXT = 123456789012345678901234567890123                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10      TO   BFP-SRT
           MOVE      1234560 TO   FLT-SRT
           DIVIDE    BFP-SRT INTO FLT-SRT
           IF   FLT-SRT = 123456                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10               TO   BFP-LNG
           MOVE      1234567890123450 TO   FLT-LNG
           DIVIDE    BFP-LNG          INTO FLT-LNG
           IF   FLT-LNG = 123456789012345                                         
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                 TO   BFP-EXT
           MOVE      1234567890123456789012345678901230 TO   FLT-EXT
           DIVIDE    BFP-EXT                            INTO FLT-EXT
           IF   FLT-EXT = 123456789012345678901234567890123                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10       TO   DFP-SRT
           MOVE      1234560  TO   FLT-SRT
           DIVIDE    DFP-SRT  INTO FLT-SRT
           IF   FLT-SRT = 123456                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10               TO   DFP-LNG
           MOVE      1234567890123450 TO   FLT-LNG
           DIVIDE    DFP-LNG          INTO FLT-LNG
           IF   FLT-LNG = 123456789012345                                         
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                 TO   DFP-EXT
           MOVE      1234567890123456789012345678901230 TO   FLT-EXT
           DIVIDE    DFP-EXT                            INTO FLT-EXT 
           IF   FLT-EXT = 123456789012345678901234567890123                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10      TO   HW 
           MOVE      1234560 TO   FLT-SRT      
           DIVIDE    HW      INTO FLT-SRT
           IF   FLT-SRT = 123456                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10               TO   FW     
           MOVE      1234567890123450 TO   FLT-LNG 
           DIVIDE    FW               INTO FLT-LNG
           IF   FLT-LNG = 123456789012345                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10               TO   DW     
           MOVE      1234567890123450 TO   FLT-LNG 
           DIVIDE    DW               INTO FLT-LNG
           IF   FLT-LNG = 123456789012345                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                 TO   QW      
           MOVE      1234567890123456789012345678901230 TO   FLT-EXT 
           DIVIDE    QW                                 INTO FLT-EXT
           IF   FLT-EXT = 123456789012345678901234567890123                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                  TO   PD      
           MOVE      1234567890123456789012345678901230  TO   FLT-EXT 
           DIVIDE    PD                                  INTO FLT-EXT
           IF   FLT-EXT = 123456789012345678901234567890123                                          
                PERFORM GOOD ELSE PERFORM BAD.
           MOVE      10                                  TO   ZD     
           MOVE      1234567890123456789012345678901230  TO   FLT-EXT 
           DIVIDE    ZD                                  INTO FLT-EXT
           IF   FLT-EXT = 123456789012345678901234567890123                                          
                PERFORM GOOD ELSE PERFORM BAD.
           DISPLAY 'TESTBFP1 GOOD TESTS =' GOOD
           STOP RUN.
       GOOD.
           ADD 1 TO GOOD.
       BAD.
           MOVE 16 TO RETURN-CODE 
           STOP RUN.
