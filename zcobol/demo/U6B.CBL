0*2830     IF GLOB-NAME = '123456789012345678901234567890'
0*****         MOVE 0         TO SUBS-NUM
0*****         MOVE GLOB-NAME TO TEST-FIELD
0*****                           FUB-NAME0
0*****         PERFORM FIND-LENGTH
0*****         MOVE FUB TO LEN0 KEEP-LEN0
0*****
0*****         IF  GLOB-SUB1 = SPACES
0*****         AND GLOB-SUB2 = SPACES
0*****         AND GLOB-SUB3 = SPACES
0*****             MOVE 'Y' TO XOK
0*****         ELSE
0*****             DISPLAY GLOB-NAME 
0*****                   ' SHOULD NOT HAVE SUBSCRIPTS'
0*****             MOVE 'N' TO XOK
0*****         END-IF
0*****         IF XOK = 'Y'
0*****         IF  MONITORING = 'Y'
0*****           IF DIZPLAY = 'Y'
0*****            MOVE SPACES        TO MONITORIT-80
0*****            MOVE 'MONITORING ' TO MONITORIT-80
0*****            MOVE FUB-NAME0 (1:KEEP-LEN0)
0*****                               TO MONITORIT-80 (12:LEN0)
0*****            ADD  12            TO LEN0
0*****            MOVE ' AT LINE '   TO MONITORIT-80 (LEN0:9)
0*****            ADD   9            TO LEN0
0*****            MOVE LINE-NUMBER (LINE-COUNT2)
0*****                               TO MONITORIT-80 (LEN0:6)
0*****            DISPLAY MONITORIT-80 (1:72)
0*2830            MOVE
0*****             TO LUMP
0*****           ELSE
0*2830            MOVE
0*****             TO LUMP
0*****           END-IF
0*****         ELSE
0*****            DISPLAY FUB-NAME0 
0*2830            MOVE
0*****              TO LUMP
0*****         END-IF
0*****         END-IF
0*****     END-IF.
           
1*2830     IF GLOB-NAME = '123456789012345678901234567890'
1*****        MOVE 1         TO SUBS-NUM
1*****        MOVE GLOB-NAME TO TEST-FIELD
1*****                          FUB-NAME0
1*****        PERFORM FIND-LENGTH
1*****        MOVE FUB TO LEN0 KEEP-LEN0
1*****     
1*****        IF  GLOB-SUB1 NOT = SPACES
1*****        AND GLOB-SUB2 = SPACES
1*****        AND GLOB-SUB3 = SPACES
1*****            MOVE 'Y' TO XOK
1*****        ELSE
1*****            DISPLAY GLOB-NAME 
1*****                  ' SHOULD HAVE 1 SUBSCRIPT'
1*****            MOVE 'N' TO XOK
1*****        END-IF
1*****        IF XOK = 'Y'
1*****        MOVE GLOB-SUB1 TO TEST-FIELD
1*****                          FUB-NAME1
1*****        PERFORM FIND-LENGTH
1*****        MOVE FUB TO LEN1
1*****        
1*****        IF  GLOB-SUB1 (1:1) NOT LESS    THAN '0'
1*****        AND GLOB-SUB1 (1:1) NOT GREATER THAN '9'
1*****           MOVE GLOB-SUB1 (1:6) TO DISP-SUB1
1*****        ELSE
1*****           MOVE GLOB-SUB1 TO TEMP-NAME
1*****           PERFORM SUBS-PROCESS
1*****           MOVE NINE06    TO DISP-SUB1
1*****        END-IF
1*****        
1*****        MOVE DISP-SUB1 TO TEST-FIELD
1*****                          FUB-NAME2
1*****        PERFORM FIND-LENGTH
1*****        MOVE FUB TO LEN2
1*****  
1*****        IF  MONITORING = 'Y'      
1*****          IF DIZPLAY = 'Y'
1*****           MOVE SPACES        TO MONITORIT-80
1*****           MOVE 'MONITORING ' TO MONITORIT-80
1*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
1*****                              TO MONITORIT-80 (12:KEEP-LEN0)
1*****           ADD  12            TO LEN0
1*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
1*****           ADD   1            TO LEN0
1*****           MOVE FUB-NAME1 (1:LEN1)
1*****                             TO MONITORIT-80 (LEN0:LEN1)
1*****           ADD LEN1           TO LEN0
1*****           MOVE ') AT LINE '  TO MONITORIT-80 (LEN0:10)
1*****           ADD  10            TO LEN0
1*****           MOVE LINE-NUMBER (LINE-COUNT2)
1*****                              TO MONITORIT-80 (LEN0:6)
1*****           DISPLAY MONITORIT-80 (1:72)
1*****           MOVE SPACES        TO MONITORIT-80
1*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
1*****                              TO MONITORIT-80 (1:KEEP-LEN0)
1*****           MOVE KEEP-LEN0     TO LEN0
1*****           ADD   1            TO LEN0
1*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
1*****           ADD   1            TO LEN0
1*****           MOVE FUB-NAME2 (1:LEN2)
1*****                              TO MONITORIT-80 (LEN0:LEN2)
1*****           ADD LEN2           TO LEN0
1*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
1*****           DISPLAY MONITORIT-80 (1:72)
1*2830           MOVE 
1*****                   (DISP-SUB1)
1*****             TO LUMP
1*****          ELSE
1*2830           MOVE
1*****                   (DISP-SUB1)
1*****             TO LUMP
1*****          END-IF               
1*****        ELSE
1*****           MOVE SPACES        TO MONITORIT-80
1*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
1*****                              TO MONITORIT-80 (1:KEEP-LEN0)
1*****           ADD   1            TO LEN0
1*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
1*****           ADD   1            TO LEN0
1*****           MOVE FUB-NAME1 (1:LEN1)
1*****                              TO MONITORIT-80 (LEN0:LEN1)
1*****           ADD LEN1           TO LEN0
1*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
1*****           DISPLAY MONITORIT-80 (1:72)
1*****           MOVE SPACES        TO MONITORIT-80
1*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
1*****                              TO MONITORIT-80 (1:KEEP-LEN0)
1*****           MOVE KEEP-LEN0     TO LEN0
1*****           ADD   1            TO LEN0
1*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
1*****           ADD   1            TO LEN0
1*****           MOVE FUB-NAME2 (1:LEN2)
1*****                              TO MONITORIT-80 (LEN0:LEN2)
1*****           ADD LEN2           TO LEN0
1*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
1*****           DISPLAY MONITORIT-80 (1:72)
1*2830           MOVE 
1*****                   (DISP-SUB1)
1*****             TO LUMP
1*****        END-IF
1*****        END-IF 
1*****     END-IF.        
          
2*2830     IF GLOB-NAME = '123456789012345678901234567890'
2*****        MOVE 2         TO SUBS-NUM
2*****        MOVE GLOB-NAME TO TEST-FIELD
2*****                          FUB-NAME0
2*****        PERFORM FIND-LENGTH
2*****        MOVE FUB TO LEN0 KEEP-LEN0
2*****
2*****        IF  GLOB-SUB1 NOT = SPACES
2*****        AND GLOB-SUB2 NOT = SPACES
2*****        AND GLOB-SUB3 = SPACES
2*****            MOVE 'Y' TO XOK
2*****        ELSE
2*****            DISPLAY GLOB-NAME 
2*****                  ' SHOULD HAVE 2 SUBSCRIPTS'
2*****            MOVE 'N' TO XOK
2*****        END-IF
2*****        IF XOK = 'Y'  
2*****        MOVE GLOB-SUB1 TO TEST-FIELD
2*****                          FUB-NAME1
2*****        PERFORM FIND-LENGTH
2*****        MOVE FUB TO LEN1
2*****        
2*****        MOVE GLOB-SUB2 TO TEST-FIELD
2*****                          FUB-NAME2
2*****        PERFORM FIND-LENGTH
2*****        MOVE FUB TO LEN2
2*****        
2*****        IF  GLOB-SUB1 (1:1) NOT LESS    THAN '0'
2*****        AND GLOB-SUB1 (1:1) NOT GREATER THAN '9'
2*****            MOVE GLOB-SUB1 (1:6) TO DISP-SUB1
2*****        ELSE
2*****            MOVE GLOB-SUB1 TO TEMP-NAME
2*****            PERFORM SUBS-PROCESS
2*****            MOVE NINE06    TO DISP-SUB1
2*****        END-IF
2*****        
2*****        MOVE DISP-SUB1 TO TEST-FIELD
2*****                          FUB-NAME3
2*****        PERFORM FIND-LENGTH
2*****        MOVE FUB TO LEN3
2*****        
2*****        IF  GLOB-SUB2 (1:1) NOT LESS    THAN '0'
2*****        AND GLOB-SUB2 (1:1) NOT GREATER THAN '9'
2*****            MOVE GLOB-SUB2 (1:6) TO DISP-SUB2
2*****        ELSE
2*****            MOVE GLOB-SUB2 TO TEMP-NAME
2*****            PERFORM SUBS-PROCESS
2*****            MOVE NINE06    TO DISP-SUB2
2*****        END-IF
2*****        
2*****        MOVE DISP-SUB2 TO TEST-FIELD
2*****                          FUB-NAME4
2*****        PERFORM FIND-LENGTH
2*****        MOVE FUB TO LEN4
2*****    
2*****        IF  MONITORING = 'Y'    
2*****          IF DIZPLAY = 'Y'
2*****           MOVE SPACES        TO MONITORIT-80
2*****           MOVE 'MONITORING ' TO MONITORIT-80
2*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
2*****                              TO MONITORIT-80 (12:KEEP-LEN0)
2*****           ADD  12            TO LEN0
2*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
2*****           ADD   1            TO LEN0
2*****           MOVE FUB-NAME1 (1:LEN1)
2*****                              TO MONITORIT-80 (LEN0:LEN1)
2*****           ADD LEN1           TO LEN0
2*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
2*****           ADD   2            TO LEN0
2*****           MOVE FUB-NAME2 (1:LEN2)
2*****                              TO MONITORIT-80 (LEN0:LEN2)
2*****           ADD LEN2           TO LEN0
2*****           MOVE ') AT LINE '  TO MONITORIT-80 (LEN0:10)
2*****           ADD  10            TO LEN0
2*****           MOVE LINE-NUMBER (LINE-COUNT2)
2*****                              TO MONITORIT-80 (LEN0:6)
2*****           DISPLAY MONITORIT-80 (1:72)
2*****           MOVE SPACES        TO MONITORIT-80
2*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
2*****                              TO MONITORIT-80 (1:KEEP-LEN0)
2*****           MOVE KEEP-LEN0     TO LEN0
2*****           ADD   1            TO LEN0
2*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
2*****           ADD   1            TO LEN0
2*****           MOVE FUB-NAME3 (1:LEN3)
2*****                              TO MONITORIT-80 (LEN0:LEN3)
2*****           ADD LEN3           TO LEN0
2*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
2*****           ADD   2            TO LEN0
2*****           MOVE FUB-NAME4 (1:LEN4)
2*****                              TO MONITORIT-80 (LEN0:LEN4)
2*****           ADD LEN4           TO LEN0
2*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
2*****           DISPLAY MONITORIT-80 (1:72)
2*2830           MOVE 
2*****                  (DISP-SUB1, DISP-SUB2)
2*****             TO LUMP
2*****          ELSE
2*2830           MOVE
2*****                  (DISP-SUB1, DISP-SUB2)
2*****             TO LUMP
2*****          END-IF
2*****        ELSE
2*****           MOVE SPACES        TO MONITORIT-80
2*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
2*****                              TO MONITORIT-80 (1:KEEP-LEN0)
2*****           ADD   1            TO LEN0
2*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
2*****           ADD   1            TO LEN0
2*****           MOVE FUB-NAME1 (1:LEN1)
2*****                              TO MONITORIT-80 (LEN0:LEN1)
2*****           ADD LEN1           TO LEN0
2*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
2*****           ADD   2            TO LEN0
2*****           MOVE FUB-NAME2 (1:LEN2)
2*****                              TO MONITORIT-80 (LEN0:LEN2)
2*****           ADD LEN2           TO LEN0
2*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
2*****           DISPLAY MONITORIT-80 (1:72)
2*****           MOVE SPACES        TO MONITORIT-80
2*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
2*****                              TO MONITORIT-80 (1:KEEP-LEN0)
2*****           MOVE KEEP-LEN0     TO LEN0
2*****           ADD   1            TO LEN0
2*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
2*****           ADD   1            TO LEN0
2*****           MOVE FUB-NAME3 (1:LEN3)
2*****                              TO MONITORIT-80 (LEN0:LEN3)
2*****           ADD LEN3           TO LEN0
2*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
2*****           ADD   2            TO LEN0
2*****           MOVE FUB-NAME4 (1:LEN4)
2*****                              TO MONITORIT-80 (LEN0:LEN4)
2*****           ADD LEN4           TO LEN0
2*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
2*****           DISPLAY MONITORIT-80 (1:72)
2*2830           MOVE 
2*****                  (DISP-SUB1, DISP-SUB2)
2*****             TO LUMP
2*****        END-IF
2*****        END-IF
2*****     END-IF.        
                 
3*2830     IF GLOB-NAME = '123456789012345678901234567890'
3*****        MOVE 3         TO SUBS-NUM
3*****        MOVE GLOB-NAME TO TEST-FIELD
3*****                          FUB-NAME0
3*****        PERFORM FIND-LENGTH
3*****        MOVE FUB TO LEN0 KEEP-LEN0
3*****
3*****        IF  GLOB-SUB1 NOT = SPACES
3*****        AND GLOB-SUB2 NOT = SPACES
3*****        AND GLOB-SUB3 NOT = SPACES
3*****            MOVE 'Y' TO XOK
3*****        ELSE
3*****            DISPLAY GLOB-NAME 
3*****                  ' SHOULD HAVE 3 SUBSCRIPTS'
3*****            MOVE 'N' TO XOK
3*****        END-IF        
3*****        IF XOK = 'Y'
3*****        MOVE GLOB-SUB1 TO TEST-FIELD
3*****                          FUB-NAME1
3*****        PERFORM FIND-LENGTH
3*****        MOVE FUB TO LEN1
3*****        
3*****        MOVE GLOB-SUB2 TO TEST-FIELD
3*****                          FUB-NAME2
3*****        PERFORM FIND-LENGTH
3*****        MOVE FUB TO LEN2
3*****        
3*****        MOVE GLOB-SUB3 TO TEST-FIELD
3*****                          FUB-NAME3
3*****        PERFORM FIND-LENGTH
3*****        MOVE FUB TO LEN3
3*****        
3*****        IF  GLOB-SUB1 (1:1) NOT LESS    THAN '0'
3*****        AND GLOB-SUB1 (1:1) NOT GREATER THAN '9'
3*****            MOVE GLOB-SUB1 (1:6) TO DISP-SUB1
3*****        ELSE
3*****            MOVE GLOB-SUB1 TO TEMP-NAME
3*****            PERFORM SUBS-PROCESS
3*****            MOVE NINE06    TO DISP-SUB1
3*****        END-IF
3*****        
3*****        MOVE DISP-SUB1 TO TEST-FIELD
3*****                          FUB-NAME4
3*****        PERFORM FIND-LENGTH
3*****        MOVE FUB TO LEN4
3*****        
3*****        IF  GLOB-SUB2 (1:1) NOT LESS    THAN '0'
3*****        AND GLOB-SUB2 (1:1) NOT GREATER THAN '9'
3*****            MOVE GLOB-SUB2 (1:6) TO DISP-SUB2
3*****        ELSE
3*****            MOVE GLOB-SUB2 TO TEMP-NAME
3*****            PERFORM SUBS-PROCESS
3*****            MOVE NINE06    TO DISP-SUB2
3*****        END-IF
3*****        
3*****        MOVE DISP-SUB2 TO TEST-FIELD
3*****                          FUB-NAME5
3*****        PERFORM FIND-LENGTH
3*****        MOVE FUB TO LEN5
3*****        
3*****        IF  GLOB-SUB3 (1:1) NOT LESS    THAN '0'
3*****        AND GLOB-SUB3 (1:1) NOT GREATER THAN '9'
3*****            MOVE GLOB-SUB3 (1:6) TO DISP-SUB3
3*****        ELSE
3*****            MOVE GLOB-SUB3 TO TEMP-NAME
3*****            PERFORM SUBS-PROCESS
3*****            MOVE NINE06    TO DISP-SUB3
3*****        END-IF
3*****        
3*****        MOVE DISP-SUB3 TO TEST-FIELD
3*****                          FUB-NAME6
3*****        PERFORM FIND-LENGTH
3*****        MOVE FUB TO LEN6
3*****        
3*****        IF  MONITORING = 'Y' 
3*****          IF DIZPLAY = 'Y'
3*****           MOVE SPACES        TO MONITORIT-80
3*****           MOVE 'MONITORING ' TO MONITORIT-80
3*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
3*****                              TO MONITORIT-80 (12:KEEP-LEN0)
3*****           ADD  12            TO LEN0
3*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
3*****           ADD   1            TO LEN0
3*****           MOVE FUB-NAME1 (1:LEN1)
3*****                              TO MONITORIT-80 (LEN0:LEN1)
3*****           ADD LEN1           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME2 (1:LEN2)
3*****                              TO MONITORIT-80 (LEN0:LEN2)
3*****           ADD LEN2           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME3 (1:LEN3)
3*****                              TO MONITORIT-80 (LEN0:LEN3)
3*****           ADD LEN3           TO LEN0
3*****           MOVE ') AT LINE '  TO MONITORIT-80 (LEN0:10)
3*****           ADD  10            TO LEN0
3*****           MOVE LINE-NUMBER (LINE-COUNT2)
3*****                              TO MONITORIT-80 (LEN0:6)
3*****           DISPLAY MONITORIT-80 (1:72)
3*****           MOVE SPACES        TO MONITORIT-80
3*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
3*****                              TO MONITORIT-80 (1:KEEP-LEN0)
3*****           MOVE KEEP-LEN0     TO LEN0
3*****           ADD   1            TO LEN0
3*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
3*****           ADD   1            TO LEN0
3*****           MOVE FUB-NAME4 (1:LEN4)
3*****                              TO MONITORIT-80 (LEN0:LEN4)
3*****           ADD LEN4           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME5 (1:LEN5)
3*****                              TO MONITORIT-80 (LEN0:LEN5)
3*****           ADD LEN5           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME6 (1:LEN6)
3*****                              TO MONITORIT-80 (LEN0:LEN6)
3*****           ADD LEN6           TO LEN0
3*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
3*****           DISPLAY MONITORIT-80 (1:72)
3*2830           MOVE 
3*****                  (DISP-SUB1, DISP-SUB2, DISP-SUB3)
3*****             TO LUMP
3*****          ELSE
3*2830           MOVE
3*****                  (DISP-SUB1, DISP-SUB2, DISP-SUB3)
3*****             TO LUMP
3*****          END-IF
3*****        ELSE
3*****           MOVE SPACES        TO MONITORIT-80
3*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
3*****                              TO MONITORIT-80 (1:KEEP-LEN0)
3*****           ADD   1            TO LEN0
3*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
3*****           ADD   1            TO LEN0
3*****           MOVE FUB-NAME1 (1:LEN1)
3*****                              TO MONITORIT-80 (LEN0:LEN1)
3*****           ADD LEN1           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME2 (1:LEN2)
3*****                              TO MONITORIT-80 (LEN0:LEN2)
3*****           ADD LEN2           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME3 (1:LEN3)
3*****                              TO MONITORIT-80 (LEN0:LEN3)
3*****           ADD LEN3           TO LEN0
3*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
3*****           DISPLAY MONITORIT-80 (1:72)
3*****           MOVE SPACES        TO MONITORIT-80
3*****           MOVE FUB-NAME0 (1:KEEP-LEN0)
3*****                              TO MONITORIT-80 (1:KEEP-LEN0)
3*****           MOVE KEEP-LEN0     TO LEN0
3*****           ADD   1            TO LEN0
3*****           MOVE '('           TO MONITORIT-80 (LEN0:1)
3*****           ADD   1            TO LEN0
3*****           MOVE FUB-NAME4 (1:LEN4)
3*****                              TO MONITORIT-80 (LEN0:LEN4)
3*****           ADD LEN4           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME5 (1:LEN5)
3*****                              TO MONITORIT-80 (LEN0:LEN5)
3*****           ADD LEN5           TO LEN0
3*****           MOVE ', '          TO MONITORIT-80 (LEN0:2)
3*****           ADD   2            TO LEN0
3*****           MOVE FUB-NAME6 (1:LEN6)
3*****                              TO MONITORIT-80 (LEN0:LEN6)
3*****           ADD LEN6           TO LEN0
3*****           MOVE ')'           TO MONITORIT-80 (LEN0:1)
3*****           DISPLAY MONITORIT-80 (1:72)
3*2830           MOVE 
3*****                  (DISP-SUB1, DISP-SUB2, DISP-SUB3)
3*****             TO LUMP
3*****        END-IF
3*****        END-IF
3*****     END-IF.