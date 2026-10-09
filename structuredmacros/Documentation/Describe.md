# Structured Macros Description

This set of structured programming macros was created to make it as simple and straightforward as possible
to implement complex logic in an Assembly Language program. These macros can be processed using the IBM High Level Assembler.

Similar to other structured programming macros, this set processes logic phrases, each enclosed within a pair of parentheses,
connected by `AND` or `OR` conjunctions. Each logic requirement must be invoked by coding the `IF` macro with its operands.
Each logic phrase must begin with the operation code of an instruction that sets the system condition code, such as a `TM` instruction,
or an `LTR`, an `AP`, or a `CLC`, etc. Any machine language instruction that sets the condition code may be used.
For an instruction that requires two operands, the operation code must be followed by the first operand, the second operand,
and the extended condition code mnemonic required for the execution of the instructions comprising the body of the `IF` group.
For any instruction used, the first positional sub-parameter must be the op-code and the last must be the extended condition code mnemonic.
Between these two should be coded one value for each parameter required by the op-code being used:

```HLASM
         IF    (TM,ERRSW,X'08',Z)
           ...
         ENDIF

         IF    (SRP,SHIFTPACKED,3,5,P)
           …
         ENDIF 

         IF    (TS,SPECIAL,Z)
           …
         ENDIF 

         LG    R1,=X'0123456789ABCDEF'
         LG    R2,=X'FEDCBA9876543210'
RISBHGZ010 IF (RISBHGZ,R1,R2,16,31,Z)
           AP    LEVEL1_PASS_COUNTER,=P'1'
         ENDIF  ,

         LG    R5,=C'12345672'
         LG    R6,=C'ABCDEFGC'
RNSBG010 IF    (RNSBG,R5,R6,56,7,0,P)
           AP    LEVEL1_PASS_COUNTER,=P'1'
         ENDIF  ,
```

In the following example the first test must result in a zero and the second in an equal, or the third in a zero.
If more than one condition is to be tested, then each phrase except the last in the list must be followed by an `AND` or an `OR`:

```HLASM
                                                                      COL 72
                                                                           |
                                                                           V
 
         IF    (TM,ERRSW,X'08',Z),AND,                                     C
               (CLC,TEST,=C'PASS',E),OR,                                   C
               (LTR,R15,R15,Z)
           ...
         ENDIF
```

The real power of this macro system is its ability for the user to form complex constructs.
Since the evaluation of `AND` in a logical expression takes precedence over `OR`,
the programmer/analyst may need to override that precedence to force the `OR` conjunction(s) to take precedence over the `AND`.
She would enclose the two or more phrases joined by an `OR` with an additional pair of parentheses.
In the following example the body of the IF-GROUP will execute if the first phrase is true AND if either of the next two phrases is true:

```HLASM
                                |                            |
                                |                            |
                                |                            |
                                V                            V
         IF    (CLC,A,Z,NE),AND,((CLC,B,Z,NE),OR,(CLC,C,Z,NE))

              ONE OR MORE INSTRUCTIONS
              TO BE EXECUTED IF THE ABOVE
              LOGIC GROUP EVALUATES TO "TRUE"

         ENDIF
```

Examples of the assembled code follow:

```HLASM
                                     298 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
                                     299 *                                                                     *
                                     300 *      SINCE THE EVALUATION OF "AND" IN A LOGICAL EXPRESSION TAKES    *
                                     301 * PRECEDENCE OVER "OR", YOU MAY NEED TO OVERRIDE THAT PRECEDENCE TO   *
                                     302 * FORCE AN "OR" CONJUNCTION TO BE EVALUATED BEFORE AN "AND". YOU      *
                                     303 * WOULD ENCLOSE THE TWO EXPRESSIONS JOINED BY AN "OR" WITH AN         *
                                     304 * ADDITIONAL PAIR OF PARENTHESES:                                     *
                                     305 *                                                                     *
                                     306 *                             |                            |          *
                                     307 *                             |                            |          *
                                     308 *                             |                            |          *
                                     309 *                             V                            V          *
                                     310 *      IF    (CLC,A,Z,NE),AND,((CLC,B,Z,NE),OR,(CLC,C,Z,NE))          *
                                     311 *                                                                     *
                                     312 *               ONE OR MORE INSTRUCTIONS                              *
                                     313 *               TO BE EXECUTED IF THE ABOVE                           *
                                     314 *               LOGIC GROUP EVALUATES TO "TRUE"                       *
                                     315 *                                                                     *
                                     316 *      ENDIF                                                          *
                                     317 *                                                                     *
                                     318 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
                                     319 *        1         2         3         4         5         6         7
                                     320 *...V....0....V....0....V....0....V....0....V....0....V....0....V....0.
                                     321 *
                                     322 *                               V<<<<<EXTRA PARENTHESES:>>>>>V
                                     323 *                               V                            V
                                     324          IF    (CLC,A,Z,NE),AND,((CLC,B,Z,NE),OR,(CLC,C,Z,NE))
 000120 D503 C338 C368 00338 00368   325+         CLC   A,Z            
 000126 A784 000F            00144   326+         JE    $MDF19               
 00012A D503 C33C C368 0033C 00368   328+         CLC   B,Z            
 000130 A774 0007            0013E   329+         JNE   $MDT20          
 000134 D503 C340 C368 00340 00368   330+         CLC   C,Z            
 00013A A784 0005            00144   331+         JE    $MDF19                                         
 00013E                              332+$MDT20   DC    0H'0'                                           
 00013E FA30 C308 C2F8 00308 002F8   333            AP    LEVEL1_PASS_COUNTER,=P'1'
                                     334          ENDIF
 000144                              335+$MDF19   DC    0H'0'                                      
                                     336 *
                                     337 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
1IFDOC    EXAMPLES OF "IF" LOGIC MACRO INSTRUCTIONS                                                             Page   10
   Active Usings: IFDOC,R12
0  Loc  Object Code    Addr1 Addr2  Stmt   Source Statement                                  HLASM R6.0  2020/09/06 03.40
0                                    338 *                                                                     *
                                     339 *      IF (A OR B) AND (C OR D)                                       *
                                     340 *                                                                     *
                                     341 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
                                     342 *        1         2         3         4         5         6         7
                                     343 *...V....0....V....0....V....0....V....0....V....0....V....0....V....0.
                                     344          IF    ((CLC,A,Z,NE),OR,(CLC,B,Z,NE)),AND,                     C
                                                        ((CLC,C,Z,NE),OR,(CLC,D,Z,NE))
 000144 D503 C338 C368 00338 00368   346+         CLC   A,Z           
 00014A A774 0007            00158   347+         JNE   $MDT21           
 00014E D503 C33C C368 0033C 00368   348+         CLC   B,Z            
 000154 A784 000F            00172   349+         JE    $MDF22                
 000158 D503 C340 C368 00340 00368   351+$MDT21   CLC   C,Z            
 00015E A774 0007            0016C   352+         JNE   $MDT23           
 000162 D503 C344 C368 00344 00368   353+         CLC   D,Z            
 000168 A784 0005            00172   354+         JE    $MDF22                                         
 00016C                              355+$MDT23   DC    0H'0'                                          
 00016C FA30 C308 C2F8 00308 002F8   356            AP    LEVEL1_PASS_COUNTER,=P'1'
                                     357          ENDIF
 000172                              358+$MDF22   DC    0H'0'                                        
 ```

If the user has the need to add additional logic within the existing phrases already surrounded by sets of double parentheses,
then she would simply enclose another group of phrases within an additional set of double parentheses, ad infinitum:

```HLASM
                                     633 *
                                     634 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
                                     635 *                                                                     *
                                     636 * IF A | B & ( C | D & ( E | F & G ) & H ) & J                        *
                                     637 *                                                                     *
                                     638 *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
                                     639 *        1         2         3         4         5         6         7
                                     640 *...V....0....V....0....V....0....V....0....V....0....V....0....V....0.
                                     641          IF    (CLC,A,Z,E),OR,(CLC,B,Z,E),AND,((CLC,C,Z,E),OR,         C
                                                        (CLC,D,Z,E),AND,((CLC,E,Z,E),OR,                        C
                                                        (CLC,F,Z,E),AND,(CLC,G,Z,E)),AND,                       C
                                                        (CLC,H,Z,E)),AND,(CLC,J,Z,E)
 00022C D503 C401 C431 00401 00431   643+         CLC   A,Z
 000232 A784 002A            00286   646+         JE    $MDT36
 000236 D503 C405 C431 00405 00431   648+         CLC   B,Z
 00023C A774 0028            0028C   651+         JNE   $MDF37
 000240 D503 C409 C431 00409 00431   656+         CLC   C,Z
 000246 A784 001B            0027C   659+         JE    $MDT38
 00024A D503 C40D C431 0040D 00431   661+         CLC   D,Z
 000250 A774 001E            0028C   664+         JNE   $MDF39
 000254 D503 C411 C431 00411 00431   669+         CLC   E,Z
 00025A A784 000C            00272   672+         JE    $MDT40
 00025E D503 C415 C431 00415 00431   674+         CLC   F,Z
 000264 A774 0014            0028C   677+         JNE   $MDF41
 000268 D503 C419 C431 00419 00431   679+         CLC   G,Z
 00026E A774 000F            0028C   684+         JNE   $MDF39
 000272 D503 C41D C431 0041D 00431   686+$MDT40   CLC   H,Z
 000278 A774 000A            0028C   691+         JNE   $MDF37
 00027C D503 C425 C431 00425 00431   693+$MDT38   CLC   J,Z
 000282 A774 0005            0028C   697+         JNE   $MDF37
 000286                              698+$MDT36   DC    0H'0'
 000286 FA30 C3D0 C3C4 003D0 003C4   699            AP    LEVEL1_PASS_COUNTER,=P'1'
                                     700          ENDIF
 00028C                              702+$MDF37   DC    0H'0'
                       0028C         703+$MDF39   EQU   $MDF37
                       0028C         704+$MDF41   EQU   $MDF39
```
