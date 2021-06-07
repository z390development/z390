      ***************************************************************
      * SOASTUB2.CPY code from linkage section to call to SOACMGR   
      ***************************************************************
           DISPLAY 'SOASTUB ' soa-msg-svc-name ' entered'.
           MOVE SOA-STUB-MSG-SEND-LEN TO SOA-MSG-LEN
           MOVE SOA-STUB-SVC-NAME     TO SOA-MSG-SVC-NAME
           CALL SOA-STUB-SVC-MGR USING SOA-MSG.
           IF SOA-MSG-LEN NOT = SOA-STUB-MSG-RECV-LEN
           OR SOA-MSG-SVC-NAME NOT = SOA-STUB-SVC-NAME
               DISPLAY 'SOACMGR ' SOA-STUB-SVC-MGR 
                                  SOA-STUB-SVC-NAME ' INVALID RECV MSG'
               GOBACK
           END-IF.
