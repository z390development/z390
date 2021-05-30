      ***************************************************************
      * SOACMGR3.CPY section from end of LINKAGE section to end       
      ***************************************************************
       procedure division using soa-msg.
           if  first-call = 'T' 
               display 'SOACMGR connecting with host ' 
                       soa-host-name ' on port ' soa-host-port
               move soa-host-name  to hostname
               move soa-host-port  to port
               move 'F' to first-call
               perform open-client-socket
           end-if
           display 'SOACMGR ' soa-msg-svc-name ' request sent'
           call 'GETTIME' using micro-start.
           perform send-soa-request
           move zero to soa-msg-len
           perform receive-soa-reply
           call 'GETTIME' using micro-end,
           compute micro-sec = micro-end - micro-start.
           display 'SOACMGR ' soa-msg-svc-name
                   ' response time = ' micro-sec ' micro-sec'.
           goback.
       send-soa-request.
           move soa-msg-len to nbyte
           perform write-msg
           .
       receive-soa-reply.
           perform receive-msg
           display 'SOACMGR ' soa-msg-svc-name ' response received'
           .
       open-client-socket.
           perform initapi
           perform get-host-by-name
           perform open-socket
           perform connect-socket
           .
       initapi.
           move 'INITAPI Call' to ws-display-line.
           perform display-line.

           move 'INITAPI' to soc-function.
           move 10 to maxsoc.
           move 'TCP' to tcpname.
           move 'ADS' to adsname.
           move -1 to maxsno.
           move 0 to errno.
           move 0 to retcode.
           call 'ezasoket' using soc-function maxsoc ident subtask
                          maxsno errno retcode.
           if retcode is equal to 0
              move 'OK' to ws-display-line
              perform display-line
           else
              move 'INITAPI Failed' to ws-display-line
              move 16 to return-code
              perform display-line
              go to abort
           end-if.

       get-host-by-name.
           move 'GETHOSTBYNAME Call' to ws-display-line.
           perform display-line.
           move 'GETHOSTBYNAME' to soc-function.
           move soa-host-name to hostname
           move length of hostname to hostnamelen.
           move 0 to hostent.
           move 0 to retcode.
           call 'ezasoket' using soc-function 
                                 hostnamelen
                                 hostname
                                 hostent retcode.
           if retcode is equal to 0
              move 'OK' to ws-display-line
              perform display-line
           else
              move 'GETHOSTNAME Failed' to ws-display-line
              move 16 to return-code
              perform display-line
              go to abort
           end-if.
       open-socket.
           move 'SOCKET Call' to ws-display-line.
           perform display-line.
           move 'SOCKET' to soc-function.
           move soa-host-port to port.
           move 0 to proto.
           move 0 to errno.
           move 0 to retcode.
           call 'ezasoket' using soc-function af soctype
                           proto errno retcode.
           move retcode to ws-socket.
           if retcode is greater than  0
              move 'OK' to ws-display-line
              perform display-line
           else
              move 'SOCKET Failed' to ws-display-line
              move 16 to return-code
              perform display-line
              go to abort
           end-if.
       connect-socket.
           move 'CONNECT Call' to ws-display-line.
           perform display-line.
           move 'CONNECT' to soc-function.
           move ws-socket to s.
           move 2 to family.
           move '127.000.000.001' to ws-ip-addr.
           move ws-ip-addr-n1 to ws-ip-wk1
           move ws-ip-wk1-b2 to ws-ipaddr-b1.
           move ws-ip-addr-n2 to ws-ip-wk1
           move ws-ip-wk1-b2 to ws-ipaddr-b2.
           move ws-ip-addr-n3 to ws-ip-wk1
           move ws-ip-wk1-b2 to ws-ipaddr-b3.
           move ws-ip-addr-n4 to ws-ip-wk1
           move ws-ip-wk1-b2 to ws-ipaddr-b4.
           move ws-ipaddress to ip-address.
           move 0 to reserved.
           move 0 to errno.
           move 0 to retcode.
           call 'ezasoket' using soc-function s port-name errno retcode.
           if retcode is equal to 0
              move 'OK' to ws-display-line
              perform display-line
           else
              move 'CONNECT Failed' to ws-display-line
              move 16 to return-code
              perform display-line
              go to abort
           end-if.
       write-msg.
           move 'WRITE Call' to ws-display-line.
           perform display-line.
           move 'WRITE' to soc-function.
           move ws-socket to s.
           call 'ezasoket' using soc-function s 
                           soa-msg-len soa-msg 
                           errno retcode.

           if retcode is less than 0
              move 'WRITE Failed' to ws-display-line
              move 16 to return-code
              perform display-line
              go to abort
           else
              move 'OK' to ws-display-line
              perform display-line
              move retcode to ws-byte-count
              move spaces to ws-display-line
              string 'No of bytes sent: ' delimited by size
                     ws-byte-count delimited by size
                into ws-display-line
              perform display-line
           end-if.
       receive-msg.
           move 'RECV Call' to ws-display-line.
           perform display-line.
           move 'RECV' to soc-function.
           move ws-socket to s.
           move length of soa-msg to nbyte.
           move zeros to soa-msg.
           call 'ezasoket' using soc-function s flags 
                           nbyte soa-msg
                           errno retcode.
           if retcode is less than 0
              move 'RECV Failed' to ws-display-line
              move 16 to return-code
              perform display-line
              go to abort
           else
              if retcode is greater than 0
                 move 'OK' to ws-display-line
                 perform display-line
                 move retcode to ws-byte-count
                 move spaces to ws-display-line
                 string 'No of bytes received: ' delimited by size
                        ws-byte-count delimited by size
                   into ws-display-line
                 perform display-line
              end-if
           end-if.
       abort.
           move 'Client Message Manager Abort' to ws-display-line.
              move 16 to return-code
           perform display-line.
           goback.
       display-line section.
      *    display ws-display-line.
       display-line-exit.
           exit.

