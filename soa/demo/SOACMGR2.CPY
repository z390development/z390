      ***************************************************************
      * SOACMSG2.CPY section from generated WS to LINKAGE SECTION.     
      ***************************************************************
      *
      * data for EZASOKET service calls to send/receive soa-msg  
      *
       01  soc-function         pic x(16).
       01  maxsoc               pic 9(4)  binary.
       01  ident.
         05  tcpname            pic x(8).
         05  adsname            pic x(8).
       01  subtask              pic x(8).
       01  maxsno               pic 9(8)  binary.
       01  errno                pic 9(8)  binary.
       01  retcode              pic s9(8) binary.

       01  af                   pic 9(8) comp value 2.
       01  soctype              pic 9(8)  binary.
         88 stream              value 1.
         88 datagram            value 2.
       01  proto                pic 9(8)  binary.

       01  s                    pic 9(4)  binary.
       01  port-name.
         05  family             pic 9(4)  binary.
         05  port               pic 9(4)  binary.
         05  ip-address         pic 9(8)  binary.
         05  reserved           pic x(8).

       01  nbyte                pic 9(8)  binary.
      *01  buf                  pic x(159).

       01  flags                pic 9(8)  binary.
         88 no-flag             value 0.
         88 oob                 value 1.
         88 peek                value 2.


       01  hostnamelen          pic 9(8)  binary.
       01  hostname             pic x(24).
       01  hostent              pic 9(8)  binary.


       01  hostent-addr         pic 9(8)  binary.
       01  hostname-length      pic 9(4)  binary.
       01  hostname-value       pic x(255).
       01  hostalias-count      pic 9(4)  binary.
       01  hostalias-seq        pic 9(4)  binary.
       01  hostalias-length     pic 9(4)  binary.
       01  hostalias-value      pic x(255).
       01  hostaddr-type        pic 9(4)  binary.
       01  hostaddr-length      pic 9(4)  binary.
       01  hostaddr-count       pic 9(4)  binary.
       01  hostaddr-seq         pic 9(4)  binary.
       01  hostaddr-value       pic 9(8)  binary.

       01  ws-socket            pic 9(4)  binary.
       01  ws-ipaddress         pic 9(8) binary.
       01  ws-ipaddress-r1 redefines ws-ipaddress.
         05  ws-ipaddr-b1       pic x(1).
         05  ws-ipaddr-b2       pic x(1).
         05  ws-ipaddr-b3       pic x(1).
         05  ws-ipaddr-b4       pic x(1).
       01  ws-ip-wk1            pic 9(4) binary.
       01  ws-ip-wk1-r1 redefines ws-ip-wk1.
         05  ws-ip-wk1-b1       pic x(1).
         05  ws-ip-wk1-b2       pic x(1).
       01  ws-ip-addr.
         05  ws-ip-addr-n1      pic 9(3).
         05  ws-ip-addr-dot1    pic x(1).
         05  ws-ip-addr-n2      pic 9(3).
         05  ws-ip-addr-dot3    pic x(1).
         05  ws-ip-addr-n3      pic 9(3).
         05  ws-ip-addr-dot3    pic x(1).
         05  ws-ip-addr-n4      pic 9(3).

       01  ws-byte-count        pic 9(5).

       01  ws-info.
         05  ws-display-line    pic x(80).
