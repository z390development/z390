# SOA services

The z390 project supports low level Service Oriented Architecture (SOA) type 
application generation and execution.

The advantage of using the SOA architecture is that services are more easily shared
across diverse applications and user networks and maintenance is simpler since server
code does not need to be statically linked into client application code.

Any z390 application program can open up to 10 different client TCP/IP ports and 
10 different TCP/IP server ports. 

The server can support up to a total of 20 concurrent connections from client. 

z390 TCP/IP client and server programs can interact with clients or server 
applications written in any language supporting compatible messaging via TCP/IP 
sockets.

## Macro reference

### SVC functions

The z390 macro TCPIO invoking svc x’7C’ supports TCP/IP sockets messaging
between client and server components.

### TCPIO - TCP/IP operations {: #TCPIO}

The TCPIO macro operations OPEN, CLOSE, SEND, and RECEIVE support
messaging between client and server programs using TCP/IP sockets. 

* Messages of any length can be exchanges between any client and server programs 
  on a TCP/IP network.
* The server program must first open a socket port. 
* Up to 20 concurrent clients can open connections to any server port. 
* The same program can have up to 10 server ports and 10 client ports open.
* This command is similar to the functions provided by the IBM z/OS Communication 
  Server macro `EZASMI`

####  OPEN

```hlasm
label    TCPIO OPEN,PORT=port,HOST=host
```

* The port can be any standard port number less than 1024 or any private port 
  number above 1024.
* The port number can be numeric constant, symbolic absolute value, or can be 
  specified in (register). 
* The only requirement is that port numbers not conflict with other port numbers 
  being used on the same network with the same processor hosts. 
* If the HOST= keyword parameter is coded specifying a specific host IP address 
  such as 162.692.1.3 or * for the current processor, then a client port will be 
  opened with a connection to the server port on the indicated processor. 
* If a connection to the specified server port cannot be made, then a return 
  code of 12 will be set. 
* If the HOST= parameter is omitted, then a server port will be opened on the 
  current processor which can handle up to 20 concurrent connections from client 
  ports on the network.

#### CLOSE 

```hlasm
label    TCPIO CLOSE,PORT=port
```

Close the specified port. 

!!! Info 
    You cannot open a client and sever port with the same number so 
    there is no need to indicate which type it is. 

* When a server port is closed, all associated port and connection threads are 
  also terminated. 
* All ports are automatically closed at program termination if not closed 
  explicitly.

#### SEND

```hlasm
label    TCPIO SEND,PORT=port,MSG=addr,LMSG=length
```

Send the message with specified address and length to the specified port.

* The message starting address can be RX type label or can be specified as (register).
* The message length can be absolute value or (register).
* If the send fails, a return code of 12 will be set.

#### RECEIVE

```hlasm
label    TCPIO RECEIVE[,NOWAIT],PORT=port,MSG=addr,LMSG=max-length[,CONN=id]
```

Receive a message from the specified port starting at address with length up to max length. 

* The message starting address can be RX type label or can be specified as (register).
* The message maximum length can be absolute value or (register).
* If the optional second positional parameter NOWAIT is specified, a return code 
  of 4 will be returned if no message is ready otherwise the RECEIVE will wait 
  until at least 1 byte of the message is available.
* If the port is a server port then the optional keyword parameter CONN=id may 
  be specified indicating a specific connection that was previously returned by 
  prior TCPIO RECEIVE in register 2.
* If a CONN value of -1 is specified or the parameter is omitted, the next 
  message from any connection will be returned along with the connection id in 
  register 2.
* At least 1 byte will be returned on a successful RECEIVE with return code 0 
  along with the number of bytes returned in register 1. Up to the max-length 
  bytes may be returned. 
* If more than one message arrives prior to RECEIVE, it is up to the user to 
  determine where one message ends and next message starts. 
    * A 4 byte message length prefix can be used to determine each message length. 
    * If passing ASCII text, use ending line feed character (hex x’0A’).

!!! Note 
    More than one RECEIVE operation may be required
    to retrieve an entire logical message since the TCP/IP network may not transfer
    the entire message in one packet on the network and a portion of the logical 
    message may be ready whereas the next RECEIVE may have to wait for the next 
    part of the message.

The TCPIO service is not sensitive to any special characters and all byte values 
are allowed anywhere in messages. 

If any TCPIO operation fails for any reason on the client or server, a non-zero
return code is returned. 

The TCPIO server port support includes multiple threads to support concurrent
connections. There is one thread for each open server port which waits for new
connections on the server port, starts new connection thread, and then returns 
to wait for another connection. 

Each connection thread waits for any current available messages to be read from 
that connection input buffer by the main TPCIO user thread. 

When all messages have been retrieved, then the connection thread issues a 
connection client socket read for first byte of the next input message. 
   
The connection thread will wait for the next message to arrive in the input 
buffer or for a disconnect. If a disconnect occurs, the connection thread is 
cancelled. 

If a pending RECEIVE is waiting on the connection which disconnected, a return 
code of 12 is returned. 

If the read of first byte is successful the thread returns to wait for the main 
TCPIO user thread to retrieve the full or partial message available in the
connection input buffer. 

Since partial messages can arrive from multiple connections in any sequence, the 
server must be sure to retrieve a complete message from a specific connection 
prior to returning to non-specific RECEIVE for the next message.

### SOAGEN - SOA app gen support {: #SOAGEN}

The macro SOAGEN can be used to generate customized client and server message 
managers plus stubs for each service called by the client application. 
   
In addition the SOAGEN macro generates two batch commands to build the SOA 
application and to execute the client server SOA application. 

The SOAGEN macro uses the z390 PUNCH extension operands `DSNAME=` and `FORMAT` 
to generate 3 or more source MLC files plus the two BAT files in one macro 
expansion execution. 

#### Parameters

Parameter   | Usage          | Comments
------------|----------------|---------
MAIN=      | name of main client program | If specified, an assembly and link of the main program with the generated service call stubs will be generated.
CTYPE=MLC/CBL | define language type for client | COBOL clients generate IBM standard EZASOKET calls to TCP/IP to connect to services. 
CLIENT=    | name of the generated client message manager called from stubs. |
SERVER=    | name of the generated service message manager which loads and calls services based on service request messages. |
HOST=      | IP address of server | host processor or * for local processor | 
PORT=      | port # for this application (must be greater than 1023) |
SERVICES=  | one or more sublists defining the name of each called service and the length of each parameter being passed to service. If the length is negative, that indicates the parameter is read only and the updated parameter will not be returned in response message. |
MACDIR=    | directory containing the SOAGEN macros |
GENDIR=    | directory to contain the generated source files and command files |
GENBLD=    | name of the generated build command file |
GENRUN=    | name of the generated run command file |
    
#### Usage

```
             SOAGEN MAIN=DEMOMAIN,    MAIN CLIENT APPLICATION PGM          X
                    CLIENT=DEMOCMGR,  SOA CLIENT MSG MGR NAME              X
                    SERVER=DEMOSMGR,  SOA SERVER MSG MGR NAME              X 
                    HOST=*, (192.168.1.3)  HOST SERVER NAME (*=LOCAL)      X
                    PORT=3900,        HOST SERVER PORT                     X
                    SERVICES=((DEMOSUB1,-45,-45,45), SERVICES WITH PARM LENX
                         (DEMOSUB2,-4,-4,4)), (NOTE -LENGTH FOR READ ONLY) X
                    MACDIR=<z390_path>\SOA\MACLIB, SOA GEN MACRO DIRECTORY X
                    GENDIR=<z390_path>\SOA\DEMO, DIRECTORY FOR SOA APPL    X
                    GENBLD=DEMOBLD,   GENERATED BUILD BAT FILE             X
                    GENRUN=DEMORUN    GENERATED RUN BAT FILE
             END
```

The above SOA application generation macro call generates server
message manager DEMOSMGR to run on the same host as client using
HOST=*. 

To generate the same application to run server on a specific
host, change the HOST= parameter to specify the IP address of the
desired server. 

The above SOAGEN macro call generates the following source files 
using z390 PUNCH extended operands DSNAME= and FORMAT to control PUNCH output 
files:

* DEMOCMGR.MLC – source macro call to SOACMGR to generate SOA client message 
  manager for the demo application.
* DEMOSMGR.MLC – source macro call to SOASMGR to generate SOA server message 
  manager for the demo application.
* SOA_STUB_DEMOSUB1.MLC - source macro call to SOASTUB to generate SOA stub for 
  DEMOSUB1 service call.
* SOA_STUB_DEMOSUB2.MLC – source macro call to SOASTUB to generate SOA stub for 
  DEMOSUB2 service call.
* DEMOBLD.BAT – generated command file to build the SOA demo application.
* DEMORUN.BAT – generated command file to start the DEMOSMGR server on the same 
  processor and then run the DEMOMAIN client application.

#### Technical Notes

The client message manager is generated using a call to the SOACMGR macro with
the required parameters from the SOAGEN macro call.

The client message manager performs the following functions when called from a
client source call stub:

* On first call, dynamically allocate the required message buffer based on the
  maximum service message required.
* On first call open a TCP/IP socket connection to the server message manager
  using the port and IP address specified.
* Build a send message with message length, time stamp, service name, and all
  the parameters required by the service. Note the service can only access the
  parameters passed with the length specified. If a service needs to access
  additional parameters such as control blocks in memory, they need to all be
  passed to the service.
* Send the message from client to server message manager using specified port
* Wait for response from the server with matching time stamp and service
  name plus updated parameters, and return code from service. Note the
  generated client message manager has logic to issue more than 1 RECEIVE
  to fetch the entire variable length response message if necessary.
* Move the returned updated parameters to the original calling list addresses.
  exit to calling stub which exits to the calling client main application program.

The server message manager is generated using a call to the SOASMGR macro with
the required parameters from the SOAGEN macro call. The server message
manager performs the following functions:

* Opens server socket for specified port which starts thread which listens for
  new connections from clients and starts new connection threads as required.
  The server port and listens for incoming messages from clients.
* Issues receive on the server port to receive all or part of a message from a
  client on an open connection. The server logic fetches at least 4 bytes and
  uses the message length in the first 4 bytes to determine how many bytes
  must be read to complete the variable length message which may require
  additional receive commands.
* Look up the service name specified. If service not found, an error is
  generated on server log and server returns to get next message.
* Build call parameter address less pointing to the parameters in the received
  message. Note this implies that all updates by the service will be made to
  parameter areas in the message buffer.
* Load the service on the first call and save entry address.
* Call the service to update parameters in the message buffer.
* Store the service return code in the message buffer.
* Build return message truncated to just the updated parameters as indicated
  by positive lengths in the SOAGEN SERVICES parameter.
* Send response message back to client message manager using same
  connection as request message.
* Return to wait for next service request message from any client connection.
* If client disconnect occurs, the disconnect is logged and server returns to wait
  for next request message from any other client connections.

Stubs for each service name called by the client application are generated using calls
to the macro SOASTUB. The functions performed by the generated stubs are:

* On first call load the client message manager and save address.
* Call the client message manager passing the name of the service and the
  calling parameter list.
* Upon return, exit to caller with return code.

If the GENBLD parameter specifies a name, then the SOAGEN macro will generate
a batch command file which assembles each of the above source programs to create
an executable SOA type client server application.

If the GENRUN parameter specifies a name, then the SOAGEN macro will generate
a batch command to start the named SOA server message manager on the same 
processor, and then run the client application. If the HOST parameter specifies 
a different processor, the generated server message manager will need to be 
copied to that processor and started prior to running the client application.

#### Demonstration

z390 distribution includes `soa` directory with SOA generation macro library and 
a demo application which can be generated and executed as either a classic 
statically linked application or an SOA generated client server application 
using TCP/IP sockets.

Refer to README.TXT in SOA folder for more details.

## References

### Original z390 SOA documents

* <http://www.z390.org/z390_SOA_User_Guide.pdf>
* <http://www.z390.org/z390_SOA_Support_for_COBOL_and_Assembler.htm>
* <http://www.z390.org/z390_SOA_Client_Server_Overview.pdf>

### TCP/IP

* [TCP/IP Transmission Control Protocol RFP](http://www.faqs.org/rfcs/rfc793.html)
* [J2SE ServerSocket Class](https://docs.oracle.com/javase/1.5.0/docs/api/java/net/ServerSocket.html)
* [J2SE Socket Class](https://docs.oracle.com/javase/1.5.0/docs/api/java/net/Socket.html)
* [z/OS Communication server IP Sockets Application Programming Interface Guide and Reference](https://www-01.ibm.com/servers/resourcelink/svc00100.nsf/pages/zOSV2R3sc273660/$file/hala001_v2r3.pdf)

### Host IP Addressing

* [J2SE InetAddress Class](https://docs.oracle.com/javase/1.5.0/docs/api/java/net/InetAddress.html)
* [IP Addressing RFC](https://www.ietf.org/rfc/rfc2373.txt)

### Socket Ports

* [Registered Ports](http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml)
* [Register a Port](https://www.iana.org/form/ports-services)
