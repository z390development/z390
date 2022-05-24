# GUAM services

<http://www.z390.org/z390_GUAM_GUI_Access_Method_Guide.pdf>

The z390 GUAM Graphical User Access Method provides support for any 
assembler program to interface with a user via a GUI window when the GUAM 
option is specified as an ez390 execution option. 

!!! Info 
    z390 has the following GUAM GUI interface support and limits: 

    * Full support for the MCS console view for WTO and WTOR commands 
    * Full support for the TN3270 edit mode form of TGET and TPUT 
    * Full support for basic TN3270 data streams including WCC, SBA, SF, IC, 
    RA, EUA, and PT commands. A cursor is supported using blinking 
    underline character at the current cursor position. The arrow, backspace, 
    delete, and tab keys are supported to control the cursor. Entering data into 
    protected field or alpha into numeric field causes beep and status line error 
    message. The 3270 screen display can be resized and the font is 
    automatically adjusted to maximum size that will still allow display of full 
    screen if possible. 
    * Limited support for TN3270 extended data streams with SFE and SA 
    commands are supported. Color attributes are supported but highlighting 
    and underlining are not yet supported. 
    * Limited support for the GUAM macro graphics commands. (The GUI 
    WINDOW, VIEW, GRAPH command can be used to display the graph view 
    but the graphic commands are targeted for future release. 


## Macro reference

### WTO

Write to operator via GUAM GUI MCS console scrolling log view.

```hlasm
label    WTO   ‘msg’
```
See [WTO in SVC services](svc_services.md#wto)
### WTOR

Write to operator with reply via GUAM GUI MCS console scrolling log view.

```hlasm
label    WTOR  ‘msg’,reply,reply_length,ecb
```  
See [WTOR in SVC services](svc_services.md#wtor)
### WAIT

Wait for WTOR reply from GUI interface and post ecb.

```hlasm
label    WAIT  ECB=ecb 
```

See [WAIT in SVC services](svc_services.md#wait)

### TGET

Read next line of text from TN3270 interface.

The default is EDIT mode and WAIT for input from keyboard.

```hlasm
label    TGET  buffer,buffer_length,EDIT,WAIT  
```

read TN3270 data stream from TN3270 screen interface. 

The TN3270 input stream consists of action code, cursor buffer address, and any 
modified input fields.

```hlasm
label    TGET  buffer,buffer_len,ASIS,WAIT
```  

### TPUT

Write next line of text to TN3270 24x80 screen view (the default is EDIT mode)

```hlasm
label    TPUT  buffer,buffer_length
```

write TN3270 data stream to TN3270 screen interface. 

The write buffer must contain escape followed by valid TN3270 extended 
data stream which may contain EBCDIC encoded data and the following commands: 

* WCC
* SF
* SFE
* SA
* SBA
* IC
* PT 

```hlasm
label    TPUT  buffer,buffer_len,FULLSCR
```

### TN3270

Macro to generate native TN3270 data streams including SBA addresses 
using symbolic references for control codes. 

The following keyboard input keystrokes can be used to generate TN3270 
compatible input data codes:

| key                      | hex   | alternate keys                       |
|--------------------------|-------|--------------------------------------|
| ++"ENTER"++              | 7D    |                                      |
| ++"PF1"++ to ++"PF9"++   | F1-F9 |                                      |
| ++"PF10"++ to ++"PF12"++ | 7A-7C |                                      |
| ++"PF13"++ to ++"PF21"++ | C1-C9 | ++ctrl+alt+f1++ to ++ctrl+alt+f9++   |
| ++"PF22"++ to ++"PF24"++ | 4A-4C | ++ctrl+alt+f10++ to ++ctrl+alt+f12++ |
| ++"PA1"++                | 6C    | ++ctrl+f1++                          |
| ++"PA2"++                | 6E    | ++ctrl+f2++                          |
| ++"PA3"++                | 6B    | ++ctrl+f3++                          |
| ++"CLEAR"++              | 6D    | ++ctrl+c++                           |

### GUAM 

Macro to perform graphics display functions similar to GDDM.

```hlasm
         GUAM  WINDOW,TITLE,’text’    set window title 
         GUAM  WINDOW,LOC,x,y         set window location from upper left in pixels 
         GUAM  WINDOW,SIZE,width,height  set window size in pixels 
         GUAM  WINDOW,FONT,size       set window character font size (8-30) 
         GUAM  WINDOW,VIEW,MCS        set scrolling MCS console log view (default) 
         GUAM  WINDOW,VIEW,SCREEN,row,col,color   set text screen view 
         GUAM  WINDOW,VIEW,GRAPH,x,y,color        set graphic display view 
         GUAM  WINDOW,GETVIEW,view    get current view 
         GUAM  SCREEN,READ,buffer,buffer_len,WAIT/NOWAIT      read text 
         GUAM  SCREEN,WRITE,row,col,buffer,buffer_len,color   write text 
         GUAM  SCREEN,FIELD,row,col,length   define field 
         GUAM  SCREEN,CURSOR, type    set cursor type 
         GUAM  SCREEN,CURSOR,row,col  set cursor position 
         GUAM  GRAPH,POINT,x,y,color  draw point 
         GUAM  GRAPH,LINE,x1,y1,x2,y2,color  draw line 
         GUAM  GRAPH,FILL,x1,y1,x2,y2,color  fill area 
         GUAM  GRAPH,TEXT,x,y,’text’,color   draw text 
         GUAM  KEYBOARD,mode,char,WAIT/NOWAIT  read keyboard 
         GUAM  MOUSE,x,y,left,right   read mouse position on graph and buttons 
         GUAM  SOUND,START,wav_file   play wav sound file 
         GUAM  SOUND,STOP
``` 

When the ez390 GUAM option is specified for execution of a z390 assembler 
program, a GUAM GUI window is opened in default MCS console view displaying 
all WTO and WTOR messages issued by program in a scrolling window. 

Whenever TPUT, or TGET macros are executed the GUI window switches to TN3290 screen 
view display mode. 

Whenever GUAM macro graphic commands are executed, the 
GUI window switches to GRAPH display mode. 

When in SCREEN or GRAPH view, WTO and WTOR commands can continue to be executed 
and displayed one at a time via the status line with command line replies as required. 

The user can switch between any of the 3 GUAM GUI views manually via view menu selection. 
Assembler application programs can set window title, location, size, font, and view 
mode at any time using the GUAM macros regardless of display view. 

Multiple user controlled GUAM GUI windows can be opened by executing different assembler 
programs as separate tasks under control of master program using the z390 
CMDPROC macro to launch separate program tasks each of which can invoke a 
GUAM GUI interface. 

## Demonstrations

The following GUI demonstration programs are included in the distribution in the `guam/demo` folder. 

1. DEMOGUI1.MLC – Issue WTOR, wait for reply via WAIT, display reply 
via WTO and repeat loop until END is entered. This program can be run in 
any of the following modes: 
    * Windows command line mode – ASMLG DEMO\DEMOGUI1 
    * Windows GUI interface – ASMLG DEMO\DEMOGUI1 GUI 

2. DEMOGUI2.MLC – Issue WTOR, execute 3 instruction loop until ECB is 
posted, display reply via WTO along with date, time, instruction loop count 
and calculated MIP rate for the 3 instruction loop. It is very interesting to 
note that on a 3 GHZ Pentium 4 system, this demo runs at about 1.1 MIPS 
using command line mode, and about 2.6 MIPS using the GUI mode. This 
program can be run in any of the following modes: 
    * Windows command line mode – ASMLG DEMO\DEMOGUI1 
    * Windows GUI interface – ASMLG DEMO\DEMOGUI1 GUI 

3. DEMOGUI3.MLC – issue TPUT and TGET with WAIT option in loop until 
END is entered. This demo uses default EDIT mode of TPUT and TGET to 
scroll the 24 lines of TN3270 screen view with wrap around after status line 
prompt for input each time last line is written. 

4. DEMOGUI4.MLC – issue TN3270 data stream TPUT and TGET with 
WAIT option in loop until END is entered. This demo writes text to fields at 
specified screen buffer addresses, reads a field at specified address, displays 
the input field at another address, and repeats the process waiting for screen 
input until PF3 or END is entered. 

5. DEMOGUI5.MLC – This demo used GUI graphic commands to draw text 
and graphics and read keyboard and mouse. (Note this program brings up 
graphic display view but the graphics commands are not implemented yet. [See RPI 137](https://github.com/z390development/z390/issues/6) 

6. DEMOGUI6.MLC – TN3270 application program to support entering name, 
address, and numeric zip fields, displaying the current values, PF1 help 
screen, display hex value of any unused AID keys entered, and PF3 for exit. 

    !!! Info
        There is an EXEC CICS version of this same demo that runs via local or remote GUAM 
        client TN3270 terminal connected to z390 CICS transaction manager. 
        The demo supports PF1 for help and PF3 for exit. It sounds alarm and displays 
        errors on status line. It also displays the hex AID code for any other 
        PF keys entered. 


7. DEMOAPL1.MLC – show use of APL graphics via X’08’ insert APL 
character command and also demo GUAM support of TN3270 color. 

## Reference

For technical reference manual see [IBM 3270 Data Stream Programmers Reference GA23-0059-07](http://bitsavers.trailing-edge.com/pdf/ibm/3270/GA23-0059-4_3270_Data_Stream_Programmers_Reference_Dec88.pdf)

For tutorial on TN3270 extended data stream programming with examples 
see <http://www.tommysprinkle.com/mvs/P3270/start.htm>.
