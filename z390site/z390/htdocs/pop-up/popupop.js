/***************************************************************
* Pop-Up Version 4.8.0 Pro H
* © 1998-2004
* Anoxy Software
* All Rights Reserved
* You are not allowed to modify anything in this Script
****************************************************************
* To get your own copy visit: http://www.anoxy.com
****************************************************************/

var slimb,slimr,trgtlay,hilay,hilaych,pcsl,wdp,hgp,wlw,wlh,popswotimer,poprestimer,popdescrtimer,popnmem,popcmem,poplmem,popvmem,popnwin,popMain,popdst,clrnum,ldbxn,pHLi,popXURLV;
var submenu = new Array();
var pmpimg = new Array();
submenu[0] = "popMain";
var posub = new Array();
var popamem = -1;
var poplevel = 0;
var Pmpat = /\D/g;

function popdsploff(s,e){
onsubop = false;
ptid = (e) ? ((e.target.parentNode) ? e.target.parentNode.id : "") : "";
if (ptid.indexOf(cf4)==0){
psnum = ptid.replace(cf4,"");
psnum = psnum.replace("arrow","");
if (pmact[psnum]>6)
onsubop = true;
}
if (!onsubop)
popsubctrl(0,s,0);
}

function popsubctrl(p1,p2,p3){
for (x=p1; x<submenu.length; x++){
if (x>=p2)
if (submenu[x] != -1){
if (pmact[pcsl] == 9 && pmiurl[pcsl] == submenu[x] && submenu[(x-1)] != -1 && p3==1 && posub[(x-1)][1]==pcsl)
break;
else{
document.getElementById(submenu[x]).style.visibility = "hidden";
if (x>0)
submenu[x] = -1;
}
}
if (x-p3<posub.length)
if (posub[x-p3][0] != -1){
clrnum = popclrindx[document.getElementById(posub[x-p3][0]).parentNode.parentNode.id.replace(Pmpat,"")] || 0;
document.getElementById(posub[x-p3][0]).style.color = pmclr[clrnum][3];
document.getElementById(posub[x-p3][0]+"d").style.visibility = "visible";
document.getElementById(posub[x-p3][0]+"h").style.visibility = "hidden";
if (x-p3>0)
document.images[cf4+posub[x-p3][1]+"arrow"].src = popaimg[clrnum].src;
posub[x-p3][0] = -1;
}
}
}
function checkresize(){
if (wlw != window.innerWidth || wlh != window.innerHeight)
setpopnpos();
poprestimer = setTimeout('checkresize()',500);
}

function setpopnpos(){
popsubctrl(0,1,0)
popsmf();
popMain.style.left = poX;
}

function popmidown(){
if (pmact[pcsl]<7)
setTimeout("popmidown2()",150);
}

function popmidown2(){
popXURLV = popXURLV || "";
switch (eval(pmact[pcsl])){
case 1:
location.href = pmiurl[pcsl]+popXURLV;
break;
case 2:
if (!popnwin || popnwin.closed)
popnwin = window.open(pmiurl[pcsl]+popXURLV,null,popwinprops);
else
popnwin.location.href = pmiurl[pcsl]+popXURLV;
break;
case 3:
tgfr = top.frames[pmitfm[pcsl]] || parent.frames[pmitfm[pcsl]] || document.frames[pmitfm[pcsl]] || "0";
if (tgfr!=0)
tgfr.location.href = pmiurl[pcsl]+popXURLV;
break;
case 4:
location.hash = pmiurl[pcsl];
break;
case 5:
setTimeout('eval(pmiurl[pcsl])',100)
break;
case 6:
top.location.href = pmiurl[pcsl]+popXURLV;
break;
}
}

function popmion(id){
if (window.print && pHLi && pHLi!="")
popmioff(pHLi,1)
pcsl = id.substring(8,id.length);
clearTimeout(popswotimer);
popdscroff();
hilay = document.getElementById(id);
clrnum = popclrindx[hilay.parentNode.id.replace(Pmpat,"")] || 0;
trgtlay = hilay.parentNode;
while (!trgtlay.id.match("pop") || trgtlay.id.match("aini") || trgtlay.id.match("Htr") && trgtlay.parentNode)
trgtlay = trgtlay.parentNode;
hilaych = document.getElementById(id+"h");
hilaych.style.left = hilay.style.pixelLeft+((trgtlay.id=="popMain") ? popMaini.offsetLeft : 0);
hilaych.style.pixelWidth = (trgtlay.id=="popMain") ? hilay.style.pixelWidth : hilay.parentNode.style.pixelWidth-((pmclr[clrnum][15]!=2) ? 2 : 0);
if (pmact[pcsl] != 8){
pHLi = hilay.id;
hilay.style.color = pmclr[clrnum][2];
hilaych.style.visibility = "visible";
document.getElementById(id+"d").style.visibility = "hidden";
if (pmsbtxt[pcsl])
window.status = unescape(pmsbtxt[pcsl]);
if (pmoimg[pcsl])
document.getElementById(cf4+pcsl+'img').src = pmoimg[pcsl].src;
for (x=0; x<submenu.length; x++)
if(trgtlay.id == submenu[x]){
poplevel = x;
break;
}
popsubctrl(poplevel+1,poplevel+1,1);
if (pmact[pcsl] == 9)
if (poplevel==0)
pophon2();
else
popmion2();
if (pmdbtxt[pcsl])
popdescrtimer = setTimeout('popdscron()',popdescrtimermsec);
}
else
if (window.print){
if (pHLi!="")
setTimeout('popmion(hilay.id)',74)
pHLi = "";
}
}

function pophon2(){
if (document.getElementById(pmiurl[pcsl]).style.visibility.toLowerCase()  == "visible" && popvmem == pcsl)
return;
popvmem = pcsl;
popamem = -1;
popsmsf();
smpx = trgtlay.style.pixelLeft+hilay.offsetLeft+popMaini.style.pixelLeft;
if (smpx+wdp >= slimr)
smpx = slimr-wdp;
if (popopendir==0)
smpy = trgtlay.style.pixelTop+trgtlay.style.pixelHeight+popMaini.style.pixelTop+1;
else
smpy = trgtlay.style.pixelTop+popMaini.style.pixelTop-hgp-1;
with(document.getElementById(pmiurl[pcsl]).style){
top = smpy;
left = smpx;
visibility = "visible";
}
}

function popmion2(){
if (document.getElementById(pmiurl[pcsl]).style.visibility.toLowerCase() == "visible" && !(popnmem != pcsl && poplmem == poplevel))
return;
document.images[cf4+pcsl+"arrow"].src = popoimg[clrnum].src;
popamem = pcsl;
popnmem = pcsl;
poplmem = poplevel;
popsmsf();
smpx = trgtlay.style.pixelLeft+trgtlay.style.pixelWidth-6;
smpy = trgtlay.style.pixelTop+hilay.offsetTop;
if (smpx+wdp >= slimr)
smpx = trgtlay.style.pixelLeft-wdp+6;
if (smpy+hgp >= slimb)
smpy = smpy-hgp+hilaych.style.pixelHeight;
if (smpx<wox)
smpx = trgtlay.style.pixelLeft+6;
if (smpy<woy)
smpy = woy;
with(document.getElementById(pmiurl[pcsl]).style){
top = smpy;
left = smpx;
visibility = "visible";
}
}

function popdscron(){
DboXobj = document.getElementById("DboX"+pcsl);
ldbxn = pcsl;
dbow = DboXobj.style.pixelWidth;
dboh = DboXobj.style.pixelHeight;
if (pmact[pcsl] == 9){
pmsol = document.getElementById(pmiurl[pcsl]);
dbxp = pmsol.style.pixelLeft+((pmsol.style.pixelWidth-dbow>0) ? (pmsol.style.pixelWidth-dbow)/2 : 0);
dbyp = pmsol.style.pixelTop+((popopendir==0) ? pmsol.style.pixelHeight+2 : dboh*-1-2);
if (dbxp+dbow >= slimr)
dbxp = slimr-dbow-2;
if (dbyp+dboh >= slimb)
dbyp = pmsol.style.pixelTop-dboh-2;
}
else{
dbxp = trgtlay.style.pixelLeft+2+ ((trgtlay.id=="popMain") ? hilay.style.pixelLeft : trgtlay.style.pixelWidth);
dbyp = trgtlay.style.pixelTop+hilay.offsetTop+((trgtlay.id=="popMain") ? ((popopendir==0) ? trgtlay.style.pixelHeight+2 : dboh*-1-2) : 0);
if (dbxp+dbow >= slimr)
dbxp = (trgtlay.id=="popMain") ? slimr-dbow-4 : trgtlay.style.pixelLeft-dbow-2;
if (dbxp<wox)
dbxp = wox;
if (dbyp+dboh >= slimb)
dbyp = slimb - dboh-2;
if (dbyp<woy)
dbyp = woy;
}
with (DboXobj.style){
left = dbxp;
top = dbyp;
visibility = "visible";
}
}

function popdscroff(){
if (ldbxn)
if (document.getElementById("DboX"+ldbxn).style.visibility.toLowerCase() == "visible")
document.getElementById("DboX"+ldbxn).style.visibility = "hidden";
clearTimeout(popdescrtimer);
}

function popsmsf(){
popsmf();
submenu[(poplevel+1)] = pmiurl[pcsl];
posub[poplevel] = new Array(hilay.id,pcsl);
document.getElementById(pmiurl[pcsl]).style.zIndex = trgtlay.style.zIndex+1;
wdp = document.getElementById(pmiurl[pcsl]).style.pixelWidth;
hgp = document.getElementById(pmiurl[pcsl]).style.pixelHeight;
}

function popsmf(){
wox = window.pageXOffset;
woy = window.pageYOffset;
wlw = window.innerWidth;
wlh = window.innerHeight;
slimb = wlh+woy;
slimr = wlw+wox;
}

function popmioff(id,p){
if (pmsbtxt[pcsl])
window.status = "";
if (popautoswitch){
clearTimeout(popswotimer);
popswotimer = window.setTimeout('popdsploff(1)',popsotimermsec);
}
deletehilay = true;
popdscroff();
for (x=0; x<posub.length; x++)
if (posub[x][0] == id){
deletehilay = false;
popamem = -1;
break;
}
if (deletehilay){
clrnum = popclrindx[document.getElementById(id+"d").parentNode.parentNode.id.replace(Pmpat,"")] || 0;
hilay.style.color = pmclr[clrnum][3];
document.getElementById(id+"d").style.visibility = "visible";
document.getElementById(id+"h").style.visibility = "hidden";
}
}

function popcacnelso(){
clearTimeout(popswotimer)
}
//Pop-Up 4.7 H Menu file (Opera)

var poptimer;
var popautoswitch = false;
var popdeltimermsec = 250;
var popsotimermsec = 800;
var popdescrtimermsec = 500;
var popopendir = 0;
var popmcheight = 15;
var popmcwidth = 489;
var popwinprops = "";
var pmclr = new Array();
var pmdbtxt = new Array();
var pmsbtxt = new Array();
var pmoimg = new Array();
var pmolimg = new Array();
pmdbtxt[2] = new Array("Assembler email groups for news, discussion, and help.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[4] = new Array("Assembler related links including other languages, linkers, history, and more.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[5] = new Array("Help links and information about this site.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[6] = new Array("List of frequently asked questions about mainframe assember with links.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[7] = new Array("FAQ about the use of the IBM-Main email group which has over 3000 members.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[8] = new Array("Useful references regarding assembler.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[9] = new Array("Some useful tips on mainframe assembler plus book references.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[10] = new Array("HLASM mainframe assembler discussion group web interface with 1300 searchable threads archived.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[11] = new Array("HLASM mainframe assembler email group with over 800 members including John Erhman, IBM father of HLASM.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[12] = new Array("IBM mainframe discussion group web interface with over 135,000 searchable archived threads.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[13] = new Array("IBM mainframe discussion email group.","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[14] = new Array("Join z390 User Group to participate in open source development and support of portable mainframe assembler and emulator.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[15] = new Array("Yahoo email archive search for over 700,000 email messages.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[16] = new Array("HLASM V1R4 Language Reference","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[17] = new Array("HLASM V1R4 Programs Guide","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[18] = new Array("z/OS and MVS macro references.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[19] = new Array("HLASM General Information","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[20] = new Array("HLASM Instatllation and Customization","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[22] = new Array("z/OS Principles of Operations Manual with all hardware instructions supported by HLASM.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[23] = new Array("Catalog of all IBM HLASM manuals.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[24] = new Array("Frequently Asked Questions and Tips.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[25] = new Array("Control Block Maps","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[26] = new Array("IBM Diagnostic Reference with SVC interfaces.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[27] = new Array("Simotime assembler resources including examples.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[28] = new Array("Assembler class online text.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[29] = new Array("HLASM common routine examples.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[30] = new Array("Table of contents of advanced assembler reference book and price comparisons.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[31] = new Array("System 390 Reference Book in PDF format with common instructions, condition codes, etc.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[32] = new Array("IBM HLL Assembler (HLASM)","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[33] = new Array("Dingus PC based mainframe assembler.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[34] = new Array("HLA High Level Assembler for Intel using MASM to assemble generated source code.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[35] = new Array("Micro Focus PC based mainframe developer environment including mainframe assembler.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[36] = new Array("Tachyon PC based mainframe assembler.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[37] = new Array("z390 portable mainframe macro assembler and emulator project.  Distributed under open source OSI GPL license.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[38] = new Array("IBM HLASM assembler, z/Architecture, and z/OS reference links.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[42] = new Array("Mainfrme emulators.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[44] = new Array("History of computing and IBM System 360, 370, 390, and z architecture.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[45] = new Array("Other mainframe related languages.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[47] = new Array("Organizations supported by mainframe developers.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[48] = new Array("References for TN3270 terminal support which z390 supports.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[49] = new Array("Dino's Webring of mainframe web sites.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[50] = new Array("Return to www.z390.org","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[51] = new Array("Fin FAQ, Getting Started Guide, User Guide, Request Form, Request Log,, User email grroup and more.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[52] = new Array("Find updates for Windows XP.  ","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[53] = new Array("Find updates for Ubuntu Linux.  z390 uses Ubuntu for Linux regression testing.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[54] = new Array("View site map with menu and searchable hypertext links.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[55] = new Array("Send feedback and suggestions to:<BR><BR>don@higgins.net","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[56] = new Array("z390 Portable Assembler, Linker, and Emulator open source Java J2SE Project","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[62] = new Array("Mainframe emulators for PC's.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[63] = new Array("IBM mainframe system user group.which meets twice a year","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[64] = new Array("z390 Presentation at SHARE oin February 2007.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[65] = new Array("Home of SHARE Program Library of shared programs including thousands of mainframe assembler macros, utilities, mods, etc.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[66] = new Array("Association of Computing Machinery","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[67] = new Array("Computer Measurement Group which focuse on performance, tuning, capacity planning, and management of mainframe system.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[68] = new Array("IBM mainframe user group.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[69] = new Array("Engineering computer user group.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[70] = new Array("National Association of System Programmers.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[72] = new Array("Computer History since 1945.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[73] = new Array("History of IBM 360, 370, 390, and z architecture.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[74] = new Array("History of PC/370","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[75] = new Array("IBM Product Timeline","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[76] = new Array("IBM Green Card System 350 reference cards.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[77] = new Array("Catalog of assembler services manuals.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[78] = new Array("Describes commonly used services","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[79] = new Array("Desribes common service macros A-H such as ABEND, CALL, FREEMAIN, and GETMAIN","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[80] = new Array("Describes common service macros I-Z such as LINK, LOAD, RETURN< SAVE, TIME.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[81] = new Array("Describes common file access macros such as OPEN, CLOSE, DCB, GET, PUT, READ, WRITE.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[82] = new Array("Authorized service macros such as DYNALLOC.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[87] = new Array("Assemblers for mainframes","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[88] = new Array("Assemblers for Intel chips","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[90] = new Array("Don's Cobol Links","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[91] = new Array("Find Java tool links on the Automated Software Tools web site.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[92] = new Array("IBM PL/I Language ","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[93] = new Array("HLA High Level Assembler for Intel compatible chips supporting macros and multiple backend assemblers such as MASM for Windows.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[94] = new Array("Microsoft MSASM Intel assembler that comes with C products.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[95] = new Array("Intel Pentium hardware reference manuals.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[96] = new Array("z/OS data control block maps and cross references.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[101] = new Array("z390 InstallShield downloads for Windows, file image zips for Linux, plus optional regression test and other downloads.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[102] = new Array("z390 requires J2RE 1.5.0 or later Java 2 Download and install J2RE 1.5.0+ Java Runtime from Sun Microsystems Inc. (Note JDK and J2EE include the J2RE but are not required to run z390.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[103] = new Array("Eclipse is the recommended IDE for interactive debugging of the z390 open source Java code but this is not required to run or debug z390 assembler.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[104] = new Array("Visit www.mfatc.org for additional demos you can run on Windows written in 5 languages - HLASM, HLA, C++, COBOL, and Java.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[105] = new Array("z390 Getting Started guide to installing and running demo \"Hello World\" assembly, link, and execution.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[107] = new Array("z390 User Guide in PDF format","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[108] = new Array("z390 guide on sequential and random access file support contributed by Melvyn Maltz.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[110] = new Array("2390 Graphical User Access Method Guide describing support for MCS console interface, TN3270 interface, and graphics interface.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[112] = new Array("z390 Macro Pseudo Code support for 300% faster processing and detail variable tracing","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[113] = new Array("z390 program services guide by Melvyn Maltz.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[114] = new Array("z390 Service Oriented Architecture overview of client server TCP/IP sockets support.  View slides in full screen mode.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[115] = new Array("z390 Service Oriented Architecture application generator support using TCP/IP Sockets","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[116] = new Array("z390 SOA User Guide with details on TCPIO macro and svc TCP/IP multi-threaded sockets support.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[117] = new Array("z390 storage services guide by Melvyn Maltz.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[118] = new Array("High Level Assembler Reference Manuals","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[119] = new Array("z390 Service Oriented Architecture client server overview slides.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[120] = new Array("Find more information about z390 support.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[121] = new Array("Submit RPI request for fixes and enhancements","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[122] = new Array("View log of pending RPI fixes and enhancements for future z390 releases.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[123] = new Array("Join the z390 Email User Group Forum to receive updates on each release and to participate in dicussion of z390 related topics.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[124] = new Array("MFATC Mainframe Assembler Technique Comparisons with HLASM, HLA, C++, COBOL, and Java","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[125] = new Array("Information on z390 development project","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[126] = new Array("Information on the regression tests included with z390","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[127] = new Array("Information on benchmarks included with z390","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[129] = new Array("z390 macro pseudo code support for 300% faster processing and detail variable tracing","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[130] = new Array("M","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[135] = new Array("CBT mainframe system downloads and links","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[139] = new Array("Linux Reference Links. ","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[142] = new Array("Java 2 Runtime API's available for use by z390","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[143] = new Array("Java 2 Standard Edition","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[144] = new Array("Java 2 Enterprise Edition","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[145] = new Array("Eclipse open source IDE recommended for use with z390","#000000","#FF0000","4px Inset #999999","150");
pmdbtxt[148] = new Array("IBM CICS Product web site","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmdbtxt[149] = new Array("Using EXEC CICS interface with examples for different languages including assembler.","#000099","#FFFFFF","4px Ridge #6699FF","150");
pmclr[0] = new Array("#0000FF","#0000FF","#FFFFFF","#FFFFFF","#7F7F7F","#FFFFFF","#FFFFFF","#7F7F7F","#C6C3C6","#000000","#7F7F7F","#FFFFFF",0,0,"",0,100);
var popclrindx = new Array("0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0");
var pmiurl = new Array('pop15submain','pop16submain','pop17submain','pop18submain','pop6submain','pop7submain','http://www.planetmvs.com/hlasm/s390faq.html','http://www.planetmvs.com/ibm-main/index.html','http://www.planetmvs.com/hlasm/tips.html','http://www.wynsoft.co.uk/mainframe/assembler.htm','http://groups.google.com/groups?hl=en&lr=lang_en&ie=UTF-8&group=bit.listserv.asm370','http://www.lsoft.com/scripts/wl.exe?SL1=ASSEMBLER-LIST&H=LISTSERV.UGA.EDU','http://groups.google.com/groups?hl=en&lr=lang_en&ie=UTF-8&group=bit.listserv.ibm-main','http://bama.ua.edu/cgi-bin/wa?SUBED1=ibm-main&A=1','http://groups.yahoo.com/group/z390/','http://groups.google.com/advanced_group_search','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/ASMR1010/CCONTENTS?DT=20040728153937','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/ASMVP000/CCONTENTS?DT=19950308015432','pop11submain','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/ASMG1010/CCONTENTS?DT=20040728142404','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/ASMVI000/CCONTENTS?DT=19950307194755','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/CEEA3130/CCONTENTS?DT=20020625092930','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/DZ9ZR003/CCONTENTS?SHELF=DZ9ZBK03&DN=SA22-7832-03&DT=20040504121320','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/FINDBOOK?filter=hlasm&SUBMIT=Find','pop1submain','pop14submain','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2V230/CCONTENTS?DT=20020625143559','http://www.simotime.com/indexasm.htm','http://www.wiu.edu/users/mflll/cs310/head.html','http://www.sysprog.net/hlasm.html','http://www.isbn.nu/toc/0471361763','http://publibz.boulder.ibm.com/epubs/pdf/sa227209.pdf','http://www-3.ibm.com/software/ad/hlasm/','http://www.dignus.com/','pop24submain','http://www.microfocus.com/products/mainframeexpress/index.asp?bhcp=1','http://tachyonsoft.com/','http://www.z390.org','http://www.automatedsoftwaretools.com/z390/index.html#IBM Reference Links','pop2submain','pop4submain','pop5submain','pop8submain','pop22submain','pop10submain','pop12submain','pop19submain','pop9submain','pop21submain','http://n.webring.com/hub?ring=dinos','http://www.z390.org','http://www.automatedsoftwaretools.com/z390/z390_Support.htm','http://update.microsoft.com/','http://www.ubuntu.com/support','http://www.automatedsoftwaretools.com/z390/pop-up/sitemap.html','mailto:don@higgins.net?subject=z390 Portable Assembler','http://www.z390.org','http://www.funsoft.com/','http://www.openmainframe.com/','http://www.conmicro.cx/hercules/','http://home.tampabay.rr.com/dhiggin1/pc370.htm','http://www.platform-solutions.com/','http://www.umxtech.com/','http://www.share.org/','http://www.automatedsoftwaretools.com/z390/SHARE_2845_z390_Portable_Mainframe_Assembler_and_Emulator.pdf','http://www.cbttape.org/','http://www.acm.org/','http://www.cmg.org/','http://www.common.org/index.html','http://computer.org/','http://www.naspa.com/','http://www.opensource.org/','http://www.computerhistory.org/','http://www.beagle-ears.com/lars/engineer/comphist/ibm360.htm','http://home.tampabay.rr.com/dhiggin1/pc370.htm','http://www.cs.clemson.edu/~mark/acs_timeline.html','http://www.planetmvs.com/greencard/index.html','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/FINDBOOK?filter=assembler+services&Collection=0','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A630/CCONTENTS?DT=20020625111237','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A760/CCONTENTS?DT=20050714101633','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A960/CCONTENTS?DT=20050713031522','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/DGT2D520/CCONTENTS?DT=20050708033618','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A860/CCONTENTS?DT=20050714101051','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A150/CCONTENTS?DT=20050714024756','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A260/CCONTENTS?DT=20050714132120','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A360/CCONTENTS?DT=20050722023819','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2A450/CCONTENTS?DT=20050712233359','pop5submain','pop13submain','pop23submain','http://home.tampabay.rr.com/dhiggin1/Cobol_Links/index.html','pop20submain','http://www-306.ibm.com/software/awdtools/pli/plizos/','pop24submain','http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vcmasm/html/vcoriMicrosoftAssemblerMacroLanguage.asp','http://www.intel.com/design/pentium/manuals/','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2D130/CCONTENTS?DT=20020717133052','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2D230/CCONTENTS?DT=20020717133221','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2D330/CCONTENTS?DT=20020717133739','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2D430/CCONTENTS?DT=20020717151454','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/IEA2D530/CCONTENTS?DT=20020717135739','http://www.automatedsoftwaretools.com/z390/#Download%20Links','http://java.sun.com/j2se/1.5.0/download.jsp','http://eclipse.org/','http://www.mfatc.org','http://www.automatedsoftwaretools.com/z390/z390_Getting_Started.htm','http://www.automatedsoftwaretools.com/z390/z390_Frequently_Asked_Questions.htm','http://www.automatedsoftwaretools.com/z390/z390_User_Guide.pdf','http://www.automatedsoftwaretools.com/z390/z390_File_Access_Method_Guide.pdf','http://www.automatedsoftwaretools.com/z390/z390_EXEC_CICS_Compatible_Assembler_Support.htm','http://www.automatedsoftwaretools.com/z390/z390_GUAM_GUI_Access_Method_Guide.pdf','http://www.automatedsoftwaretools.com/z390/z390_Macro_and_SVC_Services_Guide.pdf','http://www.automatedsoftwaretools.com/z390/z390_Macro_Pseudo_Code.pdf','http://www.automatedsoftwaretools.com/z390/z390_Program_Services_Guide.pdf','http://www.automatedsoftwaretools.com/z390/z390_SOA_Client_Server_Overview.pdf','http://www.automatedsoftwaretools.com/z390/z390_SOA_Support.htm','http://www.automatedsoftwaretools.com/z390/z390_SOA_User_Guide.pdf','http://www.automatedsoftwaretools.com/z390/z390_Storage_Services_Guide.pdf','pop3submain','pop20submain','http://www.automatedsoftwaretools.com/z390/z390_Support.htm','http://www.automatedsoftwaretools.com/z390/z390_Support_Request_Form.htm','http://www.automatedsoftwaretools.com/z390/z390_Support_Request_Log.htm','http://groups.yahoo.com/group/z390/','http://www.mfatc.org/','http://www.automatedsoftwaretools.com/z390/z390_Development.htm','http://www.automatedsoftwaretools.com/z390/z390_Regression_Tests.htm','http://www.automatedsoftwaretools.com/z390/z390_Benchmark_Instruction_Timing.htm','http://www.automatedsoftwaretools.com/z390/z390_Statistics.htm','http://www.automatedsoftwaretools.com/z390/z390_Macro_Pseudo_Code.pdf','http://www-03.ibm.com/servers/eserver/zseries/zos/','http://www-03.ibm.com/servers/eserver/zseries/zvse/','http://www.vm.ibm.com/','http://www-03.ibm.com/servers/eserver/zseries/os/linux/','http://www-03.ibm.com/servers/eserver/zseries/zos/bkserv/','http://www.cbttape.org/','http://www.mainframes.com/','http://cbttape.org/~jmorrison/mvs38j/','http://www.planetmvs.com/siteindex.html','http://www.automatedsoftwaretools.com/z390/Linux/z390_Linux_Support.htm#Linux%20Reference%20Links','http://www.microsoft.com/windows/','http://java.sun.com/docs/books/jls/third_edition/html/j3TOC.html','http://java.sun.com/j2se/1.5.0/docs/api/','http://java.sun.com/j2se/index.jsp','http://java.sun.com/j2ee/index.jsp','http://www.eclipse.org/','http://publibz.boulder.ibm.com/cgi-bin/bookmgr_OS390/BOOKS/CN7P4000/CCONTENTS?DT=19920626112004','http://www.tommysprinkle.com/mvs/P3270/start.htm','http://www-306.ibm.com/software/htp/cics/','http://publib.boulder.ibm.com/infocenter/cicsts/v3r1/index.jsp?topic=/com.ibm.cics.ts31.doc/dfhp3/dfhp3b0056.htm','http://msdn.microsoft.com/vstudio/express/visualc/','http://msdn2.microsoft.com/en-us/library/3bstk3k5.aspx','http://www.automatedsoftwaretools.com/MFATC/mfatc_hello_demo.htm#VCE_HELLO_HELP','http://webster.cs.ucr.edu/AsmTools/HLA/index.html','http://webster.cs.ucr.edu/AsmTools/HLA/HLADoc/HLARef/HLARefTOC.html','http://www.automatedsoftwaretools.com/MFATC/mfatc_hello_demo.htm#HLA_HELLO_HELP','http://webster.cs.ucr.edu/AsmTools/HLA/hla_examples/index.html');
var pmact = new Array('9','9','9','9','9','9','1','1','1','1','1','1','1','1','1','1','1','1','9','1','1','1','1','1','9','9','1','1','1','1','1','1','1','1','9','1','1','1','1','9','9','9','9','9','9','9','9','9','9','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','9','9','9','1','9','1','9','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','9','9','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1');
var pmitfm = new Array('','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','','');
var pmmwds = new Array('179','223','210','226','288','150','162','245','199','166','232','192','235','212','277','310','207','215','187','183','311','159','212','213');
var pmcfnt = ";font-family:Arial,Arial,Times New Roman,Verdana;font-size:12px;font-weight:bold;font-style:normal";

cf0 = '<Div Id="';
cf1 = '</Div>';
cf2 = pmcfnt+'" onmouseover="popmion(this.id)" onmouseout="popmioff(this.id)" onmousedown="popmidown()"';
cf3 = '" style="position:absolute;left:0px;top:0px;padding:'+((window.print) ? 0 : 1)+'px 0px '+((window.print) ? 1 : 0)+'px 18px;';
cf4 = 'menuItem';
cf5 = 'menuSeparator';
cf6 = '" style="padding:'+((window.print) ? 0 : 1)+'px 15px 0px 15px'+pmcfnt+'" onmouseover="popmion(this.id)" onmouseout="popmioff(this.id)" onmousedown="popmidown()">';
cf7 = '" style="background-color: '+pmclr[0][1]+';color: '+pmclr[0][2]+';position:absolute;top:0px;padding:0px 15px 0px 15px;visibility:hidden;';
cf8 = '<TD width="4"><Div style="border-left: 1px solid '+pmclr[0][10]+';border-right:1px solid '+pmclr[0][11]+';height:'+popmcheight+'px;position:relative;left:1px;width:0px"></Div></TD>';
cf9 = '<TD id="';
cfB = "";

ccf0 = new Array();
ccf1 = new Array();
ccf2 = new Array();
ccf3 = new Array();
ccf4 = new Array();
popaimg = new Array();
popoimg = new Array();
for (x=0; x<pmclr.length;x++){
popaimg[x] = new Image();
popoimg[x] = new Image();
popaimg[x].src = popbasedir+"pop_arw"+pmclr[popclrindx[x]][12]+".gif";
popoimg[x].src = popbasedir+"popo_arw"+pmclr[popclrindx[x]][13]+".gif";
ccf0[x] = '" style="position: absolute;visibility: hidden;border: solid 1px';
if (pmclr[popclrindx[x]][8]=="transparent")
ccf0[x] += ';border-left:none;border-top:none';
else
ccf0[x] += ';border-left-color:'+pmclr[popclrindx[x]][8]+';border-top-color:'+pmclr[popclrindx[x]][8]
if (pmclr[popclrindx[x]][9]=="transparent")
ccf0[x] += ';border-bottom:none;border-right:none';
else
ccf0[x] += ';border-bottom-color:'+pmclr[popclrindx[x]][9]+';border-right-color:'+pmclr[popclrindx[x]][9];
ccf1[x] = '" style="position: relative;left:0;color:'+pmclr[popclrindx[x]][3];
ccf2[x] = ((pmclr[popclrindx[x]][14]) ? ';background-image:url('+popbasedir+pmclr[popclrindx[x]][14]+')' : '')+';background-color:'+pmclr[popclrindx[x]][0]+';border: solid 1px';
if (pmclr[popclrindx[x]][6]=="transparent")
ccf2[x] += ';border-left:none;border-top:none';
else
ccf2[x] += ';border-left-color:'+pmclr[popclrindx[x]][6]+';border-top-color:'+pmclr[popclrindx[x]][6]
if (pmclr[popclrindx[x]][7]=="transparent")
ccf2[x] += ';border-bottom:none;border-right:none';
else
ccf2[x] += ';border-bottom-color:'+pmclr[popclrindx[x]][7]+';border-right-color:'+pmclr[popclrindx[x]][7];
ccf3[x] = '" onmouseover="popcacnelso()" style="position:absolute;border-bottom: 1px solid '+pmclr[popclrindx[x]][11];
if (pmclr[popclrindx[x]][15]!=2)
ccf3[x]+= ';height:0;border-top: 1px solid '+pmclr[popclrindx[x]][10]+';left:';
if (pmclr[popclrindx[x]][15]==0)
ccf3[x] += '3px';
else
if (pmclr[popclrindx[x]][15]==1)
ccf3[x] += '0px';
else
ccf3[x] += ';height:1;left:0px';
ccf4[x] = 'SRC="'+popaimg[x].src+'" BORDER="0">';
}

var pmimsa = new Array('pop1submain',ccf0[0]+';width:199px;height:68px"','','','pop1submaini',ccf1[0]+ccf2[0]+';width:197px;height:66px"','','',cf4+'6',ccf1[0]+';top:0'+cf2,'Dave\'s Assembler FAQ',cf1,cf4+'7',ccf1[0]+';top:15'+cf2,'IBM Main List FAQ',cf1,cf5+'0',ccf3[0]+';top:33;width:193px"','',cf1,cf4+'8',ccf1[0]+';top:36'+cf2,'Dave\'s Assembler Tips',cf1,cf4+'9',ccf1[0]+';top:51'+cf2,'Wynsoft Assembler Tips',cf1+cf1+cf1,'pop2submain',ccf0[0]+';width:243px;height:92px"','','','pop2submaini',ccf1[0]+ccf2[0]+';width:241px;height:90px"','','',cf4+'10',ccf1[0]+';top:0'+cf2,'Assembler ASM370 Group',cf1,cf4+'11',ccf1[0]+';top:15'+cf2,'Assembler ASM370 List',cf1,cf4+'12',ccf1[0]+';top:30'+cf2,'IBM-Main Group',cf1,cf4+'13',ccf1[0]+';top:45'+cf2,'IBM-Main List',cf1,cf4+'14',ccf1[0]+';top:60'+cf2,'z390 Assembler/Emulator Group',cf1,cf4+'15',ccf1[0]+';top:75'+cf2,'Yahoo Group Search',cf1+cf1+cf1,'pop3submain',ccf0[0]+';width:230px;height:128px"','','','pop3submaini',ccf1[0]+ccf2[0]+';width:228px;height:126px"','','',cf4+'16',ccf1[0]+';top:0'+cf2,'Language Reference V1R5',cf1,cf4+'17',ccf1[0]+';top:15'+cf2,'Programers Guide V1R5',cf1,cf4+'18',ccf1[0]+';top:30'+cf2,'Macro References','<IMG Id="'+cf4+'18arrow" style="position:absolute;left:218px;top:4px" '+ccf4[0]+cf1,cf5+'1',ccf3[0]+';top:48;width:224px"','',cf1,cf4+'19',ccf1[0]+';top:51'+cf2,'General Information',cf1,cf4+'20',ccf1[0]+';top:66'+cf2,'Installation and Customization',cf1,cf4+'21',ccf1[0]+';top:81'+cf2,'Language Environment',cf1,cf4+'22',ccf1[0]+';top:96'+cf2,'z/Architecture POP ',cf1,cf4+'23',ccf1[0]+';top:111'+cf2,'Catalog of HLASM Manuals',cf1+cf1+cf1,'pop4submain',ccf0[0]+';width:246px;height:128px"','','','pop4submaini',ccf1[0]+ccf2[0]+';width:244px;height:126px"','','',cf4+'24',ccf1[0]+';top:0'+cf2,'Assembler FAQ\'s','<IMG Id="'+cf4+'24arrow" style="position:absolute;left:234px;top:4px" '+ccf4[0]+cf1,cf4+'25',ccf1[0]+';top:15'+cf2,'Control Blocks','<IMG Id="'+cf4+'25arrow" style="position:absolute;left:234px;top:4px" '+ccf4[0]+cf1,cf4+'26',ccf1[0]+';top:30'+cf2,'Diagnostic Reference ',cf1,cf4+'27',ccf1[0]+';top:45'+cf2,'Simotime Assembler Connection',cf1,cf5+'2',ccf3[0]+';top:63;width:240px"','',cf1,cf4+'28',ccf1[0]+';top:66'+cf2,'Assembler Class CS-310',cf1,cf4+'29',ccf1[0]+';top:81'+cf2,'HLASM Common Routines',cf1,cf4+'30',ccf1[0]+';top:96'+cf2,'Adsvanced ASM and Interfaces',cf1,cf4+'31',ccf1[0]+';top:111'+cf2,'System Reference Card (PDF)',cf1+cf1+cf1,'pop5submain',ccf0[0]+';width:308px;height:98px"','','','pop5submaini',ccf1[0]+ccf2[0]+';width:306px;height:96px"','','',cf4+'32',ccf1[0]+';top:0'+cf2,'IBM HLASM Assembler',cf1,cf5+'3',ccf3[0]+';top:18;width:302px"','',cf1,cf4+'33',ccf1[0]+';top:21'+cf2,'Dingus',cf1,cf4+'34',ccf1[0]+';top:36'+cf2,'HLA High Level Intel Assembler','<IMG Id="'+cf4+'34arrow" style="position:absolute;left:296px;top:4px" '+ccf4[0]+cf1,cf4+'35',ccf1[0]+';top:51'+cf2,'Micro Focus Mainframe Express Assembler',cf1,cf4+'36',ccf1[0]+';top:66'+cf2,'Tachyon Software',cf1,cf4+'37',ccf1[0]+';top:81'+cf2,'z390 Portable Assembler/Emulator',cf1+cf1+cf1,'pop6submain',ccf0[0]+';width:170px;height:188px"','','','pop6submaini',ccf1[0]+ccf2[0]+';width:168px;height:186px"','','',cf4+'38',ccf1[0]+';top:0'+cf2,'HLASM References',cf1,cf4+'39',ccf1[0]+';top:15'+cf2,'Email Groups','<IMG Id="'+cf4+'39arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'40',ccf1[0]+';top:30'+cf2,'Education','<IMG Id="'+cf4+'40arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf5+'4',ccf3[0]+';top:48;width:164px"','',cf1,cf4+'41',ccf1[0]+';top:51'+cf2,'Assemblers','<IMG Id="'+cf4+'41arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'42',ccf1[0]+';top:66'+cf2,'Emulators','<IMG Id="'+cf4+'42arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'43',ccf1[0]+';top:81'+cf2,'EXEC CICS','<IMG Id="'+cf4+'43arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'44',ccf1[0]+';top:96'+cf2,'History','<IMG Id="'+cf4+'44arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'45',ccf1[0]+';top:111'+cf2,'Languages','<IMG Id="'+cf4+'45arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'46',ccf1[0]+';top:126'+cf2,'Operating Systems','<IMG Id="'+cf4+'46arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'47',ccf1[0]+';top:141'+cf2,'Organizations','<IMG Id="'+cf4+'47arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'48',ccf1[0]+';top:156'+cf2,'TN3270 Terminals','<IMG Id="'+cf4+'48arrow" style="position:absolute;left:158px;top:4px" '+ccf4[0]+cf1,cf4+'49',ccf1[0]+';top:171'+cf2,'Webring - Dinos',cf1+cf1+cf1,'pop7submain',ccf0[0]+';width:182px;height:92px"','','','pop7submaini',ccf1[0]+ccf2[0]+';width:180px;height:90px"','','',cf4+'50',ccf1[0]+';top:0'+cf2,'Home',cf1,cf4+'51',ccf1[0]+';top:15'+cf2,'z390 Support',cf1,cf4+'52',ccf1[0]+';top:30'+cf2,'Windows Update',cf1,cf4+'53',ccf1[0]+';top:45'+cf2,'Ubuntu Linux Support',cf1,cf4+'54',ccf1[0]+';top:60'+cf2,'Site Map',cf1,cf4+'55',ccf1[0]+';top:75'+cf2,'About',cf1+cf1+cf1,'pop8submain',ccf0[0]+';width:265px;height:113px"','','','pop8submaini',ccf1[0]+ccf2[0]+';width:263px;height:111px"','','',cf4+'56',ccf1[0]+';top:0'+cf2,'z390 Portable Assembler/Emulator',cf1,cf5+'5',ccf3[0]+';top:18;width:259px"','',cf1,cf4+'57',ccf1[0]+';top:21'+cf2,'Flexes S/390 on Intel',cf1,cf4+'58',ccf1[0]+';top:36'+cf2,'Open Mainframe on Intel',cf1,cf4+'59',ccf1[0]+';top:51'+cf2,'Hercules S/390 on Windows & Linux',cf1,cf4+'60',ccf1[0]+';top:66'+cf2,'PC/370',cf1,cf4+'61',ccf1[0]+';top:81'+cf2,'Soft/390 on Intel',cf1,cf4+'62',ccf1[0]+';top:96'+cf2,'UMX OS/390 on Intel',cf1+cf1+cf1,'pop9submain',ccf0[0]+';width:219px;height:143px"','','','pop9submaini',ccf1[0]+ccf2[0]+';width:217px;height:141px"','','',cf4+'63',ccf1[0]+';top:0'+cf2,'SHARE',cf1,cf4+'64',ccf1[0]+';top:15'+cf2,'z390 Presentation at SHARE',cf1,cf4+'65',ccf1[0]+';top:30'+cf2,'CBT SPLA Repository',cf1,cf5+'6',ccf3[0]+';top:48;width:213px"','',cf1,cf4+'66',ccf1[0]+';top:51'+cf2,'ACM',cf1,cf4+'67',ccf1[0]+';top:66'+cf2,'CMG',cf1,cf4+'68',ccf1[0]+';top:81'+cf2,'COMMON',cf1,cf4+'69',ccf1[0]+';top:96'+cf2,'IEEE Computer Society',cf1,cf4+'70',ccf1[0]+';top:111'+cf2,'NASPA',cf1,cf4+'71',ccf1[0]+';top:126'+cf2,'Open Source Initiative',cf1+cf1+cf1,'pop10submain',ccf0[0]+';width:186px;height:83px"','','','pop10submaini',ccf1[0]+ccf2[0]+';width:184px;height:81px"','','',cf4+'72',ccf1[0]+';top:0'+cf2,'Computer History',cf1,cf4+'73',ccf1[0]+';top:15'+cf2,'History of 360/370/390',cf1,cf4+'74',ccf1[0]+';top:30'+cf2,'Hisotry of PC/370',cf1,cf4+'75',ccf1[0]+';top:45'+cf2,'IBM Product Timeline',cf1,cf5+'7',ccf3[0]+';top:63;width:180px"','',cf1,cf4+'76',ccf1[0]+';top:66'+cf2,'Green Cards',cf1+cf1+cf1,'pop11submain',ccf0[0]+';width:252px;height:158px"','','','pop11submaini',ccf1[0]+ccf2[0]+';width:250px;height:156px"','','',cf4+'77',ccf1[0]+';top:0'+cf2,'Assembler Services Books',cf1,cf4+'78',ccf1[0]+';top:15'+cf2,'z/OS Assembler Services',cf1,cf4+'79',ccf1[0]+';top:30'+cf2,'z/OS Assembler Services Ref. A-H',cf1,cf4+'80',ccf1[0]+';top:45'+cf2,'z/OS Assembler Services Ref. I-X',cf1,cf5+'8',ccf3[0]+';top:63;width:246px"','',cf1,cf4+'81',ccf1[0]+';top:66'+cf2,'z/OS Macros for Data Sets',cf1,cf4+'82',ccf1[0]+';top:81'+cf2,'z/OS Authorized Services Guide',cf1,cf4+'83',ccf1[0]+';top:96'+cf2,'z/OS Authorized ALE-DYN',cf1,cf4+'84',ccf1[0]+';top:111'+cf2,'z/OS Authorized ENF-IXG',cf1,cf4+'85',ccf1[0]+';top:126'+cf2,'z/OS Authorized LLA-SDU',cf1,cf4+'86',ccf1[0]+';top:141'+cf2,'z/OS Authorized SET-WTO',cf1+cf1+cf1,'pop12submain',ccf0[0]+';width:212px;height:92px"','','','pop12submaini',ccf1[0]+ccf2[0]+';width:210px;height:90px"','','',cf4+'87',ccf1[0]+';top:0'+cf2,'Assembler for Mainframes','<IMG Id="'+cf4+'87arrow" style="position:absolute;left:200px;top:4px" '+ccf4[0]+cf1,cf4+'88',ccf1[0]+';top:15'+cf2,'Assember for Intel','<IMG Id="'+cf4+'88arrow" style="position:absolute;left:200px;top:4px" '+ccf4[0]+cf1,cf4+'89',ccf1[0]+';top:30'+cf2,'C++','<IMG Id="'+cf4+'89arrow" style="position:absolute;left:200px;top:4px" '+ccf4[0]+cf1,cf4+'90',ccf1[0]+';top:45'+cf2,'Cobol Links',cf1,cf4+'91',ccf1[0]+';top:60'+cf2,'Java Links','<IMG Id="'+cf4+'91arrow" style="position:absolute;left:200px;top:4px" '+ccf4[0]+cf1,cf4+'92',ccf1[0]+';top:75'+cf2,'PLI',cf1+cf1+cf1,'pop13submain',ccf0[0]+';width:255px;height:47px"','','','pop13submaini',ccf1[0]+ccf2[0]+';width:253px;height:45px"','','',cf4+'93',ccf1[0]+';top:0'+cf2,'HLA High Level Assembler for Intel','<IMG Id="'+cf4+'93arrow" style="position:absolute;left:243px;top:4px" '+ccf4[0]+cf1,cf4+'94',ccf1[0]+';top:15'+cf2,'MASM Microsoft Assembler',cf1,cf4+'95',ccf1[0]+';top:30'+cf2,'Pentium Processor Manuals',cf1+cf1+cf1,'pop14submain',ccf0[0]+';width:232px;height:77px"','','','pop14submaini',ccf1[0]+ccf2[0]+';width:230px;height:75px"','','',cf4+'96',ccf1[0]+';top:0'+cf2,'z/OS Data Areas Vol. 1 - AB-DB',cf1,cf4+'97',ccf1[0]+';top:15'+cf2,'z/OS Data Areas Vol. 2 - DC-IT',cf1,cf4+'98',ccf1[0]+';top:30'+cf2,'z/OS Data Areas Vol. 3 - IC-RC',cf1,cf4+'99',ccf1[0]+';top:45'+cf2,'z/OS Data Areas Vol. 4 - RD-SD',cf1,cf4+'100',ccf1[0]+';top:60'+cf2,'z/OS Data Areas Vol. 5 - SS-XT',cf1+cf1+cf1,'pop15submain',ccf0[0]+';width:297px;height:68px"','','','pop15submaini',ccf1[0]+ccf2[0]+';width:295px;height:66px"','','',cf4+'101',ccf1[0]+';top:0'+cf2,'z390 Downloads',cf1,cf5+'9',ccf3[0]+';top:18;width:291px"','',cf1,cf4+'102',ccf1[0]+';top:21'+cf2,'J2RE Downloads',cf1,cf4+'103',ccf1[0]+';top:36'+cf2,'Eclipse Downloads',cf1,cf4+'104',ccf1[0]+';top:51'+cf2,'Additional demos',cf1+cf1+cf1,'pop16submain',ccf0[0]+';width:330px;height:239px"','','','pop16submaini',ccf1[0]+ccf2[0]+';width:328px;height:237px"','','',cf4+'105',ccf1[0]+';top:0'+cf2,'z390 Getting Started',cf1,cf4+'106',ccf1[0]+';top:15'+cf2,'z390 Frequently Asked Questions',cf1,cf4+'107',ccf1[0]+';top:30'+cf2,'z390 User Guide',cf1,cf5+'10',ccf3[0]+';top:48;width:324px"','',cf1,cf4+'108',ccf1[0]+';top:51'+cf2,'z390 File Services Guide',cf1,cf4+'109',ccf1[0]+';top:66'+cf2,'z390 EXEC CICS Compatible Assembler Support',cf1,cf4+'110',ccf1[0]+';top:81'+cf2,'z390 GUAM GUI Access Method Guide',cf1,cf4+'111',ccf1[0]+';top:96'+cf2,'z390 Macro Services Guide',cf1,cf4+'112',ccf1[0]+';top:111'+cf2,'z390 Macro Pseudo Code',cf1,cf4+'113',ccf1[0]+';top:126'+cf2,'z390 Program Services Guide',cf1,cf4+'114',ccf1[0]+';top:141'+cf2,'z390 SOA Client Server Overview Slides',cf1,cf4+'115',ccf1[0]+';top:156'+cf2,'z390 SOA Application Generator Support',cf1,cf4+'116',ccf1[0]+';top:171'+cf2,'z390 SOA User Guide',cf1,cf4+'117',ccf1[0]+';top:186'+cf2,'z390 Storage Services',cf1,cf5+'11',ccf3[0]+';top:204;width:324px"','',cf1,cf4+'118',ccf1[0]+';top:207'+cf2,'HLASM References','<IMG Id="'+cf4+'118arrow" style="position:absolute;left:318px;top:4px" '+ccf4[0]+cf1,cf4+'119',ccf1[0]+';top:222'+cf2,'Java References','<IMG Id="'+cf4+'119arrow" style="position:absolute;left:318px;top:4px" '+ccf4[0]+cf1+cf1+cf1,'pop17submain',ccf0[0]+';width:227px;height:68px"','','','pop17submaini',ccf1[0]+ccf2[0]+';width:225px;height:66px"','','',cf4+'120',ccf1[0]+';top:0'+cf2,'Support Information',cf1,cf4+'121',ccf1[0]+';top:15'+cf2,'RPI Request Form',cf1,cf4+'122',ccf1[0]+';top:30'+cf2,'RPI Pending Request Log',cf1,cf5+'12',ccf3[0]+';top:48;width:221px"','',cf1,cf4+'123',ccf1[0]+';top:51'+cf2,'z390 User Group Email Forum',cf1+cf1+cf1,'pop18submain',ccf0[0]+';width:235px;height:92px"','','','pop18submaini',ccf1[0]+ccf2[0]+';width:233px;height:90px"','','',cf4+'124',ccf1[0]+';top:0'+cf2,'MFATC Assembler Demos',cf1,cf4+'125',ccf1[0]+';top:15'+cf2,'Development ',cf1,cf4+'126',ccf1[0]+';top:30'+cf2,'Testing',cf1,cf4+'127',ccf1[0]+';top:45'+cf2,'Benchmarks',cf1,cf4+'128',ccf1[0]+';top:60'+cf2,'Statistics on z390 open source',cf1,cf4+'129',ccf1[0]+';top:75'+cf2,'Macro Pseudo Code',cf1+cf1+cf1,'pop19submain',ccf0[0]+';width:207px;height:173px"','','','pop19submaini',ccf1[0]+ccf2[0]+';width:205px;height:171px"','','',cf4+'130',ccf1[0]+';top:0'+cf2,'z/OS',cf1,cf4+'131',ccf1[0]+';top:15'+cf2,'z/VSE',cf1,cf4+'132',ccf1[0]+';top:30'+cf2,'z/VM',cf1,cf4+'133',ccf1[0]+';top:45'+cf2,'z/Linux',cf1,cf4+'134',ccf1[0]+';top:60'+cf2,'z/OS Internet Library',cf1,cf4+'135',ccf1[0]+';top:75'+cf2,'CBT Downloads and Links',cf1,cf4+'136',ccf1[0]+';top:90'+cf2,'Mainframe Dictionary',cf1,cf4+'137',ccf1[0]+';top:105'+cf2,'MVS 3.8J Public Domain',cf1,cf4+'138',ccf1[0]+';top:120'+cf2,'Planet MVS Dave\'s Links',cf1,cf5+'13',ccf3[0]+';top:138;width:201px"','',cf1,cf4+'139',ccf1[0]+';top:141'+cf2,'Linux',cf1,cf4+'140',ccf1[0]+';top:156'+cf2,'Windows',cf1+cf1+cf1,'pop20submain',ccf0[0]+';width:203px;height:83px"','','','pop20submaini',ccf1[0]+ccf2[0]+';width:201px;height:81px"','','',cf4+'141',ccf1[0]+';top:0'+cf2,'Java Language',cf1,cf4+'142',ccf1[0]+';top:15'+cf2,'J2RE 1.5.0 API Reference',cf1,cf5+'14',ccf3[0]+';top:33;width:197px"','',cf1,cf4+'143',ccf1[0]+';top:36'+cf2,'J2SE',cf1,cf4+'144',ccf1[0]+';top:51'+cf2,'J2EE',cf1,cf4+'145',ccf1[0]+';top:66'+cf2,'Eclipse',cf1+cf1+cf1,'pop21submain',ccf0[0]+';width:331px;height:32px"','','','pop21submaini',ccf1[0]+ccf2[0]+';width:329px;height:30px"','','',cf4+'146',ccf1[0]+';top:0'+cf2,'IBM 3270 Data Stream Programmers Reference',cf1,cf4+'147',ccf1[0]+';top:15'+cf2,'TN3270 Tutorial',cf1+cf1+cf1,'pop22submain',ccf0[0]+';width:179px;height:32px"','','','pop22submaini',ccf1[0]+ccf2[0]+';width:177px;height:30px"','','',cf4+'148',ccf1[0]+';top:0'+cf2,'IBM CICS',cf1,cf4+'149',ccf1[0]+';top:15'+cf2,'Using EXEC Interface',cf1+cf1+cf1,'pop23submain',ccf0[0]+';width:232px;height:47px"','','','pop23submaini',ccf1[0]+ccf2[0]+';width:230px;height:45px"','','',cf4+'150',ccf1[0]+';top:0'+cf2,'Visual C++ Express 2005',cf1,cf4+'151',ccf1[0]+';top:15'+cf2,'Visual C++ Reference',cf1,cf4+'152',ccf1[0]+';top:30'+cf2,'Help for C++ Hello World demo',cf1+cf1+cf1,'pop24submain',ccf0[0]+';width:233px;height:62px"','','','pop24submaini',ccf1[0]+ccf2[0]+';width:231px;height:60px"','','',cf4+'153',ccf1[0]+';top:0'+cf2,'High Level Assembler (Intel)',cf1,cf4+'154',ccf1[0]+';top:15'+cf2,'HLA User Guide',cf1,cf4+'155',ccf1[0]+';top:30'+cf2,'Help for HLA Hello World demo',cf1,cf4+'156',ccf1[0]+';top:45'+cf2,'HLA Example Programs',cf1+cf1+cf1);
var pmimsh = new Array(cf4+'0','Downloads',1,0,cf4+'1','Documentation',1,0,cf4+'2','Support',1,0,cf4+'3','Reference',1,0,cf4+'4','Links',1,0,cf4+'5','Help',1,0);

pmhcde='<Div Id="odummy" style="position:absolute;left:0;top:0;width:'+window.innerWidth+';height:'+window.innerHeight+'">&nbsp;</Div><Div Id="popMain" style="position:absolute;left:'+poX+';top:'+(poY)+';background-color:'+pmclr[0][0]+';width:'+popmcwidth+'"><TABLE cellpadding=0 cellspacing=0 Id="popMaini" width='+popmcwidth+' height='+popmcheight+' Align="Left"><TR Id="popHtr" style="color:'+pmclr[0][3]+pmcfnt+'">';
for (x=0; x<pmdbtxt.length; x++)
if (pmdbtxt[x])
pmhcde+='<Div Id="DboX'+x+'" style="position:absolute;visibility:hidden;z-Index:2000;padding:5px 5px 5px 5px;text-align:left;font-family:Arial;font-size:12px;background-color:'+pmdbtxt[x][1]+';color:'+pmdbtxt[x][2]+';border:'+pmdbtxt[x][3]+';width:'+pmdbtxt[x][4]+((pmdbtxt[x][5]) ? ";background-image:url('"+pmdbtxt[x][5]+"')" : "")+'">'+pmdbtxt[x][0]+'</Div>';

tmppat = new RegExp('Id="'+cf4+'\\dimg');
for (x=0; x<pmimsh.length; x+=4)
if (pmimsh[x].match(cf8))
pmhcde+= cf8;
else
pmhcde+= cf9+pmimsh[x]+cf6+cf0+pmimsh[x]+'d">'+((pmimsh[(x+1)].match(cfB) && pmimsh[(x+1)].match('Id="'+cf4)) ? pmimsh[(x+1)].replace(tmppat,"") : pmimsh[(x+1)])+cf1+cf0+pmimsh[x]+'h'+cf7+'padding-top:'+((window.print) ? (eval(pmimsh[(x+2)])-1) :pmimsh[(x+2)])+';padding-bottom:'+((window.print) ? (eval(pmimsh[(x+3)])+1) :pmimsh[(x+3)])+'">'+pmimsh[(x+1)]+cf1+'</TD>';
pmhcde+='</TR></TABLE></Div>';
pmlvl = 0;
for (x=0; x<pmimsa.length; x+=4){
if (pmimsa[x].match("submaini"))
pmlvl++
if (pmimsa[x].match(cf4))
pmhcde+=cf0+pmimsa[x]+pmimsa[(x+1)]+'>'+cf0+pmimsa[x]+'d'+cf3+';width:'+pmmwds[pmlvl-1]+'">'+((pmimsa[(x+2)].match(cfB) && pmimsa[(x+2)].match('Id="'+cf4)) ? pmimsa[(x+2)].replace(tmppat,"") : pmimsa[(x+2)])+cf1+cf0+pmimsa[x]+'h'+cf3+';visibility:hidden;background-color:'+pmclr[popclrindx[pmlvl]][1]+'">'+pmimsa[(x+2)]+cf1+pmimsa[(x+3)];
else
pmhcde+=cf0+pmimsa[x]+pmimsa[(x+1)]+'>'+pmimsa[(x+3)];
}
document.write(pmhcde);
document.getElementById("odummy").style.visibility = "hidden";

function popmcreate(){
popMain = document.getElementById("popMain");
popMaini = document.getElementById("popMaini");
popMain.style.zIndex = 1000;
document.onmousedown=setpopdsploff;
popsmf();
poprestimer = setTimeout('checkresize()',500);
ecY = popMain.style.pixelTop;
}

function setpopdsploff(e){
popdsploff(1,e);
}

popmcreate();
