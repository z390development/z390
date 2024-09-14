/***************************************************************
* Pop-Up Version 4.8.0 Pro H
* © 1998-2004
* Anoxy Software
* All Rights Reserved
* You are not allowed to modify anything in this Script
****************************************************************
* To get your own copy visit: http://www.anoxy.com
****************************************************************/

var slimb,slimr,trgtlay,hilay,pcsl,ecY,wox,woy,pmw,wlw,wlh,mdelaytimer,popanimtimer,popswitchtimer,popswotimer,popnpostimer,popdescrtimer,popnmem,popcmem,poplmem,popvmem,popnwin,pophbar,popvbar,popMain,clrnum,DboX,popXURLV;
var submenu = new Array();
var pmpimg = new Array();
submenu[0] = "popMain";
var posub = new Array();
var Pmpat = /\D/g;
var popamem = -1;
var poplevel = pdchk = 0;
var scc = 4;

function popsanim(lobj,cr,cw,lp,te){
lobjs = (te) ? document.getElementById(lobj).offsetHeight : document.getElementById(lobj).offsetWidth;
popsmcX = Math.floor(lobjs*(4-scc)/4);
popsmX = Math.floor(lobjs*scc/4);
if (scc>=0){
scc--;
document.getElementById(lobj).style.clip = (te) ? "rect("+(Math.abs(cr-1)*popsmX)+" "+document.getElementById(lobj).offsetWidth+" "+(cr*popsmcX+Math.abs(cr-1)*cw)+" 0)" : "rect(0 "+(Math.abs(cr-1)*popsmcX+cw)+" "+document.getElementById(lobj).offsetHeight+" "+(cr*popsmX)+")";
if (scc<3){
if (te)
document.getElementById(lobj).style.top = (cr==1) ? (lp+popsmX) : (lp-popsmX);
else
document.getElementById(lobj).style.left = (cr==1) ? (lp-popsmX) : (lp+popsmX);
}
popanimtimer = window.setTimeout('popsanim("'+lobj+'",'+cr+','+cw+','+lp+','+te+')',25);
}
else
scc = 4;
}

function popdsploff(s,e){
if (e)
if (e.originalTarget == "[object XULElement]")
return;
onsubop = false;
ptid = (e) ? e.target.id || e.target.parentNode.id || "" : "";
if (ptid.indexOf("menuItem")==0){
psnum = ptid.replace("menuItem","");
psnum = psnum.replace("arrow","");
if (pmact[psnum] >6)
onsubop = true;
}
clearTimeout(popdescrtimer);
popdcrfxoff();
if (!onsubop)
popsubctrl(0,s,0,0);
else
if (popanimenabled && pmact[psnum] == 9){
if (mdelaytimer)
clearTimeout(mdelaytimer);
if (e.target.parentNode.parentNode.id == "popHtr")
pophon2(1);
else
popmion2(1);
}
}

function popsubctrl(p1,p2,p3,p4){
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
if (x-p3+p4<posub.length)
if (posub[x-p3+p4][0] != -1){
clrnum = popclrindx[document.getElementById(posub[x-p3+p4][0]).parentNode.parentNode.id.replace(Pmpat,"")] || 0;
with(document.getElementById(posub[x-p3+p4][0])){
style.color = pmclr[clrnum][3];
style.backgroundColor = pmclr[clrnum][0];
}
if (x-p3+p4>0)
document.images["menuItem"+posub[x-p3+p4][1]+"arrow"].src = popaimg[clrnum].src;
posub[x-p3+p4][0] = -1;
if (pmpimg[posub[x-p3+p4][1]] && pmpimg[posub[x-p3+p4][1]]!=-1){
document.images[cf5+posub[x-p3+p4][1]+"img"].src = pmpimg[posub[x-p3+p4][1]];
pmpimg[posub[x-p3+p4][1]] = -1;
}
}
}
}

function setpopnpos(){
if (popnpostimer)
window.clearTimeout(popnpostimer)
popnpostimer = window.setTimeout('setpopnpos2()',1000);
}

function setpopnpos2(){
setpopvars();
}

function setpopvars(w){
wlw = window.innerWidth;
wlh = window.innerHeight;
popvbar = (document.height+16>window.innerHeight) ? 1 :0;
pophbar = (document.width+16>window.innerWidth) ? 1 : 0;
popsmf();
}

function popmidown(e){
if (pmact[pcsl]<7 && (e.button==0 || (e.button==1 && (navigator.userAgent.indexOf("Safari")>=0 || navigator.userAgent.indexOf("Opera")>=0))))
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
tgfr = top.frames[pmitfm[pcsl]] || parent.frames[pmitfm[pcsl]] || document.frames[pmitfm[pcsl]] || null;
if (tgfr)
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

function popmion(id,e){
pechk = popechk(e);
if (pechk==0 && pdchk==0){
if (popswotimer)
clearTimeout(popswotimer);
popdcrfxoff();
tpcsl = id.substring(8,id.length);
if (pmact[tpcsl] != 8){
if (popswitchtimer)
clearTimeout(popswitchtimer);
hilay = document.getElementById(id);
trgtlay = hilay.parentNode;
while (!trgtlay.id.match("pop") || trgtlay.id.match("aini") || trgtlay.id.match("Htr") && trgtlay.parentNode)
trgtlay = trgtlay.parentNode;
clrnum = popclrindx[trgtlay.id.replace(Pmpat,"")] || 0;
hilay.style.color = pmclr[clrnum][2];
hilay.style.backgroundColor = pmclr[clrnum][1];
pcsl = id.substring(8,id.length);
if (pmsbtxt[pcsl])
window.status = unescape(pmsbtxt[pcsl]);
if (pmoimg[pcsl]){
if (!pmpimg[pcsl] || pmpimg[pcsl]==-1)
pmpimg[pcsl] = document.images[cf5+pcsl+"img"].src;
document.images[cf5+pcsl+"img"].src = pmoimg[pcsl].src;
}
for (x=0; x<submenu.length; x++)
if(trgtlay.id == submenu[x]){
poplevel = x;
break;
}
popsubctrl(poplevel+1,poplevel+1,1,0);
if (poplevel==0)
popsubctrl(poplevel+1,poplevel+2,1,1);
if (pmact[pcsl] == 9)
if (poplevel==0)
pophon2(0);
else
popmion2(0);
if (pmdbtxt[pcsl])
popdescrtimer = setTimeout('popdscron()',popdescrtimermsec);
}
}
}

function pophon2(pcv){
popamem = -1;
if (document.getElementById(pmiurl[pcsl]).style.visibility == "visible" && document.getElementById(pmiurl[pcsl]).style.clip != "rect(0pt 0pt 0pt 0pt)" && popvmem == pcsl)
return;
popsmsf();
popvmem = pcsl;
smpxS = (navigator.userAgent.indexOf("Safari")>=0) ? hilay.offsetParent.offsetLeft : 0;
smpxav = (hilay.offsetParent) ? trgtlay.offsetLeft : 0;
smpx = (hilay.offsetLeft+smpxS+wdp>slimr) ? ((slimr-wdp>wox) ? slimr-wdp : wox) : hilay.offsetLeft+smpxS+smpxav;
smpyav = (hilay.offsetParent) ? trgtlay.offsetTop : 0;
smpy = (popopendir == 0) ? hilay.offsetTop+trgtlay.offsetHeight+smpyav+1 : hilay.offsetTop+smpyav-hgp-1;
with(document.getElementById(pmiurl[pcsl]).style){
top = smpy;
left = smpx;
if (popanimenabled)
clip = "rect(0pt 0pt 0pt 0pt)";
else
visibility = "visible";
}
cr = popopendir;
cw = hgp;
if (popanimenabled){
if (popcmem == trgtlay.id && scc<4)
mdelaytimer = setTimeout('pophon2(0)',200);
else{
popcmem = pmiurl[pcsl];
scc = 4;
if (popanimtimer)
clearTimeout(popanimtimer);
if (pcv==0)
mdelaytimer = window.setTimeout('document.getElementById(pmiurl[pcsl]).style.visibility = "visible";popsanim(pmiurl[pcsl],cr,cw,smpy,true)',popdeltimermsec);
else{
document.getElementById(pmiurl[pcsl]).style.visibility = "visible";
popsanim(pmiurl[pcsl],cr,cw,smpy,true);
}
}
}
}

function popdscron(){
if (scc==4){
DboX.innerHTML = pmdbtxt[pcsl][0];
with(DboX.style){
backgroundImage = (pmdbtxt[pcsl][5]) ? "url('"+pmdbtxt[pcsl][5]+"')" : "none";
backgroundColor = pmdbtxt[pcsl][1];
color = pmdbtxt[pcsl][2];
border = pmdbtxt[pcsl][3];
width = pmdbtxt[pcsl][4];
dbow = DboX.offsetWidth;
dboh = DboX.offsetHeight;
if (pmact[pcsl] == 9){
pmsol = document.getElementById(pmiurl[pcsl]);
dbxp = pmsol.offsetLeft+((pmsol.offsetWidth-dbow>0) ? (pmsol.offsetWidth-dbow)/2 : 0);
dbyp = pmsol.offsetTop+((popopendir==0) ? pmsol.offsetHeight+2 : dboh*-1-2);
if (dbxp+dbow >= slimr)
dbxp = slimr-dbow-2;
if (dbyp+dboh >= slimb)
dbyp = pmsol.offsetTop-dboh-2;
}
else{
dbxp = trgtlay.offsetLeft+2+ ((trgtlay.id=="popMain") ? hilay.offsetLeft : trgtlay.offsetWidth);
dbyp = ((hilay.offsetParent) ? trgtlay.offsetTop+hilay.offsetTop : hilay.offsetTop)+((trgtlay.id=="popMain") ? ((popopendir==0) ? trgtlay.offsetHeight+2 : dboh*-1-2) : 0);
if (dbxp+dbow >= slimr)
dbxp = (trgtlay.id=="popMain") ? slimr-dbow-4 : trgtlay.offsetLeft-dbow-2;
if (dbxp<wox)
dbxp = wox;
if (dbyp+dboh >= slimb)
dbyp = slimb-dboh-2;
if (dbyp<woy)
dbyp = woy;
}
left = dbxp;
top = dbyp;
visibility = "visible";
DboX.style.MozOpacity = 0;
setTimeout('popdcrfxon()',45);
}
}
else
popdescrtimer = setTimeout('popdscron()',100);
}

function popdcrfxoff(){
DboX.style.MozOpacity-=0.2;
if (DboX.style.MozOpacity>=0.2)
setTimeout('popdcrfxoff()',45);
else
popdscroff();
}

function popdcrfxon(){
DboX.style.MozOpacity = eval(DboX.style.MozOpacity)+0.2;
if (DboX.style.MozOpacity<=0.8)
setTimeout('popdcrfxon()',45);
}

function popdscroff(){
if (DboX.style.visibility == "visible")
DboX.style.visibility = "hidden";
}

function popsmf(){
wox = window.pageXOffset;
woy = window.pageYOffset;
slimb = wlh+woy-(17*pophbar);
slimr = wlw+wox-(17*popvbar);
}

function popsmsf(){
poplmem = poplevel;
popsmf();
submenu[(poplevel+1)] = pmiurl[pcsl];
posub[poplevel] = new Array(hilay.id,pcsl);
document.getElementById(pmiurl[pcsl]).style.zIndex = eval(trgtlay.style.zIndex)+1;
wdp = document.getElementById(pmiurl[pcsl]).offsetWidth;
hgp = document.getElementById(pmiurl[pcsl]).offsetHeight;
}

function popmion2(pcv){
if (document.getElementById(pmiurl[pcsl]).style.visibility == "visible" && document.getElementById(pmiurl[pcsl]).style.clip != "rect(0pt 0pt 0pt 0pt)" && !(popnmem != pcsl && poplmem == poplevel))
return;
document.images["menuItem"+pcsl+"arrow"].src = popoimg[clrnum].src;
popamem = pcsl;
popnmem = pcsl;
popsmsf();
popcorX = false;
smpx = trgtlay.offsetLeft+hilay.offsetWidth-2;
smpy = ((hilay.offsetParent) ? trgtlay.offsetTop+hilay.offsetTop : hilay.offsetTop);
if (smpx+wdp >= slimr){
smpx = trgtlay.offsetLeft-wdp+6;
popcorX = true;
}
if (smpy+hgp >= slimb)
smpy = smpy-hgp+hilay.offsetHeight;
if (smpx<wox)
smpx = trgtlay.offsetLeft+6;
if (smpy<woy)
smpy = woy;
with(document.getElementById(pmiurl[pcsl]).style){
top = smpy;
left = smpx;
if (popanimenabled)
clip = "rect(0pt 0pt 0pt 0pt)";
else
visibility = "visible";
}
cr = (popcorX) ? 0:1;
cw = (popcorX) ? 0:wdp;
if (popanimenabled){
if (popcmem == trgtlay.id && scc<4)
mdelaytimer = setTimeout('popmion2(0)',200);
else{
popcmem = pmiurl[pcsl];
scc = 4;
if (popanimtimer)
clearTimeout(popanimtimer);
document.getElementById(pmiurl[pcsl]).style.visibility = "visible";
if (pcv==0)
mdelaytimer = window.setTimeout('popsanim(pmiurl[pcsl],cr,cw,smpx,false)',popdeltimermsec);
else
popsanim(pmiurl[pcsl],cr,cw,smpx,false);
}
}
}

function popmioff(id,e){
pechk = popechk(e);
pnEl = (e.relatedTarget) ? ((e.relatedTarget.id == undefined && navigator.userAgent.indexOf("Safari")==0) ? e.rangeParent.parentNode.id : e.relatedTarget.id) : "";
pdchk = (e.currentTarget.id == pnEl) ? 1 : 0;
if (pechk==0 && pdchk==0){
if (pmsbtxt[pcsl])
window.status = "";
clearTimeout(popdescrtimer);
popdcrfxoff();
if (popautoswitch)
popswotimer = setTimeout('popdsploff(1)',popsotimermsec);
deletehilay = true;
for (x=0; x<posub.length; x++)
if (posub[x][0] == id){
deletehilay = false;
popamem = -1;
if (popanimenabled)
popswitchtimer = setTimeout('pophhlay()',50);
break;
}
if (deletehilay){
clrnum = popclrindx[document.getElementById(id).parentNode.parentNode.id.replace(Pmpat,"")] || 0;
with (document.getElementById(id)){
style.color = pmclr[clrnum][3];
style.backgroundColor = pmclr[clrnum][0];
}
if (pmpimg[pcsl]){
document.images[cf5+pcsl+"img"].src = pmpimg[pcsl];
pmpimg[pcsl] = -1;
}
}
if (mdelaytimer)
clearTimeout(mdelaytimer);
}
}

function pophhlay(){
e = posub[poplevel][0];
if (e!=-1){
psnum = posub[poplevel][1];
if (document.getElementById(pmiurl[pcsl]).style.clip == "rect(0pt 0pt 0pt 0pt)"){
clrnum = popclrindx[document.getElementById(e).parentNode.parentNode.id.replace(Pmpat,"")] || 0;
with (document.getElementById(e)){
style.color = pmclr[clrnum][3];
style.backgroundColor = pmclr[clrnum][0];
}
if (poplevel>0)
document.images["menuItem"+psnum+"arrow"].src = popaimg[clrnum].src;
}
}
}

function popechk(e){
pfEl = (e.originalTarget) ? e.originalTarget.id : null;
ptEl = (e.relatedTarget) ? e.relatedTarget.id : null;
if ((pfEl+"arrow" == ptEl) || (pfEl == ptEl+"arrow") || (pfEl+"Harrow" == ptEl) || (pfEl == ptEl+"Harrow") || (pfEl+"img" == ptEl) || (pfEl == ptEl+"img"))
return(1);
else
return(0);
}

function popcacnelso(){
if (popswotimer)
clearTimeout(popswotimer);
if (popdescrtimer)
clearTimeout(popdescrtimer);
popdcrfxoff();
}
//Pop-Up 4.7 H Menu file (Mozilla)

var poptimer;
var popautoswitch = false;
var popanimenabled = true;
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
var pmcfnt = ";font-family:Arial,Arial,Times New Roman,Verdana;font-size:12px;font-weight:bold;font-style:normal";
cf0 = '<Div Id="';
cf1 = '</Div>';
cf2 = '"></Span>';
cf3 = pmcfnt+';padding:0px 0px 0px 18px" onmouseover="popmion(this.id,event)" onmouseout="popmioff(this.id,event)" onmousedown="popmidown(event)"';
cf5 = 'menuItem';
cf6 = 'menuSeparator';
cf7 = '" style="padding:0px 15px 0px 15px;color:'+pmclr[0][3]+pmcfnt+'" onmouseover="popmion(this.id,event)" onmouseout="popmioff(this.id,event)" onmousedown="popmidown(event)">';
cf8 = '<TD id="';
cf9 = '<TD width="4"><Div style="border-left: 1px solid '+pmclr[0][10]+';border-right:1px solid '+pmclr[0][11]+';height:'+popmcheight+'px;position:relative;width:1px"></Div></TD>';

ccf0 = new Array();
ccf1 = new Array();
ccf2 = new Array();
ccf3 = new Array();
ccf4 = new Array();
ccf5 = new Array();
popaimg = new Array();
popoimg = new Array();
for (x=0; x<pmclr.length;x++){
popaimg[x] = new Image();
popoimg[x] = new Image();
popaimg[x].src = popbasedir+"pop_arw"+pmclr[popclrindx[x]][12]+".gif";
popoimg[x].src = popbasedir+"popo_arw"+pmclr[popclrindx[x]][13]+".gif";
ccf0[x] = '" style="'+((pmclr[popclrindx[x]][16]<100) ? '-moz-opacity:'+pmclr[popclrindx[x]][16]/100+';' : '')+'position:absolute;visibility:hidden;overflow:hidden;border:solid 1px;background-color:'+pmclr[popclrindx[x]][0];
if (pmclr[popclrindx[x]][8]=="transparent")
ccf0[x] += ';border-left:none;border-top:none';
else
ccf0[x] += ';border-left-color:'+pmclr[popclrindx[x]][8]+';border-top-color:'+pmclr[popclrindx[x]][8]
if (pmclr[popclrindx[x]][9]=="transparent")
ccf0[x] += ';border-bottom:none;border-right:none';
else
ccf0[x] += ';border-bottom-color:'+pmclr[popclrindx[x]][9]+';border-right-color:'+pmclr[popclrindx[x]][9];
ccf1[x] = '" style="position:relative;visibility:inherit;left:0;top:0;cursor:default;color:'+pmclr[popclrindx[x]][3];
ccf2[x] = ((pmclr[popclrindx[x]][14]) ? ';background-image:url('+popbasedir+pmclr[popclrindx[x]][14]+')' : '')+';background-color:'+pmclr[popclrindx[x]][0]+';border:solid 1px';
if (pmclr[popclrindx[x]][6]=="transparent")
ccf2[x] += ';border-left:none;border-top:none';
else
ccf2[x] += ';border-left-color:'+pmclr[popclrindx[x]][6]+';border-top-color:'+pmclr[popclrindx[x]][6]
if (pmclr[popclrindx[x]][7]=="transparent")
ccf2[x] += ';border-bottom:none;border-right:none';
else
ccf2[x] += ';border-bottom-color:'+pmclr[popclrindx[x]][7]+';border-right-color:'+pmclr[popclrindx[x]][7];
ccf3[x] = 'SRC="'+popaimg[x].src+'" BORDER="0">';
ccf4[x] = '<Span style="border-bottom: 1px solid '+pmclr[popclrindx[x]][11]+';position:absolute;height:0px;';
if (pmclr[popclrindx[x]][15]!=2)
ccf4[x] += 'border-top: 1px solid '+pmclr[popclrindx[x]][10];
ccf4[x] += ';width:';
ccf5[x] = '" onmouseover="popcacnelso()" style="position: relative;top:0px;visibility:inherit;cursor:default;';
if (pmclr[popclrindx[x]][15]==0)
ccf5[x] += 'left:3px;width:99%;padding:2px 5px 4px 0px';
else
if (pmclr[popclrindx[x]][15]==1)
ccf5[x] += 'left:0px;width:100%;padding:2px 0px 4px 0px';
else
ccf5[x] += 'left:0px;width:101%;margin-bottom:1px';
ccf5[x] += '"';
}

var pmimsa = new Array('pop1submain',ccf0[0]+';width:201px"','','pop1submaini',ccf1[0]+ccf2[0]+';width:199px"','',cf5+'6',ccf1[0]+cf3,'Dave\'s Assembler FAQ'+cf1,cf5+'7',ccf1[0]+cf3,'IBM Main List FAQ'+cf1,cf6+'0',ccf5[0],ccf4[0]+'193'+cf2+cf1,cf5+'8',ccf1[0]+cf3,'Dave\'s Assembler Tips'+cf1,cf5+'9',ccf1[0]+cf3,'Wynsoft Assembler Tips'+cf1+cf1+cf1,'pop2submain',ccf0[0]+';width:245px"','','pop2submaini',ccf1[0]+ccf2[0]+';width:243px"','',cf5+'10',ccf1[0]+cf3,'Assembler ASM370 Group'+cf1,cf5+'11',ccf1[0]+cf3,'Assembler ASM370 List'+cf1,cf5+'12',ccf1[0]+cf3,'IBM-Main Group'+cf1,cf5+'13',ccf1[0]+cf3,'IBM-Main List'+cf1,cf5+'14',ccf1[0]+cf3,'z390 Assembler/Emulator Group'+cf1,cf5+'15',ccf1[0]+cf3,'Yahoo Group Search'+cf1+cf1+cf1,'pop3submain',ccf0[0]+';width:232px"','','pop3submaini',ccf1[0]+ccf2[0]+';width:230px"','',cf5+'16',ccf1[0]+cf3,'Language Reference V1R5'+cf1,cf5+'17',ccf1[0]+cf3,'Programers Guide V1R5'+cf1,cf5+'18',ccf1[0]+cf3,'Macro References<IMG Id="'+cf5+'18arrow" style="position:absolute;left:221px;top:4px" '+ccf3[0]+cf1,cf6+'1',ccf5[0],ccf4[0]+'224'+cf2+cf1,cf5+'19',ccf1[0]+cf3,'General Information'+cf1,cf5+'20',ccf1[0]+cf3,'Installation and Customization'+cf1,cf5+'21',ccf1[0]+cf3,'Language Environment'+cf1,cf5+'22',ccf1[0]+cf3,'z/Architecture POP '+cf1,cf5+'23',ccf1[0]+cf3,'Catalog of HLASM Manuals'+cf1+cf1+cf1,'pop4submain',ccf0[0]+';width:248px"','','pop4submaini',ccf1[0]+ccf2[0]+';width:246px"','',cf5+'24',ccf1[0]+cf3,'Assembler FAQ\'s<IMG Id="'+cf5+'24arrow" style="position:absolute;left:237px;top:4px" '+ccf3[0]+cf1,cf5+'25',ccf1[0]+cf3,'Control Blocks<IMG Id="'+cf5+'25arrow" style="position:absolute;left:237px;top:4px" '+ccf3[0]+cf1,cf5+'26',ccf1[0]+cf3,'Diagnostic Reference '+cf1,cf5+'27',ccf1[0]+cf3,'Simotime Assembler Connection'+cf1,cf6+'2',ccf5[0],ccf4[0]+'240'+cf2+cf1,cf5+'28',ccf1[0]+cf3,'Assembler Class CS-310'+cf1,cf5+'29',ccf1[0]+cf3,'HLASM Common Routines'+cf1,cf5+'30',ccf1[0]+cf3,'Adsvanced ASM and Interfaces'+cf1,cf5+'31',ccf1[0]+cf3,'System Reference Card (PDF)'+cf1+cf1+cf1,'pop5submain',ccf0[0]+';width:310px"','','pop5submaini',ccf1[0]+ccf2[0]+';width:308px"','',cf5+'32',ccf1[0]+cf3,'IBM HLASM Assembler'+cf1,cf6+'3',ccf5[0],ccf4[0]+'302'+cf2+cf1,cf5+'33',ccf1[0]+cf3,'Dingus'+cf1,cf5+'34',ccf1[0]+cf3,'HLA High Level Intel Assembler<IMG Id="'+cf5+'34arrow" style="position:absolute;left:299px;top:4px" '+ccf3[0]+cf1,cf5+'35',ccf1[0]+cf3,'Micro Focus Mainframe Express Assembler'+cf1,cf5+'36',ccf1[0]+cf3,'Tachyon Software'+cf1,cf5+'37',ccf1[0]+cf3,'z390 Portable Assembler/Emulator'+cf1+cf1+cf1,'pop6submain',ccf0[0]+';width:172px"','','pop6submaini',ccf1[0]+ccf2[0]+';width:170px"','',cf5+'38',ccf1[0]+cf3,'HLASM References'+cf1,cf5+'39',ccf1[0]+cf3,'Email Groups<IMG Id="'+cf5+'39arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'40',ccf1[0]+cf3,'Education<IMG Id="'+cf5+'40arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf6+'4',ccf5[0],ccf4[0]+'164'+cf2+cf1,cf5+'41',ccf1[0]+cf3,'Assemblers<IMG Id="'+cf5+'41arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'42',ccf1[0]+cf3,'Emulators<IMG Id="'+cf5+'42arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'43',ccf1[0]+cf3,'EXEC CICS<IMG Id="'+cf5+'43arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'44',ccf1[0]+cf3,'History<IMG Id="'+cf5+'44arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'45',ccf1[0]+cf3,'Languages<IMG Id="'+cf5+'45arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'46',ccf1[0]+cf3,'Operating Systems<IMG Id="'+cf5+'46arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'47',ccf1[0]+cf3,'Organizations<IMG Id="'+cf5+'47arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'48',ccf1[0]+cf3,'TN3270 Terminals<IMG Id="'+cf5+'48arrow" style="position:absolute;left:161px;top:4px" '+ccf3[0]+cf1,cf5+'49',ccf1[0]+cf3,'Webring - Dinos'+cf1+cf1+cf1,'pop7submain',ccf0[0]+';width:184px"','','pop7submaini',ccf1[0]+ccf2[0]+';width:182px"','',cf5+'50',ccf1[0]+cf3,'Home'+cf1,cf5+'51',ccf1[0]+cf3,'z390 Support'+cf1,cf5+'52',ccf1[0]+cf3,'Windows Update'+cf1,cf5+'53',ccf1[0]+cf3,'Ubuntu Linux Support'+cf1,cf5+'54',ccf1[0]+cf3,'Site Map'+cf1,cf5+'55',ccf1[0]+cf3,'About'+cf1+cf1+cf1,'pop8submain',ccf0[0]+';width:267px"','','pop8submaini',ccf1[0]+ccf2[0]+';width:265px"','',cf5+'56',ccf1[0]+cf3,'z390 Portable Assembler/Emulator'+cf1,cf6+'5',ccf5[0],ccf4[0]+'259'+cf2+cf1,cf5+'57',ccf1[0]+cf3,'Flexes S/390 on Intel'+cf1,cf5+'58',ccf1[0]+cf3,'Open Mainframe on Intel'+cf1,cf5+'59',ccf1[0]+cf3,'Hercules S/390 on Windows & Linux'+cf1,cf5+'60',ccf1[0]+cf3,'PC/370'+cf1,cf5+'61',ccf1[0]+cf3,'Soft/390 on Intel'+cf1,cf5+'62',ccf1[0]+cf3,'UMX OS/390 on Intel'+cf1+cf1+cf1,'pop9submain',ccf0[0]+';width:221px"','','pop9submaini',ccf1[0]+ccf2[0]+';width:219px"','',cf5+'63',ccf1[0]+cf3,'SHARE'+cf1,cf5+'64',ccf1[0]+cf3,'z390 Presentation at SHARE'+cf1,cf5+'65',ccf1[0]+cf3,'CBT SPLA Repository'+cf1,cf6+'6',ccf5[0],ccf4[0]+'213'+cf2+cf1,cf5+'66',ccf1[0]+cf3,'ACM'+cf1,cf5+'67',ccf1[0]+cf3,'CMG'+cf1,cf5+'68',ccf1[0]+cf3,'COMMON'+cf1,cf5+'69',ccf1[0]+cf3,'IEEE Computer Society'+cf1,cf5+'70',ccf1[0]+cf3,'NASPA'+cf1,cf5+'71',ccf1[0]+cf3,'Open Source Initiative'+cf1+cf1+cf1,'pop10submain',ccf0[0]+';width:188px"','','pop10submaini',ccf1[0]+ccf2[0]+';width:186px"','',cf5+'72',ccf1[0]+cf3,'Computer History'+cf1,cf5+'73',ccf1[0]+cf3,'History of 360/370/390'+cf1,cf5+'74',ccf1[0]+cf3,'Hisotry of PC/370'+cf1,cf5+'75',ccf1[0]+cf3,'IBM Product Timeline'+cf1,cf6+'7',ccf5[0],ccf4[0]+'180'+cf2+cf1,cf5+'76',ccf1[0]+cf3,'Green Cards'+cf1+cf1+cf1,'pop11submain',ccf0[0]+';width:254px"','','pop11submaini',ccf1[0]+ccf2[0]+';width:252px"','',cf5+'77',ccf1[0]+cf3,'Assembler Services Books'+cf1,cf5+'78',ccf1[0]+cf3,'z/OS Assembler Services'+cf1,cf5+'79',ccf1[0]+cf3,'z/OS Assembler Services Ref. A-H'+cf1,cf5+'80',ccf1[0]+cf3,'z/OS Assembler Services Ref. I-X'+cf1,cf6+'8',ccf5[0],ccf4[0]+'246'+cf2+cf1,cf5+'81',ccf1[0]+cf3,'z/OS Macros for Data Sets'+cf1,cf5+'82',ccf1[0]+cf3,'z/OS Authorized Services Guide'+cf1,cf5+'83',ccf1[0]+cf3,'z/OS Authorized ALE-DYN'+cf1,cf5+'84',ccf1[0]+cf3,'z/OS Authorized ENF-IXG'+cf1,cf5+'85',ccf1[0]+cf3,'z/OS Authorized LLA-SDU'+cf1,cf5+'86',ccf1[0]+cf3,'z/OS Authorized SET-WTO'+cf1+cf1+cf1,'pop12submain',ccf0[0]+';width:214px"','','pop12submaini',ccf1[0]+ccf2[0]+';width:212px"','',cf5+'87',ccf1[0]+cf3,'Assembler for Mainframes<IMG Id="'+cf5+'87arrow" style="position:absolute;left:203px;top:4px" '+ccf3[0]+cf1,cf5+'88',ccf1[0]+cf3,'Assember for Intel<IMG Id="'+cf5+'88arrow" style="position:absolute;left:203px;top:4px" '+ccf3[0]+cf1,cf5+'89',ccf1[0]+cf3,'C++<IMG Id="'+cf5+'89arrow" style="position:absolute;left:203px;top:4px" '+ccf3[0]+cf1,cf5+'90',ccf1[0]+cf3,'Cobol Links'+cf1,cf5+'91',ccf1[0]+cf3,'Java Links<IMG Id="'+cf5+'91arrow" style="position:absolute;left:203px;top:4px" '+ccf3[0]+cf1,cf5+'92',ccf1[0]+cf3,'PLI'+cf1+cf1+cf1,'pop13submain',ccf0[0]+';width:257px"','','pop13submaini',ccf1[0]+ccf2[0]+';width:255px"','',cf5+'93',ccf1[0]+cf3,'HLA High Level Assembler for Intel<IMG Id="'+cf5+'93arrow" style="position:absolute;left:246px;top:4px" '+ccf3[0]+cf1,cf5+'94',ccf1[0]+cf3,'MASM Microsoft Assembler'+cf1,cf5+'95',ccf1[0]+cf3,'Pentium Processor Manuals'+cf1+cf1+cf1,'pop14submain',ccf0[0]+';width:234px"','','pop14submaini',ccf1[0]+ccf2[0]+';width:232px"','',cf5+'96',ccf1[0]+cf3,'z/OS Data Areas Vol. 1 - AB-DB'+cf1,cf5+'97',ccf1[0]+cf3,'z/OS Data Areas Vol. 2 - DC-IT'+cf1,cf5+'98',ccf1[0]+cf3,'z/OS Data Areas Vol. 3 - IC-RC'+cf1,cf5+'99',ccf1[0]+cf3,'z/OS Data Areas Vol. 4 - RD-SD'+cf1,cf5+'100',ccf1[0]+cf3,'z/OS Data Areas Vol. 5 - SS-XT'+cf1+cf1+cf1,'pop15submain',ccf0[0]+';width:299px"','','pop15submaini',ccf1[0]+ccf2[0]+';width:297px"','',cf5+'101',ccf1[0]+cf3,'z390 Downloads'+cf1,cf6+'9',ccf5[0],ccf4[0]+'291'+cf2+cf1,cf5+'102',ccf1[0]+cf3,'J2RE Downloads'+cf1,cf5+'103',ccf1[0]+cf3,'Eclipse Downloads'+cf1,cf5+'104',ccf1[0]+cf3,'Additional demos'+cf1+cf1+cf1,'pop16submain',ccf0[0]+';width:332px"','','pop16submaini',ccf1[0]+ccf2[0]+';width:330px"','',cf5+'105',ccf1[0]+cf3,'z390 Getting Started'+cf1,cf5+'106',ccf1[0]+cf3,'z390 Frequently Asked Questions'+cf1,cf5+'107',ccf1[0]+cf3,'z390 User Guide'+cf1,cf6+'10',ccf5[0],ccf4[0]+'324'+cf2+cf1,cf5+'108',ccf1[0]+cf3,'z390 File Services Guide'+cf1,cf5+'109',ccf1[0]+cf3,'z390 EXEC CICS Compatible Assembler Support'+cf1,cf5+'110',ccf1[0]+cf3,'z390 GUAM GUI Access Method Guide'+cf1,cf5+'111',ccf1[0]+cf3,'z390 Macro Services Guide'+cf1,cf5+'112',ccf1[0]+cf3,'z390 Macro Pseudo Code'+cf1,cf5+'113',ccf1[0]+cf3,'z390 Program Services Guide'+cf1,cf5+'114',ccf1[0]+cf3,'z390 SOA Client Server Overview Slides'+cf1,cf5+'115',ccf1[0]+cf3,'z390 SOA Application Generator Support'+cf1,cf5+'116',ccf1[0]+cf3,'z390 SOA User Guide'+cf1,cf5+'117',ccf1[0]+cf3,'z390 Storage Services'+cf1,cf6+'11',ccf5[0],ccf4[0]+'324'+cf2+cf1,cf5+'118',ccf1[0]+cf3,'HLASM References<IMG Id="'+cf5+'118arrow" style="position:absolute;left:321px;top:4px" '+ccf3[0]+cf1,cf5+'119',ccf1[0]+cf3,'Java References<IMG Id="'+cf5+'119arrow" style="position:absolute;left:321px;top:4px" '+ccf3[0]+cf1+cf1+cf1,'pop17submain',ccf0[0]+';width:229px"','','pop17submaini',ccf1[0]+ccf2[0]+';width:227px"','',cf5+'120',ccf1[0]+cf3,'Support Information'+cf1,cf5+'121',ccf1[0]+cf3,'RPI Request Form'+cf1,cf5+'122',ccf1[0]+cf3,'RPI Pending Request Log'+cf1,cf6+'12',ccf5[0],ccf4[0]+'221'+cf2+cf1,cf5+'123',ccf1[0]+cf3,'z390 User Group Email Forum'+cf1+cf1+cf1,'pop18submain',ccf0[0]+';width:237px"','','pop18submaini',ccf1[0]+ccf2[0]+';width:235px"','',cf5+'124',ccf1[0]+cf3,'MFATC Assembler Demos'+cf1,cf5+'125',ccf1[0]+cf3,'Development '+cf1,cf5+'126',ccf1[0]+cf3,'Testing'+cf1,cf5+'127',ccf1[0]+cf3,'Benchmarks'+cf1,cf5+'128',ccf1[0]+cf3,'Statistics on z390 open source'+cf1,cf5+'129',ccf1[0]+cf3,'Macro Pseudo Code'+cf1+cf1+cf1,'pop19submain',ccf0[0]+';width:209px"','','pop19submaini',ccf1[0]+ccf2[0]+';width:207px"','',cf5+'130',ccf1[0]+cf3,'z/OS'+cf1,cf5+'131',ccf1[0]+cf3,'z/VSE'+cf1,cf5+'132',ccf1[0]+cf3,'z/VM'+cf1,cf5+'133',ccf1[0]+cf3,'z/Linux'+cf1,cf5+'134',ccf1[0]+cf3,'z/OS Internet Library'+cf1,cf5+'135',ccf1[0]+cf3,'CBT Downloads and Links'+cf1,cf5+'136',ccf1[0]+cf3,'Mainframe Dictionary'+cf1,cf5+'137',ccf1[0]+cf3,'MVS 3.8J Public Domain'+cf1,cf5+'138',ccf1[0]+cf3,'Planet MVS Dave\'s Links'+cf1,cf6+'13',ccf5[0],ccf4[0]+'201'+cf2+cf1,cf5+'139',ccf1[0]+cf3,'Linux'+cf1,cf5+'140',ccf1[0]+cf3,'Windows'+cf1+cf1+cf1,'pop20submain',ccf0[0]+';width:205px"','','pop20submaini',ccf1[0]+ccf2[0]+';width:203px"','',cf5+'141',ccf1[0]+cf3,'Java Language'+cf1,cf5+'142',ccf1[0]+cf3,'J2RE 1.5.0 API Reference'+cf1,cf6+'14',ccf5[0],ccf4[0]+'197'+cf2+cf1,cf5+'143',ccf1[0]+cf3,'J2SE'+cf1,cf5+'144',ccf1[0]+cf3,'J2EE'+cf1,cf5+'145',ccf1[0]+cf3,'Eclipse'+cf1+cf1+cf1,'pop21submain',ccf0[0]+';width:333px"','','pop21submaini',ccf1[0]+ccf2[0]+';width:331px"','',cf5+'146',ccf1[0]+cf3,'IBM 3270 Data Stream Programmers Reference'+cf1,cf5+'147',ccf1[0]+cf3,'TN3270 Tutorial'+cf1+cf1+cf1,'pop22submain',ccf0[0]+';width:181px"','','pop22submaini',ccf1[0]+ccf2[0]+';width:179px"','',cf5+'148',ccf1[0]+cf3,'IBM CICS'+cf1,cf5+'149',ccf1[0]+cf3,'Using EXEC Interface'+cf1+cf1+cf1,'pop23submain',ccf0[0]+';width:234px"','','pop23submaini',ccf1[0]+ccf2[0]+';width:232px"','',cf5+'150',ccf1[0]+cf3,'Visual C++ Express 2005'+cf1,cf5+'151',ccf1[0]+cf3,'Visual C++ Reference'+cf1,cf5+'152',ccf1[0]+cf3,'Help for C++ Hello World demo'+cf1+cf1+cf1,'pop24submain',ccf0[0]+';width:235px"','','pop24submaini',ccf1[0]+ccf2[0]+';width:233px"','',cf5+'153',ccf1[0]+cf3,'High Level Assembler (Intel)'+cf1,cf5+'154',ccf1[0]+cf3,'HLA User Guide'+cf1,cf5+'155',ccf1[0]+cf3,'Help for HLA Hello World demo'+cf1,cf5+'156',ccf1[0]+cf3,'HLA Example Programs'+cf1+cf1+cf1);
var pmimsh = new Array(cf5+'0','Downloads',cf5+'1','Documentation',cf5+'2','Support',cf5+'3','Reference',cf5+'4','Links',cf5+'5','Help');

function popmcreate(){
pmhcde='<Div Id="popMain" style="position:absolute;left:'+poX+';top:'+(poY)+';background-color:'+pmclr[0][0]+((pmclr[0][16]<100) ? ';-moz-opacity:'+pmclr[0][16]/100 : '')+'"><TABLE Id="popMaini" cellpadding=0 cellspacing=0 width='+popmcwidth+' height='+popmcheight+'><TR Id="popHtr" style="cursor:default'+pmcfnt+'">';
for (x=0; x<pmimsh.length; x+=2)
pmhcde+= pmimsh[x].match(cf9) ? cf9 : cf8+pmimsh[x]+cf7+pmimsh[(x+1)]+'</TD>';
pmhcde+='</TR></TABLE></Div>'
for (x=0; x<pmimsa.length; x+=3)
pmhcde+=cf0+pmimsa[x]+pmimsa[(x+1)]+'>'+pmimsa[(x+2)];
pmhcde +='<Div Id="DboX" style="position:absolute;visibility:hidden;z-Index:2000;padding:5px 5px 5px 5px;text-align:left;font-family:Arial;font-size:12px;-moz-opacity:0"></Div>';
if (navigator.userAgent.indexOf("Opera")<0){
range = document.createRange();
range.setStartBefore(document.body.firstChild);
inscde = range.createContextualFragment(pmhcde);
document.body.appendChild(inscde)
}
else
document.body.insertAdjacentHTML('afterBegin',pmhcde);
popMain = document.getElementById("popMain");
popMain.style.zIndex = 1000;
pmw = popMain.style.width = popmcwidth;
document.onmouseup = setpopdsploff;
window.onresize = setpopnpos;
DboX = document.getElementById("DboX");
ecY = popMain.offsetTop;
setTimeout('setpopvars()',40);
}

function setpopdsploff(e){
if (e.which==1)
popdsploff(1,e);
}

popmcreate();
