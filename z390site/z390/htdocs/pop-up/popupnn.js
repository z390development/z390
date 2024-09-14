/***************************************************************
* Pop-Up Version 4.8.0 Pro H
* © 1998-2004
* Anoxy Software
* All Rights Reserved
* You are not allowed to modify anything in this Script
****************************************************************
* To get your own copy visit: http://www.anoxy.com
****************************************************************/

var slimb,slimr,trgtlay,tplay,cslay,pcsl,mousex,mousey,ecY,wlw,wlh,wox,woy,hgp,mwd,clobj,mdelaytimer,popanimtimer,popswitchtimer,popswotimer,popdescrtimer,popnmem,poplmem,popcmem,popvmem,popnwin,pbfl,pophbar,popvbar,clrnum,DboX,popXURLV;
var pbsl = new Array();
var pbdl = new Array(); 
var submenu = new Array();
submenu[0] = "popMain";
var posub = new Array();
var layerchange = false;
var popamem = -1;
var layernum = poplevel =  pmscwd = 0;
var scc = 4;
var Pmpat = /\D/g;

function resz(){
if (wlw != window.innerWidth || wlh != window.innerHeight)
location.reload();
}

function popminit(){
with (Popupml.clip){
width = wlw;
height = wlh;
}
for (j=0 ; j<Popupml.layers.length; j++){
pbsl[j] = Popupml.layers[j];
pbdl[j] = pbsl[j].layers[8];
pbsl[j].bgColor = pmclr[popclrindx[j+1]][0];
pbdl[j].clip.width = pmmwds[j];
pbsl[j].clip.width = pbdl[j].clip.width+4;
pbsl[j].clip.height = pbdl[j].clip.height+4;
if (popautoswitch)
pbsl[j].onmouseout = popsomenu;
brdrsettings = new Array(pbdl[j].clip.width+1,1,pmclr[popclrindx[j+1]][8],pbdl[j].left-1,pbdl[j].top-2,1,pbdl[j].clip.height+4,pmclr[popclrindx[j+1]][8],pbdl[j].left-2,pbdl[j].top-2,pbdl[j].clip.width+1,1,pmclr[popclrindx[j+1]][9],pbdl[j].left-1,pbdl[j].top+pbdl[j].clip.height+1,1,pbdl[j].clip.height+4,pmclr[popclrindx[j+1]][9],pbdl[j].left+pbdl[j].clip.width+1,pbdl[j].top-2,pbdl[j].clip.width,1,pmclr[popclrindx[j+1]][6],pbdl[j].left,pbdl[j].top-1,1,pbdl[j].clip.height+2,pmclr[popclrindx[j+1]][6],pbdl[j].left-1,pbdl[j].top-1,pbdl[j].clip.width,1,pmclr[popclrindx[j+1]][7],pbdl[j].left,pbdl[j].top+pbdl[j].clip.height,1,pbdl[j].clip.height+2,pmclr[popclrindx[j+1]][7],pbdl[j].left+pbdl[j].clip.width,pbdl[j].top-1);
for (z=0; z<8; z++){
with(pbsl[j].layers[z]){
clip.width = brdrsettings[(z*5)];
clip.height = brdrsettings[(z*5+1)];
bgColor = brdrsettings[(z*5+2)];
left = brdrsettings[(z*5+3)]-((z==3 && pmclr[popclrindx[j+1]][15]==2) ? 1 : 0);
top = brdrsettings[(z*5+4)];
}
}
for (x=0; x<pbdl[j].document.layers.length; x++){
if (pbdl[j].layers[x].name.indexOf(cf8) != -1){
with(pbdl[j].layers[x]){
clip.height = (pmclr[popclrindx[j+1]][15]!=2) ? 2 : 1;
pMCw = (pmclr[popclrindx[j+1]][15]==0) ? pbdl[j].clip.width-4-left : pbdl[j].clip.width-left;
clip.width = pMCw;
with (document.layers[0]){
clip.height = 1;
clip.width = pMCw;
bgColor = pmclr[popclrindx[j+1]][10];
}
with(document.layers[1]){
clip.height = 1;
clip.width = pMCw;
bgColor = pmclr[popclrindx[j+1]][11];
}
}
}
if (pbdl[j].layers[x].name.indexOf(cf6) != -1){
l = pbdl[j].layers[x];
l.clip.width = pbdl[j].clip.width;
l.onmouseover = popmion;
pbdl[j].onmouseout = popmioff;
pbdl[j].onmouseover = popmclay;
}
}
}
pbfl = Popuphl.layers["nndummyItem"];
pbfl.captureEvents(Event.MOUSEDOWN);
pbfl.onmousedown = popmidown;
pbfl.onmouseout = popmioff;
for (x=0; x<pbsl.length; x++)
pbsl[x].moveAbove(Popupml);
pmHLm = Popuphl.layers["popMain"];
pmHicn = 0;
for (x=0; x<pmHLm.layers.length; x++){
if (pmHLm.layers[x].name.indexOf(cf9)>=0){
pmHLm.layers[x].clip.width = pmhwds[pmHicn];
pmHLm.layers[x].onmouseover = popmion;
pmHicn++;
}
else
if (pmHLm.layers[x].name.indexOf(cfA)>=0)
with(pmHLm.layers[x]){
if (!(pmclr[0][15]==2 && name.indexOf("r")>0)){
clip.width = 1;
clip.height = popmcheight;
bgColor = (name.indexOf("l")>0) ? pmclr[0][10] : pmclr[0][11];
}
}
}
with(pmHLm){
moveAbove(Popuphl)
bgColor= pmclr[0][0];
clip.width = mwd;
left = poX;
top = poY;
visibility = "show"
}
DboX = document.layers["DboX"];
DboX.moveAbove(Popupml);
if (popautoswitch)
pmHLm.onmouseout = popsomenu;
}

function popsanim(lobj,cr,cw,lp,te){
clobj = lobj;
popsmcX = Math.floor(pmscwd*(4-scc)/4);
popsmX = Math.floor(pmscwd*scc/4);
if (scc>=0){
scc--;
with (Popupml.layers[lobj].clip){
if (te){
top = Math.abs(cr-1)*popsmX;
bottom = cr*popsmcX+Math.abs(cr-1)*cw;
}
else{
right = Math.abs(cr-1)*popsmcX+cw;
left = cr*popsmX;
}
}
if (te)
Popupml.layers[lobj].top = (cr==1) ? (lp+popsmX) : (lp-popsmX);
else
Popupml.layers[lobj].left = (cr==1) ? (lp-popsmX) : (lp+popsmX);
popanimtimer = window.setTimeout('popsanim(this.clobj,'+cr+','+cw+','+lp+','+te+')',25);
}
else
scc = 4;
}

function popdsploff(s){
popsubctrl(0,s,0,0);
popdscroff();
pbfl.visibility = "hide";
if (popanimenabled){
window.clearTimeout(popanimtimer);
window.clearTimeout(mdelaytimer);
}
}

function popsubctrl(p1,p2,p3,p4){
for (x=p1; x<submenu.length; x++){
if (pmact[pcsl] == 9 && pmiurl[pcsl] == submenu[x+1] && submenu[x] != -1 && p3 == 1 && p4!=1 && posub[x][1]==pcsl)
break;
if (x<posub.length)
if (posub[x][0] != -1){
if (posub[x][0].match("Hitem")){
ostplay = pmHLm.layers[posub[x][0]];
cstml = "menHitem"+posub[x][1];
pdela = false;
}
else{
ostplay = pbdl[posub[x][2]].layers[posub[x][0]];
cstml = "menuItem"+posub[x][1];
pdela = true;
}
sctrlclrnum = (pdela) ? popclrindx[posub[x][2]+1] : 0;
ostplay.layers[cstml+"h"].visibility = "hide";
ostplay.layers[cstml+"d"].visibility = "inherit";
ostplay.bgColor = pmclr[sctrlclrnum][0];
if (pdela)
ostplay.layers[cstml+"arrow"].document.images["popmenuarrow"+posub[x][1]].src = popaimg[sctrlclrnum].src;
if ((x==0 || p4==1) && pdela)
pbfl.moveBelow(ostplay);
posub[x][0] = -1;
if (p4==1)
break;
}
if (x>=p2 && p4!=1)
if (submenu[x] != -1){
Popupml.layers[submenu[x]].visibility = "hide";
if (x>0)
submenu[x] = -1;
}
}
}

function popmidown(e){
onsubop = pmact[pcsl];
if (e.which == 1 && onsubop< 7)
setTimeout("popmidown2()",200);
if (popanimenabled && onsubop == 9){
window.clearTimeout(mdelaytimer);
if (tplay.name.indexOf("Hitem")>0)
pophon2(1);
else
popmion2(1);
}
if (onsubop<7){
popmioff();
popdsploff(1);
}
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

function popmclay(e){
newpoplevel = true;
bodypl = e.target.name+"";
for (x=0; x<pbsl.length; x++)
if (pbsl[x].name+"i" == bodypl){
layernum = x;
break;
}
layerchange = true;
for (x=0; x<=submenu.length; x++)
if (pbsl[layernum].name == submenu[x]){
poplevel = x;
newpoplevel = false;
break;
}
if (newpoplevel)
poplevel++;
}

function popmion(e){
ttrgtlay = e.target.name;
pcsl = ttrgtlay.substring(8,ttrgtlay.length);
window.clearTimeout(popswotimer);
popdscroff();
if (pmact[pcsl] != 8){
if (popsndenabled && Popupml.document.embeds[0]){
with (Popupml.document.embeds[0]){
stop();
play();
}
}
trgtlay = e.target.name;
if (trgtlay && !layerchange)
popmioff();
layerchange = false;
pmlo = (trgtlay.indexOf("menH") == 0) ? 1 : 0;
tplay = (pmlo==0) ? pbdl[layernum].layers[trgtlay] : pmHLm.layers[trgtlay];
with (pbfl){
top = tplay.top;
left = tplay.left;
clip.height = tplay.clip.height;
clip.width = tplay.clip.width + 5;
visibility = "show";
moveAbove(tplay);
}
clrnum = (pmlo==1) ? 0 : popclrindx[layernum+1];
cslay = ((pmlo==0) ? "menuItem" : "menHitem")+pcsl;
tplay.layers[cslay+"d"].visibility = "hide";
tplay.layers[cslay+"h"].visibility = "inherit";
tplay.bgColor = pmclr[clrnum][1];
if (pmlo==0 || (pmlo==1 && poplevel==0))
window.clearTimeout(popswitchtimer);
if (pmlo == 1)
poplevel = 0;
if (pmsbtxt[pcsl])
window.status = unescape(pmsbtxt[pcsl]);
if (pmoimg[pcsl])
tplay.layers[cslay+"h"].document.images[cf6+pcsl+"img"].src = pmoimg[pcsl].src;
popsubctrl(poplevel,poplevel+1,1,0)
if (pmact[pcsl] == 9)
if (pmlo==0)
popmion2(0);
else
pophon2(0);
if (pmdbtxt[pcsl])
popdescrtimer = setTimeout('popdscron()',popdescrtimermsec);
}
}

function pophon2(pcv){
if (Popupml.layers[pmiurl[pcsl]].visibility == "show" && eval(Popupml.layers[pmiurl[pcsl]].clip.height!=0) && popvmem==pcsl)
return;
popamem = -1;
popvmem = pcsl;
popsmsf();
pmscwd = eval(pmhhgs[popnmem])+4;
smpx = pmHLm.left+tplay.left;
if (popopendir==0)
smpy = pmHLm.top+tplay.clip.height+1;
else
smpy = pmHLm.top-hgp-1;
with(Popupml.layers[pmiurl[pcsl]]){
top = smpy;
left = smpx;
if (popanimenabled){
clip.top = 0;
clip.bottom = 0;
}
visibility = "show";
}
if (popanimenabled){
window.clearTimeout(popanimtimer);
scc = 4;
cr = popopendir;
cw = pmscwd;
if (pcv==0)
mdelaytimer = window.setTimeout('popsanim(pmiurl[pcsl],cr,cw,smpy,true)',popdeltimermsec);
else
popsanim(pmiurl[pcsl],cr,cw,smpy,true);
}
}

function popdscron(){
if (scc==4){
DboX.clip.width = pmdbtxt[pcsl][4];
DboX.document.write('<Layer '+((pmdbtxt[pcsl][5]) ? 'background="'+pmdbtxt[pcsl][5]+'" ' : '')+'bgcolor="'+pmdbtxt[pcsl][1]+'" style="border:'+pmdbtxt[pcsl][3]+';width:'+pmdbtxt[pcsl][4]+'"><Span style="color:'+pmdbtxt[pcsl][2]+popdboxfnt+'">'+pmdbtxt[pcsl][0]+'</Span></Layer>');
DboX.document.close();
dbow = DboX.clip.width;
dboh = DboX.clip.height;
if (pmact[pcsl] == 9){
pmsol = Popupml.layers[pmiurl[pcsl]];
dbxp = pmsol.left+((pmmwds[popnmem]-dbow>0) ? (pmmwds[popnmem]-dbow)/2 : 0);
dbyp = pmsol.top+((popopendir==0) ? pmsol.clip.height+2 : dboh*-1-2);
if (dbxp+dbow >= slimr)
dbxp = slimr-dbow-2;
if (dbyp+dboh >= slimb)
dbyp = pmsol.top-dboh-2;
}
else{
dbxp = (tplay.name.indexOf("menH")==0) ? pmHLm.left+tplay.left+2 : pbsl[layernum].left+pbsl[layernum].clip.width+2;
dbyp = (tplay.name.indexOf("menH")==0) ? pmHLm.top+tplay.top+((popopendir==0) ? tplay.clip.height+4 : dboh*-1-2) : pbsl[layernum].top+tplay.top;
if (dbxp+dbow >= slimr)
dbxp = (tplay.name.indexOf("menH")==0) ? slimr-dbow-4 : pbsl[layernum].left-dbow-2;
if (dbxp<wox)
dbxp = wox;
if (dbyp+dboh >= slimb)
dbyp = slimb-dboh-2;
if (dbyp<woy)
dbyp = woy;
}
DboX.left = dbxp;
DboX.top = dbyp;
DboX.visibility = "visible";
}
else
popdescrtimer = setTimeout('popdscron()',100);
}

function popdscroff(){
if (DboX.visibility == "show")
DboX.visibility = "hide";
clearTimeout(popdescrtimer);
}

function popsmf(){
wox = window.pageXOffset;
woy = window.pageYOffset;
slimb = wlh+woy-(14*pophbar);
slimr = wlw+wox-(14*popvbar);
}

function popsmsf(){
submenu[(poplevel+1)] = pmiurl[pcsl];
posub[poplevel] = new Array(trgtlay,pcsl,layernum);
popnmem = pmiurl[pcsl];
popnmem = popnmem.substring(3,popnmem.length);
popnmem = popnmem.split("submain");
popnmem = popnmem[0];
popnmem = eval(popnmem)-1;
hgp = Popupml.layers[pmiurl[pcsl]].clip.height;
}

function popmion2(pcv){
if (Popupml.layers[pmiurl[pcsl]].visibility == "show" && eval(Popupml.layers[pmiurl[pcsl]].clip.width!=0) && !(popcmem != pcsl && poplmem == poplevel))
return;
tplay.layers["menuItem"+pcsl+"arrow"].document.images["popmenuarrow"+pcsl].src = popoimg[clrnum].src;
popamem = pcsl;
if (clobj == submenu[poplevel] && scc<4)
mdelaytimer = setTimeout ('popmion2()',200);
else{
popcmem = pcsl;
poplmem = poplevel;
popcorX = false;
popsmf();
popsmsf();
pmscwd = eval(pmmwds[popnmem])+4;
wdp = pmscwd;
smpx = pbsl[layernum].left+pbsl[layernum].clip.width-6;
smpy = pbsl[layernum].top+tplay.top;
if (smpx+wdp>= slimr){
smpx = pbsl[layernum].left-wdp+6;
popcorX = true;
}
if (smpy+hgp>= slimb)
smpy = smpy-hgp+tplay.clip.height+2;
if (smpy<woy)
smpy = woy;
if (smpx<wox)
smpx = pbsl[layernum].left+6;
with(Popupml.layers[pmiurl[pcsl]]){
top = smpy;
left = smpx;
moveAbove(pbsl[layernum]);
if (popanimenabled){
clip.left = 0;
clip.right = 0;
}
else
visibility = "show";
}
if (popanimenabled){
window.clearTimeout(popanimtimer);
scc = 4;
cr = (popcorX) ? 0:1;
cw = (popcorX) ? 0:pmscwd;
if (pcv==0)
mdelaytimer=window.setTimeout('popsanim(pmiurl[pcsl],cr,cw,smpx);Popupml.layers[pmiurl[pcsl]].visibility = "show"',popdeltimermsec);
else{
popsanim(pmiurl[pcsl],cr,cw,smpx);
Popupml.layers[pmiurl[pcsl]].visibility = "show";
}
}
}
}

function popmioff(){
if (tplay){
if (pmsbtxt[pcsl])
window.status = "";
deletehilay = true;
popdscroff();
for (x=0; x<posub.length; x++)
if (posub[x][0] == trgtlay){
deletehilay = false;
popamem = -1;
if (popanimenabled){
window.clearTimeout(popswitchtimer);
popswitchtimer = window.setTimeout('pophhlay('+poplevel+')',50);
}
}
if (deletehilay){
tplay.layers[cslay+"h"].visibility = "hide";
tplay.layers[cslay+"d"].visibility = "inherit";
tplay.bgColor = pmclr[clrnum][0];
pbfl.moveBelow(tplay);
}
window.clearTimeout(mdelaytimer);
}
}

function pophhlay(lvl){
if (posub[lvl])
if (Popupml.layers[pmiurl[posub[lvl][1]]].clip.right == 0 || Popupml.layers[pmiurl[posub[lvl][1]]].clip.bottom == 0)
popsubctrl(lvl,0,0,1);
if (lvl == 0)
pbfl.visibility = "hide";
}

function popsomenu(){
popswotimer = window.setTimeout('popdsploff(1)',popsotimermsec);
}
//Pop-Up 4.7 H Menu file (NN 4.x)

var poptimer;
var popautoswitch = false;
var popanimenabled = true;
var popsndenabled = false;
var popdboxfnt = ";font-family:Arial;font-size:12px;text-align:left";
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
var pmmwds = new Array('197','241','228','244','306','168','180','263','217','184','250','210','253','230','295','328','225','233','205','201','329','177','230','231');
var pmhwds = new Array('93','116','75','89','61','55');
var pmhhgs = new Array('66','90','126','126','96','186','90','111','141','81','156','90','45','75','66','237','66','90','171','81','30','30','45','60');
var pmcfnt = ";font-family:Arial,Arial,Times New Roman,Verdana;font-size:12px;font-weight:bold;font-style:normal";
cf0 = '<Div Id="';
cf1 = '</Div>';
cf2 = '" style="position:absolute';
cf3 = pmcfnt+";padding-top:0";
cf4 = ";left:15;color:"+pmclr[0][3];
cf5 = ";left:15;visibility:hide;color:"+pmclr[0][2];
cf6 = "menuItem";
cf7 = "popmenuarrow";
cf8 = "menuSeparator";
cf9 = "menHitem";
cfA = "menSitem";

ccf0 = new Array();
ccf1 = new Array();
ccf2 = new Array();
ccf3 = new Array();
popaimg = new Array();
popoimg = new Array();
for (x=0; x<pmclr.length;x++){
popaimg[x] = new Image();
popoimg[x] = new Image();
popaimg[x].src = popbasedir+"pop_arw"+pmclr[popclrindx[x]][12]+".gif";
popoimg[x].src = popbasedir+"popo_arw"+pmclr[popclrindx[x]][13]+".gif";
ccf0[x] = ' SRC="'+popaimg[x].src+'" BORDER="0">';
ccf1[x] = ";left:18;color:"+pmclr[popclrindx[x]][3];
ccf2[x] = ";left:18;visibility:hide;color:"+pmclr[popclrindx[x]][2];
ccf3[x] = (pmclr[popclrindx[x]][14]) ? (";layer-background-image:url("+popbasedir+pmclr[popclrindx[x]][14]+");") : "";
}
var pmimsa = new Array('pop1submain',';visibility:hide'+ccf3[0],'','pop1submaini',';left:2;top:2','',cf6+'6',';left:0;height:15;top:0','Dave\'s Assembler FAQ',cf1,cf6+'7',';left:0;height:15;top:15','IBM Main List FAQ',cf1,cf8+'0',';left:3;top:32','',cf6+'8',';left:0;height:15;top:36','Dave\'s Assembler Tips',cf1,cf6+'9',';left:0;height:15;top:51','Wynsoft Assembler Tips',cf1+cf1+cf1,'pop2submain',';visibility:hide'+ccf3[0],'','pop2submaini',';left:2;top:2','',cf6+'10',';left:0;height:15;top:0','Assembler ASM370 Group',cf1,cf6+'11',';left:0;height:15;top:15','Assembler ASM370 List',cf1,cf6+'12',';left:0;height:15;top:30','IBM-Main Group',cf1,cf6+'13',';left:0;height:15;top:45','IBM-Main List',cf1,cf6+'14',';left:0;height:15;top:60','z390 Assembler/Emulator Group',cf1,cf6+'15',';left:0;height:15;top:75','Yahoo Group Search',cf1+cf1+cf1,'pop3submain',';visibility:hide'+ccf3[0],'','pop3submaini',';left:2;top:2','',cf6+'16',';left:0;height:15;top:0','Language Reference V1R5',cf1,cf6+'17',';left:0;height:15;top:15','Programers Guide V1R5',cf1,cf6+'18',';left:0;height:15;top:30','Macro References',cf6+'18arrow',';left:218;top:4','<IMG Name="'+cf7+'18"'+ccf0[0]+cf1+cf1,cf8+'1',';left:3;top:47','',cf6+'19',';left:0;height:15;top:51','General Information',cf1,cf6+'20',';left:0;height:15;top:66','Installation and Customization',cf1,cf6+'21',';left:0;height:15;top:81','Language Environment',cf1,cf6+'22',';left:0;height:15;top:96','z/Architecture POP ',cf1,cf6+'23',';left:0;height:15;top:111','Catalog of HLASM Manuals',cf1+cf1+cf1,'pop4submain',';visibility:hide'+ccf3[0],'','pop4submaini',';left:2;top:2','',cf6+'24',';left:0;height:15;top:0','Assembler FAQ\'s',cf6+'24arrow',';left:234;top:4','<IMG Name="'+cf7+'24"'+ccf0[0]+cf1+cf1,cf6+'25',';left:0;height:15;top:15','Control Blocks',cf6+'25arrow',';left:234;top:4','<IMG Name="'+cf7+'25"'+ccf0[0]+cf1+cf1,cf6+'26',';left:0;height:15;top:30','Diagnostic Reference ',cf1,cf6+'27',';left:0;height:15;top:45','Simotime Assembler Connection',cf1,cf8+'2',';left:3;top:62','',cf6+'28',';left:0;height:15;top:66','Assembler Class CS-310',cf1,cf6+'29',';left:0;height:15;top:81','HLASM Common Routines',cf1,cf6+'30',';left:0;height:15;top:96','Adsvanced ASM and Interfaces',cf1,cf6+'31',';left:0;height:15;top:111','System Reference Card (PDF)',cf1+cf1+cf1,'pop5submain',';visibility:hide'+ccf3[0],'','pop5submaini',';left:2;top:2','',cf6+'32',';left:0;height:15;top:0','IBM HLASM Assembler',cf1,cf8+'3',';left:3;top:17','',cf6+'33',';left:0;height:15;top:21','Dingus',cf1,cf6+'34',';left:0;height:15;top:36','HLA High Level Intel Assembler',cf6+'34arrow',';left:296;top:4','<IMG Name="'+cf7+'34"'+ccf0[0]+cf1+cf1,cf6+'35',';left:0;height:15;top:51','Micro Focus Mainframe Express Assembler',cf1,cf6+'36',';left:0;height:15;top:66','Tachyon Software',cf1,cf6+'37',';left:0;height:15;top:81','z390 Portable Assembler/Emulator',cf1+cf1+cf1,'pop6submain',';visibility:hide'+ccf3[0],'','pop6submaini',';left:2;top:2','',cf6+'38',';left:0;height:15;top:0','HLASM References',cf1,cf6+'39',';left:0;height:15;top:15','Email Groups',cf6+'39arrow',';left:158;top:4','<IMG Name="'+cf7+'39"'+ccf0[0]+cf1+cf1,cf6+'40',';left:0;height:15;top:30','Education',cf6+'40arrow',';left:158;top:4','<IMG Name="'+cf7+'40"'+ccf0[0]+cf1+cf1,cf8+'4',';left:3;top:47','',cf6+'41',';left:0;height:15;top:51','Assemblers',cf6+'41arrow',';left:158;top:4','<IMG Name="'+cf7+'41"'+ccf0[0]+cf1+cf1,cf6+'42',';left:0;height:15;top:66','Emulators',cf6+'42arrow',';left:158;top:4','<IMG Name="'+cf7+'42"'+ccf0[0]+cf1+cf1,cf6+'43',';left:0;height:15;top:81','EXEC CICS',cf6+'43arrow',';left:158;top:4','<IMG Name="'+cf7+'43"'+ccf0[0]+cf1+cf1,cf6+'44',';left:0;height:15;top:96','History',cf6+'44arrow',';left:158;top:4','<IMG Name="'+cf7+'44"'+ccf0[0]+cf1+cf1,cf6+'45',';left:0;height:15;top:111','Languages',cf6+'45arrow',';left:158;top:4','<IMG Name="'+cf7+'45"'+ccf0[0]+cf1+cf1,cf6+'46',';left:0;height:15;top:126','Operating Systems',cf6+'46arrow',';left:158;top:4','<IMG Name="'+cf7+'46"'+ccf0[0]+cf1+cf1,cf6+'47',';left:0;height:15;top:141','Organizations',cf6+'47arrow',';left:158;top:4','<IMG Name="'+cf7+'47"'+ccf0[0]+cf1+cf1,cf6+'48',';left:0;height:15;top:156','TN3270 Terminals',cf6+'48arrow',';left:158;top:4','<IMG Name="'+cf7+'48"'+ccf0[0]+cf1+cf1,cf6+'49',';left:0;height:15;top:171','Webring - Dinos',cf1+cf1+cf1,'pop7submain',';visibility:hide'+ccf3[0],'','pop7submaini',';left:2;top:2','',cf6+'50',';left:0;height:15;top:0','Home',cf1,cf6+'51',';left:0;height:15;top:15','z390 Support',cf1,cf6+'52',';left:0;height:15;top:30','Windows Update',cf1,cf6+'53',';left:0;height:15;top:45','Ubuntu Linux Support',cf1,cf6+'54',';left:0;height:15;top:60','Site Map',cf1,cf6+'55',';left:0;height:15;top:75','About',cf1+cf1+cf1,'pop8submain',';visibility:hide'+ccf3[0],'','pop8submaini',';left:2;top:2','',cf6+'56',';left:0;height:15;top:0','z390 Portable Assembler/Emulator',cf1,cf8+'5',';left:3;top:17','',cf6+'57',';left:0;height:15;top:21','Flexes S/390 on Intel',cf1,cf6+'58',';left:0;height:15;top:36','Open Mainframe on Intel',cf1,cf6+'59',';left:0;height:15;top:51','Hercules S/390 on Windows & Linux',cf1,cf6+'60',';left:0;height:15;top:66','PC/370',cf1,cf6+'61',';left:0;height:15;top:81','Soft/390 on Intel',cf1,cf6+'62',';left:0;height:15;top:96','UMX OS/390 on Intel',cf1+cf1+cf1,'pop9submain',';visibility:hide'+ccf3[0],'','pop9submaini',';left:2;top:2','',cf6+'63',';left:0;height:15;top:0','SHARE',cf1,cf6+'64',';left:0;height:15;top:15','z390 Presentation at SHARE',cf1,cf6+'65',';left:0;height:15;top:30','CBT SPLA Repository',cf1,cf8+'6',';left:3;top:47','',cf6+'66',';left:0;height:15;top:51','ACM',cf1,cf6+'67',';left:0;height:15;top:66','CMG',cf1,cf6+'68',';left:0;height:15;top:81','COMMON',cf1,cf6+'69',';left:0;height:15;top:96','IEEE Computer Society',cf1,cf6+'70',';left:0;height:15;top:111','NASPA',cf1,cf6+'71',';left:0;height:15;top:126','Open Source Initiative',cf1+cf1+cf1,'pop10submain',';visibility:hide'+ccf3[0],'','pop10submaini',';left:2;top:2','',cf6+'72',';left:0;height:15;top:0','Computer History',cf1,cf6+'73',';left:0;height:15;top:15','History of 360/370/390',cf1,cf6+'74',';left:0;height:15;top:30','Hisotry of PC/370',cf1,cf6+'75',';left:0;height:15;top:45','IBM Product Timeline',cf1,cf8+'7',';left:3;top:62','',cf6+'76',';left:0;height:15;top:66','Green Cards',cf1+cf1+cf1,'pop11submain',';visibility:hide'+ccf3[0],'','pop11submaini',';left:2;top:2','',cf6+'77',';left:0;height:15;top:0','Assembler Services Books',cf1,cf6+'78',';left:0;height:15;top:15','z/OS Assembler Services',cf1,cf6+'79',';left:0;height:15;top:30','z/OS Assembler Services Ref. A-H',cf1,cf6+'80',';left:0;height:15;top:45','z/OS Assembler Services Ref. I-X',cf1,cf8+'8',';left:3;top:62','',cf6+'81',';left:0;height:15;top:66','z/OS Macros for Data Sets',cf1,cf6+'82',';left:0;height:15;top:81','z/OS Authorized Services Guide',cf1,cf6+'83',';left:0;height:15;top:96','z/OS Authorized ALE-DYN',cf1,cf6+'84',';left:0;height:15;top:111','z/OS Authorized ENF-IXG',cf1,cf6+'85',';left:0;height:15;top:126','z/OS Authorized LLA-SDU',cf1,cf6+'86',';left:0;height:15;top:141','z/OS Authorized SET-WTO',cf1+cf1+cf1,'pop12submain',';visibility:hide'+ccf3[0],'','pop12submaini',';left:2;top:2','',cf6+'87',';left:0;height:15;top:0','Assembler for Mainframes',cf6+'87arrow',';left:200;top:4','<IMG Name="'+cf7+'87"'+ccf0[0]+cf1+cf1,cf6+'88',';left:0;height:15;top:15','Assember for Intel',cf6+'88arrow',';left:200;top:4','<IMG Name="'+cf7+'88"'+ccf0[0]+cf1+cf1,cf6+'89',';left:0;height:15;top:30','C++',cf6+'89arrow',';left:200;top:4','<IMG Name="'+cf7+'89"'+ccf0[0]+cf1+cf1,cf6+'90',';left:0;height:15;top:45','Cobol Links',cf1,cf6+'91',';left:0;height:15;top:60','Java Links',cf6+'91arrow',';left:200;top:4','<IMG Name="'+cf7+'91"'+ccf0[0]+cf1+cf1,cf6+'92',';left:0;height:15;top:75','PLI',cf1+cf1+cf1,'pop13submain',';visibility:hide'+ccf3[0],'','pop13submaini',';left:2;top:2','',cf6+'93',';left:0;height:15;top:0','HLA High Level Assembler for Intel',cf6+'93arrow',';left:243;top:4','<IMG Name="'+cf7+'93"'+ccf0[0]+cf1+cf1,cf6+'94',';left:0;height:15;top:15','MASM Microsoft Assembler',cf1,cf6+'95',';left:0;height:15;top:30','Pentium Processor Manuals',cf1+cf1+cf1,'pop14submain',';visibility:hide'+ccf3[0],'','pop14submaini',';left:2;top:2','',cf6+'96',';left:0;height:15;top:0','z/OS Data Areas Vol. 1 - AB-DB',cf1,cf6+'97',';left:0;height:15;top:15','z/OS Data Areas Vol. 2 - DC-IT',cf1,cf6+'98',';left:0;height:15;top:30','z/OS Data Areas Vol. 3 - IC-RC',cf1,cf6+'99',';left:0;height:15;top:45','z/OS Data Areas Vol. 4 - RD-SD',cf1,cf6+'100',';left:0;height:15;top:60','z/OS Data Areas Vol. 5 - SS-XT',cf1+cf1+cf1,'pop15submain',';visibility:hide'+ccf3[0],'','pop15submaini',';left:2;top:2','',cf6+'101',';left:0;height:15;top:0','z390 Downloads',cf1,cf8+'9',';left:3;top:17','',cf6+'102',';left:0;height:15;top:21','J2RE Downloads',cf1,cf6+'103',';left:0;height:15;top:36','Eclipse Downloads',cf1,cf6+'104',';left:0;height:15;top:51','Additional demos',cf1+cf1+cf1,'pop16submain',';visibility:hide'+ccf3[0],'','pop16submaini',';left:2;top:2','',cf6+'105',';left:0;height:15;top:0','z390 Getting Started',cf1,cf6+'106',';left:0;height:15;top:15','z390 Frequently Asked Questions',cf1,cf6+'107',';left:0;height:15;top:30','z390 User Guide',cf1,cf8+'10',';left:3;top:47','',cf6+'108',';left:0;height:15;top:51','z390 File Services Guide',cf1,cf6+'109',';left:0;height:15;top:66','z390 EXEC CICS Compatible Assembler Support',cf1,cf6+'110',';left:0;height:15;top:81','z390 GUAM GUI Access Method Guide',cf1,cf6+'111',';left:0;height:15;top:96','z390 Macro Services Guide',cf1,cf6+'112',';left:0;height:15;top:111','z390 Macro Pseudo Code',cf1,cf6+'113',';left:0;height:15;top:126','z390 Program Services Guide',cf1,cf6+'114',';left:0;height:15;top:141','z390 SOA Client Server Overview Slides',cf1,cf6+'115',';left:0;height:15;top:156','z390 SOA Application Generator Support',cf1,cf6+'116',';left:0;height:15;top:171','z390 SOA User Guide',cf1,cf6+'117',';left:0;height:15;top:186','z390 Storage Services',cf1,cf8+'11',';left:3;top:203','',cf6+'118',';left:0;height:15;top:207','HLASM References',cf6+'118arrow',';left:318;top:4','<IMG Name="'+cf7+'118"'+ccf0[0]+cf1+cf1,cf6+'119',';left:0;height:15;top:222','Java References',cf6+'119arrow',';left:318;top:4','<IMG Name="'+cf7+'119"'+ccf0[0]+cf1+cf1+cf1+cf1,'pop17submain',';visibility:hide'+ccf3[0],'','pop17submaini',';left:2;top:2','',cf6+'120',';left:0;height:15;top:0','Support Information',cf1,cf6+'121',';left:0;height:15;top:15','RPI Request Form',cf1,cf6+'122',';left:0;height:15;top:30','RPI Pending Request Log',cf1,cf8+'12',';left:3;top:47','',cf6+'123',';left:0;height:15;top:51','z390 User Group Email Forum',cf1+cf1+cf1,'pop18submain',';visibility:hide'+ccf3[0],'','pop18submaini',';left:2;top:2','',cf6+'124',';left:0;height:15;top:0','MFATC Assembler Demos',cf1,cf6+'125',';left:0;height:15;top:15','Development ',cf1,cf6+'126',';left:0;height:15;top:30','Testing',cf1,cf6+'127',';left:0;height:15;top:45','Benchmarks',cf1,cf6+'128',';left:0;height:15;top:60','Statistics on z390 open source',cf1,cf6+'129',';left:0;height:15;top:75','Macro Pseudo Code',cf1+cf1+cf1,'pop19submain',';visibility:hide'+ccf3[0],'','pop19submaini',';left:2;top:2','',cf6+'130',';left:0;height:15;top:0','z/OS',cf1,cf6+'131',';left:0;height:15;top:15','z/VSE',cf1,cf6+'132',';left:0;height:15;top:30','z/VM',cf1,cf6+'133',';left:0;height:15;top:45','z/Linux',cf1,cf6+'134',';left:0;height:15;top:60','z/OS Internet Library',cf1,cf6+'135',';left:0;height:15;top:75','CBT Downloads and Links',cf1,cf6+'136',';left:0;height:15;top:90','Mainframe Dictionary',cf1,cf6+'137',';left:0;height:15;top:105','MVS 3.8J Public Domain',cf1,cf6+'138',';left:0;height:15;top:120','Planet MVS Dave\'s Links',cf1,cf8+'13',';left:3;top:137','',cf6+'139',';left:0;height:15;top:141','Linux',cf1,cf6+'140',';left:0;height:15;top:156','Windows',cf1+cf1+cf1,'pop20submain',';visibility:hide'+ccf3[0],'','pop20submaini',';left:2;top:2','',cf6+'141',';left:0;height:15;top:0','Java Language',cf1,cf6+'142',';left:0;height:15;top:15','J2RE 1.5.0 API Reference',cf1,cf8+'14',';left:3;top:32','',cf6+'143',';left:0;height:15;top:36','J2SE',cf1,cf6+'144',';left:0;height:15;top:51','J2EE',cf1,cf6+'145',';left:0;height:15;top:66','Eclipse',cf1+cf1+cf1,'pop21submain',';visibility:hide'+ccf3[0],'','pop21submaini',';left:2;top:2','',cf6+'146',';left:0;height:15;top:0','IBM 3270 Data Stream Programmers Reference',cf1,cf6+'147',';left:0;height:15;top:15','TN3270 Tutorial',cf1+cf1+cf1,'pop22submain',';visibility:hide'+ccf3[0],'','pop22submaini',';left:2;top:2','',cf6+'148',';left:0;height:15;top:0','IBM CICS',cf1,cf6+'149',';left:0;height:15;top:15','Using EXEC Interface',cf1+cf1+cf1,'pop23submain',';visibility:hide'+ccf3[0],'','pop23submaini',';left:2;top:2','',cf6+'150',';left:0;height:15;top:0','Visual C++ Express 2005',cf1,cf6+'151',';left:0;height:15;top:15','Visual C++ Reference',cf1,cf6+'152',';left:0;height:15;top:30','Help for C++ Hello World demo',cf1+cf1+cf1,'pop24submain',';visibility:hide'+ccf3[0],'','pop24submaini',';left:2;top:2','',cf6+'153',';left:0;height:15;top:0','High Level Assembler (Intel)',cf1,cf6+'154',';left:0;height:15;top:15','HLA User Guide',cf1,cf6+'155',';left:0;height:15;top:30','Help for HLA Hello World demo',cf1,cf6+'156',';left:0;height:15;top:45','HLA Example Programs',cf1+cf1+cf1);
var pmimsh = new Array(cf9+'0',0,0,'Downloads',cf9+'1',0,93,'Documentation',cf9+'2',0,209,'Support',cf9+'3',0,284,'Reference',cf9+'4',0,373,'Links',cf9+'5',0,434,'Help');
var pmnnb = new Array('btopo','blefto','bbottomo','brighto','btopi','blefti','bbottomi','brighti');

function popmcreate(){
mwd = popmcwidth;
pmcde = cf0+'popMain'+cf2+ccf3[0]+'">';
for (x=0; x<pmimsh.length; x+=4){
if (pmimsh[x].indexOf(cf9) !=-1){
pmcde +=  cf0+pmimsh[x]+cf2+';padding-top:'+pmimsh[(x+1)]+pmcfnt+';height:'+popmcheight+';left:'+pmimsh[(x+2)]+'">';
pmcde += cf0+(pmimsh[x]+"d")+cf2+cf4+'">'+pmimsh[(x+3)]+cf1;
pmcde += cf0+(pmimsh[x]+"h")+cf2+cf5+'">'+pmimsh[(x+3)]+cf1+cf1;
}
else
pmcde +=  cf0+pmimsh[x]+"l"+cf2+';left:'+pmimsh[(x+2)]+'">'+cf1+cf0+pmimsh[x]+"r"+cf2+';left:'+(pmimsh[(x+2)]+1)+'">'+cf1;
}
pmcde += cf1+cf0+'nndummyItem'+cf2+';visibility:hide">&nbsp;'+cf1;
pmhcde = '';
pmlvl = 0;
for (x=0; x<pmimsa.length; x+=3){
if (pmimsa[x].indexOf("submaini") !=-1)
pmlvl++
if (pmimsa[x].indexOf(cf6) !=-1){
pmhcde += cf0+pmimsa[x]+cf2+pmimsa[(x+1)]+cf3+'">';
pmhcde += cf0+(pmimsa[x]+"d")+cf2+ccf1[popclrindx[pmlvl]]+'">'+pmimsa[(x+2)]+cf1;
pmhcde += cf0+(pmimsa[x]+"h")+cf2+ccf2[popclrindx[pmlvl]]+'">'+pmimsa[(x+2)]+cf1;
if (pmimsa[(x+3)].indexOf("arrow") !=-1){
pmhcde += cf0+pmimsa[(x+3)]+cf2+pmimsa[(x+4)]+'">'+pmimsa[(x+5)];
x+=3;
}
else{
pmhcde += pmimsa[(x+3)]
x++;
}
}
else{
if (pmimsa[x].indexOf(cf8) !=-1){
pmhcde += cf0+pmimsa[x]+cf2+pmimsa[(x+1)]+';height:4px">';
pmhcde += cf0+(pmimsa[x]+"Top")+cf2+'">'+cf1;
pmhcde += cf0+(pmimsa[x]+"Bottom")+cf2+';top:1">'+cf1+cf1;
pmhcde += pmimsa[(x+2)];
}
else
pmhcde += cf0+pmimsa[x]+cf2+pmimsa[(x+1)]+'">'+pmimsa[(x+2)];
}
if (pmimsa[x].indexOf("main") != -1 && pmimsa[x].indexOf("maini") == -1){
pat=/\D/g;
popmbnum = pmimsa[x].replace(pat,"");
if (popmbnum != '')
popmbnum = "s"+popmbnum;
for (k=0; k<pmnnb.length; k++)
pmhcde +=  cf0+'popm'+popmbnum+pmnnb[k]+cf2+'">'+cf1;
}
}

Popuphl = new Layer(1000);
Popupml = new Layer(1000);
with (Popuphl){
document.open("text/html");
document.writeln(pmcde);
document.close();
}
with(Popupml){
document.open("text/html");
document.writeln(pmhcde);
document.close();
}
wlw = window.innerWidth;
wlh = window.innerHeight;
window.scrollBy(1,1);
popminit();
document.onmousedown = setpopdsploff;
popvbar = (window.pageYOffset>0) ? 1:0;
pophbar = (window.pageXOffset>0) ? 1:0;
window.scrollBy(-1,-1);
window.onresize = resz;
ecY = pmHLm.top;
popsmf();
}

function setpopdsploff(e){
if (e.which==1)
popdsploff(1);
}

