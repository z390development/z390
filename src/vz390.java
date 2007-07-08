public  class  vz390 {
   /*****************************************************
	
    z390 portable mainframe assembler and emulator.
	
    Copyright 2006 Automated Software Tools Corporation
	 
    z390 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    z390 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with z390; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    vz390 is the emulator component of z390 called from sz390
    to performwhich VSAM access method services.

    ****************************************************
    * Maintenance
    ****************************************************
    * 06/22/07 initial coding
    ********************************************************
    * Global variables                       (last RPI)
    *****************************************************/
    tz390 tz390 = null;
    pz390 pz390 = null;
	int vsam_rpl = 0;
    /****************************************************
     * end of global variables
     ****************************************************/
    public void init_vz390(tz390 shared_tz390,pz390 shared_pz390){
    	/*
    	 * init vz390
    	 */
    	tz390 = shared_tz390;
    	pz390 = shared_pz390;
    }
    public void svc_vsam(){
   /*
    *  execute VSAM access method service requested
    *
    *  R0 = function code
    *  
    */
    switch (pz390.reg.get(pz390.r0+3)){
    case 1: // PUT  R1=A(RPL)
         vsam_rpl = pz390.reg.getInt(pz390.r1);
         break;
    default:
         pz390.set_psw_check(pz390.psw_pic_oper);
    }
}

/*
 *  end of vz390 code 
 */
}