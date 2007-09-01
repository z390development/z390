import java.io.File;

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
    * 08/27/07 RPI 688 misc. fixes:
    *          1) return code 8 if open ACB fails
    *          2) return code 4 if close fails
    *          3) set RPLXRBA field to last VX0 read offset
    ********************************************************
    * Global variables                       (last RPI)
    *****************************************************/
    tz390 tz390 = null;
    pz390 pz390 = null;
    sz390 sz390 = null;
    byte cur_vsam_op   = 0;
    byte vsam_op_open  = 19;
    byte vsam_op_close = 20;
    /*
     * VCDT - VSAM cluster definition table
     */
    int  cur_vcdt_addr = 0;
    int  cur_vcdt_flag = 0; 
    int  cur_vcdt_lrec = 0;
    int  cur_vcdt_naix = 0;
    int  cur_vcdt_paix = 0;
    int  cur_vcdt_tiot = 0;
    int  cur_vcdt_aves = 0;
    int  cur_vcdt_avx0 = 0;
    int  cur_vcdt_klen = 0;
    int  cur_vcdt_koff = 0;
    int  cur_vcdt_aix  = 0;
    String cur_vcdt_file_name;
    int  vcdt_id   = 0; // vcdt offset to id C'VCDT'
    int  vcdt_flag = 4; // vcdt option flags
    int  vcdt_flag_recv = 0x80000000; // variable record length
    int  vcdt_flag_ksds = 0x08000000; // KSDS key sequential (default)
    int  vcdt_flag_ruse = 0x40000000; // reset EOF at ACB open
    int  vcdt_flag_path = 0x20000000; // use KSDS alternate path in VCDTPAIX
    int  vcdt_flag_rrds = 0x04000000; // RRDS relative record data set
    int  vcdt_flag_esds = 0x02000000; // KSDS key sequenced data set
    int  vcdt_flag_lds  = 0x01000000; // LDS  linear
    int  vcdt_lrec      = 8; // vcdt offset to max or fixed record length
    int  vcdt_naix      = 12; // vcdt number of KSDS alternate indexes
    int  vcdt_paix      = 16; // vcdt addr path aix section with VX0DCB, ALEN, AOFF
    int  vcdt_tiot      = 20; // vcdt TIOT table index after loading
    int  vcdt_aves      = 24; // vcdt offset to addr of VESDCB for ESDS data file
    int  vcdt_avx0      = 28; // vcdt offset to addr of VX0DCB for RRDS index file
    int  vcdt_klen      = 32; // vcdt KSDS primary key length
    int  vcdt_koff      = 36; // vcdt KSDS primary key offset
    int  vcdt_aix       = 40; // vcdt start of alternate index sections
	/* 
	 * AIX alternate index section within VCDT
	 */
    int cur_aix_sect = 0; // cur aix section addr
    int aix_adcb     = 0; // cur aid vxn dcb address
    int aix_klen     = 0; // cur aix vxn key length in VES
    int aix_koff     = 0; // cur aix vxn key offset in VES
    int aix_sect_len = 4; // length of each aix section
    /*
	 * ACB 
	 */
	int  cur_acb_addr  = 0;
	int  cur_acb_macrf = 0;
    String cur_vcdt_path;
	byte   cur_acb_oflgs   = 0;
	boolean acb_seq_out = false;
	int  acb_id       = 0;  // acb offset to id
	int  acb_macrf    = 12; // acb offset to macrf flags
	int  acb_macrf_key = 0x80000000; // KSDS key access
	int  acb_macrf_adr = 0x40000000; // RBA access
	int  acb_macrf_cnv = 0x20000000; // control interval access (not suppored)
	int  acb_macrf_seq = 0x10000000; // sequential access
	int  acb_macrf_dir = 0x08000000; // direct access
	int  acb_macrf_in  = 0x04000000; // input only
	int  acb_macrf_out = 0x02000000; // output add, update, delete
	int  acb_macrf_ubf = 0x01000000; // user buffer management (ignored)
	int  acb_macrf_skp = 0x00800000; // skip sequential access
	int  acb_macrf_nlogon = 0x00400000; // no logon required
	int  acb_macrf_rst = 0x00200000; // data set reusable (reset rba at open)
	int  acb_macrf_dsn = 0x00100000; // subtask sharing based on DSN
	int  acb_macrf_aix = 0x00080000; // process alt. index versus base
	int  acb_macrf_lsr = 0x00040000; // local shared resources
	int  acb_macrf_gsr = 0x00020000; // global shared resources
	int  acb_macrf_ici = 0x00010000; // improve control interval processing
	int  acb_macrf_dfr = 0x00008000; // defer puts until WRTBUF or required
	int  acb_macrf_sis = 0x00004000; // sequential insert strategy
	int  acb_macrf_cfx = 0x00002000; // fix control blocks and buffers
	int  acb_vcdt      = 16; // acb offset to vcdt addr
	int  acb_ddnam     = 20; // acb offset to DDNAME
	int  acb_dsnam     = 28; // acb offset to DSNAME addr
	int  acb_oflgs     = 32; // acb offset to open flags
	byte acb_oflgs_open = (byte)0x80;  // acb open
	byte acb_oflgs_in   = 0x40;  // input only
	byte acb_oflgs_out  = 0x20;  // output add, update, delete
    /*
     * RPL request list
     */
	int cur_rpl_addr  = 0; // cur RPL address
	int cur_rpl_ecb   = 0; // addr ECB to post completion
	int cur_rpl_feedb = 0; // feedback codes
	int cur_rpl_lkey  = 0; // generic key length
	int cur_rpl_area  = 0; // addr record area
	int cur_rpl_arg   = 0; // argument with KSDS key, RRDS rec #, or RBA/XRBA
	int cur_rpl_opt   = 0; // RPL option flags
	int cur_rpl_next  = 0; // RPL next RPL address in chained requests
	int cur_rpl_lrec  = 0; // RPL record length for RECV PUT
	long cur_rpl_xrba  = 0; // RPL last VX0 XRBA offset
	int cur_rpl_larea = 0; // RPL area length
	int rpl_opt_loc    = 0x8000; // RPL option leave rec in buffer
	int rpl_opt_dir    = 0x4000; // RPL option direct access 
	int rpl_opt_seq    = 0x2000; // RPL option sequential access
	int rpl_opt_skp    = 0x1000; // RPL option skip sequential access
	int rpl_opt_asy    = 0x0800; // RPL option 
	int rpl_opt_kge    = 0x0400; // RPL option 
	int rpl_opt_gen    = 0x0200; // RPL option 
	int rpl_opt_xrba   = 0x0100; // RPL option 
	int rpl_opt_key    = 0x0080; // RPL option 
	int rpl_opt_adr    = 0x0040; // RPL option 
	int rpl_opt_cnv    = 0x0020; // RPL option 
	int rpl_opt_bwd    = 0x0010; // RPL option 
	int rpl_opt_lrd    = 0x0008; // RPL option 
	int rpl_opt_waitx  = 0x0004; // RPL option 
	int rpl_opt_upd    = 0x0002; // RPL option 
	int rpl_opt_nsp    = 0x0001; // RPL option 
	int rpl_ecb   = 4;  // RPL address ecb to post
	int rpl_feedb = 8;  // RPL feedback
	int rpl_lkey  = 12; // RPL length of generic key
	int rpl_acb   = 16; // RPL acb addr 
	int rpl_area  = 20; // RPL record area
	int rpl_arg   = 24; // RPL argment for KSDS key, RRDS #, ESDS/LDS RBA/XRBA
	int rpl_opt   = 28; // RPL options 2 bytes and 2 bytes filler
	int rpl_next  = 32; // RPL next RPL in chain
	int rpl_lrec  = 36; // RPL length of record for recv PUT
    int rpl_xrba  = 40; // RPL XRBA of last VX0 ESDS pointer access
	int rpl_larea = 48; // RPL length of record area
	/*
	 * VSAM RPL feedback codes 4 bytes (PDF,RC,CC,RNC)
	 */
	byte pdf_def  = 0;  // default Problem Determination Field 
    byte rc_ok    = 0;  // operation sccessful
    byte rc_log   = 8;  // rpl logical error
    byte rc_phy   = 12; // rpl physical error
    byte cmp_base = 0;  // RPLBASER error accessing ES base
    byte cmp_aix  = 2;  // RPLAIXER error accessing AIX index
    // rc_ok reason codes (see MVS 3.8 IDARMRCD.MAC)
    byte rn_ok      = 0; // ok
    byte rn_dup_aix_key = 8; // RPLMOKEY dup aix key exists
    // rc_log logical error reason codes
    byte rn_eod          = 4;   // RPLEODER end of data
    byte rn_dup_key      = 8;   // RPLDUP attempt to write dup pri or unique aix key
    byte rn_out_of_seq   = 12;  // RPLSEQCK skip seq key out of sequence
    byte rn_rcd_not_fnd  = 16;  // RPLNOREC record not found
    byte rn_rba_not_rcd  = 32;  // RPLINRBA RBA not a record address
    byte rn_area_len_err = 44;  // RPLINBUF record area too small
    byte rn_inv_acc_type = 68;  // RPLINACC invalid RPL access for ACB MACRF
    byte rn_inv_key_req  = 72;  // RPLINKEY invalid key req for ESDS/RRDS
    byte rn_inv_rpl_opt  = 104; // RPLINVP invalid RPL options
    byte rn_inv_rec_len  = 108; // RPLINLEN rec len > max or <> fixed len
    byte rn_inv_key_len  = 112; // RPLKEYLC key len > max or 0
    byte rn_inv_rec_num  = (byte)192; // RPLIRRNO invalid RRDS record #
    byte rn_inv_rba_req  = (byte)196; // RPLRRADR invalid RBA req to RRDS
    byte rn_acb_not_open = (byte)235; // z390 catch all 
    // rc_phy physical error reason codes
    byte rn_read_data_err   = 4;  // RPLRDERD data read error
    byte rn_read_index_err  = 8;  // RPLRDERI index read error
    byte rn_write_data_err  = 16; // RPLWTERD data write error
    byte rn_write_index_err = 20; // RPLWTERI index write error
	/*
	 * excp level I/O
	 */
    long rrds_ves_xrba_min = 0x10;
    long cur_read_index = 0; // xbra read by excp_read_index
    int  cur_ves_tiot_index = 0;
    long cur_ves_xrba = 0;
    int  cur_vx0_tiot_index = 0;
    long cur_vx0_xrba = 0;
    long cur_vx0_ves_xrba = 0;
    long cur_vxn_xrba = 0;
    /*
     * stats option statistics for log
     */
    int    tot_vsam_oper   = 0;
    int    tot_acb_open   = 0;
    int    tot_acb_close  = 0;
    int    tot_rpl_get    = 0;
    int    tot_rpl_put    = 0;
    /****************************************************
     * end of global variables
     ****************************************************/
    public void init_vz390(tz390 shared_tz390,pz390 shared_pz390,sz390 shared_sz390){
    	/*
    	 * init vz390
    	 */
    	tz390 = shared_tz390;
    	pz390 = shared_pz390;
    	sz390 = shared_sz390;
    }
    public void svc_vsam(){
   /*
    *  execute VSAM access method service requested
    *  
    */
    switch (cur_vsam_op){
    case 1: // GET  R1=A(RPL)
    	 svc_rpl_get();
         break;
    case 2: // PUT  R1=A(RPL)
        svc_rpl_put();
        break;
    case 3: // ERASE R1=A(RPL)
    	svc_rpl_erase();
    	break;
    case 4: // POINT R1=A(RPL)
    	svc_rpl_point();
    	break;
    case 19: // OPEN R1=A(ACB)
    	svc_open_acb();
    	break;
    case 20: // CLOSE R1=A(ACB)
    	svc_close_acb();
    	break;
    default:
         pz390.set_psw_check(pz390.psw_pic_oper);
    }
}
    public void svc_open_acb(){
    	/*
    	 * open ACB defining VSAM ESDS, RRDS, or KSDS
    	 *    1.  Use DDNAME/DSNAME to load VCDT 
    	 *    2.  Verify ACB vs VCDT options
    	 *    3.  Open VESDCB ESDS data record file
    	 *    4.  Open VX0DCB RRDS index file
    	 * Notes:
    	 *   1.  Issue ABEND 813 if open fails.
    	 */
    	tot_acb_open++;
    	tot_vsam_oper++;
    	cur_acb_addr   = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
        fetch_acb_fields();
    	if (cur_acb_oflgs != 0){
    		pz390.mem.putInt(pz390.r15,4);
    		return;
    	}
    	if (load_vcdt()
    		&& check_acb_macrf()
    		&& open_ves_dcb()
    	    && open_vx0_dcb()
    		&& open_vxn_dcbs()){
    		// set acb oflg open flag and in/out flags
    		pz390.mem.put(cur_acb_addr+acb_oflgs,(byte)(sz390.cur_open_opt | acb_oflgs_open));
            pz390.mem.putInt(cur_acb_addr+acb_vcdt,cur_vcdt_addr);
    		pz390.reg.putInt(pz390.r15,0);
    	} else {
    		pz390.reg.putInt(pz390.r15,8); // RPI 688 VSAM open failed
    	}
    }
    public void svc_close_acb(){
    	/*
    	 * close open acb
    	 *   1.  close VESDCB and VX0DCB
    	 *   2.  If KSDS index updates pending,
    	 *       rewrite VXNDCB's from key index trees.
    	 */
    	tot_acb_close++;
    	tot_vsam_oper++;
    	cur_acb_addr   = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
    	fetch_acb_fields();
    	if ((cur_acb_oflgs & acb_oflgs_open) == 0){
    		pz390.reg.putInt(pz390.r15,4);
    		return;
    	}
   		fetch_vcdt_fields();
  		if (close_ves_dcb()
	        && close_vx0_dcb()
 			&& close_vxn_dcbs()){
  			// reset acb oflg open flag and in/out flags 
  			pz390.mem.put(cur_acb_addr+acb_oflgs,(byte)0);
  			pz390.reg.putInt(pz390.r15,0);
  		} else {
  			pz390.reg.putInt(pz390.r15,4); //rpi 688 close failed
  		}
    }
    private boolean load_vcdt(){
    	/*
    	 * load VCDT using ACB DSNAME or DDNAME
    	 * 
    	 */
    	int cur_dsn_addr = pz390.mem.getInt(cur_acb_addr + acb_dsnam);
    	if (cur_dsn_addr == 0){
    		pz390.reg.putInt(pz390.r15,(cur_acb_addr + acb_ddnam) | 0x80000000);
    	} else {
    		pz390.reg.putInt(pz390.r15,cur_dsn_addr);
    	}
    	sz390.svc_load();
    	if (pz390.reg.getInt(pz390.r15) == 0){
    		// set current VCDT address and TIOT index 
    		cur_vcdt_addr = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
    		pz390.mem.putInt(cur_acb_addr + acb_vcdt,cur_vcdt_addr);
    		pz390.mem.putInt(cur_vcdt_addr + vcdt_tiot,sz390.cur_cde+1); // store +1 (nz indicates set)
    		fetch_vcdt_fields();
    		return true;
    	}
    	return false;
    }
    private void fetch_acb_fields(){
    	/*
    	 * fetch acb fields from cur_acb_addr
    	 */
    	cur_acb_macrf  = pz390.mem.getInt(cur_acb_addr + acb_macrf);
    	cur_vcdt_addr = pz390.mem.getInt(cur_acb_addr + acb_vcdt);
    	cur_acb_oflgs  = pz390.mem.get(cur_acb_addr + acb_oflgs);   		
    }
    private void fetch_vcdt_fields(){
    	/* 
    	 * fetch fields from cur_vcdt_addr 
    	 * for use by open, get, put, close
    	 */
    	cur_vcdt_flag = pz390.mem.getInt(cur_vcdt_addr + vcdt_flag);
		cur_vcdt_lrec = pz390.mem.getInt(cur_vcdt_addr + vcdt_lrec);
		cur_vcdt_naix = pz390.mem.getInt(cur_vcdt_addr + vcdt_naix);
		cur_vcdt_paix = pz390.mem.getInt(cur_vcdt_addr + vcdt_paix);
		cur_vcdt_tiot = pz390.mem.getInt(cur_vcdt_addr + vcdt_tiot);
		cur_vcdt_aves = pz390.mem.getInt(cur_vcdt_addr + vcdt_aves);
		cur_vcdt_avx0 = pz390.mem.getInt(cur_vcdt_addr + vcdt_avx0);
		cur_vcdt_klen = pz390.mem.getInt(cur_vcdt_addr + vcdt_klen);
		cur_vcdt_koff = pz390.mem.getInt(cur_vcdt_addr + vcdt_koff);
		cur_vcdt_aix  = pz390.mem.getInt(cur_vcdt_addr + vcdt_aix);
    }
    private void fetch_rpl_fields(){
    	/*
    	 * fetch RPL, ACB, and VCDT fields for 
    	 * GET, PUT, etc.
    	 */
        cur_rpl_addr = pz390.reg.getInt(pz390.r1) & pz390.psw_amode;
        cur_rpl_ecb  = pz390.mem.getInt(cur_rpl_addr + rpl_ecb);
        cur_rpl_lkey = pz390.mem.getInt(cur_rpl_addr + rpl_lkey);
        cur_acb_addr = pz390.mem.getInt(cur_rpl_addr + rpl_acb);
        cur_rpl_area = pz390.mem.getInt(cur_rpl_addr + rpl_area);
        cur_rpl_arg  = pz390.mem.getInt(cur_rpl_addr + rpl_arg);
        cur_rpl_opt  = pz390.mem.getShort(cur_rpl_addr + rpl_opt);
        cur_rpl_next = pz390.mem.getInt(cur_rpl_addr + rpl_next);
        cur_rpl_lrec = pz390.mem.getInt(cur_rpl_addr + rpl_lrec);
        cur_rpl_larea= pz390.mem.getInt(cur_rpl_addr + rpl_larea);
    }
    private boolean check_acb_macrf(){
    	/*
    	 * check for consistency between
    	 * VCDT and ACB options and if 
    	 * seq out then set acb_seq_out
    	 */
    	if ((cur_vcdt_flag & vcdt_flag_esds) != 0){
    		// ESDS
    		if ((cur_acb_macrf & acb_macrf_seq) != 0
    			&& (cur_acb_macrf & acb_macrf_out) != 0){
    			acb_seq_out = true;
    		} else { 
    			acb_seq_out = false;
    		}
    	} else if ((cur_vcdt_flag & vcdt_flag_rrds) != 0){
    		// RRDS
    		if ((cur_acb_macrf & acb_macrf_dir) != acb_macrf_dir
    			 && (cur_acb_macrf & acb_macrf_key) != acb_macrf_key){
    			sz390.put_log("OPEN ACB RRDS requires KEY,DIR rel rcd access");
    			pz390.reg.putInt(pz390.r15,8);
    			return false;
    		}
    	}
    	pz390.reg.putInt(pz390.r15,0);
    	return true;
    }
    private boolean open_ves_dcb(){
    	/*
    	 * open VES DCB for ESDS data record file
    	 */
        // use same flags in r0 for open acb and dcb's
    	sz390.cur_dcb_addr = cur_vcdt_aves;
    	get_vcdt_path();
    	sz390.svc_open_dcb(cur_vcdt_path);
    	if (pz390.reg.getInt(pz390.r15) == 0){
    		if (acb_seq_out){
    			reuse_file(cur_vcdt_aves);
    		}
    		return true;
    	} else {
    		return false;
    	}
    }
    private boolean open_vx0_dcb(){
    	/*
    	 * open VX0 DCB for KSDS< RRDS, ESDS only
    	 * (not used for LDS RBA access with variable length)
    	 */
    	if ((cur_vcdt_flag & vcdt_flag_lds) != 0){
            // skip open for LDS linear RBA access
    		pz390.reg.putInt(pz390.r15,0);
    		return true; 
    	}
        // use same flags in r0 for open acb and dcb's
    	sz390.cur_dcb_addr = cur_vcdt_avx0;
    	get_vcdt_path();
    	sz390.svc_open_dcb(cur_vcdt_path);
    	if (pz390.reg.getInt(pz390.r15) == 0){
    		if (acb_seq_out){
    			reuse_file(cur_vcdt_avx0);
    		}
    		return true;
    	} else {
    		return false;
    	}
    }
    private boolean open_vxn_dcbs(){
    	/*
    	 * open VXN DCB's for KSDS index files
    	 */
    	if ((cur_vcdt_flag & vcdt_flag_ksds) == 0){
            // skip open if not KSDS
    		pz390.reg.putInt(pz390.r15,0);
    		return true; 
    	}
    	cur_aix_sect = cur_vcdt_aix;
    	int index = 0;
    	while (index <= cur_vcdt_naix){
            // use same flags in r0 for open acb and dcb's
        	sz390.cur_dcb_addr = pz390.mem.getInt(cur_aix_sect + aix_adcb);
        	get_vcdt_path();
        	sz390.svc_open_dcb(cur_vcdt_path);
        	if (pz390.reg.getInt(pz390.r15) != 0){
        		return false;
        	}
    		if (acb_seq_out){
    			reuse_file(sz390.cur_dcb_addr);
    		}
    		cur_aix_sect = cur_aix_sect + aix_sect_len;
        	index++;
    	}
    	return true;
    }
    private boolean close_ves_dcb(){
    	/*
    	 * close VES DCB for ESDS data record file
    	 */
    	sz390.cur_dcb_addr = cur_vcdt_aves;
    	sz390.svc_close_dcb();
    	if (pz390.reg.getInt(pz390.r15) == 0){
    		return true;
    	} else {
    		return false;
    	}
    }
    private boolean close_vx0_dcb(){
    	/*
    	 * close VX0 DCB for KSDS, RRDS, ESDS only
    	 * (not used for LDS RBA access with variable length)
    	 */
    	if ((cur_vcdt_flag & vcdt_flag_lds) != 0){
            // skip open for LDS linear RBA access
    		pz390.reg.putInt(pz390.r15,0);
    		return true; 
    	}
    	sz390.cur_dcb_addr = cur_vcdt_avx0;
    	sz390.svc_close_dcb();
    	if (pz390.reg.getInt(pz390.r15) == 0){
    		return true;
    	} else {
    		return false;
    	}
    }
    private boolean close_vxn_dcbs(){
    	/*
    	 * close VXN DCB's for KSDS index files
    	 * after building/rebuilding each KSDS
    	 * linear XRBA key index to VES data file. 
    	 */
    	if ((cur_vcdt_flag & vcdt_flag_ksds) == 0){
            // skip open if not KSDS
    		pz390.reg.putInt(pz390.r15,0);
    		return true; 
    	}
    	cur_aix_sect = cur_vcdt_aix;
    	int index = 0;
    	while (index <= cur_vcdt_naix){
        	sz390.cur_dcb_addr = pz390.mem.getInt(cur_aix_sect + aix_adcb);
    		sz390.svc_close_dcb();
        	if (pz390.reg.getInt(pz390.r15) != 0){
        		return false;
        	}
    		cur_aix_sect = cur_aix_sect + aix_sect_len;
        	index++;
    	}
    	return true;
    }
    private void get_vcdt_path(){
    	/*
    	 * get VCDT path for use in VX?DCB
    	 * file opens
    	 */
    	int index = sz390.cde_file[cur_vcdt_tiot-1].lastIndexOf(File.separator);
    	if (index > 0){
    		cur_vcdt_path = sz390.cde_file[cur_vcdt_tiot-1].substring(0,index+1);
    	} else {
    		cur_vcdt_path = "";
    	}    	
    }
    private void svc_rpl_get(){
    	/*
    	 * retrieve record from open VSAM 
    	 * ACB/VCDT cluster.
    	 */
    	tot_rpl_get++;
    	tot_vsam_oper++;
        fetch_rpl_fields();
        fetch_acb_fields();
    	if ((cur_acb_oflgs & acb_oflgs_open) == 0){
    		// acb not open
    		set_feedback(pdf_def,rc_log,cmp_base,rn_acb_not_open);
    		return;
    	}
       	if ((cur_vcdt_flag & vcdt_flag_esds) != 0){
       		// ESDS get
       		if ((cur_acb_macrf & acb_macrf_seq) == acb_macrf_seq){
       		    rpl_get_esds_seq();
       		} else if ((cur_acb_macrf & acb_macrf_adr) == acb_macrf_adr){
       			rpl_get_esds_adr();
       		}
       	} else if ((cur_vcdt_flag & vcdt_flag_rrds) != 0){
       		// RRDS get
       		rpl_get_rrds();
       	} else {
       		// add KSDS, LDS support
       		set_feedback(pdf_def,rc_log,cmp_base,rn_inv_rpl_opt);
       	}
    }
    private void rpl_get_esds_seq(){
    	/*
    	 * ESDS seq get
    	 */
        // read rec xrba from VX0 and read corresponding rec from VES
    	cur_vx0_tiot_index = pz390.mem.getInt(cur_vcdt_avx0 + sz390.dcb_iobad)-1;
    	cur_vx0_xrba       = sz390.tiot_cur_rba[cur_vx0_tiot_index];
    	if (cur_vx0_xrba >= sz390.tiot_eof_rba[cur_vx0_tiot_index]){
        	// return logical end of file
    		set_feedback(pdf_def,rc_log,cmp_base,rn_eod);
        	return;
    	} else {
    		rpl_set_xrba();
    	}
    	// read next vse record xrba from vx0
    	if (!excp_read_index(cur_vx0_tiot_index,cur_vx0_xrba)){
    		return;
    	} else {
    		rpl_set_xrba();
    	}
    	if  (cur_read_index <= 0){
    		// inserted or deleted records index
    		// return log index error for now
    		set_feedback(pdf_def,rc_phy,cmp_base,rn_read_index_err);
    	}
    	cur_ves_tiot_index = pz390.mem.getInt(cur_vcdt_aves + sz390.dcb_iobad)-1;
    	// read ves record at cur vs0 index 
    	if (!excp_read_rec(cur_ves_tiot_index,cur_read_index-1)){ // rpi 688
    		return;
    	}
    	try {
    		sz390.tiot_cur_rba[cur_ves_tiot_index] = sz390.tiot_file[cur_ves_tiot_index].getFilePointer();
    	} catch (Exception e){
    		set_feedback(pdf_def,rc_phy,cmp_base,rn_read_data_err);
    	}
    	// return successful write of ves rec and vx0 index
    	set_feedback(pdf_def,rc_ok,cmp_base,rn_ok);
    }
    private void rpl_get_esds_adr(){
    	/*
    	 * ESDS get by rba or xrba
    	 */
        // get vx0 rba or xrba from RPLARG addr
    	cur_vx0_tiot_index = pz390.mem.getInt(cur_vcdt_avx0 + sz390.dcb_iobad)-1;
    	if ((cur_rpl_opt & rpl_opt_xrba) == rpl_opt_xrba){
        	cur_vx0_xrba = pz390.mem.getLong(cur_rpl_arg);
        } else {
        	cur_vx0_xrba = pz390.mem.getInt(cur_rpl_arg);
        }
    	if (cur_vx0_xrba >= sz390.tiot_eof_rba[cur_vx0_tiot_index]
    	    || cur_vx0_xrba < 0){
        	// return logical invalid rba request error
    		set_feedback(pdf_def,rc_log,cmp_base,rn_inv_rba_req);
        	return;
    	}
    	// read next ves record xrba from vx0
    	if (!excp_read_index(cur_vx0_tiot_index,cur_vx0_xrba)){
    		return;
    	} else {
            rpl_set_xrba();
    	}
    	if  (cur_read_index <= 0){
    		// inserted or deleted records index
    		// return log index error for now
    		set_feedback(pdf_def,rc_phy,cmp_base,rn_read_index_err);
    	}
    	cur_ves_tiot_index = pz390.mem.getInt(cur_vcdt_aves + sz390.dcb_iobad)-1;
    	// read ves record at cur vs0 index 
    	if (!excp_read_rec(cur_ves_tiot_index,cur_read_index-1)){ // RPI 688
    		return;
    	}
    	// return successful read of ves rec and vx0 index
    	set_feedback(pdf_def,rc_ok,cmp_base,rn_ok);
    }
    private void rpl_set_xrba(){
    	/*
    	 * set RPL get/put cur_rpl_xrba
    	 */
		cur_rpl_xrba = cur_vx0_xrba;
		pz390.mem.putLong(cur_rpl_addr+rpl_xrba,cur_rpl_xrba);
    }
    private void rpl_get_rrds(){
    	/*
    	 * get for open RRDS file
    	 * Notes:
    	 *   1.  Read vx0 XRBA for rel. rcd #
    	 *   2.  If XRBA = 0, add rec to VES else rewrite
    	 */
    	cur_ves_tiot_index = pz390.mem.getInt(cur_vcdt_aves + sz390.dcb_iobad)-1;
		cur_vx0_tiot_index = pz390.mem.getInt(cur_vcdt_avx0 + sz390.dcb_iobad)-1;
		if (!get_rrds_rec_xrba()){
			// RRDS rec not written or too big
			set_feedback(pdf_def,rc_phy,cmp_base,rn_rcd_not_fnd);
			return;
		}
		// read existing record
		if (!excp_read_rec(cur_ves_tiot_index,cur_read_index)){
			return;
		}
		// return successful read of ves rec and vx0 index
		set_feedback(pdf_def,rc_ok,cmp_base,rn_ok);
    }
    private boolean get_rrds_rec_xrba(){
    	/*
    	 * 1.  Set cur_vx0_xrba = rpl rec # * 8
    	 *     and read index xrba int cur_ves_xrba
    	 * 2.  Set cur_ves_xrba as follows:
    	 *     a.  0 if no record found and return false
    	 *     b.  xrba and return true.
    	 * Note:
    	 *   1.  VX0 XRBA's for valid records are stored +1.
    	 *       to distinguish 0 as unwritten VES XRBA. 
    	 */
    	cur_vx0_xrba = pz390.mem.getInt(cur_rpl_arg) << 3;
    	if (cur_vx0_xrba >= sz390.tiot_eof_rba[cur_vx0_tiot_index]){
			// RRDS rec not written yet
			cur_ves_xrba = 0;
			return false;
		}
		if (!excp_read_index(cur_vx0_tiot_index,cur_vx0_xrba)){
			// I/O error on index
			cur_ves_xrba = -1;
			return false;
		} else {
            rpl_set_xrba();
		}
		if (cur_read_index == 0){
			// RRDS record not found
			cur_ves_xrba = 0;
			return false;
		}
		// remove odd bit from RRDS VES XRBA
		cur_read_index--;
		return true;
    }
    private void svc_rpl_put(){
    	/*
    	 * update or write new record in open VSAM 
    	 * ACB/VCDT cluster.
    	 */
    	tot_rpl_put++;
    	tot_vsam_oper++;
        fetch_rpl_fields();
        fetch_acb_fields();
    	if ((cur_acb_oflgs & acb_oflgs_open) == 0){
    		set_feedback(pdf_def,rc_log,cmp_base,rn_acb_not_open);
    		return;
    	}
    	if ((cur_acb_oflgs & acb_oflgs_out) == 0
    		|| (cur_acb_macrf & acb_macrf_out) == 0){
    		set_feedback(pdf_def,rc_log,cmp_base,rn_inv_rpl_opt);
    		return;
    	}
    	if ((cur_vcdt_flag & vcdt_flag_esds) != 0){
    		// ESDS out - add rec to VES
            rpl_put_esds();
    	} else if ((cur_vcdt_flag & vcdt_flag_rrds) != 0){
    		// RRDS
    		rpl_put_rrds();
    	} else {
    		// add KSDS, LDS support
    		set_feedback(pdf_def,rc_log,cmp_base,rn_inv_rpl_opt);
    	}
    }
    private void rpl_put_esds(){
    	/*
    	 * put for open ESDS output file
    	 */
		cur_ves_tiot_index = pz390.mem.getInt(cur_vcdt_aves + sz390.dcb_iobad)-1;
		cur_ves_xrba = sz390.tiot_eof_rba[cur_ves_tiot_index];
		if (!excp_write_rec(cur_ves_tiot_index,cur_ves_xrba)){
			return;
		}
		try {
			sz390.tiot_eof_rba[cur_ves_tiot_index] = sz390.tiot_file[cur_ves_tiot_index].length();
		} catch (Exception e){
			set_feedback(pdf_def,rc_phy,cmp_base,rn_write_data_err);
		}
		// add rec xrba index to VX0
		cur_vx0_tiot_index = pz390.mem.getInt(cur_vcdt_avx0 + sz390.dcb_iobad)-1;
		cur_vx0_xrba       = sz390.tiot_eof_rba[cur_vx0_tiot_index];
		if (!excp_write_index(cur_vx0_tiot_index,cur_vx0_xrba,cur_ves_xrba+1)){ // rpi 688
			return;
		} else {
			rpl_set_xrba();
		}
		try {
			sz390.tiot_eof_rba[cur_vx0_tiot_index] = sz390.tiot_file[cur_vx0_tiot_index].length();
		} catch (Exception e){
			set_feedback(pdf_def,rc_phy,cmp_base,rn_write_index_err);
		}
		// return successful write of ves rec and vx0 index
		set_feedback(pdf_def,rc_ok,cmp_base,rn_ok);
    }
    private void rpl_put_rrds(){
    	/*
    	 * put for open RRDS output file
    	 * Notes:
    	 *   1.  Read vx0 XRBA for rel. rcd #
    	 *   2.  If XRBA = 0, add rec to VES else rewrite
    	 */
    	cur_ves_tiot_index = pz390.mem.getInt(cur_vcdt_aves + sz390.dcb_iobad)-1;
		cur_vx0_tiot_index = pz390.mem.getInt(cur_vcdt_avx0 + sz390.dcb_iobad)-1;
		if (get_rrds_rec_xrba()){
			// rewrite existing record
			if (!excp_write_rec(cur_ves_tiot_index,cur_read_index)){
				return;
			}
		} else if (cur_ves_xrba == 0) {
			// add record to VES and update VX0 XRBA
			cur_ves_xrba = sz390.tiot_eof_rba[cur_ves_tiot_index];
			if (!excp_write_rec(cur_ves_tiot_index,cur_ves_xrba)){
				return;
			}
			try {
				// update VES end of file addr
				sz390.tiot_eof_rba[cur_ves_tiot_index] = sz390.tiot_file[cur_ves_tiot_index].length();
			} catch (Exception e){
				set_feedback(pdf_def,rc_phy,cmp_base,rn_write_data_err);
			}
			// update vx0 index to VES XRBA+1 to indicate valid record
			cur_vx0_tiot_index = pz390.mem.getInt(cur_vcdt_avx0 + sz390.dcb_iobad)-1;
			if (!excp_write_index(cur_vx0_tiot_index,cur_vx0_xrba,cur_ves_xrba+1)){ // rpi 688
				return;
			}
		} else {
			// exit with invalid RRDS rec #
			return;
		}
		// return successful write of ves rec and vx0 index
		set_feedback(pdf_def,rc_ok,cmp_base,rn_ok);
    }
    private void svc_rpl_erase(){
    	/*
    	 * erase current record retrieved from open VSAM 
    	 * ACB/VCDT cluster.
    	 * Notes:
    	 *   1.  The current XRBA in VX0 primary index is set to 
    	 *   high values.
    	 */
        fetch_rpl_fields();
        fetch_acb_fields();
    	if ((cur_acb_oflgs & acb_oflgs_open) == 0){
    		set_feedback(pdf_def,rc_log,cmp_base,rn_acb_not_open);
    		return;
    	}
    }
    private void svc_rpl_point(){
    	/*
    	 * set current position to specified
    	 * key, record, or RBA in ESDS base
    	 */
        fetch_rpl_fields();
        fetch_acb_fields();
    	if ((cur_acb_oflgs & acb_oflgs_open) == 0){
    		set_feedback(pdf_def,rc_log,cmp_base,rn_acb_not_open);
    		return;
    	}
    }
    private void set_feedback(byte pdf,byte rc,byte cmp,byte rn){
    	/*
    	 * store RPLFEEDB 4 byte field with:
    	 *   0 - pdf_ Problem Determination Field
    	 *   1 - rc_ return code (also stored in R15)
    	 *   2 - cmp_ component code
    	 *   3 - rn_ reason code for corresponding rc_
    	 *   
    	 */
    	pz390.mem.putInt(cur_rpl_addr + rpl_feedb,((pdf << 8 | rc) << 8 | cmp) << 8 | (rn & 0xff));
    	pz390.reg.putInt(pz390.r15,rc & 0xff);
    }
    private boolean excp_read_rec(int tiot_index,long xrba){
    	/*
    	 * read record from VES into RPLAREA and set length in RPLLREC
    	 * to the VES ESDS base cluster data file at specified xrba.
    	 */
    	if (xrba > tz390.max_file_size){
    		set_feedback(pdf_def,rc_phy,cmp_base,rn_inv_rba_req);
			return false;
    	}
    	if ((cur_vcdt_flag & vcdt_flag_recv) != 0){
    		// read variable length record putting
    		// 4 length prefix into RPLLREC and the remainer in RPLAREA
    		try {
    			sz390.tiot_file[tiot_index].seek(xrba);
    			cur_rpl_lrec = sz390.tiot_file[tiot_index].readInt();
    			pz390.mem.putInt(cur_rpl_addr + rpl_lrec,cur_rpl_lrec); // RPI 688
    			sz390.tiot_file[tiot_index].read(pz390.mem_byte,cur_rpl_area,cur_rpl_lrec);
    			if (tz390.opt_tracev){
    				tz390.put_trace("VSAM EXCP READ VREC  XRBA=" + tz390.get_long_hex(xrba + 4,16) + " LEN=" + tz390.get_hex(cur_rpl_lrec,8));
    				sz390.dump_mem(cur_rpl_area,cur_rpl_lrec);
    			}
    			sz390.tot_dcb_read++;
        		sz390.tot_dcb_oper++;
    			return true;
    		} catch (Exception e){
    			set_feedback(pdf_def,rc_phy,cmp_base,rn_read_data_err);
    			return false;
    		}
    	} else {
            // read fixed length record
    		cur_rpl_lrec = cur_vcdt_lrec;
    		try {
    			sz390.tiot_file[tiot_index].seek(xrba);
    			sz390.tiot_file[tiot_index].read(pz390.mem_byte,cur_rpl_area,cur_rpl_lrec);
    			if (tz390.opt_tracev){
    				tz390.put_trace("VSAM EXCP READ FREC XRBA=" + tz390.get_long_hex(xrba,16) + " LEN=" + tz390.get_hex(cur_rpl_lrec,8));
    				sz390.dump_mem(cur_rpl_area,cur_rpl_lrec);
    			}
    			sz390.tot_dcb_read++;
        		sz390.tot_dcb_oper++;
    			return true;
    		} catch (Exception e){
    			set_feedback(pdf_def,rc_phy,cmp_base,rn_read_data_err);
    			return false;
    		}
    	}
    }
    private boolean excp_write_rec(int tiot_index,long xrba){
    	/*
    	 * write current RPL record in RPLAREA with length RPLLREC
    	 * to the VES ESDS base cluster data file at specified xrba.
    	 */
    	if (xrba > tz390.max_file_size){
    		set_feedback(pdf_def,rc_phy,cmp_base,rn_inv_rba_req);
			return false;
    	}
    	if ((cur_vcdt_flag & vcdt_flag_recv) != 0){
    		// write variable length record with 4 byte prefixed length
    		int save_rec_pfx = pz390.mem.getInt(cur_rpl_area - 4);
    		pz390.mem.putInt(cur_rpl_area-4,cur_rpl_lrec);
    		try {
    			sz390.tiot_file[tiot_index].seek(xrba);
    			sz390.tiot_file[tiot_index].write(pz390.mem_byte,cur_rpl_area-4,cur_rpl_lrec+4);
    			pz390.mem.putInt(cur_rpl_area-4,save_rec_pfx);
    			if (tz390.opt_tracev){
    				tz390.put_trace("VSAM EXCP WRITE VREC  XRBA=" + tz390.get_long_hex(xrba+4,16) + " LEN=" + tz390.get_hex(cur_rpl_lrec,8));
    				sz390.dump_mem(cur_rpl_area,cur_rpl_lrec);
    			}
    			sz390.tot_dcb_write++;
        		sz390.tot_dcb_oper++;
    			return true;
    		} catch (Exception e){
    			pz390.mem.putInt(cur_rpl_area-4,save_rec_pfx);
    			set_feedback(pdf_def,rc_phy,cmp_base,rn_write_data_err);
    			return false;
    		}
    	} else {
            // write fixed length record
    		cur_rpl_lrec = cur_vcdt_lrec;
    		try {
    			sz390.tiot_file[tiot_index].seek(xrba);
    			sz390.tiot_file[tiot_index].write(pz390.mem_byte,cur_rpl_area,cur_rpl_lrec);
    			if (tz390.opt_tracev){
    				tz390.put_trace("VSAM EXCP WRITE FREC XRBA=" + tz390.get_long_hex(xrba,16) + " LEN=" + tz390.get_hex(cur_rpl_lrec,8));
    				sz390.dump_mem(cur_rpl_area,cur_rpl_lrec);
    			}
    			sz390.tot_dcb_write++;
        		sz390.tot_dcb_oper++;
    			return true;
    		} catch (Exception e){
    			set_feedback(pdf_def,rc_phy,cmp_base,rn_write_data_err);
    			return false;
    		}
    	}
    }
    private boolean excp_read_index(int tiot_index,long xrba){
    	/*
    	 * read index entry in vx? at xrba
    	 * and store in cur_read_index.
    	 */
    	if (xrba > tz390.max_file_size){
    		set_feedback(pdf_def,rc_phy,cmp_base,rn_inv_rba_req);
			return false;
    	}
    	try {
    		sz390.tiot_file[tiot_index].seek(xrba);
    		cur_read_index = sz390.tiot_file[tiot_index].readLong();
    		sz390.tiot_cur_rba[tiot_index] = sz390.tiot_file[tiot_index].getFilePointer();
			if (tz390.opt_tracev){
				tz390.put_trace("VSAM EXCP READ INDEX XRBA=" + tz390.get_long_hex(xrba,16) + " ESDS XRBA=" + tz390.get_long_hex(cur_read_index,16));
			}
    		sz390.tot_dcb_read++;
    		sz390.tot_dcb_oper++;
    		return true;
    	} catch (Exception e){
    		set_feedback(pdf_def,rc_phy,cmp_aix,rn_write_index_err);
    		return false;
    	}
    }
    private boolean excp_write_index(int tiot_index,long xrba,long index){
    	/*
    	 * write index entry in vx? at xrba.
    	 */
    	if (xrba > tz390.max_file_size){
    		set_feedback(pdf_def,rc_phy,cmp_base,rn_inv_rba_req);
			return false;
    	}
    	try {
    		sz390.tiot_file[tiot_index].seek(xrba);
    		sz390.tiot_file[tiot_index].writeLong(index);
			if (tz390.opt_tracev){
				tz390.put_trace("VSAM EXCP WRITE INDEX XRBA=" + tz390.get_long_hex(xrba,16) + " ESDS XRBA=" + tz390.get_long_hex(index,16));
			}
    		sz390.tot_dcb_write++;
    		sz390.tot_dcb_oper++;
    		return true;
    	} catch (Exception e){
    		set_feedback(pdf_def,rc_phy,cmp_aix,rn_write_index_err);
    		return false;
    	}
    }
    private boolean reuse_file(int adcb){
    	/*
    	 * reset file length and tiot_eof addr for
    	 * files being resused either due to
    	 * reuse option or seq out options..
    	 */
    	int tiot_index = pz390.mem.getInt(adcb + sz390.dcb_iobad)-1;
    	try {
    		sz390.tiot_file[tiot_index].setLength(0);
    		sz390.tiot_eof_rba[tiot_index] = 0;
    		return true;
    	} catch (Exception e){
    		set_feedback(pdf_def,rc_phy,cmp_aix,rn_write_index_err);
    		return false;    		
    	}
    }
/*
 *  end of vz390 code 
 */
}