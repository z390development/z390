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
    * 09/07/07 RPI 681 support new VCDT catalog with entries:
    *          1) VCLR - base clusters
    *          2) VAIX - alternate indexes
    *          3) VPTH - path to vaix or alias vclr
    *          Dynamically allocate DCB's for ACB
    *          Use ACB DDNAME file spec to load catalog and
    *          use option .name suffix to specify entry name
    *          else use ACB name to find entry in catalog.          
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
     * VCDT - VSAM Catalog Definition Table
     * (See mac\VCDTD for VCDT, VCLR, VAIX, VPTH DSECTS)
     */
    int    cur_vcdt_addr = 0; // addr loaded VCDT VSAM catalog
    String cur_vcdt_id;     // VCDTID C'VCDT'
    String cur_vcdt_name;   // VCDTNAME name of VCDT catalog
    int    cur_vcdt_clrt = 0; // VCDTCLRT total base clusters
    int    cur_vcdt_aixt = 0; // VCDTAIXT tot aix indexes
    int    cur_vcdt_aixa = 0; // VCDTAIXA addr aix upgrade table of vaix entries
    int    cur_vcdt_ptht = 0; // VCDTPTHT total paths
    int    cur_vcdt_ptha = 0; // VCDTPTHA addr path entry
    int    cur_vcdt_dcba = 0;
    int    vcdt_id   = 0;  // VCDTID C'VCDT'
    int    vcdt_name = 4;  // VCDTNAME name of catalog
    int    vcdt_clrt = 12; // VCDTCLRT tot base clusters
    int    vcdt_clra = 16; // VCDTCLRA addr base cluster entry
    int    vcdt_aixt = 20; // VCDTAIXT tot aix indexes
    int    vcdt_aixa = 24; // VCDTAIXA addr aix index
    int    vcdt_ptht = 28; // VCDTPTHT tot paths
    int    vcdt_ptha = 32; // VCDTPTHA addr path 
    int    vcdt_dcba = 36; // VCDTDCBA addr model DCB for ACB allocation
    String cur_vcdt_file_name; // from ACB DDNAME/DSNAME
    String cur_vcdt_path;    
    int    cur_vcdt_tiot = 0;  // index of tiot entry + 1
    /*
     * VCLR - VSAM Cluster entry in VCDT catalog 
     */
    String cur_vcltr_id;        // VCLRID C"VCLR"
    String cur_vclr_name;       // VCLRNAME name of base cluster
    String cur_vclr_type;       // VCLRTYPE type of base cluster
    int    cur_vclr_flag = 0;   // VCLRFLAG 4 bytes of flags
    int    cur_vclr_lavg = 0;   // VCLRLAVG average record length for VREC
    int    cur_vclr_lrec = 0;   // VCLRLREC max length or fixed length
    int    cur_vclr_klen = 0;   // VCLRKLEN KSDS primary key length
    int    cur_vclr_koff = 0;   // VCLRKOFF KSDS primary key offset
    int    cur_vclr_vesa = 0;   // VCLRVESA addr DSNAME override for VES
    int    cur_vclr_vx0a = 0;   // VCLRVX0A addr DSNAME override for VX0
    int    cur_vclr_aixn = 0;   // VCLRAIXN total alterante indexes with upgrade for cluster7
    int    cur_vclr_aixa = 0;   // VCLRAIXA addr of VAIX addr table
    int    vclr_id   = 0;  // VCLRID C'VCLR'
    int    vclr_name = 4;  // VCLRNAME name of base cluster
    int    vclr_type = 12; // VCLRTYPE ESDS/RRDS/ESDS/LDS
    int    vclr_flag = 16; // VCLRFLAG option flags
    int    vclr_flag_vrec = 0x80000000; // VCLRVREC variable record length
    int    vclr_flag_ruse = 0x40000000; // VCLRRUSE reset EOF at ACB open
    int    vclr_flag_ksds = 0x08000000; // VCLRKSDS key sequential (default)
    int    vclr_flag_rrds = 0x04000000; // VCLRRRDS relative record data set
    int    vclr_flag_esds = 0x02000000; // VCLRESDS entry sequenced data set
    int    vclr_flag_lds  = 0x01000000; // VCLRLDS  linear
    int    vclr_lavg = 20; // VCLRLAVG average record length for VREC 
    int    vclr_lrec = 24; // VCLRLREC max or fixed record length
    int    vclr_klen = 28; // VCLRKLEN KSDS primary key length
    int    vclr_koff = 32; // VCLRKOFF KSDS primary key offset
    int    vclr_vesa = 36; // VCLRVESA addr optional VES DSNAME (Def NAME.VES)
    int    vclr_vx0a = 40; // VCLRVX0A addr optional VX0 DSNAME (Def NAME.VX0)
    int    vclr_aixt = 44; // VCLRAIXT total AIX with upgrade for cluster changes
    int    vclr_aixa = 48; // VCLRAIXA addr of table with AIX upgrade catalog entries
    int    vclr_len  = 52; // VCLRLEN  length of VCLR catalog entry
    /* 
	 * VAIX alternate index VCDT catalog entries
	 */
    int cur_vaix_addr = 0; // cur aix section addr
    String cur_vaix_id;    // VAIXID C'VAIX'
    String cur_vaix_name;  // VAIXNAME name of alternate index
    String cur_vaix_reln;  // VAIXRELN name of related VCLR base cluster
    int cur_vaix_flag = 0; // VAIXFLAG option flags
    int cur_vaix_klen = 0; // VAIXKLEN length of aix key in base cluster
    int cur_vaix_koff = 0; // VAIXKOFF offset of aix key in base cluster
    int cur_vaix_vxna = 0; // VAIXVXNA addr optional DSNAME (def. NAME.VXN)
    int cur_vaix_rela = 0; // VAIXRELA addr related VCLR base cluster catalog entry
    int vaix_id   =  0; // VAIXID C'VAIX'
    int vaix_name =  4; // VAIXNAME name of AIX
    int vaix_reln = 12; // VAIXRELN name of related VCLR base cluster
    int vaix_flag = 16; // VAIXFLAG 4 bytes of option flags
    int vaix_flag_ruse = 0x80000000; // VAIXRUSE reset aix eof at open
    int vaix_flag_ukey = 0x40000000; // VAIXUKEY inforce unique keys
    int vaix_flag_uaix = 0x20000000; // VAIXUAIX upgrade AIX for base cluster updates
    int vaix_klen = 20; // VAIXKLEN aix key length in VES
    int vaix_koff = 24; // VAIXKOFF aix key offset in VES
    int vaix_vxna = 28; // VAIXVXNA addr optional VXN DSNAME (def. NAME.VXN)
    int vaix_rela = 32; // VAIXRELA addr of related VCLR base cluster catalog entry
    int vaix_len  = 36; // VAIXLEN  length of VAIX catalog entry
    /*
     * VPTH path VCDT catalog entry
     */
    int cur_vpth_addr = 0; // addr current VPTH path entry in VCDT
    String cur_vpth_id;    // VPTHID C'VPTH'
    String cur_vpth_name;  // VPTHNAME name of path
    String cur_vpth_entn;  // VPTHENTN name of path (VAIX or VCLR)
    int cur_vpth_flag = 0; // VPTHFLAG 4 byte option flags
    int cur_vpth_enta = 0; // VPTHENTA addr of VAIX or VCLR entry for path
    int vpth_id   =  0; // VPTHID C'VPTH'
    int vpth_name =  4; // VPTHNAME name of path
    int vpth_entn = 12; // VPTHENTN name of entry VAIX or VCLR path
    int vpth_flag = 20; // VPTHFLAG 4 bytes of option flags
    int vpth_flag_aixp = 0x80000000; // VPTHUAIX update all upgrade AIX's for base cluster
    int vpth_flag_aixu = 0x40000000; // VPTHPAIX path is for VAIX vs alias VCLR 
    int vpth_enta = 24; // VPTHENTA addr of entry VAIX or VCLR entry    
    int vpth_len  = 28; // VPTHLEN  length of VPTH catalog entry
    /*
	 * ACB 
	 */
	int   cur_acb_addr  = 0;
	byte  cur_acb_id;        // ACBID   x'A0'
	byte  cur_acb_stype;     // ACBSTYPE x'11' - x'1F' VSAM vs VTAM types
    short cur_acb_len;       // ACBLEN   length of ACB	
	int   cur_acb_ambl  = 0; // ACBAMBL  AMB list pointer
	int   cur_acb_ifr   = 0; // ACBIFR   VTAM interface routine 0 for VSAM
	int   cur_acb_macrf = 0; // ACBMACRF 4 bytes of option bits
    byte   cur_acb_oflgs   = 0;
	boolean acb_seq_out = false;
    String cur_acb_vclrn;    // ACBVCLRN label of ACB (def. VCLR/VPTH entry)
    int   cur_acb_vclra = 0; // ACBVCLRA addr of VCLR entry in VCDT catalog
    int   cur_acb_vaixa = 0; // ACBVAIXA addr of VAIX entry in VCDT catalog for path
    int   cur_acb_dcbt = 0;  // ACBDCBT  total DCB's for VES, VX0, and VNN upgrades
    int   cur_acb_dcba = 0;  // ACBDCBA  addr of dyn alloc DCB table
	int  acb_id       =  0; // ACBID   x'A0'
	int  acb_stype    =  1; // ACBSTYPE x'11' - x'1F' for VSAM vs VTAM 
	int  acb_len      =  2; // ACBLEN   half word length of ACB
	int  acb_ambl     =  4; // ACBAMBL  AMB list
	int  acb_ifr      =  8; // ACBIFR   VTAM interface routine (0 for VSAM)
	int  acb_macrf    = 12; // ACBMACRF macrf flags
	int  acb_macrf_key = 0x80000000; // ACBMACR1_KEY key access
	int  acb_macrf_adr = 0x40000000; // ACBMACR1_ADR access bu RBA or XRBA
	int  acb_macrf_cnv = 0x20000000; // ACBMACR1_CNV control interval access (not suppored)
	int  acb_macrf_seq = 0x10000000; // ACBMACR1_SEQ sequential access
	int  acb_macrf_dir = 0x08000000; // ACBMACR1_DIR direct access
	int  acb_macrf_in  = 0x04000000; // ACBMACR1_IN  input only
	int  acb_macrf_out = 0x02000000; // ACBMACR1_OUT output add, update, delete
	int  acb_macrf_ubf = 0x01000000; // ACBMACR1_UBF user buffer management (ignored)
	int  acb_macrf_skp = 0x00800000; // ACBMACR2_SKP skip sequential access
	int  acb_macrf_nlogon = 0x00400000; // ACBMACR2_NLOGON no logon required
	int  acb_macrf_rst = 0x00200000; // ACBMACR2_RST data set reusable (reset rba at open)
	int  acb_macrf_dsn = 0x00100000; // ACBMACR2_DSN subtask sharing based on DSN
	int  acb_macrf_aix = 0x00080000; // ACBMACR2_AIX process alt. index versus base
	int  acb_macrf_lsr = 0x00040000; // ACBMACR2_LSR local shared resources
	int  acb_macrf_gsr = 0x00020000; // ACBMACR2_GSR global shared resources
	int  acb_macrf_ici = 0x00010000; // ACBMACR2_ICI improve control interval processing
	int  acb_macrf_dfr = 0x00008000; // ACBMACR3_DFR defer puts until WRTBUF or required
	int  acb_macrf_sis = 0x00004000; // ACBMACR3_SIS sequential insert strategy
	int  acb_macrf_cfx = 0x00002000; // ACBMACR3_CFX fix control blocks and buffers
	int  acb_oflgs     = 16; // ACBPFLGS offset to open flag
	byte acb_oflgs_open = (byte)0x80;  // ACB_OPEN open
	byte acb_oflgs_in   = (byte)0x40;  // ACBGET only
	byte acb_oflgs_out  = (byte)0x20;  // ACBPUT output add, update, delete
    byte acb_oflgs_aixp = (byte)0x10;  // ACBAIXP use aix vs primary key
    byte acb_oflgs_aixu = (byte)0x08;  // ACBAIXU ugrade aix indexes for VCLR
	int  acb_ddnam     = 20; // ACBDDNAM DDNAME > env. var.> VCDT[.VCLR/VPTH)
	int  acb_dsnam     = 28; // ACBDSNAM DSNAME addr > VCDT[.VCLR/VPTH]
	int  acb_vclrn     = 32; // ACBVCLRN name from label field (def VCDT entry)
	int  acb_vclra     = 40; // ACBVCLRA addr VCLR in VCDT catalog
	int  acb_vaixa     = 44; // ACBVAIXA addr VAIX in VCDT catalog for alt path
	int  acb_dcbt      = 48; // ACBDCBN  total DCB's for this ACB
	int  acb_dcba      = 52; // ACBDCBA  addr of dynamically allocated DCB's
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
    int  cur_ves_dcba = 0;
    int  cur_ves_tiot_index = 0;
    long cur_ves_xrba = 0;
    int  cur_vx0_dcba = 0;
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
    	 *    1.  Use DDNAME/DSNAME to load VCDT and
    	 *        find VCLR entry based on cat.name or 
    	 *        search for VCLR with matching ACBNAME. RPI 681
    	 *    2.  Verify ACB vs VCDT options
    	 *    3.  Open VES, VX0, and any upgrade VXN's
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
    	if (load_vcdt()           // load VCDT catalog 
    		&& find_vclr()        // find VCLR cluster
    		&& check_acb_macrf()  // check VCLR/ACB options
    		&& open_acb_dcbs()){        // open DCB's for VES,VX0,VXN's
    		// set acb oflg open flag and in/out flags
    		pz390.mem.put(cur_acb_addr+acb_oflgs,(byte)(sz390.cur_open_opt | acb_oflgs_open));
            pz390.mem.putInt(cur_acb_addr + acb_vclra,cur_acb_vclra);
            pz390.mem.putInt(cur_acb_addr + acb_vaixa,cur_acb_vaixa);
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
  		if (close_acb_dcbs()){
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
    	 *   1. If file spec includes dot, use
    	 *      suffix to find VCDT entry else
    	 *      use ACBNAME field.
    	 *   2. Set ACBDCBN, and ACBDCBA from VCDT 
    	 *      VCLR or VPTH entry. 
    	 */
    	int cur_dsn_addr = pz390.mem.getInt(cur_acb_addr + acb_dsnam);
    	if (cur_dsn_addr == 0){
    		pz390.reg.putInt(pz390.r15,(cur_acb_addr + acb_ddnam) | 0x80000000);
    	} else {
    		pz390.reg.putInt(pz390.r15,cur_dsn_addr);
    	}
    	sz390.load_vcdt_mode = true;
    	sz390.svc_load();
    	sz390.load_vcdt_mode = false;
    	if (pz390.reg.getInt(pz390.r15) == 0){
    		// set current VCDT address, path, tiot index 
    		cur_vcdt_addr = pz390.reg.getInt(pz390.r0) & pz390.psw_amode;
    		cur_vcdt_path = sz390.load_pgm_dir;
    		cur_vcdt_tiot = sz390.cur_cde + 1;
    	    sz390.put_ascii_string(sz390.load_vcdt_entry,cur_acb_addr + acb_vclrn,8,' ');
   			return true;
    	}
    	return false;
    }
    private boolean find_vclr(){
    	/*
    	 * find VCLR/VPTH entry in VCDT
    	 *   1.  Set cur_vclra_addr
    	 *   2.  Set cur_vptha_addr or 0
    	 *       a.  If vpth_flag_aixp, set acb_oflgs_aixp else 0
    	 *       b.  If vpth_flag_aixu, set acb_oflgs_aixu else 0
    	 *   3.  Se cur_vcdt_dcba for use by init_acb_dcb.
    	 */
    	cur_vcdt_dcba  = pz390.mem.getInt(cur_vcdt_addr + vcdt_dcba);
    	cur_vcdt_clrt  = pz390.mem.getInt(cur_vcdt_addr + vcdt_clrt);
    	cur_acb_vclra = pz390.mem.getInt(cur_vcdt_addr + vcdt_clra);
    	while (cur_vcdt_clrt > 0){
    		cur_vclr_name = sz390.get_ascii_string(cur_acb_vclra + vclr_name,8,false);
    		if (sz390.load_vcdt_entry.equals(cur_vclr_name)){
    			pz390.mem.putInt(cur_acb_addr + acb_vclra,cur_acb_vclra);
    			fetch_vclr_fields();
    			cur_vpth_addr = 0; // no path
    			return true;
    		}
    		cur_acb_vclra = cur_acb_vclra + vclr_len;
    	    cur_vcdt_clrt--;
    	}
    	int cur_vcdt_ptht = pz390.mem.getInt(cur_vcdt_addr + vcdt_ptht);
    	int cur_vcdt_ptha = pz390.mem.getInt(cur_vcdt_addr + vcdt_ptha);
    	while (cur_vcdt_ptht > 0){
    		if (sz390.load_vcdt_entry.equals(sz390.get_ascii_string(cur_vcdt_ptha + vpth_name,8,false))){
    			cur_vpth_flag = pz390.mem.getInt(cur_vpth_addr + vpth_flag);
    			if ((cur_vpth_flag & vpth_flag_aixp) != 0){
    			    cur_acb_vaixa = pz390.mem.getInt(cur_vcdt_ptha + vpth_enta);
    			    cur_acb_oflgs = (byte)(cur_acb_oflgs | acb_oflgs_aixp); // use aix path access vs primary
    			    cur_acb_vclra = pz390.mem.getInt(cur_acb_vaixa + vaix_rela);
    			} else {
    				cur_acb_vclra = pz390.mem.getInt(cur_vpth_addr + vpth_enta);
    				cur_acb_vaixa = 0;
    			}
    			pz390.mem.putInt(cur_acb_addr + acb_vclra,cur_acb_vclra);
    			pz390.mem.putInt(cur_acb_addr + acb_vaixa,cur_acb_vaixa);
				if ((cur_vpth_flag & vpth_flag_aixu) != 0){
					cur_acb_oflgs = (byte)(cur_acb_oflgs | acb_oflgs_aixp); // allow aix  ugrades if any
				}
			    pz390.mem.putInt(cur_acb_addr + acb_vclra,cur_acb_vclra);
			    fetch_vclr_fields();
			    pz390.mem.putInt(cur_acb_addr + acb_vaixa,cur_vaix_addr);
    			return true;
    		}
    		cur_vcdt_ptha = cur_vcdt_ptha + vpth_len;
    	    cur_vcdt_ptht--;
    	}
    	return false;
    }
    private void fetch_acb_fields(){
    	/*
    	 * fetch acb fields from cur_acb_addr
    	 */
    	cur_acb_macrf  = pz390.mem.getInt(cur_acb_addr + acb_macrf);
    	cur_acb_oflgs  = pz390.mem.get(cur_acb_addr + acb_oflgs);
    	cur_acb_vclra  = pz390.mem.getInt(cur_acb_addr + acb_vclra);
    	cur_acb_vaixa  = pz390.mem.getInt(cur_acb_addr + acb_vaixa);
    	cur_acb_dcbt   = pz390.mem.getInt(cur_acb_addr + acb_dcbt);
    	cur_acb_dcba   = pz390.mem.getInt(cur_acb_addr + acb_dcba);
        cur_ves_dcba       = cur_acb_dcba;
        cur_ves_tiot_index = pz390.mem.getInt(cur_ves_dcba + sz390.dcb_iobad);
        cur_vx0_dcba       = cur_acb_dcba + sz390.dcb_len;
        cur_vx0_tiot_index = pz390.mem.getInt(cur_vx0_dcba + sz390.dcb_iobad);
    }
    private void fetch_vclr_fields(){
    	/*
    	 * fetch current vclr fields used by rpl_get/put.
    	 * Note open_acb does additional vclr field fetches,
    	 */
    	cur_vclr_flag = pz390.mem.getInt(cur_acb_vclra + vclr_flag);
    	cur_vclr_lrec = pz390.mem.getInt(cur_acb_vclra + vclr_lrec);
    	cur_vclr_klen = pz390.mem.getInt(cur_acb_vclra + vclr_klen);
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
    	if ((cur_vclr_flag & vclr_flag_esds) != 0){
    		// ESDS
    		if ((cur_acb_macrf & acb_macrf_seq) != 0
    			&& (cur_acb_macrf & acb_macrf_out) != 0){
    			acb_seq_out = true;
    		} else { 
    			acb_seq_out = false;
    		}
    	} else if ((cur_vclr_flag & vclr_flag_rrds) != 0){
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
    private boolean open_acb_dcbs(){
    	/*
    	 * 1.  dynamically allocate and open
    	 *     dcbs required for VES, VX0, and
    	 *     any upgrade VXN's.
    	 */
    	int save_open_flags = pz390.reg.getInt(pz390.r0);
    	if ((cur_acb_oflgs & acb_oflgs_aixu) != 0){
    		cur_acb_dcbt = pz390.mem.getInt(cur_acb_addr + acb_dcbt);
    	} else if ((cur_vclr_flag & vclr_flag_lds) != 0){
    		cur_acb_dcbt = 1; // open VES and VX0 with no AIX upgrades
    	} else {
    		cur_acb_dcbt = 2;
    	}
    	pz390.mem.putInt(cur_acb_addr + acb_dcbt,cur_acb_dcbt);
    	pz390.reg.putInt(pz390.r0,0);  // force 24 bit 
    	pz390.reg.putInt(pz390.r1,cur_acb_dcbt * sz390.dcb_len);
    	sz390.svc_getmain();
    	if (pz390.reg.getInt(pz390.r15) != 0){
    		pz390.set_psw_check(pz390.psw_pic_gm_err);
    	}
    	pz390.reg.putInt(pz390.r0,save_open_flags); // restore open options
    	cur_acb_dcba = pz390.reg.getInt(pz390.r1);
    	pz390.mem.putInt(cur_acb_addr + acb_dcba,cur_acb_dcba);
    	cur_vclr_vesa = pz390.mem.getInt(cur_acb_vclra + vclr_vesa);
    	init_acb_dcb(cur_acb_dcba,cur_vclr_lrec,cur_vclr_vesa);
    	if (!open_acb_dcb(cur_acb_dcba)){
    		return false;
    	}
    	if (cur_acb_dcbt > 1){
    		cur_vclr_vx0a = pz390.mem.getInt(cur_acb_vclra + vclr_vx0a);
        	init_acb_dcb(cur_acb_dcba + sz390.dcb_len,8+cur_vclr_klen,cur_vclr_vx0a);
    		if (!open_acb_dcb(cur_acb_dcba + sz390.dcb_len)){
    			return false;
    		}
    		cur_acb_dcba = cur_acb_dcba + 2 * sz390.dcb_len;
    		int index = 2;
    		while (index < cur_acb_dcbt){
    			// open aix upgrade dcb's
    			cur_vaix_addr = pz390.mem.getInt(cur_acb_vaixa);
    			cur_vaix_vxna = pz390.mem.getInt(cur_vaix_addr + vaix_vxna);
    			cur_vaix_klen = pz390.mem.getInt(cur_vaix_addr + vaix_klen);
    			init_acb_dcb(cur_acb_dcba,8+cur_vclr_klen,cur_vaix_vxna);
    			if (!open_acb_dcb(cur_acb_dcba)){
    				return false;
    			}
    			cur_acb_dcba  = cur_acb_dcba + sz390.dcb_len;
    			cur_acb_vaixa = cur_acb_vaixa + 4;
    			index++;
    		}
    	}
    	return true;
    }
    private void init_acb_dcb(int dcb_addr,int dcb_lrecl_f,int dcb_dsname){
    	/*
    	 * copy model dcb from vcdt_dcba to new
    	 * dynamcially allocated dcb address 
    	 * and set  DCBLRECLF and DCBDSNAM fields
    	 */
    	System.arraycopy(pz390.mem_byte, cur_vcdt_dcba, pz390.mem_byte, dcb_addr, sz390.dcb_len);
    	pz390.mem.putInt(dcb_addr + sz390.dcb_lrecl_f,dcb_lrecl_f);
        pz390.mem.putInt(dcb_addr + sz390.dcb_dsnam,dcb_dsname);
    }
    private boolean open_acb_dcb(int dcb_addr){
    	/*
    	 * dynamically allocate and open
    	 *  DCB for VES, VX0, VXN's
    	 */
        // use same flags in r0 for open acb and dcb's
    	sz390.cur_dcb_addr = dcb_addr;
    	get_vcdt_path();
    	sz390.svc_open_dcb(cur_vcdt_path);
    	if (pz390.reg.getInt(pz390.r15) == 0){
    		if (acb_seq_out){
    			reuse_file(dcb_addr);
    		}
    		return true;
    	} else {
    		return false;
    	}
    }
    private boolean close_acb_dcbs(){
    	/*
    	 * close dynamically allocated ACB DCB's
    	 */    	
    	if ((cur_acb_oflgs & acb_oflgs_aixu) != 0){
    		cur_acb_dcbt = pz390.mem.getInt(cur_acb_addr + acb_dcbt);
    	} else if ((cur_vclr_flag & vclr_flag_lds) != 0){
    		cur_acb_dcbt = 1; // open VES and VX0 with no AIX upgrades
    	} else {
    		cur_acb_dcbt = 2;
    	}
    	int index = 0;
    	while (index < cur_acb_dcbt){
        	sz390.cur_dcb_addr = cur_acb_dcba;
    		sz390.svc_close_dcb();
        	if (pz390.reg.getInt(pz390.r15) != 0){
        		return false;
        	}
        	cur_acb_dcba = cur_acb_dcba + sz390.dcb_len;
    	    index++;
    	}
    	pz390.reg.putInt(pz390.r0,cur_acb_dcbt * sz390.dcb_len);
    	cur_acb_dcba = pz390.mem.getInt(cur_acb_addr + acb_dcba);
    	pz390.reg.putInt(pz390.r1,cur_acb_dcba);
    	sz390.svc_freemain();
    	if (pz390.reg.getInt(pz390.r15) != 0){
    		pz390.set_psw_check(pz390.psw_pic_gm_err);
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
       	if ((cur_vclr_flag & vclr_flag_esds) != 0){
       		// ESDS get
       		if ((cur_acb_macrf & acb_macrf_seq) == acb_macrf_seq){
       		    rpl_get_esds_seq();
       		} else if ((cur_acb_macrf & acb_macrf_adr) == acb_macrf_adr){
       			rpl_get_esds_adr();
       		}
       	} else if ((cur_vclr_flag & vclr_flag_rrds) != 0){
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
    	cur_vx0_tiot_index = pz390.mem.getInt(cur_vx0_dcba + sz390.dcb_iobad)-1;
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
    	cur_ves_tiot_index = pz390.mem.getInt(cur_ves_dcba + sz390.dcb_iobad)-1;
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
    	cur_vx0_tiot_index = pz390.mem.getInt(cur_vx0_dcba + sz390.dcb_iobad)-1;
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
    	cur_ves_tiot_index = pz390.mem.getInt(cur_ves_dcba + sz390.dcb_iobad)-1;
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
    	cur_ves_tiot_index = pz390.mem.getInt(cur_ves_dcba + sz390.dcb_iobad)-1;
		cur_vx0_tiot_index = pz390.mem.getInt(cur_vx0_dcba + sz390.dcb_iobad)-1;
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
        fetch_vclr_fields();
    	if ((cur_acb_oflgs & acb_oflgs_open) == 0){
    		set_feedback(pdf_def,rc_log,cmp_base,rn_acb_not_open);
    		return;
    	}
    	if ((cur_acb_oflgs & acb_oflgs_out) == 0
    		|| (cur_acb_macrf & acb_macrf_out) == 0){
    		set_feedback(pdf_def,rc_log,cmp_base,rn_inv_rpl_opt);
    		return;
    	}
    	if ((cur_vclr_flag & vclr_flag_esds) != 0){
    		// ESDS out - add rec to VES
            rpl_put_esds();
    	} else if ((cur_vclr_flag & vclr_flag_rrds) != 0){
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
		cur_ves_tiot_index = pz390.mem.getInt(cur_ves_dcba + sz390.dcb_iobad)-1;
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
		cur_vx0_tiot_index = pz390.mem.getInt(cur_vx0_dcba + sz390.dcb_iobad)-1;
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
    	cur_ves_tiot_index = pz390.mem.getInt(cur_ves_dcba + sz390.dcb_iobad)-1;
		cur_vx0_tiot_index = pz390.mem.getInt(cur_vx0_dcba + sz390.dcb_iobad)-1;
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
			cur_vx0_tiot_index = pz390.mem.getInt(cur_vx0_dcba + sz390.dcb_iobad)-1;
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
    	if ((cur_vclr_flag & vclr_flag_vrec) != 0){
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
    		cur_rpl_lrec = cur_vclr_lrec;
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
    	if ((cur_vclr_flag & vclr_flag_vrec) != 0){
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
    		cur_rpl_lrec = cur_vclr_lrec;
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