/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Prefix Block - Prefix area                                                |
 |                                                                            |
 |  The PFXPFX resides in the Prefix Block vzPFX                              |
 |      and contains control and verificatin data for the cluster and file    |
 |                                                                            |
\* -------------------------------------------------------------------------- */

/* ***************************************************
 * 
 * z390 portable mainframe assembler and emulator.
 * 
 * Copyright 2011 Automated Software Tools Corporation
 * 
 * z390 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * z390 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with z390; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 * zACB is part of the zVSAM v2 component of z390.
 * It is the java implementation of the ACB with its associated operations
 * and thus represents a single cluster.
 *
 ****************************************************
 * Maintenance
 ****************************************************
 * 
 * 2021-04    Initially created by Abe
 * 
 ****************************************************/

/**
 * Defines the Prefix data area of a Prefix Block
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class vzPFXPFX extends zControlBlock
   {private byte[] pfxeye  ;   // eyecatcher
    private int    pfxrclen;   // record length, max length if variable
    private int    pfxkylen;   // key length
    private int    pfxkyoff;   // key offset, excluding SDW/RLF
    private short  pfxdvol_at; // offset to the data volume label(n/a)
    private short  pfxdnam_at; // offset to the data filename
    private short  pfxdpat_at; // offset to the data pathname
    private short  pfxxvol_at; // offset to the index volume label(n/a)
    private short  pfxxnam_at; // offset to the index filename
    private short  pfxxpat_at; // offset to the index pathname
    private byte   pfxixlvl;   // no. of index levels
    private byte   pfxalthr;   // allocation redrive threshold
    private int    pfxblksz;   // actual Blocksize used for all Blocks except Prefix Block
    private long   pfxhxlra;   // XLRA of highest allocated block
    private long   pfxbmap ;   // XLRA of first spacemap block
    private long   pfxemap ;   // XLRA of last spacemap block
    private long   pfxmapnw;   // XLRA of spacemap block last used for allocation
    private long   pfxbdata;   // XLRA of first data block
    private long   pfxedata;   // XLRA of last data block
    private long   pfxbsegm;   // XLRA of first Segment block
    private long   pfxesegm;   // XLRA of last Segment block
    private long   pfxroot ;   // XLRA of root index block
    private long   pfxblvl0;   // XLRA of Header Block index level 0
    private long   pfxelvl0;   // XLRA of End Block index level 0
    private long   pfxblvl1;   // XLRA of Header Block index level 1
    private long   pfxelvl1;   // XLRA of End Block index level 1
    private long   pfxblvl2;   // XLRA of Header Block index level 2
    private long   pfxelvl2;   // XLRA of End Block index level 2
    private long   pfxblvl3;   // XLRA of Header Block index level 3
    private long   pfxelvl3;   // XLRA of End Block index level 3
    private long   pfxblvl4;   // XLRA of Header Block index level 4
    private long   pfxelvl4;   // XLRA of End Block index level 4
    private long   pfxblvl5;   // XLRA of Header Block index level 5
    private long   pfxelvl5;   // XLRA of End Block index level 5
    private long   pfxblvl6;   // XLRA of Header Block index level 6
    private long   pfxelvl6;   // XLRA of End Block index level 6
    private long   pfxblvl7;   // XLRA of Header Block index level 7
    private long   pfxelvl7;   // XLRA of End Block index level 7
    private long   pfxblvl8;   // XLRA of Header Block index level 8
    private long   pfxelvl8;   // XLRA of End Block index level 8
    private long   pfxblvl9;   // XLRA of Header Block index level 9
    private long   pfxelvl9;   // XLRA of End Block index level 9
    private long   pfxblvla;   // XLRA of Header Block index level 10
    private long   pfxelvla;   // XLRA of End Block index level 10
    private long   pfxblvlb;   // XLRA of Header Block index level 11
    private long   pfxelvlb;   // XLRA of End Block index level 11
    private long   pfxblvlc;   // XLRA of Header Block index level 12
    private long   pfxelvlc;   // XLRA of End Block index level 12
    private long   pfxblvld;   // XLRA of Header Block index level 13
    private long   pfxelvld;   // XLRA of End Block index level 13
    private long   pfxblvle;   // XLRA of Header Block index level 14
    private long   pfxelvle;   // XLRA of End Block index level 14
    private long   pfxblvlf;   // XLRA of Header Block index level 15
    private long   pfxelvlf;   // XLRA of End Block index level 15
    private int    pfxmapof;   // offset within spacemap block to last used byte for allocation
    private long   pfxdtskc;   // STCK of data creation
    private long   pfxixskc;   // STCK of index creation
    private long   pfxdtsku;   // STCK of last data update
    private long   pfxixsku;   // STCK of last index update
    private long   pfxmapdt;   // STCK of last allocation action
    private byte   pfxfrspc;   // initial freespace % within block
    private short  pfxfrblk;   // initial freespace blocks
    private short  pfxfrint;   // Initial freespace interval between free blocks
    private byte   pfxfflgs;   // file flags
    private byte   pfxrflgs;   // record flags
    private byte   pfxaixn ;   // no. of AIX's on the upgrade set
    
    private String pfxdvol ;   // data volume label(n/a)
    private String pfxdnam ;   // data filename
    private String pfxdpat ;   // data pathname
    private String pfxxvol ;   // index volume label(n/a)
    private String pfxxnam ;   // index filename
    private String pfxxpat ;   // index pathname
    
    /* 
     * Offsets of fields within the Prefix data block
     * 
     */
    private static final short PFXEYE      = 0x0000;
    private static final short PFXEYE_LEN  = 0x0004;
    private static final short PFXRCLEN    = 0x0004;
    private static final short PFXKYLEN    = 0x0008;
    private static final short PFXKYOFF    = 0x000C;
    private static final short PFXDVOL_AT  = 0x0010;
    private static final short PFXDNAM_AT  = 0x0012;
    private static final short PFXDPAT_AT  = 0x0014;
    private static final short PFXXVOL_AT  = 0x0016;
    private static final short PFXXNAM_AT  = 0x0018;
    private static final short PFXXPAT_AT  = 0x001A;
    private static final short PFXIXLVL    = 0x001C;
    private static final short PFXALTHR    = 0x001D;
    private static final short PFXBLKSZ    = 0x001E;
    private static final short PFXHXLRA    = 0x0022;
    private static final short PFXBMAP     = 0x002A;
    private static final short PFXEMAP     = 0x0032;
    private static final short PFXMAPNW    = 0x003A;
    private static final short PFXBDATA    = 0x0042;
    private static final short PFXEDATA    = 0x004A;
    private static final short PFXBSEGM    = 0x0052;
    private static final short PFXESEGM    = 0x005A;
    private static final short PFXROOT     = 0x0062;
    private static final short PFXBLVL0    = 0x006A;
    private static final short PFXELVL0    = 0x0072;
    private static final short PFXBLVL1    = 0x007A;
    private static final short PFXELVL1    = 0x0082;
    private static final short PFXBLVL2    = 0x008A;
    private static final short PFXELVL2    = 0x0092;
    private static final short PFXBLVL3    = 0x009A;
    private static final short PFXELVL3    = 0x00A2;
    private static final short PFXBLVL4    = 0x00AA;
    private static final short PFXELVL4    = 0x00B2;
    private static final short PFXBLVL5    = 0x00BA;
    private static final short PFXELVL5    = 0x00C2;
    private static final short PFXBLVL6    = 0x00CA;
    private static final short PFXELVL6    = 0x00D2;
    private static final short PFXBLVL7    = 0x00DA;
    private static final short PFXELVL7    = 0x00E2;
    private static final short PFXBLVL8    = 0x00EA;
    private static final short PFXELVL8    = 0x00F2;
    private static final short PFXBLVL9    = 0x00FA;
    private static final short PFXELVL9    = 0x0102;
    private static final short PFXBLVLA    = 0x010A;
    private static final short PFXELVLA    = 0x0112;
    private static final short PFXBLVLB    = 0x011A;
    private static final short PFXELVLB    = 0x0122;
    private static final short PFXBLVLC    = 0x012A;
    private static final short PFXELVLC    = 0x0132;
    private static final short PFXBLVLD    = 0x013A;
    private static final short PFXELVLD    = 0x0142;
    private static final short PFXBLVLE    = 0x014A;
    private static final short PFXELVLE    = 0x0152;
    private static final short PFXBLVLF    = 0x015A;
    private static final short PFXELVLF    = 0x0162;
    private static final short PFXMAPOF    = 0x016A;
    private static final short PFXDTSKC    = 0x0170;
    private static final short PFXIXSKC    = 0x0178;
    private static final short PFXDTSKU    = 0x0180;
    private static final short PFXIXSKU    = 0x0188;
    private static final short PFXMAPDT    = 0x0190;
    private static final short PFXFRSPC    = 0x0198;
    private static final short PFXFRBLK    = 0x0199;
    private static final short PFXFRINT    = 0x019B;
    private static final short PFXFFLGS    = 0x019D;
    private static final short PFXRFLGS    = 0x019E;
    private static final short PFXAIXN     = 0x019F;
    public  static final short PFXLENG     = 0x01A0;
    
    /* 
     * Values of constants
     * 
     */
    private static final String       EyeCatcher       = "zPFX";
    private static final ebcdicStryng ebcdicEyeCatcher = new ebcdicStryng(EyeCatcher);
    private static final byte         PFX_ESDS         = (byte) 0x80; // ESDS
    private static final byte         PFX_KSDS         = (byte) 0x40; // KSDS
    private static final byte         PFX_RRDS         = (byte) 0x20; // RRDS
    private static final byte         PFX_LDS          = (byte) 0x10; // LDS
    private static final byte         PFX_AIX          = (byte) 0x08; // AIX
    private static final byte         PFX_INDX         = (byte) 0x01; // index component
    private static final byte         PFX_RFIX         = (byte) 0x80; // 1=fixed       0=variable
    private static final byte         PFX_RSPN         = (byte) 0x40; // 1=spanned     0=non-spanned
    private static final byte         PFX_KUNQ         = (byte) 0x20; // 1=AIX unique  0=AIX non-unique
    private static final byte         PFX_AIXT         = (byte) 0x10; // 1=AIX on KSDS 0=AIX on ESDS




    /**
     *
     * Constructor to create vzPFXPFX object from an existing prefix Block
     *
     * Parameters: byte[]     holding a Prefix Block
     *             int        holding the offset of the Prefix data area in the Block
     *             int        specifying the size of the underlying byte array
     *
     */
    public vzPFXPFX(byte[] parm_memBuff, int parm_addr, int parm_size)
       {super(parm_memBuff, parm_addr, PFXLENG);
        
        // Ensure the input is a valid Prefix data area
        if (parm_addr != vzBHDR.BHDRLENG)
           throw new zException("Prefix data area must always follow the Block Header", 8, 8);
        if (parm_addr + PFXLENG - 1 > parm_size)
           throw new zException("Prefix data area is too small", 8, 8);        
        
        // Allocate dependent fields and refresh all object data
        pfxeye  = new byte[PFXEYE_LEN];
        pfxdvol = new String();
        pfxdnam = new String();
        pfxdpat = new String();
        pfxxvol = new String();
        pfxxnam = new String();
        pfxxpat = new String();
        refresh();
        }




    /**
     *
     * Refresh Prefix data object from underlying array data
     *
     * Parameters: none
     *
     */
    public void refresh()
       {// Retrieve all Prefix data fields from the input buffer
        System.arraycopy(this.array(), this.get_offset() + PFXEYE, pfxeye, 0, PFXEYE_LEN);
        pfxrclen     = get_int   (PFXRCLEN);
        pfxkylen     = get_int   (PFXKYLEN);
        pfxkyoff     = get_int   (PFXKYOFF);
        pfxdvol_at   = get_short (PFXDVOL_AT);
        pfxdnam_at   = get_short (PFXDNAM_AT);
        pfxdpat_at   = get_short (PFXDPAT_AT);
        pfxxvol_at   = get_short (PFXXVOL_AT);
        pfxxnam_at   = get_short (PFXXNAM_AT);
        pfxxpat_at   = get_short (PFXXPAT_AT);
        pfxixlvl     = get_byte  (PFXIXLVL);
        pfxalthr     = get_byte  (PFXALTHR);
        pfxblksz     = get_int   (PFXBLKSZ);
        pfxhxlra     = get_long  (PFXHXLRA);
        pfxbmap      = get_long  (PFXBMAP );
        pfxemap      = get_long  (PFXEMAP );
        pfxmapnw     = get_long  (PFXMAPNW);
        pfxbdata     = get_long  (PFXBDATA);
        pfxedata     = get_long  (PFXEDATA);
        pfxbsegm     = get_long  (PFXBSEGM);
        pfxesegm     = get_long  (PFXESEGM);
        pfxroot      = get_long  (PFXROOT );
        pfxblvl0     = get_long  (PFXBLVL0);
        pfxelvl0     = get_long  (PFXELVL0);
        pfxblvl1     = get_long  (PFXBLVL1);
        pfxelvl1     = get_long  (PFXELVL1);
        pfxblvl2     = get_long  (PFXBLVL2);
        pfxelvl2     = get_long  (PFXELVL2);
        pfxblvl3     = get_long  (PFXBLVL3);
        pfxelvl3     = get_long  (PFXELVL3);
        pfxblvl4     = get_long  (PFXBLVL4);
        pfxelvl4     = get_long  (PFXELVL4);
        pfxblvl5     = get_long  (PFXBLVL5);
        pfxelvl5     = get_long  (PFXELVL5);
        pfxblvl6     = get_long  (PFXBLVL6);
        pfxelvl6     = get_long  (PFXELVL6);
        pfxblvl7     = get_long  (PFXBLVL7);
        pfxelvl7     = get_long  (PFXELVL7);
        pfxblvl8     = get_long  (PFXBLVL8);
        pfxelvl8     = get_long  (PFXELVL8);
        pfxblvl9     = get_long  (PFXBLVL9);
        pfxelvl9     = get_long  (PFXELVL9);
        pfxblvla     = get_long  (PFXBLVLA);
        pfxelvla     = get_long  (PFXELVLA);
        pfxblvlb     = get_long  (PFXBLVLB);
        pfxelvlb     = get_long  (PFXELVLB);
        pfxblvlc     = get_long  (PFXBLVLC);
        pfxelvlc     = get_long  (PFXELVLC);
        pfxblvld     = get_long  (PFXBLVLD);
        pfxelvld     = get_long  (PFXELVLD);
        pfxblvle     = get_long  (PFXBLVLE);
        pfxelvle     = get_long  (PFXELVLE);
        pfxblvlf     = get_long  (PFXBLVLF);
        pfxelvlf     = get_long  (PFXELVLF);
        pfxmapof     = get_int   (PFXMAPOF);
        pfxdtskc     = get_long  (PFXDTSKC);
        pfxixskc     = get_long  (PFXIXSKC);
        pfxdtsku     = get_long  (PFXDTSKU);
        pfxixsku     = get_long  (PFXIXSKU);
        pfxmapdt     = get_long  (PFXMAPDT);
        pfxfrspc     = get_byte  (PFXFRSPC);
        pfxfrblk     = get_short (PFXFRBLK);
        pfxfrint     = get_short (PFXFRINT);
        pfxfflgs     = get_byte  (PFXFFLGS);
        pfxrflgs     = get_byte  (PFXRFLGS);
        pfxaixn      = get_byte  (PFXAIXN );
        
        pfxdvol      = get_hw_string((int) pfxdvol_at);
        pfxdnam      = get_hw_string((int) pfxdnam_at);
        pfxdpat      = get_hw_string((int) pfxdpat_at);
        pfxxvol      = get_hw_string((int) pfxxvol_at);
        pfxxnam      = get_hw_string((int) pfxxnam_at);
        pfxxpat      = get_hw_string((int) pfxxpat_at);
        
        // Additional validations
        if (! ebcdicEyeCatcher.equals(pfxeye) )
           throw new zException("BFTR eyecatcher is incorrect", 8, 8);
        return;
        }
    }
