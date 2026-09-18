/* -------------------------------------------------------------------------- *\
 |                                                                            |
 |  Definition of a Control Block                                             |
 |                                                                            |
 |  Intended to be used for Control Block objects that have a counterpart     |
 |           in z390 memory - hence can be accessed by user progams           |
 |                            running in the emulator environment             |
 |  The Control Block can also be used for any other Control Block            |
 |           as long as it is backed by a byte array                          |
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
 * 2021-04    Created by Abe / Hugh
 * 
 ****************************************************/

/**
 * Holds a single Control Block in z390 memory
 * 
 * 
 * @author  Hugh Sweeney / Abe Kornelis
 * 
 */
public class zControlBlock
   {private byte[]     mem_array; // byte array holding the Control Block
    private int        cb_offset; // offset / address of Control Block in the byte array
    private int        cb_length; // length of the Control Block in bytes




    /**
     *
     * Constructor to create zControlBlock object
     *
     * Parameters: byte[]     array representing z390 memory
     *             int        offset of ControlBlock in the array (address of CB in z390 memory)
     *             int        length of ControlBlock in the array
     *
     */
    public zControlBlock(byte[] parm_array, int parm_offset, int parm_length)
       {// Validate input parms
        if (parm_offset < 0)
            throw new zException("zControlBlock: negative offset not supported", 8, 8);
        if (parm_length < 4)
            throw new zException("zControlBlock: length must be at least 4", 8, 8);
        
        // Save input parms
        mem_array = parm_array;
        cb_offset = parm_offset;
        cb_length = parm_length;
        }




    /**
     *  Get a one-bit flag field from the Control Block
     *
     *  Parameters:  int        offset of desired byte within the Control Block
     *               byte       flag mask
     *
     *  @return      boolean value extracted from the Control Block
     */
    public boolean get_boolean(int parm_offset, byte parm_mask)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.get_boolean: negative offset not supported", 8, 8);
        if (parm_offset > cb_length)
            throw new zException("zControlBlock.get_boolean: access beyond end of CB not allowed", 8, 8);
        return ((mem_array[cb_offset + parm_offset] & parm_mask) == parm_mask) ? true : false;
        }




    /**
     *  Set a one-bit flag field in the Control Block
     *
     *  Parameters:  int        offset of desired byte within the Control Block
     *               byte       flag mask
     *               boolean    value to be set
     *
     *  @return      void
     */
    public  void set_boolean(int parm_offset, byte parm_mask, boolean parm_value)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.set_boolean: negative offset not supported", 8, 8);
        if (parm_offset > cb_length)
            throw new zException("zControlBlock.set_boolean: access beyond end of CB not allowed", 8, 8);
        if (parm_value)
            mem_array[cb_offset + parm_offset] = (byte) (mem_array[cb_offset + parm_offset] | parm_mask);
        else
            mem_array[cb_offset + parm_offset] = (byte) (mem_array[cb_offset + parm_offset] & ~parm_mask);
        return;
        }




    /**
     *  Get a one-byte field from the Control Block
     *
     *  Parameters:  int        offset of desired byte within the Control Block
     *
     *  @return      byte value extracted from the Control Block
     */
    public byte get_byte(int parm_offset)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.get_byte: negative offset not supported", 8, 8);
        if (parm_offset > cb_length)
            throw new zException("zControlBlock.get_byte: access beyond end of CB not allowed", 8, 8);
        return mem_array[cb_offset + parm_offset];
        }




    /**
     *  Set a one-byte field in the Control Block
     *
     *  Parameters:  int        offset of desired byte within the Control Block
     *               byte       value to be set
     *
     *  @return      void
     */
    public void set_byte(int parm_offset, byte parm_value)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.set_byte: negative offset not supported", 8, 8);
        if (parm_offset > cb_length)
            throw new zException("zControlBlock.set_byte: access beyond end of CB not allowed", 8, 8);
        mem_array[cb_offset + parm_offset] = parm_value;
        return;
        }




    /**
     *  Get a two-byte field from the Control Block
     *
     *  Parameters:  int        offset of desired short within the Control Block
     *
     *  @return      short value extracted from the Control Block
     */
    public short get_short(int parm_offset)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.get_short: negative offset not supported", 8, 8);
        if (parm_offset + 1 > cb_length)
            throw new zException("zControlBlock.get_short: access beyond end of CB not allowed", 8, 8);
        int return_value = (int) mem_array[cb_offset + parm_offset] & 0xff;
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        return (short) return_value;
        }




    /**
     *  Set a two-byte field in the Control Block
     *
     *  Parameters:  int        offset of desired short within the Control Block
     *               short      value to be set
     *
     *  @return      void
     */
    public void set_short(int parm_offset, short parm_value)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.set_short: negative offset not supported", 8, 8);
        if (parm_offset + 1 > cb_length)
            throw new zException("zControlBlock.set_short: access beyond end of CB not allowed", 8, 8);
        mem_array[cb_offset + parm_offset + 1] = (byte) parm_value;
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 8);
        return;
        }




    /**
     *  Get a three-byte field from the Control Block
     *
     *  Parameters:  int        offset of desired field within the Control Block
     *
     *  @return      24-bit int value extracted from the Control Block
     */
    public int get_int24(int parm_offset)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.get_int24: negative offset not supported", 8, 8);
        if (parm_offset + 2 > cb_length)
            throw new zException("zControlBlock.get_int24: access beyond end of CB not allowed", 8, 8);
        int return_value = (int) mem_array[cb_offset + parm_offset] & 0xff;
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 2] & 0xff);
        return return_value;
        }




    /**
     *  Set a three-byte field in the Control Block
     *
     *  Parameters:  int        offset of desired field within the Control Block
     *               int        value to be set
     *
     *  @return      void
     */
    public void set_int24(int parm_offset, int parm_value)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.set_int24: negative offset not supported", 8, 8);
        if (parm_offset + 2 > cb_length)
            throw new zException("zControlBlock.set_int24: access beyond end of CB not allowed", 8, 8);
        mem_array[cb_offset + parm_offset + 2] = (byte) parm_value;
        mem_array[cb_offset + parm_offset + 1] = (byte) (parm_value >> 8);
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 16);
        return;
        }




    /**
     *  Get a four-byte field from the Control Block
     *
     *  Parameters:  int        offset of desired int within the Control Block
     *
     *  @return      int value extracted from the Control Block
     */
    public int get_int(int parm_offset)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.get_int: negative offset not supported", 8, 8);
        if (parm_offset + 3 > cb_length)
            throw new zException("zControlBlock.get_int: access beyond end of CB not allowed", 8, 8);
        int return_value = (int) mem_array[cb_offset + parm_offset] & 0xff;
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 2] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 3] & 0xff);
        return return_value;
        }




    /**
     *  Set a four-byte field in the Control Block
     *
     *  Parameters:  int        offset of desired int within the Control Block
     *               int        value to be set
     *
     *  @return      void
     */
    public void set_int(int parm_offset, int parm_value)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.set_int: negative offset not supported", 8, 8);
        if (parm_offset + 3 > cb_length)
            throw new zException("zControlBlock.set_int: access beyond end of CB not allowed", 8, 8);
        mem_array[cb_offset + parm_offset + 3] = (byte) parm_value;
        mem_array[cb_offset + parm_offset + 2] = (byte) (parm_value >> 8);
        mem_array[cb_offset + parm_offset + 1] = (byte) (parm_value >> 16);
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 24);
        return;
        }




    /**
     *  Get an eight-byte field from the Control Block
     *
     *  Parameters:  int        offset of desired int within the Control Block
     *
     *  @return      int value extracted from the Control Block
     */
    public long get_long(int parm_offset)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.get_long: negative offset not supported", 8, 8);
        if (parm_offset + 7 > cb_length)
            throw new zException("zControlBlock.get_long: access beyond end of CB not allowed", 8, 8);
        long return_value = (long) mem_array[cb_offset + parm_offset] & 0xff;
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 2] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 3] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 4] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 5] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 6] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 7] & 0xff);
        return return_value;
        }




    /**
     *  Set an eight-byte field in the Control Block
     *
     *  Parameters:  int        offset of desired int within the Control Block
     *               long       value to be set
     *
     *  @return      void
     */
    public void set_long(int parm_offset, long parm_value)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.set_long: negative offset not supported", 8, 8);
        if (parm_offset + 7 > cb_length)
            throw new zException("zControlBlock.set_long: access beyond end of CB not allowed", 8, 8);
        mem_array[cb_offset + parm_offset + 7] = (byte)  parm_value;
        mem_array[cb_offset + parm_offset + 6] = (byte) (parm_value >> 8);
        mem_array[cb_offset + parm_offset + 5] = (byte) (parm_value >> 16);
        mem_array[cb_offset + parm_offset + 4] = (byte) (parm_value >> 24);
        mem_array[cb_offset + parm_offset + 3] = (byte) (parm_value >> 32);
        mem_array[cb_offset + parm_offset + 2] = (byte) (parm_value >> 40);
        mem_array[cb_offset + parm_offset + 1] = (byte) (parm_value >> 48);
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 56);
        return;
        }




    /**
     *  Get a halfword-prefixed String from the Control Block
     *
     *  Parameters:  int        offset of desired string within the Control Block
     *
     *  @return      String value extracted from the Control Block
     */
    public String get_hw_string(int parm_offset)
       {if (parm_offset < 0)
            throw new zException("zControlBlock.get_hw_string: negative offset not supported", 8, 8);
        if (parm_offset + 1 > cb_length)
            throw new zException("zControlBlock.get_hw_string: access beyond end of CB not allowed", 8, 8);
        
        int string_len = (int) mem_array[cb_offset + parm_offset] & 0xff;
        string_len = string_len << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        if (string_len < 0)
            throw new zException("zControlBlock.get_hw_string: negative string length not supported", 8, 8);
        if (parm_offset + 2 + string_len > cb_length)
            throw new zException("zControlBlock.get_hw_string: string location beyond end of CB not allowed", 8, 8);
        
        byte[] return_array = new byte[string_len];
        System.arraycopy(mem_array, parm_offset + 2, return_array, 0, string_len);
        String return_value = new String(return_array);
        return return_value;
        }




    /**
     *  Get array pointer
     *
     *  Parameters:  none
     *
     *  @return      byte[]
     */
    public byte[] array()
       {return mem_array;
        }




    /**
     *  Get offset
     *
     *  Parameters:  none
     *
     *  @return      int
     */
    public int get_offset()
       {return cb_offset;
        }




    /**
     *  Get length
     *
     *  Parameters:  none
     *
     *  @return      int
     */
    public int get_length()
       {return cb_length;
        }
    }

