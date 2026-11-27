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
 ****************************************************
 * Maintenance
 ****************************************************
 *
 * 2021-04    Created by Abe / Hugh
 * 2026-10-27 Lifted from #908 for catalog support, reformatted for the linter,
 *            get_hw_string reads from the CB offset, string getters added
 *
 ****************************************************/



/**
 * Holds a single Control Block in z390 memory
 *
 * @author  Hugh Sweeney / Abe Kornelis
 */
public class zControlBlock {
    /** byte array holding the Control Block                  */ private byte[] mem_array;
    /** offset / address of Control Block in the byte array   */ private int    cb_offset;
    /** length of the Control Block in bytes                  */ private int    cb_length;



    /**
     * Constructor to create zControlBlock object
     *
     * @param parm_array  array representing z390 memory
     * @param parm_offset offset of ControlBlock in the array (address of CB in z390 memory)
     * @param parm_length length of ControlBlock in the array
     */
    public zControlBlock(byte[] parm_array, int parm_offset, int parm_length) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock: negative offset not supported", 8, 8);
        }
        if (parm_length < 4) {
            throw new zException("zControlBlock: length must be at least 4", 8, 8);
        }
        mem_array = parm_array;
        cb_offset = parm_offset;
        cb_length = parm_length;
    }



    /**
     * Get a bit or set of bits from the Control Block
     *
     * @param parm_offset offset of the flag byte within the Control Block
     * @param parm_mask   bit mask to test
     * @return true when all bits in the mask are on
     */
    public boolean get_boolean(int parm_offset, byte parm_mask) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_boolean: negative offset not supported", 8, 8);
        }
        if (parm_offset > cb_length) {
            throw new zException("zControlBlock.get_boolean: access beyond end of CB not allowed", 8, 8);
        }
        return (mem_array[cb_offset + parm_offset] & parm_mask) == parm_mask;
    }



    /**
     * Set a bit or set of bits in the Control Block
     *
     * @param parm_offset offset of the flag byte within the Control Block
     * @param parm_mask   bit mask to set or reset
     * @param parm_value  true to set, false to reset
     */
    public void set_boolean(int parm_offset, byte parm_mask, boolean parm_value) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.set_boolean: negative offset not supported", 8, 8);
        }
        if (parm_offset > cb_length) {
            throw new zException("zControlBlock.set_boolean: access beyond end of CB not allowed", 8, 8);
        }
        if (parm_value) {
            mem_array[cb_offset + parm_offset] = (byte) (mem_array[cb_offset + parm_offset] | parm_mask);
        } else {
            mem_array[cb_offset + parm_offset] = (byte) (mem_array[cb_offset + parm_offset] & ~parm_mask);
        }
    }



    /**
     * Get a byte from the Control Block
     *
     * @param parm_offset offset of the byte within the Control Block
     * @return byte value
     */
    public byte get_byte(int parm_offset) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_byte: negative offset not supported", 8, 8);
        }
        if (parm_offset > cb_length) {
            throw new zException("zControlBlock.get_byte: access beyond end of CB not allowed", 8, 8);
        }
        return mem_array[cb_offset + parm_offset];
    }



    /**
     * Set a byte in the Control Block
     *
     * @param parm_offset offset of the byte within the Control Block
     * @param parm_value  value to be set
     */
    public void set_byte(int parm_offset, byte parm_value) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.set_byte: negative offset not supported", 8, 8);
        }
        if (parm_offset > cb_length) {
            throw new zException("zControlBlock.set_byte: access beyond end of CB not allowed", 8, 8);
        }
        mem_array[cb_offset + parm_offset] = parm_value;
    }



    /**
     * Get a two-byte field from the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @return short value
     */
    public short get_short(int parm_offset) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_short: negative offset not supported", 8, 8);
        }
        if (parm_offset + 1 > cb_length) {
            throw new zException("zControlBlock.get_short: access beyond end of CB not allowed", 8, 8);
        }
        int return_value = (int) mem_array[cb_offset + parm_offset] & 0xff;
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        return (short) return_value;
    }



    /**
     * Set a two-byte field in the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @param parm_value  value to be set
     */
    public void set_short(int parm_offset, short parm_value) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.set_short: negative offset not supported", 8, 8);
        }
        if (parm_offset + 1 > cb_length) {
            throw new zException("zControlBlock.set_short: access beyond end of CB not allowed", 8, 8);
        }
        mem_array[cb_offset + parm_offset + 1] = (byte) parm_value;
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 8);
    }



    /**
     * Get a three-byte field from the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @return int value
     */
    public int get_int24(int parm_offset) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_int24: negative offset not supported", 8, 8);
        }
        if (parm_offset + 2 > cb_length) {
            throw new zException("zControlBlock.get_int24: access beyond end of CB not allowed", 8, 8);
        }
        int return_value = (int) mem_array[cb_offset + parm_offset] & 0xff;
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 2] & 0xff);
        return return_value;
    }



    /**
     * Set a three-byte field in the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @param parm_value  value to be set
     */
    public void set_int24(int parm_offset, int parm_value) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.set_int24: negative offset not supported", 8, 8);
        }
        if (parm_offset + 2 > cb_length) {
            throw new zException("zControlBlock.set_int24: access beyond end of CB not allowed", 8, 8);
        }
        mem_array[cb_offset + parm_offset + 2] = (byte) parm_value;
        mem_array[cb_offset + parm_offset + 1] = (byte) (parm_value >> 8);
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 16);
    }



    /**
     * Get a four-byte field from the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @return int value
     */
    public int get_int(int parm_offset) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_int: negative offset not supported", 8, 8);
        }
        if (parm_offset + 3 > cb_length) {
            throw new zException("zControlBlock.get_int: access beyond end of CB not allowed", 8, 8);
        }
        int return_value = (int) mem_array[cb_offset + parm_offset] & 0xff;
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 2] & 0xff);
        return_value = return_value << 8 | (mem_array[cb_offset + parm_offset + 3] & 0xff);
        return return_value;
    }



    /**
     * Set a four-byte field in the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @param parm_value  value to be set
     */
    public void set_int(int parm_offset, int parm_value) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.set_int: negative offset not supported", 8, 8);
        }
        if (parm_offset + 3 > cb_length) {
            throw new zException("zControlBlock.set_int: access beyond end of CB not allowed", 8, 8);
        }
        mem_array[cb_offset + parm_offset + 3] = (byte) parm_value;
        mem_array[cb_offset + parm_offset + 2] = (byte) (parm_value >> 8);
        mem_array[cb_offset + parm_offset + 1] = (byte) (parm_value >> 16);
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 24);
    }



    /**
     * Get an eight-byte field from the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @return long value
     */
    public long get_long(int parm_offset) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_long: negative offset not supported", 8, 8);
        }
        if (parm_offset + 7 > cb_length) {
            throw new zException("zControlBlock.get_long: access beyond end of CB not allowed", 8, 8);
        }
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
     * Set an eight-byte field in the Control Block
     *
     * @param parm_offset offset of the field within the Control Block
     * @param parm_value  value to be set
     */
    public void set_long(int parm_offset, long parm_value) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.set_long: negative offset not supported", 8, 8);
        }
        if (parm_offset + 7 > cb_length) {
            throw new zException("zControlBlock.set_long: access beyond end of CB not allowed", 8, 8);
        }
        mem_array[cb_offset + parm_offset + 7] = (byte)  parm_value;
        mem_array[cb_offset + parm_offset + 6] = (byte) (parm_value >> 8);
        mem_array[cb_offset + parm_offset + 5] = (byte) (parm_value >> 16);
        mem_array[cb_offset + parm_offset + 4] = (byte) (parm_value >> 24);
        mem_array[cb_offset + parm_offset + 3] = (byte) (parm_value >> 32);
        mem_array[cb_offset + parm_offset + 2] = (byte) (parm_value >> 40);
        mem_array[cb_offset + parm_offset + 1] = (byte) (parm_value >> 48);
        mem_array[cb_offset + parm_offset]     = (byte) (parm_value >> 56);
    }



    /**
     * Get a halfword-prefixed String from the Control Block
     *
     * @param parm_offset offset of the string within the Control Block
     * @return String value extracted from the Control Block
     */
    public String get_hw_string(int parm_offset) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_hw_string: negative offset not supported", 8, 8);
        }
        if (parm_offset + 1 > cb_length) {
            throw new zException("zControlBlock.get_hw_string: access beyond end of CB not allowed", 8, 8);
        }
        int string_len = (int) mem_array[cb_offset + parm_offset] & 0xff;
        string_len = string_len << 8 | (mem_array[cb_offset + parm_offset + 1] & 0xff);
        if (parm_offset + 2 + string_len > cb_length) {
            throw new zException("zControlBlock.get_hw_string: string location beyond end of CB not allowed", 8, 8);
        }
        byte[] return_array = new byte[string_len];
        System.arraycopy(mem_array, cb_offset + parm_offset + 2, return_array, 0, string_len);
        return new String(return_array);
    }



    /**
     * Get a fixed-length character field from the Control Block,
     * trailing blanks dropped
     *
     * @param parm_offset offset of the field within the Control Block
     * @param parm_length length of the field
     * @param tz          tables for the ASCII or EBCDIC translation
     * @return String value
     */
    public String get_string(int parm_offset, int parm_length, tz390 tz) {
        if (parm_offset < 0) {
            throw new zException("zControlBlock.get_string: negative offset not supported", 8, 8);
        }
        if (parm_offset + parm_length > cb_length) {
            throw new zException("zControlBlock.get_string: access beyond end of CB not allowed", 8, 8);
        }
        return to_text(cb_offset + parm_offset, parm_length, false, tz);
    }



    /**
     * Get a null-terminated string that lives elsewhere in the same memory,
     * such as the DSN a catalog entry points at
     *
     * @param parm_addr  address of the string
     * @param parm_limit longest string accepted
     * @param tz         tables for the ASCII or EBCDIC translation
     * @return String value, empty when the first byte is X'00'
     */
    public String get_cstring(int parm_addr, int parm_limit, tz390 tz) {
        if (parm_addr <= 0 || parm_addr >= mem_array.length) {
            throw new zException("zControlBlock.get_cstring: address outside memory", 8, 8);
        }
        return to_text(parm_addr, Math.min(parm_limit, mem_array.length - parm_addr), true, tz);
    }



    /**
     * Translate bytes to text the way sz390.get_ascii_string does
     *
     * @param addr      first byte
     * @param len       maximum number of bytes
     * @param null_term stop at X'00'
     * @param tz        translation tables
     * @return text with trailing blanks dropped
     */
    private String to_text(int addr, int len, boolean null_term, tz390 tz) {
        StringBuilder text = new StringBuilder(len);
        for (int index = 0; index < len; index++) {
            int b = mem_array[addr + index] & 0xff;
            if (null_term && b == 0) {
                break;
            }
            if (!tz.opt_ascii) {
                b = tz.ebcdic_to_ascii[b] & 0xff;
            }
            text.append(tz.ascii_table.charAt(b));
        }
        int end = text.length();
        while (end > 0 && text.charAt(end - 1) == ' ') {
            end--;
        }
        return text.substring(0, end);
    }



    /**
     * Get array pointer
     *
     * @return byte[]
     */
    public byte[] array() {
        return mem_array;
    }



    /**
     * Get offset
     *
     * @return int
     */
    public int get_offset() {
        return cb_offset;
    }



    /**
     * Get length
     *
     * @return int
     */
    public int get_length() {
        return cb_length;
    }
}
