/*_***************************************************

 z390 portable mainframe assembler and emulator.

 Copyright 2011 Automated Software Tools Corporation

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

 zException was built as part of the zVSAM v2 component of z390.
 It is the preferred interface for handling unexpected conditions
 and is intended for use in other z390 components as well

 ****************************************************
 * Maintenance
 ****************************************************
 * 2021           Initial construction by Hugh Sweeney and Abe Kornelis
 * 2026-10-27     Lifted from #908 for catalog support, reformatted for the linter
 ****************************************************/



/**
 * Exception information including return and reason codes.
 * The exception condition consists of a message with return and reason codes.
 *
 * @author   Hugh Sweeney / Abe Kornelis
 * @version  2.0
 */
public class zException extends RuntimeException {
    /** return code                 */ private int     ReturnCode;
    /** reason code                 */ private int     ReasonCode;
    /** class that raised it        */ private String  ClassName;
    /** method that raised it       */ private String  MethodName;
    /** message text                */ private String  MessageText;
    /** symptom string              */ private String  SymptomString;
    /** symptom data                */ private String  SymptomData;
    /** active or not               */ private boolean isActive;



    /**
     * Constructor, stores error message plus return and reason codes
     * and SymptomString / SymptomData in an active zException
     *
     * @param msg message text
     * @param ret return code
     * @param rsn reason code
     * @param sym symptom string
     * @param symdata symptom data
     */
    public zException(String msg, int ret, int rsn, String sym, String symdata) {
        super(msg);
        this.ReturnCode    = ret;
        this.ReasonCode    = rsn;
        this.ClassName     = new Exception().getStackTrace()[0].getMethodName();
        this.MethodName    = new Exception().getStackTrace()[0].getClassName();
        this.MessageText   = msg;
        this.SymptomString = sym;
        this.SymptomData   = symdata;
        this.isActive      = true;
    }



    /**
     * Constructor, stores error message plus return and reason codes
     * in an active zException
     *
     * @param msg message text
     * @param ret return code
     * @param rsn reason code
     */
    public zException(String msg, int ret, int rsn) {
        super(msg);
        this.ReturnCode    = ret;
        this.ReasonCode    = rsn;
        this.ClassName     = new Exception().getStackTrace()[0].getMethodName();
        this.MethodName    = new Exception().getStackTrace()[0].getClassName();
        this.MessageText   = msg;
        this.SymptomString = "";
        this.SymptomData   = "";
        this.isActive      = true;
    }



    /**
     * Constructor, initializes empty and inactive zException
     */
    public zException() {
        super("");
        this.ReturnCode    = 0;
        this.ReasonCode    = 0;
        this.ClassName     = "";
        this.MethodName    = "";
        this.MessageText   = "";
        this.SymptomString = "";
        this.SymptomData   = "";
        this.isActive      = false;
    }



    /**
     * reset Exception status
     */
    public void reset() {
        this.ReturnCode    = 0;
        this.ReasonCode    = 0;
        this.ClassName     = "";
        this.MethodName    = "";
        this.MessageText   = "";
        this.SymptomString = "";
        this.SymptomData   = "";
        this.isActive      = false;
    }



    /**
     * Activate Exception
     *
     * @param msg message text
     * @param ret return code
     * @param rsn reason code
     */
    public void activate(String msg, int ret, int rsn) {
        this.ReturnCode    = ret;
        this.ReasonCode    = rsn;
        this.ClassName     = new Exception().getStackTrace()[0].getMethodName();
        this.MethodName    = new Exception().getStackTrace()[0].getClassName();
        this.MessageText   = msg;
        this.isActive      = true;
    }



    /**
     * Determine zException activation status
     *
     * @return true when active
     */
    public boolean isActive() {
        return isActive;
    }



    /**
     * Add an element to the symptom string
     *
     * @param sym symptom to add
     */
    public void addSymptom(String sym) {
        this.SymptomString = (SymptomString.isEmpty()) ? sym : SymptomString + "." + sym;
    }



    /**
     * Add an element to the symptom data string
     *
     * @param data symptom data to add
     */
    public void addSymData(String data) {
        this.SymptomData = (SymptomData.isEmpty()) ? data : SymptomData + ", " + data;
    }



    /**
     * Get the error message with the return and reason codes appended
     *
     * @return formatted message
     */
    public String toString() {
        return getMessage()
            + " - RetCode=X'" + String.format((ReturnCode < 256) ? "%1$X2" : "%1$X8", ReturnCode)
            + "' Reason=X'" + String.format("%1$X8", ReasonCode)
            + "'";
    }



    /**
     * Get the error message
     *
     * @return message text
     */
    public String getMessageText() {
        return MessageText;
    }



    /**
     * Get the return code
     *
     * @return return code
     */
    public int getReturnCode() {
        return ReturnCode;
    }



    /**
     * Get the reason code
     *
     * @return reason code
     */
    public int getReasonCode() {
        return ReasonCode;
    }



    /**
     * Get the symptom string
     *
     * @return symptom string
     */
    public String getSymptomString() {
        return SymptomString;
    }



    /**
     * Get the symptom data
     *
     * @return symptom data
     */
    public String getSymptomData() {
        return SymptomData;
    }
}
