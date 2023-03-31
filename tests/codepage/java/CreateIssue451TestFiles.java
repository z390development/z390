import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;

/**
 * Class to create test files for issue #451.
 * 
 * All the files contain the 96 ISO-8859-1 characters
 * 0xA0..0xFF. See https://www.ascii-code.com/ISO-8859-1
 * for a complete table of the characters.
 * 
 * While the files can be created directly using a
 * text editor, GitHub does not properly handle the
 * 96 characters. The files are used to test the z390
 * changes for issue #451.
 * 
 * Files created:
 *     T1#451.CPY
 *     T1#451.DAT
 *     T1P#451.MLC
 *     T1#451.MLC
 *     
 * The first three are created "from scratch". These
 * are all data only files -- no code.
 * 
 * The fourth, T1#451.MLC, is the actual test program
 * that makes use of the first three files. It is
 * created using a "skeleton" version of itself.
 * 
 * The "skeleton" version of T1@451.MLC, T1#451.MLC.SKEL,
 * resides in the build directory,
 */
public class CreateIssue451TestFiles
{
    static final String newline = System.getProperty("line.separator");
    static final String codepage="ISO-8859-1";
    static final String skelLiteral = "*SKELETON>";
    
    static String dir = null; //"/home/john/z390/temp/mlc/";
    static String cpyFile = "T1#451.CPY";
    static String datFile = "T1#451.DAT";
    static String pchFile = "T1P#451.MLC";
    static String mlcFile = "T1#451.MLC";
    static String mlcFileSkeleton = "T1#451.MLC.SKEL";
    
    /**
     * Create test files for issue #451.
     * 
     * @param args args[0] full path to build directory
     * 
     */
    public static void main(String[] args)
    {
        // args[0] is directory to contain files
        if (args.length != 1 || args[0].length() < 2)
        {
            System.out.println("Usage: java CreateIssue451TestFiles <full path to build directory>");
            System.exit(4);
        }
        
        String d = args[0];
        if( !d.substring(d.length()-1).equals(File.separator) )
        {
            d = d+File.separator;
        }
        dir = d;
        
        System.out.println("Using build directory "+dir);
        
        System.out.println("User directory is "+System.getProperty("user.dir"));
        
        /*
         *  build 2-dim array of integers for
         *  the extended ASCII characters 
         *    0xA0..0xAF
         *    0xB0..0xBF
         *    ...
         *    0xF0..0xFF
         */
        int[][] v = buildIntegerArray();
        int numErrors = 0;
        
        if (createCpyFile(v, dir+cpyFile))
        {
            System.out.println("Successfully created " + cpyFile);
        }
        else
        {
            numErrors++;
        }
        
        if (createDatFile(v, dir+datFile))
        {
            System.out.println("Successfully created " + datFile);
        }
        else
        {
            numErrors++;
        }
        
        if (createPchFile(v, dir+pchFile))
        {
            System.out.println("Successfully created " + pchFile);
        }
        else
        {
            numErrors++;
        }
        
        if (createMlcFile(v, dir+mlcFile, dir+mlcFileSkeleton))
        {
            System.out.println("Successfully created " + mlcFile);
        }
        else
        {
            numErrors++;
        }

        int rc = 0;
        if (numErrors != 0)
        {
            System.out.println("Number of errors: "+numErrors);
            rc = 8;
        }
        else
        {
            System.out.println("All files created successfully");
        }
        System.exit(rc);
    }
    
    /**
     * Create T1#451.CPY file
     * 
     * @param v 2-dimensional array of extended ASCII values
     * @param fileName full path name of the file
     * @return true if file created, false otherwise
     */
    public static boolean createCpyFile(int[][] v, String fileName)
    {
        File f;
        BufferedWriter bw;

        boolean success = true;
        
        f = new File(fileName);
        try
        {
            bw = getWriterForCharset(f, codepage);
        }
        catch (Exception e)
        {
            System.out.println("Error creating BufferedWriter for "+fileName);
            return false;
        }

        try
        {
            for (int i = 0; i < 6; i++)
            {
                bw.write("         DC    C'");
                for (int j = 0; j < 16; j++)
                {
                    bw.write(v[i][j]);
                }
                bw.write("'" + newline);
            }
        }
        catch (IOException e)
        {
            System.out.println("Write error for "+fileName);
            success = false;
        }
        finally
        {
            try
            {
                bw.close();
            }
            catch (IOException e)
            {
                System.out.println("Close error for "+fileName);
                success = false;
            }
        }
        
        return success;
    }
     
    /**
     * Create T1#451.DAT file
     * 
     * @param v 2-dimensional array of extended ASCII values
     * @param fileName full path name of the file
     * @return true if file created, false otherwise
     */
    public static boolean createDatFile(int[][] v, String fileName)
    {
        File f;
        BufferedWriter bw;

        boolean success = true;
        
        /*
         *  create T1#451.DAT file
         */
        f = new File(fileName);
        try
        {
            bw = getWriterForCharset(f, codepage);
        }
        catch (Exception e)
        {
            System.out.println("Error creating BufferedWriter for "+fileName);
            return false;
        }

        try
        {
            for (int i = 0; i < 6; i++)
            {
                for (int j = 0; j < 16; j++)
                {
                    bw.write(v[i][j]);
                }
                bw.write(newline);
            }
        }
        catch (IOException e)
        {
            System.out.println("Write error for "+fileName);
        }
        finally
        {
            try
            {
                bw.close();
            }
            catch (IOException e)
            {
                System.out.println("Close error for "+fileName);
            }
        } 
        
        return success;
    }
    
    /**
     * Create T1P#451.MLC file of PUNCH statements
     * 
     * @param v 2-dimensional array of extended ASCII values
     * @param fileName full path name of the file
     * @return true if file created, false otherwise
     */
    public static boolean createPchFile(int[][] v, String fileName)
    {
        File f;
        BufferedWriter bw;

        boolean success = true;
        
        /*
         * create T1P#451.MLC PUNCH file
         */
        f = new File(fileName);
        try
        {
            bw = getWriterForCharset(f, codepage);
        }
        catch (Exception e)
        {
            System.out.println("Error creating BufferedWriter for "+fileName);
            return false;
        }

        try
        {
            bw.write("T1P#451  CSECT" + newline);
            bw.write("         DC    C'");
            bw.write(v[0][6]);
            bw.write("'" + newline+newline);
            
            for (int i = 0; i < 6; i++)
            {
                bw.write("         PUNCH '");
                bw.write("         DC    C''");
                for (int j = 0; j < 16; j++)
                {
                    bw.write(v[i][j]);
                }
                bw.write("''    ");
                if (i == 0) bw.write(" ");
                int k = (0xA0 + (i << 4)) >> 4;
                String s = String.format("%1X0-%1XF", k, k);
                bw.write(s + "'" + newline);
            }
            bw.write("*" + newline);
            bw.write("         END" + newline);
        }
        catch (IOException e)
        {
            System.out.println("Write error for "+fileName);
//            e.printStackTrace();
        }
        finally
        {
            try
            {
                bw.close();
            }
            catch (IOException e)
            {
                System.out.println("Close error for "+fileName);
            }
        }
        
        return success;
    }
    
    /**
     * Create T1#451.MLC file using file T1#451.MLC.SKELETON
     * 
     * @param v 2-dimensional array of extended ASCII values
     * @param fileName name of the created file
     * @param skelFileName name of skeleton file used to create the file
     * @return true if file created, false otherwise
     */
    public static boolean createMlcFile(int[][] v, String fileName, String skelFileName)
    {
        File f;
        BufferedWriter bw;

        boolean success = true;
                
        File fSkel;
        BufferedReader br;
        
        // create T1#451.MLC file using T1#451.MLC.SKELETON file
        
        // input file is the file skeleton, T1#451.MLC.SKELETON
        fSkel = new File(skelFileName);
        
        try
        {
            br = new BufferedReader(new FileReader(fSkel));
        }
        catch (IOException e)
        {
            System.out.println("Error creating BufferedReader for "+skelFileName);
            return false;
        }

        // output file is T1#451.MLC
        f = new File(fileName);
        
        try
        {
            bw = getWriterForCharset(f, codepage);
        }
        catch (Exception e)
        {
            System.out.println("Error creating BufferedWriter for "+fileName);
            return false;
        }
        
        /*
         *  copy input to output until reach "skeleton" lines
         *  for each "skeleton" line:
         *      output DC statement plus "skeleton" line comment
         *  end for
         *  error if wrong number of "skeleton" lines
         *  copy remaining input to output
         */
        try
        {
            int numLines = 0;
            int numSkels = 0;
            
            String s = br.readLine();
            while (s != null && !s.startsWith(skelLiteral))
            {
                numLines++;
                bw.write(s+newline);
                s = br.readLine();
            }
            if (s == null)
            {
                System.out.println("Error; no \"skeleton\" lines found");
                throw new Exception();
            }
            while (s != null && s.startsWith(skelLiteral) && ++numSkels <= v.length)
            {
                numLines++;
                bw.write("         DC    C'");
                for (int j = 0; j < 16; j++)
                {
                    bw.write(v[numSkels-1][j]);
                }
                bw.write("'" + s.substring(skelLiteral.length()) + newline);
                s = br.readLine();
            }
            if (numSkels != v.length)
            {
                System.out.println("numSkels = "+numSkels+"  expected "+v.length);
                System.out.println("Error: wrong number of \"skeleton\" lines");
                throw new Exception();
            }
            while (s != null)
            {
                numLines++;
                bw.write(s+newline);
                s = br.readLine();
            }
                
//            System.out.println("numLines = "+numLines+"  numSkels = "+numSkels);
        }
        catch (Exception e)
        {
            System.out.println("Read error for "+skelFileName);
            success = false;
        }
        finally
        {
            try 
            {
                br.close();
                bw.close();
            }
            catch (IOException e) {
                System.out.println("Close error for "+skelFileName+" or "+fileName);
                success = false;
            }
        }
        
        return success;
    }
   
    /**
     * Build 2-dimensional array of integers containing the
     * hexadecimal values
     *     0xA0..0xAF
     *     0xB0..0xBF
     *     ...
     *     0xF0..0xFF
     *     
     * @return the array
     */
    public static int[][] buildIntegerArray()
    {
        int[][] a = new int[6][16];
        
        for (int i = 0; i < 6; i++)
        {
            int k = 0xA0 + (i << 4);
            for (int j = 0; j < 16; j++)
            {
                a[i][j] = k + j;
            }
        }
        return a;
    }

    // next method copied from tz390.java

    /**
     * Gets a BufferedWriter for a file using specified Charset name.
     *
     * <p>No Charset is used if {@code charsetName} is null or the empty string.
     (
     * @param file        existing File object for the BufferedWriter
     * @param charsetName the name of the Charset to use. If null or empty string,
                          no Charset is used.
     * @return the BufferedWriter
     * @throws FileNotFoundException if {@code file} does not exist.
     * @throws UnsupportedEncodingException if {@code charsetName} encoding
     *         is not supported.
     */
    static public BufferedWriter getWriterForCharset(File file, String charsetName)
            throws Exception
    {
        BufferedWriter bw = null;
        if (charsetName != null && charsetName.length() > 0)
        {
            bw = new BufferedWriter(
                new OutputStreamWriter(new FileOutputStream(file),
                    charsetName));
        }
        else
        {
            bw = new BufferedWriter(new FileWriter(file));
        }
        return bw;
    }
}
