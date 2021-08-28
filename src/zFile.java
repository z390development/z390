import java.io.File;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.security.InvalidParameterException;
import java.util.LinkedList;

/*****************************************************
 * 
 * z390 portable mainframe assembler and emulator.
 * 
 * Copyright 2011 Automated Software Tools Corporation
 * 
 * z390 is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
 * 
 * z390 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 * of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with z390; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * zoutput is the output management component of and for z390 files.
 ****************************************************
 * 
 * Maintenance
 ****************************************************
 * 04/08/2018 dak RPI 1618 Create zoutput  object to separate sequential output file handling from the main body of z390 classes
 * 25/07/2021 afk RPI 1618 Create zOutFile object to separate sequential output file handling from the main body of z390 classes
 *                         Based on Dave Kienow's prior work on zoutput
 ****************************************************/



  /** ***********************************************************************
   * 
   *  Class zFile encapsulates all file handling operations in z390
   *  Main purpose of this class is:
   *  - Consolidate information on a file in one location
   *  - When options processing is not yet completes, convert open request to a pending open request
   *  - When options processing is not yet completes, buffer output data when a write request is issued
   *  - When options processing completes, open all files with a pending open request and write all buffered output data
   *  - when program terninates ensure all buffers are flushed and all files are closed
   *  - when program terminates abnormally before options processing is complete, attemtp to open/write/close all files with pending writes
   * 
   *  *********************************************************************** */
public class zFile
 {String file_path;
  String file_name;
  String file_type;



  /**
  *
  * Constructor
  *
  * Parameters: String   holding the intended file extension
  *
  */
  zFile(String new_type)
   {file_path = null;
    file_name = null;
    file_type = new_type;
    }



  /**
  *
  * Set_Path method - sets the path to the file
  *
  * Parameters: String     new path value
  *
  * Returns:    void
  *
  */
  void Set_Path(String new_path)
  {file_path = new_path;
   }



  /**
  *
  * Add_Path method - adds a path to the list of searchable directories
  * 
  * Function:   If new path starts with a plus-sign, add it to the extisting path,
  *                otherwise replace the existing path
  * 
  * Parameters: String     new path value
  *
  * Returns:    void
  * 
  */
  void Add_Path(String new_path)
  {if (new_path.charAt(0) == '+')
    {file_path = file_path + new_path;
     }
   else
    {file_path = new_path;
     }
   }



  /**
  *
  * Get_Path method - returns the path (concatenation) to the file
  *
  * Parameters: none
  *
  * Returns:    String     path name or concatenation
  *
  */
  String Get_Path()
  {return file_path;
   }



  /**
  *
  * Set_Name method - sets the name of the file
  *
  * Parameters: String     new filename value
  *
  * Returns:    void
  *
  */
  void Set_Name(String new_name)
  {file_name = new_name;
   }



  /**
  *
  * Get_FileSpec method - returns the full file spec: path, name, and extension
  * 
  * Function: Construct full file spec as follows:
  *           1. Strip long spacey name quotes if found from path and file
  *           2. Replace . and ..\ with current directory
  *           3. Check for overriding path in filename and ignore default path
  *           4. Check for overriding filename in path and ignore default filename
  *           5. Add directory, name, and/or type if not specified
  *           6. Replace \ with / if Linux
  * 
  * Parameters: String     overriding path
  *             String     overriding file name
  *             String     overriding extension name
  *
  * Returns:    String     full path spec
  *
  */
  String Get_FileSpec(String ovr_path, String ovr_name, String ovr_type)
  {if(ovr_path != null && ovr_path.length() > 0)
    {file_path = ovr_path;
     }
   if(ovr_name != null && ovr_name.length() > 0)
    {file_name = ovr_path;
     }
   if(ovr_type != null /* && ovr_type.length() > 0 */ ) // RPI 903 file_type with zero length is okay
    {file_type = ovr_type;
     }
   
   /* Validate all name components are valid */
  if (   file_path == null
      || file_name == null
      || file_type == null
      || file_path.length() == 0
      || file_name.length() == 0
      //|| file_type.length() == 0 // RPI 903 file_type with zero length is okay
      )
    {return null; // RPI 880
     }
  int last_path_sep = file_path.lastIndexOf(File.separatorChar); // RPI 1191
/*
	        file_dir = fix_file_separators(file_dir);  // RPI 1080
	        file_name = fix_file_separators(file_name); // RPI 1080
	        if (file_name.charAt(0) == '\"'
	    		|| file_name.charAt(0) == '\''){
	    		file_name = file_name.substring(1,file_name.length() - 1);
	    	}
	    	File temp_file;
	    	int index = file_name.indexOf(File.separator); // RPI 866
	    	if (index >= 0
	    		|| (file_name.length() > 2 && file_name.charAt(1) == ':')){
	    		// path found in file_name so ignore file_dir
	    		temp_file = new File(file_name);
	    		file_name = temp_file.getAbsolutePath();
	    	} else {
	    		if (file_dir == null
	    			|| file_dir.length() == 0
	    			|| file_dir.equals(".")){
	    			temp_file = new File(dir_cur);
	    			if (temp_file.isDirectory()){
	    				file_name = temp_file.getAbsolutePath() + File.separator + file_name;
	    			} else {
	    				return null; // rpi 880
	    			}
	    		} else {
	    			temp_file = new File(file_dir + File.separator); // RPI 1210
	    			index = file_dir.lastIndexOf("."); // RPI 1191
	    			if (index > last_path_sep){        // RPI 1191
	    				// file_dir has filename.sfx so ignore file_name
		    			if (file_dir.charAt(index-1) == '*'){  // RPI 908
		    				if (index > 1){
		    					// path\*.sfx
		    				   temp_file = new File(file_dir.substring(0,index-1) + file_name + file_dir.substring(index));
		    				} else {
		    					// *.sfx - replace sfx
		    					temp_file = new File(dir_pgm + File.separator + file_name + file_dir.substring(index));
		    				}
		    			}
    					file_name = temp_file.getAbsolutePath(); // RPI 908 remove file exist chk
	    			} else {
	    				// concatenate file_dir with file_name
	    				if (temp_file.isDirectory()){
	    					file_name = temp_file.getAbsolutePath() + File.separator + file_name;
	    				} else {
	    					return null; // rpi 880
	    				}
	    			}
	    		}
	    	}
	    	index = file_name.lastIndexOf(".");
	    	int index1 = file_name.lastIndexOf(File.separatorChar); // RPI 1210
	    	if (index <= index1){ // RPI 1210
	    		// concat default type if none
	    		file_name = file_name.trim() + file_type;
	    	}
	    	return file_name;
}
*/




   return file_spec;
   }



  /**
  *
  * fix_file_separators method - changes file separators to match host OS requirements
  * 
  * Function: Construct full file spec as follows:
  *           1. Replace \ with / if Linux else / with |
  *           2. Replace ..\ or ../ with parent path
  *           3. Remove embedded ./ or .\
  * 
  * Parameters: String     input path or file specification
  *
  * Returns:    String     corrected version of input path or file specification
  *
  */
  private String fix_file_separators(String name)
  {if (z390_os_type == z390_os_linux){ // RPI 532 file separator fix
    	name = find_bslash.matcher(name).replaceAll("/");  // RPI 1080
    } else {
    	name = find_slash.matcher(name).replaceAll("\\\\");  // RPI 1080
    }
	// proces any ..\..\ relative paths RPI 1097
	File temp_file = new File(System.getProperty("user.dir"));
	boolean parent_path = false;
	while (name.length() >= 3 && name.substring(0,3).equals(".." + File.separator)){
		parent_path = true;
		temp_file = new File(temp_file.getParent());
		name = name.substring(3);
	}
	// remove leading .\ for rel file RPI 1097
	if (name.length() >= 2 && name.substring(0,2).equals("." + File.separator)){
		name = name.substring(2);
	}
	// replace embeeded .\ with \  RPI 1097
	int index = 0;
	while (index < name.length() - 1){
		if (name.charAt(index) == '.'){
			if (name.charAt(index+1) == File.separatorChar){
				name = name.substring(0,index) + name.substring(index+2);
				index--;
			}
		}
		index++;
	}
	// remove trailing \ RPI 1097
	index = name.length()-1;
	while (index >= 0 && name.charAt(index) == File.separatorChar){
		name = name.substring(0,index);
		index--;
	}
	// prefix parent path if any  RPI 1097
	if (parent_path){
		name = temp_file.getPath() + File.separator + name;
	}
	return name;
} }