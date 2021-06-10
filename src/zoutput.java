
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
 * 04/08/18 dak RPI 1618 Create zoutput object to separate sequential output file handling from the main body of z390 classes
 ****************************************************/

public class zoutput {

	LinkedList<BufferedWriter> pool;
	LinkedList<RandomAccessFile> poolraf;
	LinkedList<String> filenames;
	LinkedList<String> filenamesraf;
	int filesopen = 0;
	boolean debugtrace = false; // this, if true will show who called the method
	boolean debug = false; // this, if true will show some debugging messages

	// the following two statements are needed only to support debug=true processing
	StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
	String ste = "1 " + stackTraceElements[1].toString() + ": " + System.currentTimeMillis() + "\n" + "2 " + stackTraceElements[2].toString() + ": "
					+ System.currentTimeMillis() + "\n" + "3 " + stackTraceElements[3].toString() + ": " + System.currentTimeMillis() + "\n" + "4 "
					+ stackTraceElements[4].toString() + ": " + System.currentTimeMillis();

	// open BufferedWriter
	public BufferedWriter open(String filename, String type) {
		if (!ste.isEmpty()) {
			if (debug) {
				System.out.println(ste);
				System.out.println((Thread.currentThread().getStackTrace()[1].getClassName()) + ": (constructor) zoutput initialized");
			}
			ste = "";
		}

		if (debugtrace) {
			StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
			//System.out.println("0 " + stackTraceElements[10] + ": " + System.currentTimeMillis());
			System.out.println("1 " + stackTraceElements[1] + ": " + System.currentTimeMillis());
			System.out.println("2 " + stackTraceElements[2] + ": " + System.currentTimeMillis());
			System.out.println("3 " + stackTraceElements[3] + ": " + System.currentTimeMillis());
			System.out.println("4 " + stackTraceElements[4] + ": " + System.currentTimeMillis());
		}

		BufferedWriter filehandle = null;

		if (pool == null) {
			pool = new LinkedList<>();
			if (filenames == null) {
				filenames = new LinkedList<>();
			}
		}
		try {
			filehandle = new BufferedWriter(new FileWriter(filename));
		} catch (IOException ex) {
			ex.printStackTrace();
			System.exit(128);
		}
		
		pool.add(filehandle);
		filenames.add(filename);
		filesopen++;

		if (debug) {
			System.out.println("zoutput: (open) file opened=" + filename + " type=" + type + " " + System.currentTimeMillis());
			System.out.println("zoutput: (open) files open=" + filesopen + " " + System.currentTimeMillis());
		}
		return filehandle;
	}

	// open RandomAccessFile
	public RandomAccessFile openraf(String filename, String type, String mode) {
		if (!ste.isEmpty()) {
			if (debug) {
				System.out.println(ste);
				System.out.println((Thread.currentThread().getStackTrace()[1].getClassName()) + ": (constructor) zoutput initialized");
			}
			ste = "";
		}

		if (debugtrace) {
			StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
			//System.out.println("0 " + stackTraceElements[10] + ": " + System.currentTimeMillis());
			System.out.println("1 " + stackTraceElements[1] + ": " + System.currentTimeMillis());
			System.out.println("2 " + stackTraceElements[2] + ": " + System.currentTimeMillis());
			System.out.println("3 " + stackTraceElements[3] + ": " + System.currentTimeMillis());
			System.out.println("4 " + stackTraceElements[4] + ": " + System.currentTimeMillis());
		}

		RandomAccessFile filehandle = null;

		if (poolraf == null) {
			poolraf = new LinkedList<>();
			if (filenamesraf == null) {
				filenamesraf = new LinkedList<>();
			}
		}
		if (filename == null) {
			System.out.println("filename is null");
		}
		if (mode == null) {
			System.out.println("mode is null");
		}

		try {
			filehandle = new RandomAccessFile(filename, mode);
		} catch (Exception ex) {
			ex.printStackTrace();
			System.exit(128);
		}
		
		poolraf.add(filehandle);
		filenamesraf.add(filename);
		filesopen++;

		if (debug) {
			System.out.println(
							"zoutput: (openRandomAccessFile) file opened=" + filename + " type=" + type + " mode=" + mode + " " + System.currentTimeMillis());
			System.out.println("zoutput: (openRandomAccessFile) files open=" + filesopen + " " + System.currentTimeMillis());
		}
		return filehandle;
	}

	// close BufferedWriter or RandomAcessFile
	public void close(Object filehandle) {
		if (!ste.isEmpty()) {
			if (debug) {
				System.out.println(ste);
				System.out.println((Thread.currentThread().getStackTrace()[1].getClassName()) + ": (constructor) zoutput initialized");
			}
			ste = "";
		}
		if (debugtrace) {
			StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
			System.out.println("1 " + stackTraceElements[1] + ": " + System.currentTimeMillis());
			System.out.println("2 " + stackTraceElements[2] + ": " + System.currentTimeMillis());
			System.out.println("3 " + stackTraceElements[3] + ": " + System.currentTimeMillis());
			System.out.println("4 " + stackTraceElements[4] + ": " + System.currentTimeMillis());
		}
		if (filehandle == null) {
			return;
		}

		if ((filehandle instanceof BufferedWriter)) {
			if (pool == null) {
				return;
			}

			try {
				((BufferedWriter) filehandle).close();
				if (debug) {
					System.out.println("zoutput: (close BufferedWriter) file closed=" + filenames.get(pool.size() - 1) + " " + System.currentTimeMillis());
				}
				pool.remove(filehandle);
				filesopen--;
				if (debug) {
					System.out.println("zoutput: (close BufferedWriter) files open=" + filesopen + " " + System.currentTimeMillis());
				}
			} catch (IOException ex) {
				ex.printStackTrace();
				System.exit(128);
			}

			if (pool.isEmpty()) {
				pool = null;
				filenames = null;
				if (debug) {
					System.out.println("zoutput: (close) pool BufferedWriter freed" + " " + System.currentTimeMillis());
				}
			}
			return;
		} else if ((filehandle instanceof RandomAccessFile)) {
			if (poolraf == null) {
				return;
			}

			try {
				((RandomAccessFile) filehandle).close();
				if (debug) {
					System.out.println(
									"zoutput: (close RandomAccessFile) file closed=" + filenamesraf.get(poolraf.size() - 1) + " " + System.currentTimeMillis());
				}
				poolraf.remove(filehandle);
				filesopen--;
				if (debug) {
					System.out.println("zoutput: (close RandomAccessFile) files open=" + filesopen + " " + System.currentTimeMillis());
				}
			} catch (IOException ex) {
				ex.printStackTrace();
				System.exit(128);
			}

			if (poolraf.isEmpty()) {
				poolraf = null;
				filenamesraf = null;
				if (debug) {
					System.out.println("zoutput: (close) pool RandomAccessFile freed" + " " + System.currentTimeMillis());
				}
			}
		} else {
			// parameter to close must be either a BufferedWriter or RandamAccessFile object
			System.out.println(filehandle);
			throw new InvalidParameterException();
		}
	}

	// closeAll will close all open files
	public void closeAll() {
		if (!ste.isEmpty()) {
			if (debug) {
				System.out.println(ste);
				System.out.println((Thread.currentThread().getStackTrace()[1].getClassName()) + ": (constructor) zoutput initialized");
			}
			ste = "";
		}
		if (debugtrace) {
			StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
			System.out.println("1 " + stackTraceElements[1] + ": " + System.currentTimeMillis());
			System.out.println("2 " + stackTraceElements[2] + ": " + System.currentTimeMillis());
			System.out.println("3 " + stackTraceElements[3] + ": " + System.currentTimeMillis());
			System.out.println("4 " + stackTraceElements[4] + ": " + System.currentTimeMillis());
		}

		if (pool == null) {
			if (debug) {
				System.out.println("zoutput: (closeAll) all BufferedWriter files are already closed" + " " + System.currentTimeMillis());
			}
		}
		if (poolraf == null) {
			if (debug) {
				System.out.println("zoutput: (closeAll) all RandomAccessFile files are already closed" + " " + System.currentTimeMillis());
			}
		}
		if ((pool == null) && (poolraf == null)) {
			return;
		}

		if (pool != null) {
			BufferedWriter filehandle = null;

			for (int i = pool.size() - 1; i >= 0; i--) {
				filehandle = pool.get(i);
				try {
					close(filehandle);
					if (pool == null) {
						break;
					}

					pool.remove(filehandle);
					if (debug) {
						System.out.println("zoutput: (closeAll) closed " + filenames.get(i) + " " + System.currentTimeMillis());
					}
					filenames.remove(i);

					if (debug) {
						System.out.println("zoutput: (closeAll) filehandle " + i + " closed" + " " + System.currentTimeMillis());
						System.out.println("zoutput: (closeAll) BufferedWriter files open=" + pool.size() + " " + System.currentTimeMillis());
					}
				} catch (Exception ex) {
					ex.printStackTrace();
					System.exit(128);
				}
			}
		}

		if (poolraf != null) {
			RandomAccessFile filehandle = null;
			int size = poolraf.size() - 1;

			for (int i = size; i >= 0; i--) {
				filehandle = poolraf.get(i);
				try {
					close(filehandle);
					if (pool == null) {
						break;
					}

					poolraf.remove(filehandle);
					if (debug) {
						System.out.println("zoutput: (closeAll) closed " + filenamesraf.get(i) + " " + System.currentTimeMillis());
					}
					filenamesraf.remove(i);

					if (debug) {
						System.out.println("zoutput: (closeAll) filehandle " + i + " closed" + " " + System.currentTimeMillis());
						System.out.println("zoutput: (closeAll) RandomAccessFile files open=" + poolraf.size() + " " + System.currentTimeMillis());
					}
				} catch (Exception ex) {
					ex.printStackTrace();
					System.exit(128);
				}
			}
		}

		if ((pool == null) || (pool.isEmpty())) {
			if (pool == null) {
				return;
			}
			pool = null;
			filenames = null;
			if (debug) {
				System.out.println("zoutput: (closeAll) pool BufferedWriter freed" + " " + System.currentTimeMillis());
			}
		}

		if ((poolraf == null) || (poolraf.isEmpty())) {
			if (poolraf == null) {
				return;
			}
			poolraf = null;
			filenamesraf = null;
			if (debug) {
				System.out.println("zoutput: (closeAll) pool RandomAccessFile freed" + " " + System.currentTimeMillis());
			}
		}
	}
}