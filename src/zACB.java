/*
z390 - Mainframe assembler emulator and run-time engine
Copyright (C) 2021 z390 Assembler LLC

This file is part of z390.
z390 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

z390 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, see <https://www.gnu.org/licenses/>.
*/
public class zACB
{/***************************************************************************
  *
  * The zACB object is the Java-level representation of an ACB
  * The zACB class handles all ACB-based requests: Openn and Close
  *
  * *************************************************** Maintenance
  * 2016-12-24 initial coding
  *****************************************************/
  private int acb_pointer;
  private String ddname;
  private int create_count;
  private int open_count;
  private int point_count;
  private int get_count;
  private int insert_count;
  private int update_count;
  private int erase_count;
  private int close_count;
  private int eof_count;
  private int error_count;

  /* Constructor for new ACB */
  public zACB(int acb_addr)
   {acb_pointer = acb_addr;
	ddname = "";
	create_count = 0;
	open_count = 0;
	point_count = 0;
	get_count = 0;
	insert_count = 0;
	update_count = 0;
	erase_count = 0;
	close_count = 0;
	eof_count = 0;
	error_count = 0;
	System.out.println("zACB constructed");
	}

  /* Open new ACB */
  public int Open()
   {System.out.println("Open invoked");
	return 0;
	}

  /* Close ACB */
  public int Close()
   {System.out.println("Close invoked");
	return 0;
	}

 /* end of module zACB */
 }