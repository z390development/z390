<!DOCTYPE HTML PUBLIC
                 "-//W3C//DTD HTML 4.01 Transitional//EN"
                 "http://www.w3.org/TR/html401/loose.dtd">
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  <title>query1 - ROSNF Find Rotarians table structure query</title>
</head>
<body><pre>
<?php
   // Open a connection to the server and USE the rosnf
   $connection = mysql_connect("localhost","Don","dsh747");
   mysql_select_db("rosnf", $connection);

   // Run a query on the wine table in the winestore database to retrieve
   // one row
   $result = mysql_query ("SELECT * FROM Rotarians LIMIT 1", $connection);

   // Output a header, with headers spaced by padding
   print str_pad("Field", 20) .
         str_pad("Type", 14) .
         str_pad("Null", 6) .
         str_pad("Key", 5) .
         str_pad("Extra", 12) . "\n";

   // How many attributes are there?
   $x = mysql_num_fields($result);

   // for each of the attributes in the result set
   for($y=0;$y<$x;$y++)
   {
      // Get the meta-data for the attribute
      $info = mysql_fetch_field ($result);

      // Print the attribute name
      print str_pad($info->name, 20);

      // Print the data type
      print str_pad($info->type, 6);

      // Print the field length in brackets e.g.(2)
      print str_pad("({$info->max_length})", 8);

      // Print out YES if attribute can be NULL
      if ($info->not_null != 1)
          print " YES ";
      else
          print "     ";

      // Print out selected index information
      if ($info->primary_key == 1)
         print " PRI ";
      elseif ($info->multiple_key == 1)
         print " MUL ";
      elseif ($info->unique_key == 1)
         print " UNI ";

      // If zero-filled, print this
      if ($info->zerofill)
         print " Zero filled";

      // Start a new line
      print "\n";
   }
?>
</pre>
</body>
</html>

 

