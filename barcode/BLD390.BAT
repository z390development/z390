rem asml routines
call bat\asm  barcode\encodetr sysmac(+barcode) syscpy(+barcode) %1 %2 %3 %4
call bat\asml barcode\s4bcdrva sysmac(+barcode) syscpy(+barcode) %1 %2 %3 %4
copy barcode\*.390 linklib 
