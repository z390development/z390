rem asml routines
call asm linklib\barcode\encodetr sysmac(+linklib\barcode) syscpy(+linklib\barcode) %1 %2 %3 %4
call asml linklib\barcode\s4bcdrva sysmac(+linklib\barcode) syscpy(+linklib\barcode) %1 %2 %3 %4
copy linklib\barcode\*.390 linklib 
