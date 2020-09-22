rem test rpi 2210 equ value 0 on first use
cd..
call bat\ASMLG rpi\rpi2210 sysmac(rpi+mac) trace
pause verify test rc=0 
