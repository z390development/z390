rem compare source at %1\%2 and %1\save\%2
call bat\COMPARETEXTFILE.BAT %1\%2 %1\save\%2 > temp.txt
me temp.txt
