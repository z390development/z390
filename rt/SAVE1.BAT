rem save files %1\%2 to %1\SAVE for RT comparisons                
erase %1\%2.bak
erase rt\%1\save\%2.* /q
copy  %1\%2.* rt\%1\save 
