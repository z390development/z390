rem save rt\test files for rttest.bat comparison
erase rt\test.bak
erase rt\test\save\*.* /q
copy rt\test\*.* rt\test\save 
