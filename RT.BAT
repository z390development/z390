rem run test and demo regression tests
rem process stops on errors
rem check d* difference files for changes > 81
call rttest
call rtdemo
erase ?z390.dif
erase temp\*.di?
copy demo\*.di? temp
copy test\*.di? temp
dir temp\*.di?/o-s/p

