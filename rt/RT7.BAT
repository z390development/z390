rem test and verify compile, link, execute with no rc check on ez390 
call ASML  %1 bal notiming %2 %3 %4 %5 %6 %7 %8 %9
call EZ390 %1 notiming stats %2 %3 %4 %5 %6 %7 %8 %9
