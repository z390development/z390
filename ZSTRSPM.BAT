rem translate all the SPM's in mac to mac\spm
call zstrmac mac\IF.MAC       mac\spm\IF.MAC
call zstrmac mac\ELSE.MAC     mac\spm\ELSE.MAC
call zstrmac mac\ELSEIF.MAC   mac\spm\ELSEIF.MAC
call zstrmac mac\ENDIF.MAC    mac\spm\ENDIF.MAC
call zstrmac mac\DO.MAC       mac\spm\DO.MAC
call zstrmac mac\ENDDO.MAC    mac\spm\ENDDO.MAC
call zstrmac mac\SELECT.MAC   mac\spm\SELECT.MAC
call zstrmac mac\WHEN.MAC     mac\spm\WHEN.MAC
call zstrmac mac\OTHRWISE.MAC mac\spm\OTHRWISE.MAC
call zstrmac mac\ENDSEL.MAC   mac\spm\ENDSEL.MAC
call zstrmac mac\CASENTRY.MAC mac\spm\CASENTRY.MAC
call zstrmac mac\CASE.MAC     mac\spm\CASE.MAC
call zstrmac mac\ENDCASE.MAC  mac\spm\ENDCASE.MAC
rem translate shared support files
call zstrmac mac\ASMMSP.CPY   mac\spm\ASMMSP.CPY
call zstrmac mac\ZSTRMAC.CPY  mac\spm\ZSTRMAC.CPY
call zstrmac mac\ZSTRGBL.CPY  mac\spm\ZSTRGBL.CPY
call zstrmac mac\ZSTREQU.MAC  mac\spm\ZSTREQU.MAC

