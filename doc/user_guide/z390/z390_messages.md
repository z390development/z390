# Java message inventory (z390 runtime)

Exhaustive list of messages emitted from `src/*.java`, one row per call site or
message-bearing assignment. Duplicates are retained. Variable parts are shown as
`{placeholder}` tokens.

| #   | Message text                                                                                                        | Source module |
|-----|---------------------------------------------------------------------------------------------------------------------|---------------|
|     | COPY file not found - {zc_copy_file_name}                                                                           | zc390.java    |
|     | Couldn't find file: {path}                                                                                          | gz390.java    |
|     | Couldn't find file: {path}                                                                                          | z390.java     |
|     | EXEC CICS statements require CICS option                                                                            | zc390.java    |
|     | EXEC statement missing END-EXEC                                                                                     | zc390.java    |
|     | EZ390E CLOSE FILES FAILED                                                                                           | sz390.java    |
|     | EZ390E CMDPROC SHUTDOWN FAILED                                                                                      | sz390.java    |
|     | EZ390E GUAM SHUTDOWN FAILED                                                                                         | sz390.java    |
|     | EZ390E TCPIO SHUTDOWN FAILED                                                                                        | sz390.java    |
|     | EZ390E abort recursive exit                                                                                         | sz390.java    |
|     | EZ390E close files failed                                                                                           | sz390.java    |
|     | I/O error opening copy file - {exception}                                                                           | zc390.java    |
|     | Invalid sequence ID in paragraph definition: {section_specification}                                                | zc390.java    |
|     | Inverted parentheses in paragraph definition: {section_specification}                                               | zc390.java    |
|     | LNK unknown command ignored - {lkd_cmd}                                                                             | lz390.java    |
|     | LZ390W warning - ignoring duplicate CSECT - {obj_esd_name}                                                          | lz390.java    |
|     | Missing close parenthesis in paragraph definition: {section_specification}                                          | zc390.java    |
|     | Missing equal sign in paragraph definition: {section_specification}                                                 | zc390.java    |
|     | Missing open parenthesis in paragraph definition: {section_specification}                                           | zc390.java    |
|     | Missing period after Division name: {section_specification}                                                         | zc390.java    |
|     | Missing period after Section name: {section_specification}                                                          | zc390.java    |
|     | OPEN ACB RRDS requires KEY rel rcd access                                                                           | vz390.java    |
|     | Permission to read files denied - continuing in demo mode                                                           | gz390.java    |
|     | Permission to read files denied - continuing in demo mode                                                           | z390.java     |
|     | REPLACE maximum replacement pairs (100) exceeded                                                                    | zc390.java    |
|     | REPLACE missing ==pseudo-text== after BY                                                                            | zc390.java    |
|     | REPLACE missing BY after ==pseudo-text==                                                                            | zc390.java    |
|     | S003 z390 log file close error - {exception}                                                                        | z390.java     |
|     | S075 z390 parm error - {args}                                                                                       | z390.java     |
|     | SZ390E error 15 batch log permission denied - aborting                                                              | z390.java     |
|     | Spurious period after Paragraph name: {section_specification}                                                       | zc390.java    |
|     | TCPIO close connection failed {exception}                                                                           | sz390.java    |
|     | TCPIO disconnect while switching state for conn={conn_index}                                                        | sz390.java    |
|     | TCPIO error checking conn msg available on conn={conn_index} - {exception}                                          | sz390.java    |
|     | TCPIO error closing client port {tcpio_port}                                                                        | sz390.java    |
|     | TCPIO error invalid operation {tcpio_op}                                                                            | sz390.java    |
|     | TCPIO error max client ports exceeded                                                                               | sz390.java    |
|     | TCPIO error on open get local host failed                                                                           | sz390.java    |
|     | TCPIO error open client get host failed                                                                             | sz390.java    |
|     | TCPIO error open client host not found {tcpio_host_ip_text}                                                         | sz390.java    |
|     | TCPIO error open client socket failed for port {tcpio_port}                                                         | sz390.java    |
|     | TCPIO error open server socket {exception}                                                                          | sz390.java    |
|     | TCPIO error receive port not found {tcpio_port}                                                                     | sz390.java    |
|     | TCPIO error send failed on port={tcpio_port}                                                                        | sz390.java    |
|     | TCPIO error send port not found {tcpio_port}                                                                        | sz390.java    |
|     | TCPIO error starting connection for port={tcp_server_port}                                                          | sz390.java    |
|     | TCPIO error storing message from conn={conn_index} - {exception}                                                    | sz390.java    |
|     | TCPIO error waiting for server message on any conn                                                                  | sz390.java    |
|     | TCPIO error waiting for server message on conn={tcpio_conn}                                                         | sz390.java    |
|     | TCPIO errpr semd failed for port={tcpio_port}                                                                       | sz390.java    |
|     | TCPIO open client port already open                                                                                 | sz390.java    |
|     | TCPIO open server failed - no ports available                                                                       | sz390.java    |
|     | TCPIO receive error msg length out of range {tcpio_lmsg}                                                            | sz390.java    |
|     | TCPIO receive error on client port={tcpio_port}                                                                     | sz390.java    |
|     | TCPIO send error msg length out of range {tcpio_lmsg}                                                               | sz390.java    |
|     | TCPIO server port alrady open                                                                                       | sz390.java    |
|     | TZ390E abort error {error} - {msg}                                                                                  | tz390.java    |
|     | TZ390E invalid option={option} ({opt_file_name}/{opt_file_line})                                                    | tz390.java    |
|     | TZ390E recursive abort exit                                                                                         | tz390.java    |
|     | TZ390E stats file close error - {exception}                                                                         | tz390.java    |
|     | TZ390E systerm file close error - {exception}                                                                       | tz390.java    |
|     | TZ390E thread sleep error - {exception}                                                                             | tz390.java    |
|     | Trailing data in paragraph definition: {section_specification}                                                      | zc390.java    |
|     | Unable to set version - property version not set                                                                    | tz390.java    |
|     | Unable to set version - unexpected exception                                                                        | tz390.java    |
|     | Unable to set version - version empty                                                                               | tz390.java    |
|     | Unable to set version - z390.properties file not found                                                              | tz390.java    |
|     | Unsupported option in Paragraph definition: {section_specification}                                                 | zc390.java    |
|     | VSAM AVL NODE HEIGHT ERROR                                                                                          | vz390.java    |
|     | VSAM AVL UNBALANCED KSIR ERROR TYPE {type}                                                                          | vz390.java    |
|     | VSAM EXCP READ ERROR PFX RECLEN = {hex} SFX RECLEN={hex}                                                            | vz390.java    |
|     | VSAM INVALID BWD XRBA={hex}RECLEN={hex}                                                                             | vz390.java    |
|     | VSAM INVALID EXCP READ PREV SFX RECLEN={hex}                                                                        | vz390.java    |
|     | VSAM KSIR BROKEN LINK {type} XRBA1={hex} XRBA1={hex}                                                                | vz390.java    |
|     | VSAM READ INVALID VREC PFX LREC = {hex} AT XRBA={hex}                                                               | vz390.java    |
|     | VSAM RPL FEEDBACK={hex} TYPE={type} REASON={reason}                                                                 | vz390.java    |
|     | Wildcard not allowed for Division name in: {section_specification}                                                  | zc390.java    |
|     | Wildcard not required for Paragraph name in: {section_specification}                                                | zc390.java    |
|     | az390 set symbol lock{desc}                                                                                         | az390.java    |
|     | copy ddname not found - {zc_copy_ddname}                                                                            | zc390.java    |
|     | division({sc_current_division}) section({sc_current_section}) paragraph({sc_current_paragraph}) invalid combination | zc390.java    |
|     | division({sc_current_division}) section({sc_current_section}) paragraph({sc_current_paragraph}) out of sequence     | zc390.java    |
|     | duplicate ascii/ebcdic conversion at - {Integer}/{Integer}                                                          | tz390.java    |
|     | invalid WS level number                                                                                             | zc390.java    |
|     | key search table exceeded adding {tot_cpz_file_name}                                                                | zc390.java    |
|     | maximum nested copy files exceeded                                                                                  | zc390.java    |
|     | maximum nested copy reps exceeded                                                                                   | zc390.java    |
|     | missing COPY member                                                                                                 | zc390.java    |
|     | only use lit ( in WS                                                                                                | zc390.java    |
|     | only use lit ) in WS                                                                                                | zc390.java    |
|     | split pseudo COPY replacement not supported                                                                         | zc390.java    |
|     | unknown COPY parm - {zc_next_token}                                                                                 | zc390.java    |
|     | z390 internal system exception abort {exception}                                                                    | z390.java     |
|     | z390 log file I/O error - use LOG command to open new log {exception}                                               | z390.java     |
|     | zc390 exception - {exception}                                                                                       | zc390.java    |
|     | zcob END_EXEC not preceeded by EXEC statement                                                                       | zc390.java    |
|     | zcob split literal format error                                                                                     | zc390.java    |
|     | zcobol cbl data name pattern errror - {exception}                                                                   | zc390.java    |
|     | zcobol cbl data token pattern errror - {exception}                                                                  | zc390.java    |
|     | zcobol cbl proc token pattern errror - {exception}                                                                  | zc390.java    |
|     | zcobol file I/O error - {exception}                                                                                 | zc390.java    |
|     | zcobol file close error {zc_file_name}                                                                              | zc390.java    |
|     | zcobol invalid ws level {ws_item_lvl}                                                                               | zc390.java    |
|     | zcobol level 01 must be in A field                                                                                  | zc390.java    |
|     | zcobol read error on CBL/CPY close file - {exception}                                                               | zc390.java    |
|     | zcobol read error on CBL/CPY file - {exception}                                                                     | zc390.java    |
|     | zcobol section definition errror - {exception}                                                                      | zc390.java    |
|     | zcobol: file not found - {zc_file_name}                                                                             | zc390.java    |
|     | zcobol: invalid file name - {lab_file_name}                                                                         | zc390.java    |
|     | zero length token parsing error                                                                                     | zc390.java    |
|   1 | dcc pattern errror - {exception}                                                                                    | az390.java    |
|   1 | extrn pattern errror - {exception}                                                                                  | az390.java    |
|   1 | label pattern errror - {exception}                                                                                  | az390.java    |
|   1 | log file open - {exception}                                                                                         | z390.java     |
|   1 | opcode tables out of sync - aborting                                                                                | tz390.java    |
|   1 | pch pattern error - {exception}                                                                                     | mz390.java    |
|   1 | var pattern errror - {exception}                                                                                    | mz390.java    |
|   2 | exec pattern error - {exception}                                                                                    | mz390.java    |
|   2 | expression pattern errror - {exception}                                                                             | az390.java    |
|   2 | opcode max type out of sync - {max_type} vs {max_op_type_offset}                                                    | tz390.java    |
|   2 | proto pattern error - {exception}                                                                                   | mz390.java    |
|   2 | write to log error - {exception}                                                                                    | z390.java     |
|   3 | I/O error on log file close - {exception}                                                                           | sz390.java    |
|   3 | I/O error on obj close - {exception}                                                                                | lz390.java    |
|   3 | label pattern error - {exception}                                                                                   | mz390.java    |
|   3 | opcode total out of sync - aborting                                                                                 | tz390.java    |
|   4 | I/O error on lst close - {exception}                                                                                | lz390.java    |
|   4 | I/O error on obj open - {exception}                                                                                 | az390.java    |
|   4 | I/O error on prn open - {exception}                                                                                 | az390.java    |
|   4 | MNOTE 4,'Duplicate USING ranges found for - {use_reg} and {cur_use_reg} using highest'                              | az390.java    |
|   4 | expression pattern error - {exception}                                                                              | mz390.java    |
|   4 | invalid input file option - {args}                                                                                  | tz390.java    |
|   5 | max errors exceeded                                                                                                 | lz390.java    |
|   5 | max errors exceeded                                                                                                 | sz390.java    |
|   5 | missing file option                                                                                                 | tz390.java    |
|   5 | signed decimal pattern error - {exception}                                                                          | mz390.java    |
|   6 | I/O error on bal open - {exception}                                                                                 | az390.java    |
|   6 | I/O error on log file write msg - {msg}                                                                             | sz390.java    |
|   6 | binary digits pattern error - {exception}                                                                           | mz390.java    |
|   7 | I/O error on BAL file close {exception}                                                                             | az390.java    |
|   7 | command error on -{cmd_line}                                                                                        | gz390.java    |
|   7 | decimal digits pattern error - {exception}                                                                          | mz390.java    |
|   8 | I/O error on BAL open - {exception}                                                                                 | mz390.java    |
|   8 | continuation line < {bal_ictl_cont} characters - {temp_line}                                                        | az390.java    |
|   8 | font outside fixed width font limits                                                                                | gz390.java    |
|   8 | hexadecimal digits pattern error - {exception}                                                                      | mz390.java    |
|   9 | I/O error on file read {exception}                                                                                  | az390.java    |
|   9 | I/O error on log file open - {exception}                                                                            | sz390.java    |
|   9 | I/O error on lst open - {exception}                                                                                 | lz390.java    |
|  10 | maximum errors exceeded                                                                                             | z390.java     |
|  10 | maximum symbol table size exceeded                                                                                  | az390.java    |
|  10 | systerm file open error {exception}                                                                                 | tz390.java    |
|  11 | ABEND PSW={psw} {instruction_hex} {instruction_name} ABEND {abend_code}                                             | sz390.java    |
|  11 | I/O error on systerm file {exception}                                                                               | tz390.java    |
|  11 | continuation line < {mac_ictl_cont} characters - {temp_line}                                                        | mz390.java    |
|  11 | invalid operator class for - {exp_prev_op}                                                                          | az390.java    |
|  12 | I/O error on systerm file {exception}                                                                               | tz390.java    |
|  12 | execution startup error {exception}                                                                                 | gz390.java    |
|  12 | invalid operator class - {exp_op}                                                                                   | az390.java    |
|  12 | program aborting due to abend {abend_code}                                                                          | sz390.java    |
|  13 | I/O error on BAL file close {exception}                                                                             | lz390.java    |
|  13 | I/O error on BAL write - {exception}                                                                                | mz390.java    |
|  13 | expression parsing error                                                                                            | az390.java    |
|  13 | find parm pattern errror - {exception}                                                                              | tz390.java    |
|  13 | replace " parm pattern errror - {exception}                                                                         | tz390.java    |
|  13 | replace "" parm pattern errror - {exception}                                                                        | tz390.java    |
|  13 | replace & parm pattern errror - {exception}                                                                         | tz390.java    |
|  13 | replace && parm pattern errror - {exception}                                                                        | tz390.java    |
|  13 | replace ' parm pattern errror - {exception}                                                                         | tz390.java    |
|  13 | replace '' parm pattern errror - {exception}                                                                        | tz390.java    |
|  13 | replace - parm pattern errror - {exception}                                                                         | tz390.java    |
|  13 | replace / parm pattern errror - {exception}                                                                         | tz390.java    |
|  13 | replace \ parm pattern errror - {exception}                                                                         | tz390.java    |
|  14 | I/O error on obj read - {exception}                                                                                 | lz390.java    |
|  14 | parm pattern errror - {exception}                                                                                   | tz390.java    |
|  15 | Permission to write log in current directory denied                                                                 | z390.java     |
|  15 | getmain for svc 8 load failed - PGM={pgm_name}LEN={load_code_len}                                                   | sz390.java    |
|  15 | maximum global ESDS exceeded                                                                                        | lz390.java    |
|  15 | trace file close failed {exception}                                                                                 | tz390.java    |
|  16 | editor not found - {z390_editor}                                                                                    | z390.java     |
|  16 | trace file open failed - {exception}                                                                                | tz390.java    |
|  16 | {mac_name} undefined {bal_parms_fragment}                                                                           | mz390.java    |
|  17 | Permission for file execute denied                                                                                  | z390.java     |
|  17 | expression parsing error                                                                                            | az390.java    |
|  17 | ignoring duplicate ENTRY - {obj_esd_name}                                                                           | lz390.java    |
|  17 | invalid set variable type for - {bal_label}                                                                         | mz390.java    |
|  17 | trace file write error {exception}                                                                                  | tz390.java    |
|  18 | I/O error on 390 load module file - {exception}                                                                     | sz390.java    |
|  18 | expression error                                                                                                    | mz390.java    |
|  18 | expression parsing error                                                                                            | az390.java    |
|  18 | maximum obj files exceeded                                                                                          | lz390.java    |
|  18 | maximum trace file size exceeded                                                                                    | tz390.java    |
|  19 | I/O error on stats file {exception}                                                                                 | tz390.java    |
|  19 | exp variable stack exceeded                                                                                         | mz390.java    |
|  19 | no csect object code text defined for load module {obj_file_name}                                                   | lz390.java    |
|  19 | start editor failed - {z390_editor}                                                                                 | z390.java     |
|  20 | dcb tiot index invalid DCB={hex}                                                                                    | sz390.java    |
|  20 | stack missing setc value                                                                                            | mz390.java    |
|  20 | stack operation size exceeded                                                                                       | az390.java    |
|  20 | stats file open error {exception}                                                                                   | tz390.java    |
|  20 | unknown obj record type - {obj_line}                                                                                | lz390.java    |
|  21 | expression parsing error                                                                                            | az390.java    |
|  21 | ignoring {opt_file_name} as it has already been processed as an option file, referenced in {optfilenames}           | tz390.java    |
|  21 | invalid options - {invalid_options}                                                                                 | tz390.java    |
|  21 | key search table exceeded                                                                                           | sz390.java    |
|  21 | maximum tiot files open exceeded                                                                                    | sz390.java    |
|  21 | z390 rld table exceeded                                                                                             | lz390.java    |
|  22 | CD missing directory                                                                                                | z390.java     |
|  22 | I/O error on z390 load module file - {exception}                                                                    | lz390.java    |
|  22 | VSAM DCB/TIOT Corrupted                                                                                             | vz390.java    |
|  22 | internal error - invalid case index                                                                                 | tz390.java    |
|  22 | max op type offset and max op type mask tables out of sync {max_op_type_offset} vs {max_op_type_mask}               | pz390.java    |
|  22 | max op type offset tables out of sync {max_op_type_offset} vs {max_op_type_offset}                                  | pz390.java    |
|  22 | max op type setup cases out of sync {max_op_type_offset} vs {max_op_type_setup}                                     | pz390.java    |
|  22 | maximum stack variables exceeded                                                                                    | az390.java    |
|  22 | op code and op trace type tables out of sync {length} vs {length}                                                   | pz390.java    |
|  23 | I/O error on stats file {exception}                                                                                 | tz390.java    |
|  23 | Permission for CD change directory denied                                                                           | z390.java     |
|  23 | TCPIO free conn internal error - aborting                                                                           | sz390.java    |
|  23 | expression error                                                                                                    | mz390.java    |
|  23 | internal system exception - {exception}                                                                             | lz390.java    |
|  23 | undefined svc - {svc_id}                                                                                            | sz390.java    |
|  23 | wtor reply error                                                                                                    | gz390.java    |
|  24 | COPYLOG not available in command mode                                                                               | z390.java     |
|  24 | I/O error on obj close - {exception}                                                                                | az390.java    |
|  24 | I/O error on prn close - {exception}                                                                                | az390.java    |
|  24 | I/O error on stats file close {exception}                                                                           | tz390.java    |
|  24 | time limit exceeded                                                                                                 | lz390.java    |
|  24 | undefined macro variable - {exp_token}                                                                              | mz390.java    |
|  25 | invalid object code offset - {obj_line} in {obj_file_name}                                                          | lz390.java    |
|  25 | invalid symbol for length attribute operator                                                                        | az390.java    |
|  25 | invalid symbol for scale attribute operator                                                                         | az390.java    |
|  25 | macro label not found - {label_name}                                                                                | mz390.java    |
|  25 | option FLOAT must be DECIMAL, BINARY, or HEX                                                                        | tz390.java    |
|  26 | I/O error opening file - {exception}                                                                                | mz390.java    |
|  26 | NOASM requires CHKMAC(0)                                                                                            | tz390.java    |
|  26 | entry csect not found for - {obj_esd_name}                                                                          | lz390.java    |
|  26 | missing symbol for integer attribute                                                                                | az390.java    |
|  26 | missing symbol for length attribute                                                                                 | az390.java    |
|  27 | NOASM requires CHKSRC(0-2)                                                                                          | tz390.java    |
|  27 | location counter undefined                                                                                          | az390.java    |
|  27 | max macros exceeded                                                                                                 | mz390.java    |
|  27 | unresolved external reference - {gbl_esd_name}                                                                      | lz390.java    |
|  28 | I/O error on OBJ file write - {exception}                                                                           | az390.java    |
|  28 | invalid 8 byte RLD field at offset {obj_line}                                                                       | lz390.java    |
|  28 | option SectionSize must be 8, 16, 32, 64, 128, 256, or 512                                                          | tz390.java    |
|  29 | ERRSUM missing macro = {bal_op}                                                                                     | az390.java    |
|  29 | I/O error on file read {exception}                                                                                  | mz390.java    |
|  29 | Partial Sums Number should be at least 1                                                                            | tz390.java    |
|  29 | Partial Sums Number should not exceed Section Size                                                                  | tz390.java    |
|  29 | invalid object text esd - {obj_text_esd} in {obj_file_name}                                                         | lz390.java    |
|  30 | ERRSUM requires option ASM                                                                                          | tz390.java    |
|  30 | invalid rld esd - {obj_rld_esd} in {obj_file_name}                                                                  | lz390.java    |
|  30 | max level of nested macros exceeded                                                                                 | mz390.java    |
|  30 | option VSAM must be 0, 1 or 2                                                                                       | tz390.java    |
|  31 | read at end of file and no EODAD for {tiot_ddnam}                                                                   | sz390.java    |
|  31 | unexpected parm value after pos parm value                                                                          | mz390.java    |
|  31 | {msg}                                                                                                               | tz390.java    |
|  32 | invalid absolute value                                                                                              | az390.java    |
|  32 | maximum 390 file size exceeded                                                                                      | lz390.java    |
|  33 | invalid relative value                                                                                              | az390.java    |
|  33 | maximum 390 file size exceeded                                                                                      | lz390.java    |
|  34 | I/O error on BAL close - {exception}                                                                                | mz390.java    |
|  34 | maximum lst file size exceeded                                                                                      | lz390.java    |
|  35 | expression parsing error - total stack values={tot_exp_stk_var} total ops={tot_exp_stk_op}                          | mz390.java    |
|  35 | expression parsing error                                                                                            | az390.java    |
|  35 | invalid obj file {file}                                                                                             | lz390.java    |
|  36 | invalid hex value - {cmd_parm}                                                                                      | z390.java     |
|  36 | invalid object record type - {text} in {obj_file_name}                                                              | lz390.java    |
|  37 | directory not found - {new_dir}                                                                                     | z390.java     |
|  37 | directory not selected                                                                                              | z390.java     |
|  37 | file not selected                                                                                                   | z390.java     |
|  37 | invalid ESD type in {obj_file_name}                                                                                 | lz390.java    |
|  37 | invalid expression variable - {exp_token}                                                                           | mz390.java    |
|  38 | Permision for directory selection denied                                                                            | z390.java     |
|  38 | expression parsing error - prev op ={exp_prev_first} next op ={exp_next_first}                                      | mz390.java    |
|  38 | object file truncated - {obj_file_name}                                                                             | lz390.java    |
|  38 | object file truncated {obj_file_name}                                                                               | lz390.java    |
|  39 | LNK command file I/O error {exception}                                                                              | lz390.java    |
|  39 | Permission for file selection denied                                                                                | z390.java     |
|  39 | file not found - {load_file_name}                                                                                   | mz390.java    |
|  40 | Java Permissions denied due to z390 /NP command option                                                              | z390.java     |
|  40 | MOD file cannot contain RLD's ={tot_rld}                                                                            | lz390.java    |
|  40 | Missing colon in instruction format definition {opcode_formats}                                                     | tz390.java    |
|  40 | Missing colon in optable option definition {entry}                                                                  | tz390.java    |
|  40 | Missing comma in first half of instruction format definition {opcode_formats}                                       | tz390.java    |
|  40 | Missing equal sign in machine option definition {entry}                                                             | tz390.java    |
|  40 | Missing equal sign in optable option definition {entry}                                                             | tz390.java    |
|  40 | Missing equal-sign in mask definition {opcode_masks}                                                                | tz390.java    |
|  40 | invalid index register                                                                                              | az390.java    |
|  40 | invalid macro label - {mac_label}                                                                                   | mz390.java    |
|  41 | Base optable not defined for optable definition {entry}                                                             | tz390.java    |
|  41 | Error in instruction format definition {opcode_formats} - {exception}                                               | tz390.java    |
|  41 | Error in machine option definition {machine_optable_equivalence} - {exception}                                      | tz390.java    |
|  41 | Error in mask definition {opcode_masks} - {exception}                                                               | tz390.java    |
|  41 | Error in optable option definition {optable_optable_equivalence} - {exception}                                      | tz390.java    |
|  41 | LNK ENTRY {lkd_entry} NOT FOUND                                                                                     | lz390.java    |
|  41 | Optable not defined for machine definition {entry}                                                                  | tz390.java    |
|  41 | Start error for {url}                                                                                               | z390.java     |
|  41 | invalid register value                                                                                              | az390.java    |
|  41 | invalid vector register value                                                                                       | az390.java    |
|  41 | unknown java version {java_vendor} {java_version}                                                                   | lz390.java    |
|  42 | Missing equal-sign in mask definition {opcode_masks_short}                                                          | tz390.java    |
|  42 | invalid 2 byte RLD offset over 64k                                                                                  | lz390.java    |
|  42 | invalid 3 byte RLD offset over 16 MB                                                                                | lz390.java    |
|  42 | invalid byte value                                                                                                  | az390.java    |
|  42 | invalid expression                                                                                                  | az390.java    |
|  42 | invalid halfword value                                                                                              | az390.java    |
|  42 | invalid immediate integer bit count {bits}                                                                          | az390.java    |
|  42 | invalid immediate integer bits = {bits} value ={exp_val}                                                            | az390.java    |
|  42 | invalid relative offset bits {bits}                                                                                 | az390.java    |
|  42 | invalid relative offset expression                                                                                  | az390.java    |
|  43 | Error in mask definition {opcode_masks_short} - {exception}                                                         | tz390.java    |
|  43 | I/O error for DCB={hex} DDNAME={cur_ddnam} FILE={cur_file}                                                          | sz390.java    |
|  43 | invalid dc duplication factor                                                                                       | az390.java    |
|  43 | maximum local variables exceeded                                                                                    | mz390.java    |
|  44 | Missing equal-sign in opcode definition {op_tables}                                                                 | tz390.java    |
|  44 | invalid dc type delimiter                                                                                           | az390.java    |
|  44 | lcla size out of range {set_name}({new_size})                                                                       | mz390.java    |
|  44 | maximum stack operations exceeded                                                                                   | mz390.java    |
|  45 | Missing first comma in opcode definition {op_tables}                                                                | tz390.java    |
|  45 | invalid dc delimiter for type - {dc_field_fragment}                                                                 | az390.java    |
|  45 | lclb size out of range {set_name}({new_size})                                                                       | mz390.java    |
|  46 | LNK INCLUDE NOT FOUND - {lkd_cmd}                                                                                   | lz390.java    |
|  46 | Missing second comma in opcode definition {op_tables}                                                               | tz390.java    |
|  46 | lclc size out of range {set_name}({new_size})                                                                       | mz390.java    |
|  47 | Error in opcode definition {op_tables} - {exception}                                                                | tz390.java    |
|  47 | LNK ALIAS CREATE FAILED FOR - {alias}                                                                               | lz390.java    |
|  47 | macro operation not supported - {bal_op}                                                                            | mz390.java    |
|  48 | Invalid number {optype} in opcode definition {op_tables} - {error_msg}                                              | tz390.java    |
|  48 | LNK INCLUDE SYNTAX ERROR - {lkd_cmd}                                                                                | lz390.java    |
|  48 | lcl seta sub out of range - {lcl_set_name}({expand_sub})                                                            | mz390.java    |
|  49 | Specified optype {optype} out of range in opcode definition {op_tables}                                             | tz390.java    |
|  49 | lcl setb sub out of range - {lcl_set_name}({expand_sub})                                                            | mz390.java    |
|  49 | max errors exceeded                                                                                                 | az390.java    |
|  50 | Illegal hex digit in opcode {opcode} in opcode definition {op_tables}                                               | tz390.java    |
|  50 | lcl setc sub out of range - {lcl_set_name}({expand_sub})                                                            | mz390.java    |
|  50 | missing immediate data parm                                                                                         | z390.java     |
|  50 | missing operand comma - {exp_text_fragment}                                                                         | az390.java    |
|  51 | Invalid number {tracetype} in opcode definition {op_tables} - {exception}                                           | tz390.java    |
|  51 | command error on -{cmd_line}                                                                                        | z390.java     |
|  51 | invalid dc type - {dc_field_fragment}                                                                               | az390.java    |
|  51 | invalid substring subscripts                                                                                        | mz390.java    |
|  52 | Specified tracetype {tracetype} out of range in opcode definition {op_tables}                                       | tz390.java    |
|  52 | invalid dc character literal - {dc_field_fragment}                                                                  | az390.java    |
|  52 | invalid install directory - {install_loc}                                                                           | z390.java     |
|  52 | invalid substring expression                                                                                        | mz390.java    |
|  53 | Conflicting mask indicators in opcode definition {op_tables}                                                        | tz390.java    |
|  53 | Non-matching mask positions in opcode definition {op_tables}                                                        | tz390.java    |
|  53 | expression type error                                                                                               | mz390.java    |
|  53 | invalid equ expression                                                                                              | az390.java    |
|  54 | interactive command not supported in batch                                                                          | z390.java     |
|  54 | interactive editor not supported in batch                                                                           | z390.java     |
|  54 | invalid dc field terminator ={dc_field} len={dc_field} char={dc_field}                                              | az390.java    |
|  54 | invalid subscripted variable                                                                                        | mz390.java    |
|  54 | {entry} allowable mask values not specified for masked opcode definition {op_tables}                                | tz390.java    |
|  54 | {entry} mask value {hex_digit} is not valid for opcode definition {op_tables}                                       | tz390.java    |
|  54 | {entry} unsupported overrides for non-masked opcode definition {op_tables}                                          | tz390.java    |
|  55 | CDE maximum 390 load modules exceeded                                                                               | sz390.java    |
|  55 | Missing equal-sign in first override in opcode definition {op_tables}                                               | tz390.java    |
|  55 | invalid register expression - {exp_val}                                                                             | az390.java    |
|  55 | invalid vector register expression - {exp_val}                                                                      | az390.java    |
|  55 | invalid vector register number 1-4                                                                                  | az390.java    |
|  55 | maximum global variables exceeded                                                                                   | mz390.java    |
|  56 | Missing equal-sign in second override in opcode definition {op_tables}                                              | tz390.java    |
|  56 | gbla size out of range {set_name}({new_size})                                                                       | mz390.java    |
|  56 | incorrect register specification - R3 must not equal R1                                                             | az390.java    |
|  56 | incorrect register specification - R3 must not equal R2                                                             | az390.java    |
|  56 | incorrect register specification - even register required - {exp_val}                                               | az390.java    |
|  56 | incorrect register specification - register 0 not allowed - {exp_val}                                               | az390.java    |
|  56 | incorrect register specification - {exp_val}                                                                        | az390.java    |
|  56 | test error in expression pattern - {exception}                                                                      | sz390.java    |
|  57 | More than two overrides found in opcode definition {op_tables}                                                      | tz390.java    |
|  57 | gblb size out of range {set_name}({new_size})                                                                       | mz390.java    |
|  57 | literal table size exceeded                                                                                         | az390.java    |
|  57 | test input file for ddname {test_ddname} not found - {test_file_name}                                               | sz390.java    |
|  58 | GUAM GUI startup abort                                                                                              | sz390.java    |
|  58 | GUAM GUI window closed                                                                                              | gz390.java    |
|  58 | gblc size out of range {set_name}({new_size})                                                                       | mz390.java    |
|  58 | invalid rld multiplication - {exp_text_fragment}                                                                    | az390.java    |
|  59 | GUAM GUI tput external abort                                                                                        | sz390.java    |
|  59 | GUAM GUI tput length too long                                                                                       | sz390.java    |
|  59 | gbl seta sub out of range - {gbl_set_name}({expand_sub})                                                            | mz390.java    |
|  59 | invalid rld division - {exp_text_fragment}                                                                          | az390.java    |
|  60 | GUAM GUI tget abort                                                                                                 | sz390.java    |
|  60 | invalid rld division - {exp_text_fragment}                                                                          | az390.java    |
|  60 | maximum macros exceeded for - {mac_op}                                                                              | mz390.java    |
|  61 | GUAM GUI svc abort                                                                                                  | sz390.java    |
|  61 | gbl setb sub out of range - {gbl_set_name}({expand_sub})                                                            | mz390.java    |
|  61 | invalid complex rld expression: {exp_text_fragment}                                                                 | az390.java    |
|  62 | GUAM GUI put_log abort                                                                                              | sz390.java    |
|  62 | ddname={ddname} not found                                                                                           | mz390.java    |
|  62 | unsupported operation code {bal_op}                                                                                 | az390.java    |
|  63 | expression compare error                                                                                            | mz390.java    |
|  63 | font outside fixed width font limits                                                                                | z390.java     |
|  63 | guam wtor reply abort                                                                                               | sz390.java    |
|  64 | invalid window location                                                                                             | z390.java     |
|  65 | gbl setc sub out of range - {gbl_set_name}({expand_sub})                                                            | mz390.java    |
|  65 | invalid binary dc data {dc_field_fragment}                                                                          | az390.java    |
|  65 | invalid window size request                                                                                         | z390.java     |
|  65 | test loop detected - aborting                                                                                       | sz390.java    |
|  66 | CMD task startup error rc = {rc}                                                                                    | z390.java     |
|  66 | exec execution interruption error{exception}                                                                        | z390.java     |
|  66 | execution startup error - {exception}                                                                               | sz390.java    |
|  66 | execution startup error - {exception}                                                                               | z390.java     |
|  66 | execution startup error {exception}                                                                                 | ez390.java    |
|  66 | execution startup error {exception}                                                                                 | z390.java     |
|  66 | invalid floating point data field                                                                                   | az390.java    |
|  66 | syslist reference only allowed in macro                                                                             | mz390.java    |
|  67 | A' missing variable                                                                                                 | mz390.java    |
|  67 | CMD task startup error rc = {rc}                                                                                    | sz390.java    |
|  67 | exec execution output error                                                                                         | z390.java     |
|  67 | invalid character in P type data field - {dc_field}                                                                 | az390.java    |
|  67 | invalid character in Z type data field - {dc_field}                                                                 | az390.java    |
|  68 | P type field too long - {dc_field}                                                                                  | az390.java    |
|  68 | execution input error{exception}                                                                                    | z390.java     |
|  68 | previous command execution cancelled                                                                                | sz390.java    |
|  69 | CMD command timeout error - command aborted                                                                         | z390.java     |
|  69 | I/O error on AREAD file ID={index} close - {exception}                                                              | mz390.java    |
|  69 | field 1 hex length > 16 = {hex_ll}                                                                                  | az390.java    |
|  69 | previous command execution ended with rc ={rc}                                                                      | sz390.java    |
|  70 | execution startup error {exception}                                                                                 | sz390.java    |
|  70 | field 2 hex length > 16 = {hex_ll}                                                                                  | az390.java    |
|  70 | field 2 length = {l2} must be less than field 1 length = {l1}                                                       | az390.java    |
|  70 | field 2 length = {l2} must be no more than 8                                                                        | az390.java    |
|  70 | previous command execution cancelled                                                                                | z390.java     |
|  71 | I/O error on AREAD file read - {exception}                                                                          | mz390.java    |
|  71 | execution input error{exception}                                                                                    | sz390.java    |
|  71 | missing opcode - {bal_line}                                                                                         | az390.java    |
|  71 | previous command execution ended with rc ={rc}                                                                      | z390.java     |
|  72 | aread past end of inline AREAD data                                                                                 | mz390.java    |
|  72 | duplicate symbol {sym_name} on line {bal_line_num} and {bal_line_num}                                               | az390.java    |
|  72 | startup file I/O error - {exception}                                                                                | z390.java     |
|  73 | cmd process output error - {exception}                                                                              | sz390.java    |
|  73 | exec execution output error                                                                                         | z390.java     |
|  73 | invalid punch parm {pch_parms}                                                                                      | mz390.java    |
|  74 | exec execution output error                                                                                         | sz390.java    |
|  74 | missing log command file or OFF parm                                                                                | z390.java     |
|  75 | CMD command timeout error {monitor_cmd_time_total} > {max_time_seconds}                                             | ez390.java    |
|  75 | I/O error on PUNCH open - {exception}                                                                               | mz390.java    |
|  75 | previous command execution cancelled                                                                                | z390.java     |
|  76 | I/O error on PUNCH file write - {exception}                                                                         | mz390.java    |
|  76 | cmd read I/O error - {exception}                                                                                    | sz390.java    |
|  76 | relative offset not in same esd                                                                                     | az390.java    |
|  77 | I/O error on PUNCH file ID={index} close - {exception}                                                              | mz390.java    |
|  77 | Wait interrupted {exception}                                                                                        | z390.java     |
|  77 | cmd process output queue io error                                                                                   | sz390.java    |
|  77 | invalid hex code {hex_code}                                                                                         | az390.java    |
|  78 | aborting due to external shutdown request                                                                           | z390.java     |
|  78 | invalid hex dc data {dc_field_fragment}                                                                             | az390.java    |
|  78 | missing NOT operand                                                                                                 | mz390.java    |
|  79 | cmd proc wait error {exception}                                                                                     | sz390.java    |
|  79 | internal system exception - {exception}                                                                             | az390.java    |
|  79 | missing AND operand                                                                                                 | mz390.java    |
|  80 | invalid DCB address or ID at DCB=({hex})={pz390}                                                                    | sz390.java    |
|  80 | missing OR operand                                                                                                  | mz390.java    |
|  80 | time limit exceeded                                                                                                 | az390.java    |
|  81 | invalid field value 0-15 {exp_val}                                                                                  | az390.java    |
|  81 | missing XOR operand                                                                                                 | mz390.java    |
|  82 | DDNAME={ddname} not defined                                                                                         | sz390.java    |
|  82 | actr limit exceeded                                                                                                 | mz390.java    |
|  82 | invalid field                                                                                                       | az390.java    |
|  83 | DSNAME invalid field at {hex}                                                                                       | sz390.java    |
|  83 | maximum errors exceeded                                                                                             | mz390.java    |
|  83 | maximum source lines exceeded                                                                                       | az390.java    |
|  84 | internal system exception - {exception}                                                                             | mz390.java    |
|  85 | file size exceeds available memory                                                                                  | sz390.java    |
|  85 | invalid sublist index - {sublist_index}                                                                             | mz390.java    |
|  86 | svc 8 I/O error reading RLD                                                                                         | sz390.java    |
|  86 | time limit exceeded                                                                                                 | mz390.java    |
|  87 | key search table exceeded                                                                                           | az390.java    |
|  87 | maximum source lines exceeded                                                                                       | mz390.java    |
|  87 | no program or file name found                                                                                       | sz390.java    |
|  87 | opcode key search table exceeded                                                                                    | az390.java    |
|  88 | delete cde system error                                                                                             | sz390.java    |
|  88 | invalid data field expression - {dc_field}                                                                          | az390.java    |
|  88 | unknown java version {java_vendor} {java_version}                                                                   | az390.java    |
|  89 | floating point value out of range                                                                                   | az390.java    |
|  89 | lcl key search table exceeded                                                                                       | mz390.java    |
|  90 | LOCTR must follow CSECT or DSECT                                                                                    | az390.java    |
|  90 | duplicate positional parm - {pos_parm_name}                                                                         | mz390.java    |
|  91 | csect start change error - {sym_name} old start={hex} new start={hex}                                               | az390.java    |
|  91 | duplicate keyword parm definition - {kwd_parm_name}                                                                 | mz390.java    |
|  92 | csect end change error - {sym_name} old end ={hex} new end ={hex}                                                   | az390.java    |
|  93 | dsect end change error - {sym_name} old end ={hex} new end ={hex}                                                   | az390.java    |
|  93 | wtor reply I/O error - {exception}                                                                                  | ez390.java    |
|  94 | invalid string operator - {exp_prev_op}                                                                             | mz390.java    |
|  94 | loctr section start change error - {sym_name} old start={hex} new start={hex}                                       | az390.java    |
|  94 | undefined GUAM GUI Window command - {guam_minor}                                                                    | sz390.java    |
|  95 | loctr section end change error - {sym_name} old end ={hex} new end ={hex}                                           | az390.java    |
|  95 | undefined GUAM GUI Screen command - {guam_minor}                                                                    | sz390.java    |
|  96 | maximum esds exceeded                                                                                               | az390.java    |
|  96 | undefined GUAM GUI Graph command - {guam_minor}                                                                     | sz390.java    |
|  97 | invalid entry type symbol - {sym_name}                                                                              | az390.java    |
|  97 | undefined GUAM GUI Keyboard command - {guam_minor}                                                                  | sz390.java    |
|  98 | symbol not found - {exp_token}                                                                                      | az390.java    |
|  98 | undefined GUAM GUI Mouse command - {guam_minor}                                                                     | sz390.java    |
|  99 | invalid length for S type                                                                                           | az390.java    |
|  99 | undefined GUAM GUI Sound command - {guam_minor}                                                                     | sz390.java    |
| 100 | ESTAE abort due to invalid return code = {estae_exit_rc}                                                            | sz390.java    |
| 100 | maximum active using table exceeded                                                                                 | az390.java    |
| 100 | maximum nested copy files exceeded                                                                                  | mz390.java    |
| 101 | invalid register expression - {exp_text}                                                                            | az390.java    |
| 101 | maximum errors exceeded                                                                                             | gz390.java    |
| 101 | maximum file size exceeded for {tiot_dsn}                                                                           | sz390.java    |
| 101 | missing copy = {mac_parms}                                                                                          | mz390.java    |
| 102 | GUAM GUI terminating due to external shutdown request                                                               | gz390.java    |
| 102 | maximum file size exceeded for {tiot_dsn}                                                                           | sz390.java    |
| 102 | org expression must be in same section                                                                              | az390.java    |
| 102 | start expression must be absolute                                                                                   | az390.java    |
| 102 | xctl program not found - {load_pgm_name}                                                                            | sz390.java    |
| 103 | LINK program not found - {load_pgm_name}                                                                            | sz390.java    |
| 103 | maximum file size exceeded for {tiot_dsn}                                                                           | sz390.java    |
| 103 | missing end of range value                                                                                          | az390.java    |
| 103 | missing variable for created set variable                                                                           | mz390.java    |
| 103 | rld table exceeded                                                                                                  | az390.java    |
| 103 | tn3270 ra addr error                                                                                                | gz390.java    |
| 104 | GUAM GUI option not specified - aborting                                                                            | sz390.java    |
| 104 | maximum file size exceeded for {tiot_dsn}                                                                           | sz390.java    |
| 104 | missing domain for using                                                                                            | az390.java    |
| 104 | set/parm variable conflict - {text_fragment}                                                                        | mz390.java    |
| 104 | tget input buffer overrun                                                                                           | gz390.java    |
| 105 | invalid dc data terminator - {dc_field_fragment}                                                                    | az390.java    |
| 105 | maximum file size exceeded for {tiot_dsn}                                                                           | sz390.java    |
| 105 | syntax error at {text_fragment}                                                                                     | mz390.java    |
| 105 | tput read past end of buffer                                                                                        | gz390.java    |
| 106 | invalid dc data terminator - {dc_field_fragment}                                                                    | az390.java    |
| 106 | maximum file size exceeded for {tiot_dsn}                                                                           | sz390.java    |
| 106 | set local/global conflict for - {text_fragment}                                                                     | mz390.java    |
| 106 | tn3270 command buffer overrun                                                                                       | gz390.java    |
| 107 | invalid data field terminator - {dc_field}                                                                          | az390.java    |
| 107 | maximum log file size exceeded                                                                                      | sz390.java    |
| 107 | set type conflict for - {text_fragment}                                                                             | mz390.java    |
| 108 | invalid data field terminator - {dc_field}                                                                          | az390.java    |
| 108 | tget/tput fullscr type requires GUAM option                                                                         | sz390.java    |
| 108 | wtor already running                                                                                                | gz390.java    |
| 109 | invalid data field terminator - {dc_field}                                                                          | az390.java    |
| 109 | invalid dc data terminator - {dc_field_fragment}                                                                    | az390.java    |
| 109 | quitting test mode                                                                                                  | sz390.java    |
| 110 | GUAM GUI Keyboard read wait exception -{exception}                                                                  | gz390.java    |
| 110 | literal not found - {exp_token}                                                                                     | az390.java    |
| 110 | {mac_name} maximum macro labels exceeded                                                                            | mz390.java    |
| 111 | invalid literal token                                                                                               | az390.java    |
| 111 | invalid tn3270 sfe count {count}                                                                                    | gz390.java    |
| 111 | {(mac_name)} duplicate {mac_label} at {mac_lab_num}                                                                 | mz390.java    |
| 112 | invalid tn3270 sfe type code {hex}                                                                                  | gz390.java    |
| 112 | relate target address odd - {hex}                                                                                   | az390.java    |
| 112 | relative target address odd - {hex}                                                                                 | az390.java    |
| 112 | unrecognized floating point constant {fp_text}                                                                      | az390.java    |
| 112 | {mac_name} invalid AGO label - {mac_parms}                                                                          | mz390.java    |
| 113 | tget input buffer overrun                                                                                           | gz390.java    |
| 113 | {mac_name} invalid AIF label - {mac_parms}                                                                          | mz390.java    |
| 114 | invalid put ascii text address for msg = {text} LEN={mem_len}ADDR={hex}                                             | sz390.java    |
| 114 | {mac_name} invalid AIF label - {mac_parms}                                                                          | mz390.java    |
| 115 | WTOR REPLY FLUSH I/O ERROR                                                                                          | sz390.java    |
| 116 | CMD MSG MAXQUE EXCEEDED - COPYING ALL CMD OUTPUT TO LOG                                                             | sz390.java    |
| 116 | invalid ascii source line {line_number} in {path}                                                                   | az390.java    |
| 117 | missing continue source line {line_number} in {path}                                                                | az390.java    |
| 117 | z390 abort request from CMD ID={cmd_id}                                                                             | sz390.java    |
| 118 | 1 abort on add opcode {op_name} (index={index})                                                                     | tz390.java    |
| 118 | 2 abort on duplicate opcode {op_name} (index={index})                                                               | tz390.java    |
| 118 | 3 abort on duplicate opcode {op_name} (index={index})                                                               | tz390.java    |
| 118 | 4 abort on duplicate opcode {op_name} (index={index})                                                               | tz390.java    |
| 118 | 5 abort on add opcode {op_name} (index={index})                                                                     | tz390.java    |
| 118 | 6 abort on duplicate opcode {op_name} (index={index})                                                               | tz390.java    |
| 118 | ALIAS LOAD FAILED FOR {load_pgm_name}                                                                               | sz390.java    |
| 118 | invalid ascii source line {line_number} in {path}                                                                   | az390.java    |
| 118 | maximum prn file size exceeded                                                                                      | az390.java    |
| 118 | opcode key table error - aborting                                                                                   | mz390.java    |
| 119 | invalid RLD length in load module -{pgm_name} at offset {hex}                                                       | sz390.java    |
| 119 | macro index table exceeded                                                                                          | mz390.java    |
| 119 | maximum bal file size exceeded                                                                                      | mz390.java    |
| 119 | maximum obj file size exceeded                                                                                      | az390.java    |
| 120 | loader 2 byte RLD address{hex} too high in - {pgm_name}                                                             | sz390.java    |
| 120 | maximum pch file size exceeded                                                                                      | mz390.java    |
| 121 | loader 3 byte RLD address too high in - {pgm_name}                                                                  | sz390.java    |
| 122 | DC field length out of range {dc_len}                                                                               | az390.java    |
| 122 | duplicate lcl system variable - {sys_name}                                                                          | mz390.java    |
| 122 | extra parameter found - {exp_text_fragment}                                                                         | az390.java    |
| 122 | getmain for svc 19 open failed - PGM={pgm_name}LEN={load_code_len}                                                  | sz390.java    |
| 122 | hex code not equal instruction length                                                                               | az390.java    |
| 123 | error allocating CDE for LOAD                                                                                       | sz390.java    |
| 123 | invalid {s} string - {setc_text}                                                                                    | mz390.java    |
| 123 | missing unary operand value                                                                                         | az390.java    |
| 124 | invalid prefix operator type                                                                                        | az390.java    |
| 124 | missing unary operator value                                                                                        | mz390.java    |
| 124 | {msg}                                                                                                               | sz390.java    |
| 125 | ZSORT CLOSE FAILED {e}                                                                                              | sz390.java    |
| 125 | invalid pop parm - {parm}                                                                                           | az390.java    |
| 126 | SYSTRACE space terminator not found                                                                                 | sz390.java    |
| 126 | maximum push print exceeded                                                                                         | az390.java    |
| 127 | maximum push using exceeded                                                                                         | az390.java    |
| 128 | DC value out of range {dc_len}                                                                                      | az390.java    |
| 128 | invalid exp_calc text index - {exp_text}({exp_start_index})                                                         | mz390.java    |
| 129 | DC value out of range {dc_len}                                                                                      | az390.java    |
| 129 | invalid character sdt {setc_value}                                                                                  | mz390.java    |
| 129 | invalid push parm - {parm}                                                                                          | az390.java    |
| 130 | invalid object record - {hex_rcd}                                                                                   | az390.java    |
| 131 | invalid ESD type {hex_rcd}                                                                                          | az390.java    |
| 132 | SD invalid 24 bit address - {hex_rcd}                                                                               | az390.java    |
| 133 | SD invalid 24 bit length - {hex_rcd}                                                                                | az390.java    |
| 133 | unbalanced macro mend in {load_macro_name}                                                                          | mz390.java    |
| 134 | LD invalid 24 bit address - {hex_rcd}                                                                               | az390.java    |
| 134 | TXT invalid 24 bit address - {hex_rcd}                                                                              | az390.java    |
| 134 | unbalanced macro mend in {load_macro_name}                                                                          | mz390.java    |
| 135 | RLD invalid 24 bit address - {hex_rcd}                                                                              | az390.java    |
| 135 | unbalanced macro mend in {load_macro_name}                                                                          | mz390.java    |
| 136 | Invalid ascii hex object code - {hex_rcd}                                                                           | az390.java    |
| 137 | invalid single {token}                                                                                              | az390.java    |
| 138 | invalid ascii source line {line_number} in {path}                                                                   | mz390.java    |
| 138 | invalid character sdt {sdt}                                                                                         | az390.java    |
| 138 | unsupported SD alignment code - {esd_align} in {obj_file_name}                                                      | lz390.java    |
| 139 | invalid opcode type index                                                                                           | az390.java    |
| 139 | missing continuation line {line_number} in {path}                                                                   | mz390.java    |
| 140 | invalid ascii source line {line_number} in {path}                                                                   | mz390.java    |
| 141 | invalid symbol - {bal_label}                                                                                        | az390.java    |
| 142 | {mac_name} AIF macro label not found - {bal_parms_fragment}                                                         | mz390.java    |
| 144 | GUAM abort due to timeout                                                                                           | gz390.java    |
| 144 | maximum positional parms exceeded                                                                                   | mz390.java    |
| 144 | no base register found                                                                                              | az390.java    |
| 145 | maximum key word parms exceeded                                                                                     | mz390.java    |
| 145 | missing DC field type                                                                                               | az390.java    |
| 146 | base out of range = {b}                                                                                             | az390.java    |
| 147 | displacement dddhh out of range = {dddhh}                                                                           | az390.java    |
| 148 | displacement ddd out of range = {dddhh}                                                                             | az390.java    |
| 148 | macro statement preceeding MACRO {mac_line}                                                                         | mz390.java    |
| 149 | AGO missing macro label operand                                                                                     | mz390.java    |
| 149 | length exceeds 256 limit = {ll}                                                                                     | az390.java    |
| 150 | AGO invald macro label operand - {bal_parms_fragment}                                                               | mz390.java    |
| 150 | invalid data field terminator - {dc_field}                                                                          | az390.java    |
| 151 | invalid AGO label - {mac_parms_fragment}                                                                            | mz390.java    |
| 151 | waiting for mz390 to pass bal line {exception}                                                                      | az390.java    |
| 152 | missing variable for D' operator                                                                                    | mz390.java    |
| 152 | missing variable for I' operator                                                                                    | mz390.java    |
| 152 | waiting for az390 to release bal line                                                                               | az390.java    |
| 153 | lcla subscript < 1 = {lcl_set_name}                                                                                 | mz390.java    |
| 153 | symbol table error on add extrn {token}                                                                             | az390.java    |
| 154 | gbla subscript < 1 = {gbl_set_name}                                                                                 | mz390.java    |
| 154 | symbol table error on add wxtrn {token}                                                                             | az390.java    |
| 155 | invalid symbol assembler attribute {sym_attra}                                                                      | az390.java    |
| 155 | lclb subscript < 1 = {lcl_set_name}                                                                                 | mz390.java    |
| 156 | ENTRY not found - {token}                                                                                           | az390.java    |
| 156 | gblb subscript < 1 = {gbl_set_name}                                                                                 | mz390.java    |
| 157 | invalid pass bal record during lookahead - {bal_line}                                                               | az390.java    |
| 157 | lclc subscript < 1 = {lcl_set_name}                                                                                 | mz390.java    |
| 158 | gblc subscript < 1 = {gbl_set_name}                                                                                 | mz390.java    |
| 158 | internal system exception - {exception}                                                                             | az390.java    |
| 159 | invalid argument for N'                                                                                             | mz390.java    |
| 159 | thread ending interruption                                                                                          | az390.java    |
| 160 | add global var failed - {sys_name}                                                                                  | mz390.java    |
| 161 | invalid decimal constant - {dc_field_fragment}                                                                      | az390.java    |
| 161 | invalid set variable name - {bal_label}                                                                             | mz390.java    |
| 162 | invalid decimal floating point constant= {fp_text}                                                                  | az390.java    |
| 162 | invalid expression operator class for - {exp_token}                                                                 | mz390.java    |
| 163 | invalid expression operator class - {exp_token}                                                                     | mz390.java    |
| 163 | invalid sdt constant - {sdt}                                                                                        | az390.java    |
| 164 | lcla subscript < 1 = {lcl_set_name}                                                                                 | mz390.java    |
| 165 | input truncated due to mz390 abort                                                                                  | az390.java    |
| 165 | lclb subscript < 1 = {lcl_set_name}                                                                                 | mz390.java    |
| 166 | lclc subscript < 1 = {lcl_set_name}                                                                                 | mz390.java    |
| 167 | gbla subscript < 1 = {gbl_set_name}                                                                                 | mz390.java    |
| 167 | invalid set sym lock request - {sym_lock_desc}                                                                      | az390.java    |
| 168 | bal pass sym lock error on line - {new_bal_line}                                                                    | az390.java    |
| 168 | gblb subscript < 1 = {gbl_set_name}                                                                                 | mz390.java    |
| 169 | gblc subscript < 1 = {gbl_set_name}                                                                                 | mz390.java    |
| 169 | invalid literal expression                                                                                          | az390.java    |
| 170 | invalid literal + offset expression                                                                                 | az390.java    |
| 170 | key search table exceeded adding {name}                                                                             | mz390.java    |
| 171 | invalid binary constant= {dcb_bin}                                                                                  | az390.java    |
| 172 | DC S invalid length                                                                                                 | az390.java    |
| 172 | key search table exceeded adding {mac_file_key}                                                                     | mz390.java    |
| 173 | DC V invalid length                                                                                                 | az390.java    |
| 174 | key search table exceeded adding {new_name}                                                                         | mz390.java    |
| 174 | location counter / hex object code error                                                                            | az390.java    |
| 175 | invalid literal complex expression                                                                                  | az390.java    |
| 175 | missing argument for prefix operator                                                                                | mz390.java    |
| 176 | invalid prefix operator                                                                                             | mz390.java    |
| 177 | invalid prefix operator                                                                                             | mz390.java    |
| 179 | DD dfp constant out of range                                                                                        | az390.java    |
| 180 | ED dfp constant out of range                                                                                        | az390.java    |
| 180 | invalid character sdt {setc_value}                                                                                  | mz390.java    |
| 181 | LD dfp constant out of range                                                                                        | az390.java    |
| 181 | invalid prefix operator                                                                                             | mz390.java    |
| 182 | Duplicate section name of different type                                                                            | az390.java    |
| 182 | undefined prefix operator                                                                                           | mz390.java    |
| 183 | no index or length comma allowed                                                                                    | az390.java    |
| 183 | undefined prefix operator                                                                                           | mz390.java    |
| 184 | missing EQU label                                                                                                   | az390.java    |
| 185 | DS/DC negative length -{dc_len}                                                                                     | az390.java    |
| 185 | invalid parm {parm}                                                                                                 | mz390.java    |
| 186 | end location changed from {hex} to {hex}                                                                            | az390.java    |
| 187 | expression missing value error                                                                                      | mz390.java    |
| 187 | first label address change for {bal_label} from {hex} to {hex}                                                      | az390.java    |
| 188 | first equ change for {sym_name} from {hex} to {hex}                                                                 | az390.java    |
| 188 | symbol table overflow adding {sym_lab}                                                                              | mz390.java    |
| 189 | DC field with no data                                                                                               | az390.java    |
| 189 | {msg_id}aborting due to az390 abort                                                                                 | mz390.java    |
| 190 | Comment must start with `*` in position 1                                                                           | az390.java    |
| 191 | invalid key index add sequence                                                                                      | mz390.java    |
| 192 | dynamic alloc failed for - {store_name}({store_sub})                                                                | mz390.java    |
| 192 | missing close )                                                                                                     | az390.java    |
| 193 | invalid pass request during lookahead                                                                               | mz390.java    |
| 193 | unexpected character before close )                                                                                 | az390.java    |
| 194 | invalid index register                                                                                              | az390.java    |
| 194 | missing close )                                                                                                     | az390.java    |
| 194 | pc aborted due to {msg}                                                                                             | mz390.java    |
| 195 | bin invalid self defining term - {setc_value}                                                                       | mz390.java    |
| 195 | dec-1 invalid self defining term - {setc_value}                                                                     | mz390.java    |
| 195 | dec-2 invalid self defining term - {setc_value}                                                                     | mz390.java    |
| 195 | dec-3 arithmetic overflow - {setc_value}                                                                            | mz390.java    |
| 195 | hex invalid self defining term - {setc_value}                                                                       | mz390.java    |
| 195 | invalid self defining term - {setc_value}                                                                           | mz390.java    |
| 195 | missing USING parms                                                                                                 | az390.java    |
| 196 | invalid character in opcode - {bal_op}                                                                              | az390.java    |
| 197 | K' missing variable                                                                                                 | mz390.java    |
| 197 | invalid binary value string - {dcb_bin}                                                                             | az390.java    |
| 198 | L' missing variable                                                                                                 | mz390.java    |
| 198 | symbol not defined {sym_name}                                                                                       | az390.java    |
| 199 | N' missing variable                                                                                                 | mz390.java    |
| 199 | max missing macro/copy exceeded                                                                                     | az390.java    |
| 200 | O' missing variable                                                                                                 | mz390.java    |
| 200 | circular EQU expression error for {sym_name}                                                                        | az390.java    |
| 201 | T' missing variable                                                                                                 | mz390.java    |
| 201 | cmd proc wait error {exception}                                                                                     | ez390.java    |
| 201 | literal modifier forward reference for {sym_name}                                                                   | az390.java    |
| 202 | GBLA add failed for computed AGO                                                                                    | mz390.java    |
| 202 | USING missing domain operand                                                                                        | az390.java    |
| 202 | pz390 processor error {exception}                                                                                   | ez390.java    |
| 203 | EZ390E monitor external shutdown request                                                                            | ez390.java    |
| 203 | key index table overflow for ago                                                                                    | mz390.java    |
| 203 | missing dc data terminator                                                                                          | az390.java    |
| 204 | START must be first CSECT                                                                                           | az390.java    |
| 204 | az390 not in lookahead wait state                                                                                   | mz390.java    |
| 204 | unknown java version {java_vendor} {java_version}                                                                   | ez390.java    |
| 205 | DS/DC missing operand                                                                                               | az390.java    |
| 205 | unknown java version {java_vendor} {java_version}                                                                   | mz390.java    |
| 205 | {msg}                                                                                                               | mz390.java    |
| 206 | file I/O error {exception}                                                                                          | mz390.java    |
| 206 | invalid length - {ll}                                                                                               | az390.java    |
| 207 | Active USING not found for DROP register - {cur_use_reg}                                                            | az390.java    |
| 208 | DS/DC missing modifier - {dc_field}                                                                                 | az390.java    |
| 208 | invalid SYSLIST string reference                                                                                    | mz390.java    |
| 209 | invalid SYSLIST substitution reference                                                                              | mz390.java    |
| 209 | invalid signed byte value                                                                                           | az390.java    |
| 210 | END entry label not found - {end_entry}                                                                             | az390.java    |
| 210 | invalid SYSLIST numeric reference                                                                                   | mz390.java    |
| 211 | END invalid 24 bit entry address - {hex_rcd}                                                                        | az390.java    |
| 211 | duplicate keyword parm on call {key}={key_parm}                                                                     | mz390.java    |
| 212 | invalid string in SETB expression                                                                                   | mz390.java    |
| 213 | invalid string in SETB expression                                                                                   | mz390.java    |
| 214 | invalid string in SETB expression                                                                                   | mz390.java    |
| 215 | invalid string in SETB expression                                                                                   | mz390.java    |
| 216 | invalid string in SETB expression                                                                                   | mz390.java    |
| 217 | invalid substring - offset={seta_value1} len={seta_value2}                                                          | mz390.java    |
| 218 | invalid charcter in variable label - {parm_value}                                                                   | mz390.java    |
| 219 | max missing copy exceeded                                                                                           | mz390.java    |
| 220 | invalid ICTL start value - {mac_ictl_start}                                                                         | mz390.java    |
| 221 | invalid ICTL end value - {mac_ictl_end}                                                                             | mz390.java    |
| 222 | invalid ICTL continue value - {mac_ictl_cont}                                                                       | mz390.java    |
| 223 | batch assemblies not supported                                                                                      | mz390.java    |
| 224 | OPSYN table exceeded                                                                                                | mz390.java    |
| 225 | missing macro - {bal_op}                                                                                            | mz390.java    |
| 226 | missing SETC variable quotes                                                                                        | mz390.java    |
| 227 | missing quotes for SETC operand                                                                                     | mz390.java    |
| 228 | undefined symbol - {setc_value}                                                                                     | mz390.java    |
| 229 | subscript required for {exp_token}                                                                                  | mz390.java    |
| 230 | subscript required for {exp_token}                                                                                  | mz390.java    |
| 231 | I/O error on MLC open {exception}                                                                                   | mz390.java    |
| 233 | invalid created set symbol name - {var_name}                                                                        | mz390.java    |
| 234 | AGO computed label not found {label_match}                                                                          | mz390.java    |
| 235 | exec ago computed label undefined {ago_err}                                                                         | mz390.java    |
| 236 | undefine ago label {bal_parms}                                                                                      | mz390.java    |
| 237 | undefined N'operand                                                                                                 | mz390.java    |
| 238 | ZSM AELSE missing AIF or ACASE                                                                                      | mz390.java    |
| 239 | ZSM AEND missing structure                                                                                          | mz390.java    |
| 239 | ZSTRMAC error adding opcode key {opcode}                                                                            | mz390.java    |
| 241 | ZSM AELSE duplicate                                                                                                 | mz390.java    |
| 242 | ZSM AELSEIF missing AIF                                                                                             | mz390.java    |
| 243 | ZSM AELSEIF missing (...)                                                                                           | mz390.java    |
| 244 | ZSM AELSE missing AIF or ACASE                                                                                      | mz390.java    |
| 245 | ZSM AENTRY cannot be nested within another structure                                                                | mz390.java    |
| 246 | ZSM AENTRY name error - {split_parms}                                                                               | mz390.java    |
| 247 | ZSM AENTRY duplicate name error - {zsm_acall_name}                                                                  | mz390.java    |
| 248 | ZSM AENTRY not used - {zsm_acall_name}                                                                              | mz390.java    |
| 249 | ZSM ACALL name error - {split_parms}                                                                                | mz390.java    |
| 249 | ZSM ACALLPRM name error - {split_parms}                                                                             | mz390.java    |
| 250 | ZSM AUNTIL missing (...)                                                                                            | mz390.java    |
| 251 | ZSM AWHILE missing (...)                                                                                            | mz390.java    |
| 252 | ZSM ACASE missing (...)                                                                                             | mz390.java    |
| 253 | ZSM AWHEN missing ACASE                                                                                             | mz390.java    |
| 254 | ZSM AWHEN invalid value -{split_parms}                                                                              | mz390.java    |
| 255 | ZSM ACASE missing ASHEN                                                                                             | mz390.java    |
| 256 | ZSM AEXIT missing structure                                                                                         | mz390.java    |
| 257 | ZSM AEXIT type not found                                                                                            | mz390.java    |
| 258 | spaces not allowed in substring notation                                                                            | mz390.java    |
| 259 | invalid character in substring notation                                                                             | mz390.java    |
| 260 | MNOTE invalid level (0 - 255) - {bal_parms}                                                                         | mz390.java    |
| 261 | MNOTE/PUNCH text must be in single quotes{bal_parms}                                                                | mz390.java    |
| 262 | MNOTE/PUNCH invalid 'text' - {bal_parms}                                                                            | mz390.java    |
| 263 | MNOTE missing level,'text'                                                                                          | mz390.java    |
| 264 | stack missing seta value                                                                                            | mz390.java    |
| 265 | stack missing setb value                                                                                            | mz390.java    |
| 266 | missing copy = {mac_parms}                                                                                          | mz390.java    |
| 267 | COPY caused hash table overflow                                                                                     | mz390.java    |
| 268 | AINSERT syntax error - {bal_parms}                                                                                  | mz390.java    |
| 269 | PUNCH syntax error - {bal_parms}                                                                                    | mz390.java    |
| 270 | maximum ainsert source lines MAXLINE exceeded                                                                       | mz390.java    |
| 270 | maxium bal lines exceeded                                                                                           | mz390.java    |
| 271 | AINSERT continuation line < {mac_ictl_cont} characters - {temp_line}                                                | mz390.java    |
| 272 | line exceeds 80 characters - {temp_line}                                                                            | mz390.java    |
| 273 | line sequence field not numeric - {temp_line}                                                                       | mz390.java    |
| 274 | line exceeds 80 characters - {temp_line}                                                                            | mz390.java    |
| 275 | line sequence field not numeric - {temp_line}                                                                       | mz390.java    |
| 276 | syslist negative subscript - {set_sub}                                                                              | mz390.java    |
| 277 | AINSERT missing FRONT/BACK{bal_parms}                                                                               | mz390.java    |
| 278 | AINSERT missing FRONT/BACK{bal_parms}                                                                               | mz390.java    |
| 279 | undefined AENTRY for ACALL - {zsm_acall_name}                                                                       | mz390.java    |
| 280 | zsm_find_acall_name hash index duplicate for - '{zsm_acall_name}' and '{acall_name}'                                | mz390.java    |
| 281 | zsm_find_acall_name hash table error                                                                                | mz390.java    |
| 282 | ACALL issused after AENTRY for {zsm_acall_name}                                                                     | mz390.java    |
| 283 | function character argument missing quotes                                                                          | mz390.java    |
| 284 | invalid sequence label {bal_label}                                                                                  | mz390.java    |
| 285 | invalid dimension {text_fragment}                                                                                   | mz390.java    |
| 286 | strings not allowed in SETA                                                                                         | mz390.java    |
| 287 | PUNCH record length = 0                                                                                             | mz390.java    |
| 288 | undefined set variable = {bal_text_fragment}                                                                        | mz390.java    |
| 289 | duplicate allocation - {exp_parse_set_name}                                                                         | mz390.java    |
| 290 | COPY MEMBER NAME > 8 - {split_label}                                                                                | mz390.java    |
| 291 | MACRO MEMBER NAME > 8 - {load_macro_name}                                                                           | mz390.java    |
| 292 | global seta subscripted scalar error - {lcl_set_name}                                                               | mz390.java    |
| 293 | local setb subscripted scalar error - {lcl_set_name}                                                                | mz390.java    |
| 294 | global setb subscripted scalar error - {lcl_set_name}                                                               | mz390.java    |
| 295 | local setc subscripted scalar error - {lcl_set_name}                                                                | mz390.java    |
| 296 | global setc subscripted scalar error - {lcl_set_name}                                                               | mz390.java    |
| 297 | local seta subscripted scalar error - {lcl_set_name}                                                                | mz390.java    |
| 773 | ? detected in entry {op_tables}                                                                                     | tz390.java    |
| 777 | Instruction length={instruction_length} mismatch ({op_type_len}) for opcode definition {op_tables}                  | tz390.java    |
| 778 | OPTABLE({opt_optable}) incompatible with MACHINE({opt_machine})                                                     | tz390.java    |
| 793 | CODEPAGE option error - {msg}                                                                                       | tz390.java    |
