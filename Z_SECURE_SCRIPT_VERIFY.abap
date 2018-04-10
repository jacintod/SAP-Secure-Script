FUNCTION z_secure_script_verify.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_SCRIPT_NAME) TYPE  CHAR0032
*"     REFERENCE(IV_TARGET_HOST) TYPE  RFCDISPLAY-RFCHOST OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_STATUS) TYPE  EXTCMDEXEX-STATUS
*"     REFERENCE(EV_EXIT_CODE) TYPE  EXTCMDEXEX-EXITCODE
*"     REFERENCE(ET_CMD_OUTPUT) TYPE  ZTT_BTCXPM
*"  EXCEPTIONS
*"      NO_RECORDS_FOUND
*"      INVALID_HASH_SHELL_EXE_SCRIPT
*"      INVALID_HASH_SCRIPT
*"      UNABLE_TO_VERIFY
*"----------------------------------------------------------------------

  DATA: it_zbw_script_conf TYPE STANDARD TABLE OF zbw_script_conf, " Internal Table zbw_script_conf
        it_cmd_output      TYPE TABLE OF btcxpm WITH HEADER LINE, " Internal Table of the SM69 Command Output
        lv_status_msg      TYPE btcxpm-message,
        lobj_execption     TYPE REF TO cx_root, " Exception Error Object
        lv_status          TYPE extcmdexex-status, " Exit Status of the SM69 Command
        lv_exit_code       TYPE extcmdexex-exitcode, " Exit Code of the SM69 Command
        wa_zbw_script_conf LIKE LINE OF it_zbw_script_conf, " Work Area of the Table zbw_script_conf
        lv_trace           TYPE extcmdexim-trace, " Trace Parameter
        lv_hash            TYPE char0064, " Python Script HASH Code
        lv_sexcs           TYPE char0032, " Shell Execution Script
        lv_sfile           TYPE char0032, " Script File Name
        lv_sname           TYPE char0010, " Script Name (Friendly Name)
        lv_spath           TYPE char0128, " Script Path
        lv_sexp            TYPE char0128, " Script Excution Path
        lv_sehash          TYPE char0064, " Execution Script HASH
        lv_sesm69          TYPE sxpgcolist-name, " Execution Script SM69 Command
        lv_hasal           TYPE char0010, " Hash Algorithm
        lv_hash_os_cmd     TYPE sxpgcolist-name, " OS Command used to validate the HASH of the file
        lv_execmd          TYPE sxpgcolist-parameters, " Full Path including the execution path and Shell Script Name
        lv_hash_len        TYPE i, " Variable used for hash code trimming (i.e. MD5, then 32 Char, etc)
        lv_hash_to_check   TYPE char0064. " Variable used to store the hash code which was dynamically read

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zbw_script_conf
          FROM zbw_script_conf WHERE ( script_name = iv_script_name ).

  IF sy-subrc EQ 0.
    " Loop through the records in the internal table
    LOOP AT it_zbw_script_conf INTO wa_zbw_script_conf.
      lv_sname = wa_zbw_script_conf-script_name. " Script Name (Friendly Name)
      lv_sfile = wa_zbw_script_conf-script_file. " Python Script File
      lv_hash = wa_zbw_script_conf-script_hash. " Script File Hash
      lv_spath = wa_zbw_script_conf-script_path. " Script File path
      lv_sexcs = wa_zbw_script_conf-exec_script. " Shell Execution File
      lv_sexp = wa_zbw_script_conf-exec_sh_path. " Shell Execution Path
      lv_sehash = wa_zbw_script_conf-exec_sh_hash. " Shell Execution Hash
      lv_hasal = wa_zbw_script_conf-hash_algo. " Hash Algorithm

      " This builds the OS command to execute (i.e. /usr/sap/SAPBW/intf/py_scripts/venv_xlsx_conv.sh) Shell Execution Script
      CONCATENATE lv_sexp lv_sexcs INTO lv_execmd.
      " Next step is validate the HASH value of the file(s)

      CASE lv_hasal.
        WHEN 'MD5'.
          lv_hash_os_cmd = 'Z_MD5'.
          lv_hash_len = 32. " The default length of characters for a MD5 has 32 Char
        WHEN 'SHA1'.
          lv_hash_os_cmd = 'Z_SHA1'.
          lv_hash_len = 40. " The default length of characters for a SHA has 40 Char
        WHEN 'SHA256'.
          lv_hash_os_cmd = 'Z_SHA256'.
          lv_hash_len = 64. " The default length of characters for a SHA256 has 64 Char
      ENDCASE.

      " 1 . Check the HASH code of the Shell Execution Script
      " In the Shell Execution script, you activate the virtualenv using the source command and then execute the python script
      " The logic with this, is to ensure that the shell execution script has not been modified

      TRY.

          CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
            EXPORTING
              commandname                   = lv_hash_os_cmd
              additional_parameters         = lv_execmd
              operatingsystem               = 'Linux'
              targetsystem                  = iv_target_host
              stdout                        = 'X'
              stderr                        = 'X'
              terminationwait               = 'X'
              trace                         = lv_trace
            IMPORTING
              status                        = lv_status
              exitcode                      = lv_exit_code
            TABLES
              exec_protocol                 = it_cmd_output " Table containing the hash code
            EXCEPTIONS
              no_permission                 = 1
              command_not_found             = 2
              parameters_too_long           = 3
              security_risk                 = 4
              wrong_check_call_interface    = 5
              program_start_error           = 6
              program_termination_error     = 7
              x_error                       = 8
              parameter_expected            = 9
              too_many_parameters           = 10
              illegal_command               = 11
              wrong_asynchronous_parameters = 12
              cant_enq_tbtco_entry          = 13
              jobcount_generation_error     = 14
              OTHERS                        = 15.

*          CALL FUNCTION 'SXPG_CALL_SYSTEM'
*            EXPORTING
*              commandname           = lv_hash_os_cmd
*              additional_parameters = lv_execmd
*            IMPORTING
*              status                = lv_status
*              exitcode              = lv_exit_code
*            TABLES
*              exec_protocol         = it_cmd_output. " Table containing the hash code

        CATCH cx_root INTO lobj_execption.
      ENDTRY.

      IF lv_exit_code EQ '0'. " Succesfully executed the command

        LOOP AT it_cmd_output.
          lv_hash_to_check = it_cmd_output-message(lv_hash_len).
        ENDLOOP.

        IF lv_hash_to_check EQ lv_sehash. " This code block will ensure that the checksum of the script is correct and matches
          " Export the command Output Internal Table
          " we need to append our hash values to the ET table for troubleshooting purposes
          CONCATENATE 'Value stored in ZBW_SECURE_SCRIPT table : ' lv_sehash INTO lv_status_msg.
          it_cmd_output-message = lv_status_msg.
          APPEND it_cmd_output TO it_cmd_output.
          et_cmd_output[] = it_cmd_output[].
          ev_status = 'S'. " Status of 'SXPG_CALL_SYSTEM' FM call
          ev_exit_code = '0'. " Exit call of 'SXPG_CALL_SYSTEM' FM call

        ELSE.
          CONCATENATE 'Value stored in ZBW_SECURE_SCRIPT table : ' lv_sehash INTO lv_status_msg.
          it_cmd_output-message = lv_status_msg.
          APPEND it_cmd_output TO it_cmd_output.
          et_cmd_output[] = it_cmd_output[].
          " Export the command Output Internal Table
          et_cmd_output[] = it_cmd_output[].
          ev_status = lv_status. " Status of 'SXPG_CALL_SYSTEM' FM call
          ev_exit_code = lv_exit_code. " Exit call of 'SXPG_CALL_SYSTEM' FM call
          RAISE invalid_hash_shell_exe_script.
        ENDIF.
      ELSE.
        " Export the command Output Internal Table
        et_cmd_output[] = it_cmd_output[].
        ev_status = lv_status. " Status of 'SXPG_CALL_SYSTEM' FM call
        ev_exit_code = lv_exit_code. " Exit call of 'SXPG_CALL_SYSTEM' FM call
        RAISE unable_to_verify.
      ENDIF.

      " Reset the working variables
      CLEAR lv_execmd.
      CLEAR lv_status.
      CLEAR lv_exit_code.
      "CLEAR it_cmd_output[]. " Output command internal table
      CLEAR lv_hash_to_check.

      " This builds the OS command to execute (i.e. /usr/sap/SAPBW/intf/py_scripts/xlsx_convert.py) Python Script
      CONCATENATE lv_spath lv_sfile INTO lv_execmd.

      TRY.

          CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
            EXPORTING
              commandname                   = lv_hash_os_cmd
              additional_parameters         = lv_execmd
              operatingsystem               = 'Linux'
              targetsystem                  = iv_target_host
              stdout                        = 'X'
              stderr                        = 'X'
              terminationwait               = 'X'
              trace                         = lv_trace
            IMPORTING
              status                        = lv_status
              exitcode                      = lv_exit_code
            TABLES
              exec_protocol                 = it_cmd_output " Table containing the hash code
            EXCEPTIONS
              no_permission                 = 1
              command_not_found             = 2
              parameters_too_long           = 3
              security_risk                 = 4
              wrong_check_call_interface    = 5
              program_start_error           = 6
              program_termination_error     = 7
              x_error                       = 8
              parameter_expected            = 9
              too_many_parameters           = 10
              illegal_command               = 11
              wrong_asynchronous_parameters = 12
              cant_enq_tbtco_entry          = 13
              jobcount_generation_error     = 14
              OTHERS                        = 15.

*          CALL FUNCTION 'SXPG_CALL_SYSTEM'
*            EXPORTING
*              commandname           = lv_hash_os_cmd
*              additional_parameters = lv_execmd
*            IMPORTING
*              status                = lv_status
*              exitcode              = lv_exit_code
*            TABLES
*              exec_protocol         = it_cmd_output. " Table containing the hash code

        CATCH cx_root INTO lobj_execption.
      ENDTRY.

      IF lv_exit_code EQ '0'. " Succesfully executed the command

        LOOP AT it_cmd_output.
          lv_hash_to_check = it_cmd_output-message(lv_hash_len).
        ENDLOOP.

        IF lv_hash_to_check EQ lv_hash. " This code block will ensure that the checksum of the script is correct and matches
          " Export the command Output Internal Table
          " we need to append our hash values to the ET table for troubleshooting purposes
          CONCATENATE 'Value stored in ZBW_SECURE_SCRIPT table : ' lv_hash INTO lv_status_msg.
          it_cmd_output-message = lv_status_msg.
          APPEND it_cmd_output TO it_cmd_output.
          et_cmd_output[] = it_cmd_output[].
          ev_status = 'S'. " Status of 'SXPG_CALL_SYSTEM' FM call
          ev_exit_code = '0'. " Exit call of 'SXPG_CALL_SYSTEM' FM call

        ELSE.
          CONCATENATE 'Value stored in ZBW_SECURE_SCRIPT table : ' lv_hash INTO lv_status_msg.
          it_cmd_output-message = lv_status_msg.
          APPEND it_cmd_output TO it_cmd_output.
          et_cmd_output[] = it_cmd_output[].
          " Export the command Output Internal Table
          et_cmd_output[] = it_cmd_output[].
          ev_status = lv_status. " Status of 'SXPG_CALL_SYSTEM' FM call
          ev_exit_code = lv_exit_code. " Exit call of 'SXPG_CALL_SYSTEM' FM call
          RAISE invalid_hash_script.
        ENDIF.
      ELSE.
        " Export the command Output Internal Table
        et_cmd_output[] = it_cmd_output[].
        ev_status = lv_status. " Status of 'SXPG_CALL_SYSTEM' FM call
        ev_exit_code = lv_exit_code. " Exit call of 'SXPG_CALL_SYSTEM' FM call
        RAISE unable_to_verify.
      ENDIF.

    ENDLOOP.

  ELSE.
    " Export the command Output Internal Table
    et_cmd_output[] = it_cmd_output[].
    ev_status = lv_status. " Status of 'SXPG_CALL_SYSTEM' FM call
    ev_exit_code = lv_exit_code. " Exit call of 'SXPG_CALL_SYSTEM' FM call

    " Unable to find any records in the ZBW_SECURE_SCRIPT table to verify
    RAISE no_records_found.
  ENDIF.

ENDFUNCTION.