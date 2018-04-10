*&---------------------------------------------------------------------*
*& Report  ZBW_PY_VENV_MANAGE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zbw_py_venv_manage.

DATA: BEGIN OF l_int_result OCCURS 0,
        str(256),"String
      END OF l_int_result.

TYPE-POOLS: vrm.

CONSTANTS : c_yes TYPE c VALUE '1',
            c_no  TYPE c VALUE '0',
            c_j   TYPE c VALUE 'J',
            c_n   TYPE c VALUE 'N',
            c_x   TYPE c VALUE 'X',
            true  TYPE boolean VALUE 'X'.

DATA: v_unix_sm69_cmd TYPE char0256,
      v_venv_name     TYPE vrm_id,
      v_venv_list     TYPE vrm_values,
      v_venv_value    LIKE LINE OF v_venv_list,
      wa_value        TYPE vrm_values WITH HEADER LINE,
      oref            TYPE REF TO cx_root.


" Mode of the program
SELECTION-SCREEN BEGIN OF BLOCK script_mode WITH FRAME TITLE text-001.
PARAMETERS :  r_create RADIOBUTTON GROUP rb1 USER-COMMAND mode DEFAULT 'X', " Create New virtualenv
              r_verify RADIOBUTTON GROUP rb1,
              r_delete RADIOBUTTON GROUP rb1. " Verify the virtualenv
SELECTION-SCREEN END OF BLOCK script_mode.
" virtualenv Details
SELECTION-SCREEN BEGIN OF BLOCK venv_details WITH FRAME TITLE text-002.
PARAMETER: p_venv TYPE char0032 MODIF ID cre LOWER CASE,
           p_path TYPE char0128 MODIF ID cre LOWER CASE DEFAULT '/usr/sap/SAPBW/intf/py_scripts'.
SELECTION-SCREEN END OF BLOCK venv_details.
" drop down list of virtualenv
SELECTION-SCREEN BEGIN OF BLOCK del_venv_name WITH FRAME TITLE text-003.
PARAMETER: p_lvenv TYPE char0032 AS LISTBOX VISIBLE LENGTH 50 LOWER CASE MODIF ID lst.
SELECTION-SCREEN END OF BLOCK del_venv_name.


AT SELECTION-SCREEN OUTPUT.
  PERFORM f_screen_sel.

AT SELECTION-SCREEN.


START-OF-SELECTION.
  IF r_create = c_x.
    PERFORM f_create.
  ELSEIF r_delete = c_x.
    PERFORM f_delete.
  ELSEIF r_verify = c_x.
    PERFORM f_verify.
  ENDIF.

FORM f_screen_sel.
  LOOP AT SCREEN.

    IF r_create EQ 'X' .
      " Show the create secure script config screen elements
      IF screen-group1 EQ 'CRE'.
        screen-active = '1'.
        screen-input = '1'.
      ENDIF.
      " Disable the search listbox
      IF screen-group1 EQ 'LST'.
        screen-active = '0'.
      ENDIF.
      " Update the Screen elements
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    IF r_delete EQ 'X' .
      " Show the create secure script config screen elements
      IF screen-group1 EQ 'CRE'.
        screen-active = '0'.
        screen-input = '0'.
      ENDIF.
      " Disable the search listbox
      IF screen-group1 EQ 'LST'.
        screen-active = '1'.
        PERFORM f_aval_venv.
      ENDIF.
      " Update the Screen elements
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    IF r_verify EQ 'X' .
      " Show the create secure script config screen elements
      IF screen-group1 EQ 'CRE'.
        screen-active = '0'.
        screen-input = '0'.
      ENDIF.
      " Disable the search listbox
      IF screen-group1 EQ 'LST'.
        screen-active = '1'.
        PERFORM f_aval_venv.
      ENDIF.
      " Update the Screen elements
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM f_create.
  " Call the shell script sh-create-venv.sh with the name of the new virtualenv
  IF p_venv IS NOT INITIAL.
    ". /usr/sap/SAPBW/intf/py_scripts/sh-create-venv.sh /usr/sap/SAPBW/intf/py_scripts test12
    CONCATENATE '. /usr/sap/SAPBW/intf/py_scripts/sh-create-venv.sh' p_path p_venv INTO v_unix_sm69_cmd SEPARATED BY space.
    CONDENSE v_unix_sm69_cmd.
*   Execute the unix command. *sys* indicates the table w/o header line
    TRY.
        CALL 'SYSTEM' ID 'COMMAND' FIELD v_unix_sm69_cmd
                      ID 'TAB'     FIELD l_int_result-*sys*.
      CATCH cx_root INTO oref.
    ENDTRY.

    " Loop through the result of the OS command call
    LOOP AT l_int_result.
      WRITE: l_int_result-str.
    ENDLOOP.

  ELSE.
    " Nothing to do, there is no value has been provided in p_venv
    WRITE: 'There is nothing to do, there is no value provided for virtualenv'.
  ENDIF.

ENDFORM.

FORM f_aval_venv.

  CONCATENATE 'ls -I sh-create-venv.sh -I sh-verify-venv.sh' p_path INTO v_unix_sm69_cmd SEPARATED BY space.
  CONDENSE v_unix_sm69_cmd.
  CLEAR v_venv_list[]. " clear the list temp table
  CLEAR l_int_result[]. " clear the OS command temp table

*   Execute the unix command. *sys* indicates the table w/o header line
  TRY.
      CALL 'SYSTEM' ID 'COMMAND' FIELD v_unix_sm69_cmd
                    ID 'TAB'     FIELD l_int_result-*sys*.
    CATCH cx_root INTO oref.
  ENDTRY.

  LOOP AT l_int_result.
    v_venv_value-key =  l_int_result-str.
    v_venv_value-text = l_int_result-str.
    APPEND v_venv_value TO v_venv_list.
  ENDLOOP.

  v_venv_name = 'p_lvenv'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = v_venv_name
      values = v_venv_list.

ENDFORM.

FORM f_delete.

  IF v_venv_list IS NOT INITIAL.

    " This section will delete the secure script configuration
    " Read the List table and retrieve the key value to use for the removal of the entry in the table
    READ TABLE v_venv_list INTO v_venv_value INDEX 1.

    ". /usr/sap/SAPBW/intf/py_scripts/sh-create-venv.sh /usr/sap/SAPBW/intf/py_scripts test12
    CONCATENATE 'rm -rf /usr/sap/SAPBW/intf/py_scripts/' v_venv_value-key INTO v_unix_sm69_cmd.
    CONDENSE v_unix_sm69_cmd.

    " Execute the unix command. *sys* indicates the table w/o header line
    TRY.
        CALL 'SYSTEM' ID 'COMMAND' FIELD v_unix_sm69_cmd
                      ID 'TAB'     FIELD l_int_result-*sys*.
      CATCH cx_root INTO oref.
    ENDTRY.

    IF sy-subrc EQ 0.
      " Loop through the result of the OS command call
      WRITE: 'Succesfully removed python virtualenv'.
    ENDIF.

  ENDIF.
ENDFORM.



FORM f_verify.

  IF v_venv_list IS NOT INITIAL.

    " This section will delete the secure script configuration
    clear v_unix_sm69_cmd.
    ". /usr/sap/SAPBW/intf/py_scripts/sh-create-venv.sh /usr/sap/SAPBW/intf/py_scripts test12
    CONCATENATE '. /usr/sap/SAPBW/intf/py_scripts/sh-verify-venv.sh /usr/sap/SAPBW/intf/py_scripts' p_lvenv INTO v_unix_sm69_cmd SEPARATED BY space.

    " Execute the unix command. *sys* indicates the table w/o header line
    TRY.
        CALL 'SYSTEM' ID 'COMMAND' FIELD v_unix_sm69_cmd
                      ID 'TAB'     FIELD l_int_result-*sys*.
      CATCH cx_root INTO oref.
    ENDTRY.

    IF sy-subrc EQ 0.
      " Loop through the result of the OS command call
      WRITE: 'Succesfully verified python virtualenv'.
      " Loop through the result of the OS command call
      LOOP AT l_int_result.
        WRITE: l_int_result-str.
      ENDLOOP.
    ENDIF.

  ENDIF.
ENDFORM.