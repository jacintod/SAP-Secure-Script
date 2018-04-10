*&---------------------------------------------------------------------*
*& Report  ZBW_SECURE_SCRIPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zbw_secure_script.

DATA: name               TYPE vrm_id,
      list               TYPE vrm_values,
      value              LIKE LINE OF list,
      wa_value           TYPE vrm_values WITH HEADER LINE,
      it_zbw_script_conf TYPE STANDARD TABLE OF zbw_script_conf,
      wa_zbw_script_conf LIKE LINE OF it_zbw_script_conf,
      lv_flag            TYPE boolean.
TABLES: sscrfields.

TYPE-POOLS: vrm.

CONSTANTS : c_yes TYPE c VALUE '1',
            c_no  TYPE c VALUE '0',
            c_j   TYPE c VALUE 'J',
            c_n   TYPE c VALUE 'N',
            c_x   TYPE c VALUE 'X',
            true  TYPE boolean VALUE 'X'.

" Mode of the program
SELECTION-SCREEN BEGIN OF BLOCK script_mode WITH FRAME TITLE text-001.
PARAMETERS :  r_create RADIOBUTTON GROUP rb1 USER-COMMAND mode DEFAULT 'X', " Create New Secure Script Config
              "r_verify RADIOBUTTON GROUP rb1,
              r_modify RADIOBUTTON GROUP rb1,
              r_delete RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK script_mode.
" Secure Script Details
SELECTION-SCREEN BEGIN OF BLOCK script_details WITH FRAME TITLE text-002.
PARAMETER: p_sname TYPE char0032 MODIF ID cre LOWER CASE,
           p_crq TYPE char32 MODIF ID cre LOWER CASE,
           p_slang TYPE char0010 MODIF ID cre LOWER CASE,
           p_sown TYPE char0064 MODIF ID cre LOWER CASE,
           p_sexcs TYPE char0032 MODIF ID cre LOWER CASE,
           p_sfile TYPE char0010 MODIF ID cre LOWER CASE,
           p_hasal TYPE char0010 MODIF ID cre LOWER CASE,
           p_shash  TYPE char0064 MODIF ID cre LOWER CASE,
           p_sehash TYPE char0064 MODIF ID cre LOWER CASE,
           p_sesm69 TYPE char0032 MODIF ID cre LOWER CASE,
           p_sexp TYPE char0128 MODIF ID cre LOWER CASE,
           p_spath TYPE char0128 MODIF ID cre LOWER CASE.
SELECTION-SCREEN END OF BLOCK script_details.
" Script Modify Search Listbox
SELECTION-SCREEN BEGIN OF BLOCK modify_script_name WITH FRAME TITLE text-003.
PARAMETER: p_ssname TYPE char0032 AS LISTBOX VISIBLE LENGTH 20 LOWER CASE MODIF ID scr.
SELECTION-SCREEN END OF BLOCK modify_script_name.
" Script Delete Search listbox
SELECTION-SCREEN BEGIN OF BLOCK delete_script_name WITH FRAME TITLE text-004.
PARAMETER: p_dsname TYPE char0032 AS LISTBOX VISIBLE LENGTH 20 LOWER CASE MODIF ID dsr.
SELECTION-SCREEN END OF BLOCK delete_script_name.
" Modify Script details section
SELECTION-SCREEN BEGIN OF SCREEN 1100.
PARAMETER: p_msname TYPE char0032 LOWER CASE,
           p_msfile TYPE char0010 LOWER CASE,
           p_msexcs TYPE char0032 LOWER CASE,
           p_mhasal TYPE char0010 LOWER CASE,
           p_msehas TYPE char0064 LOWER CASE,
           p_mses69 TYPE char0032 LOWER CASE,
           p_mcrq TYPE char32 LOWER CASE,
           p_mslang TYPE char0010 LOWER CASE,
           p_msown TYPE char0064 LOWER CASE,
           p_mshash  TYPE char0064 LOWER CASE,
           p_msexp TYPE char0128 LOWER CASE,
           p_mspath TYPE char0128 LOWER CASE.
SELECTION-SCREEN END OF SCREEN 1100.
" Verify Script Config
SELECTION-SCREEN BEGIN OF BLOCK verify_script WITH FRAME TITLE text-006.
PARAMETER: p_vsname TYPE char0032 MODIF ID ver,
           p_vsexcs TYPE char0032 MODIF ID ver,
           p_vshash  TYPE char0064 MODIF ID ver,
           p_vspath TYPE char0128 MODIF ID ver.
SELECTION-SCREEN END OF BLOCK verify_script.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_screen_sel.

AT SELECTION-SCREEN.
*{ screen validations
* Check if buttons have been
  IF sscrfields-ucomm EQ 'CRET'.
    CLEAR: sy-ucomm.
    PERFORM f_update_script.
  ENDIF.
*}

START-OF-SELECTION.
  IF r_create = c_x.
    PERFORM f_create.
  ELSEIF r_modify = c_x.
    PERFORM f_modify.
  ELSEIF r_delete = c_x.
    PERFORM f_delete.
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
      IF screen-group1 EQ 'SCR'.
        screen-active = '0'.
      ENDIF.
      " Disable the Update listbox
      IF screen-group1 EQ 'UPD'.
        screen-active = '0'.
      ENDIF.
      " Disable the Update Screen Elements
      IF screen-group1 EQ 'DSR'.
        screen-active = '0'.
      ENDIF.
      " Disable verify elements
      IF screen-group1 EQ 'VER'.
        screen-active = '0'.
      ENDIF.
      " Update the Screen elements
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
    " Modify Screen Elements
    IF r_modify EQ 'X'.
      " Disable Create Elements
      IF screen-group1 EQ 'CRE'.
        screen-active = '0'.
      ENDIF.
      " Display the Search Drop Down box
      IF screen-group1 EQ 'SCR'.
        screen-active = '1'.
        screen-input = '1'.
        PERFORM f_listbox_populate.
      ENDIF.
      " Disable the Update Screen Elements
      IF screen-group1 EQ 'UPD'.
        screen-active = '0'.
      ENDIF.
      " Disable the Update Screen Elements
      IF screen-group1 EQ 'DSR'.
        screen-active = '0'.
      ENDIF.
      " Disable verify elements
      IF screen-group1 EQ 'VER'.
        screen-active = '0'.
      ENDIF.
      " Update Screen Elements
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
    " Delete Screen Elements
    IF r_delete EQ 'X'.
      " Disable the Create Elements
      IF screen-group1 EQ 'CRE'.
        screen-active = '0'.
      ENDIF.
      " Diabled the Search Listbox Elements
      IF screen-group1 EQ 'SCR'.
        screen-active = '0'.
      ENDIF.
      " Enable Delete Search Box
      IF screen-group1 EQ 'DSR'.
        screen-active = '1'.
        screen-input = '1'.
        PERFORM f_del_listbox_populate.
      ENDIF.
      " Disable the Update Elements
      IF screen-group1 EQ 'UPD'.
        screen-active = '0'.
      ENDIF.
      " Disable verify elements
      IF screen-group1 EQ 'VER'.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
*    IF r_verify EQ 'X'.
*      " Disable the create elements
*      IF screen-group1 EQ 'CRE'.
*        screen-active = '0'.
*      ENDIF.
*      " Disable the search listbox
*      IF screen-group1 EQ 'SCR'.
*        screen-active = '0'.
*      ENDIF.
*      " Disable the Update listbox
*      IF screen-group1 EQ 'UPD'.
*        screen-active = '0'.
*      ENDIF.
*      " Disable the Update Screen Elements
*      IF screen-group1 EQ 'DSR'.
*        screen-active = '0'.
*      ENDIF.
*      " Enable the Verify Commands
*      IF screen-group1 EQ 'VER'.
*        screen-active = '1'.
*        screen-input = '1'.
*      ENDIF.
*      " Update the Screen elements
*      MODIFY SCREEN.
*      CONTINUE.
*    ENDIF.

  ENDLOOP.
ENDFORM.

FORM f_listbox_populate.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zbw_script_conf
    FROM zbw_script_conf.

  CLEAR list[].

  name = 'p_ssname'.

  LOOP AT it_zbw_script_conf INTO wa_zbw_script_conf.
    value-key =  wa_zbw_script_conf-script_hash.
    value-text = wa_zbw_script_conf-script_name.
    APPEND value TO list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = list.

ENDFORM.

FORM f_del_listbox_populate.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zbw_script_conf
    FROM zbw_script_conf.

  CLEAR list[].

  name = 'p_dsname'.

  LOOP AT it_zbw_script_conf INTO wa_zbw_script_conf.
    value-key =  wa_zbw_script_conf-script_hash.
    value-text = wa_zbw_script_conf-script_name.
    APPEND value TO list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = list.

ENDFORM.

FORM f_modify.

  READ TABLE list INTO wa_value WITH KEY p_ssname.

  IF wa_value-key IS NOT INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zbw_script_conf
      FROM zbw_script_conf
      WHERE script_hash = wa_value-key AND script_name = wa_value-text.

    CLEAR list[].

    LOOP AT it_zbw_script_conf INTO wa_zbw_script_conf.

      p_msname = wa_zbw_script_conf-script_name.
      p_msfile = wa_zbw_script_conf-script_file.
      p_mshash = wa_zbw_script_conf-script_hash.
      p_msehas = wa_zbw_script_conf-exec_sh_hash.
      p_mses69 = wa_zbw_script_conf-exec_sm69_cmd.
      p_msexcs = wa_zbw_script_conf-exec_script.
      p_mhasal = wa_zbw_script_conf-hash_algo.
      p_msexp = wa_zbw_script_conf-exec_sh_path.
      p_mcrq = wa_zbw_script_conf-itsm_crq.
      p_mslang = wa_zbw_script_conf-script_lang.
      p_msown = wa_zbw_script_conf-script_owner.
      p_mspath = wa_zbw_script_conf-script_path.

    ENDLOOP.

    " Call the modify screen with the selected Secure Script Elements that need to be modified
    CALL SELECTION-SCREEN 1100.

  ENDIF.

ENDFORM.

FORM f_update_script.
  " This form will update the values in the Secure Script Table

  "wa_zbw_script_conf-script_name = p_msname.
  wa_zbw_script_conf-script_file = p_msfile.
  wa_zbw_script_conf-script_hash = p_mshash.
  wa_zbw_script_conf-exec_sh_hash = p_msehas.
  wa_zbw_script_conf-exec_sm69_cmd = p_mses69.
  wa_zbw_script_conf-exec_script = p_msexcs.
  wa_zbw_script_conf-hash_algo = p_mhasal.
  wa_zbw_script_conf-exec_sh_path = p_msexp.
  wa_zbw_script_conf-itsm_crq = p_mcrq.
  wa_zbw_script_conf-script_lang = p_mslang.
  wa_zbw_script_conf-script_owner = p_msown.
  wa_zbw_script_conf-script_path = p_mspath.

  IF wa_zbw_script_conf IS NOT INITIAL.

    MODIFY zbw_script_conf FROM wa_zbw_script_conf.

    IF sy-subrc EQ 0.
      MESSAGE s899(bd) WITH 'Succesfully Updated Script Details'.
    ENDIF.
  ENDIF.

  LEAVE SCREEN.

ENDFORM.

FORM f_create.
  " Some basic rules
  " You can only create a secure config for a script that is already on the filesystem
  " This form will only validate that the script file and folder exists, not the execution shell script directory and path
  " This form will insert the values in the respective fields to create a script configuration

  wa_zbw_script_conf-mandt = sy-mandt.
  wa_zbw_script_conf-script_name = p_sname.
  wa_zbw_script_conf-script_file = p_sfile.
  wa_zbw_script_conf-script_hash = p_shash.
  wa_zbw_script_conf-exec_script = p_sexcs.
  wa_zbw_script_conf-exec_sh_hash = p_sehash.
  wa_zbw_script_conf-exec_sm69_cmd = p_sesm69.
  wa_zbw_script_conf-hash_algo = p_hasal.
  wa_zbw_script_conf-exec_sh_path = p_sexp.
  wa_zbw_script_conf-itsm_crq = p_crq.
  wa_zbw_script_conf-script_lang = p_slang.
  wa_zbw_script_conf-script_owner = p_sown.
  wa_zbw_script_conf-script_path = p_spath.

  " Make sure that all of the values in the structure are filled and valid
  IF  ( p_sname IS NOT INITIAL ) AND
      ( p_shash IS NOT INITIAL ) AND
      ( p_spath IS NOT INITIAL ) AND
      ( p_sexp IS NOT INITIAL ) AND
      ( p_crq IS NOT INITIAL ) AND
      ( p_sexcs IS NOT INITIAL ).

    " Do some validations before inserting the values into the table
    DATA: ld_dir_name	 TYPE ocs_file-name,
          it_dir_list	 TYPE STANDARD TABLE OF ocs_file,
          wa_dir_list	 LIKE LINE OF it_dir_list,
          ld_file_name TYPE ocs_file-name.

    " get our working variables for the FM to validate that the script files exisits
    ld_dir_name	= p_spath.
    ld_file_name = p_sfile.

    CALL FUNCTION 'OCS_GET_FILE_INFO'
      EXPORTING
        dir_name                  = ld_dir_name
        file_name                 = ld_file_name
      TABLES
        dir_list                  = it_dir_list
      EXCEPTIONS
        no_authority              = 1
        activity_unknown          = 2
        not_a_directory           = 3
        no_media_in_drive         = 4
        too_many_errors           = 5
        too_many_files            = 6
        bracket_error_in_filename = 7
        no_such_parameter         = 8.

    IF sy-subrc EQ 0.

      " loop through the list of files in the directory
      LOOP AT it_dir_list INTO wa_dir_list.
        " we need to make sure that the file exists and that the owner of the file is that of SIDadm (i.e. bwdadm). This is becuase the file gets triggered from OS CMD SM69 Transaction
        IF wa_dir_list-name = p_sfile.
          lv_flag = true.
        ENDIF.

      ENDLOOP.

      IF lv_flag = true.
        " Insert the script configuration values
        APPEND wa_zbw_script_conf TO it_zbw_script_conf.
        MODIFY zbw_script_conf FROM TABLE it_zbw_script_conf.

        IF sy-subrc EQ 0.
          WRITE: 'Successfully added new secure script configuration'.
          IF p_sexp = p_spath.
            WRITE: 'WARNING: Your shell execution script is in the same directory as your execution script.'.
            WRITE: 'WARNING: This is not recommended, it would be preferrable to have the shell execution script in the root directory of the py_scripts folder'.
          ENDIF.
        ELSE.
          WRITE: 'There was an error adding the secure script configuration'.
        ENDIF.
      ELSE.
        MESSAGE s899(bd) WITH 'Operation Cancelled, Unable to validate script' ' Unable to find if script exisits in [' p_spath ']'.
      ENDIF.

    ELSE.
      " There was an issue with the FM OCS_GET_FILE_INFO
      CASE sy-subrc.
        WHEN 1. MESSAGE e899(bd) WITH 'Error' 'No Authority' sy-subrc.
        WHEN 3. MESSAGE e899(bd) WITH 'Error' 'Not a Valid Script Path directory' sy-subrc.
        WHEN OTHERS. MESSAGE i899(bd) WITH 'Error' 'Unknown Error' sy-subrc.
      ENDCASE.
    ENDIF.

  ELSE.
    " There is some missing fields
    MESSAGE s899(bd) WITH 'Operation Cancelled, Missing Mandatory Fields' ' { Script Name and HASH Code }' '{ Script Full Path }' '{ Script Execution Shell Script }'.
  ENDIF.
ENDFORM.

FORM f_verify_config.

ENDFORM.

FORM f_delete.

  " This section will delete the secure script configuration
  " Read the List table and retrieve the key value to use for the removal of the entry in the table
  READ TABLE list INTO wa_value INDEX 1.

  IF wa_value-key IS NOT INITIAL.
    " Output some text to the console for informational purposes
    WRITE: 'The following Secure Script Configuration will be removed.'.
    WRITE: 'Removing Secure Script configuration: ', wa_value-text.
    " Remove the entries from the table
    DELETE FROM zbw_script_conf WHERE script_hash =  wa_value-key AND script_name = wa_value-text.

    IF sy-subrc EQ 0.
      WRITE: 'Succesfully removed Secure Script Configuration : ', wa_value-text.
      WRITE: 'Please Note: This only removed the Secure Script Configuration and not the physical Script files and filesystems'.
    ENDIF.

  ENDIF.


  MESSAGE s899(bd) WITH 'Succesfully removed script configuration' .


ENDFORM.