CLASS zcl_abapgit_xml_stripper DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      ty_command TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF c_command,
        remove TYPE ty_command VALUE 'R',
      END OF c_command.

    TYPES:
      BEGIN OF ty_rule,
        command  TYPE ty_command,
        obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type,
        obj_name TYPE zif_abapgit_definitions=>ty_item-obj_type,
        paths    TYPE string_table,
      END OF ty_rule.

    TYPES:
      tty_rules        TYPE STANDARD TABLE OF ty_rule WITH DEFAULT KEY,
      tts_rules_by_obj TYPE SORTED TABLE OF ty_rule WITH NON-UNIQUE KEY obj_type obj_name.

    TYPES:
      BEGIN OF ty_config,
        rules TYPE tts_rules_by_obj,
      END OF ty_config.

    CLASS-METHODS process_files
      IMPORTING
        iv_config_filename TYPE string OPTIONAL
        iv_config          TYPE string OPTIONAL
      CHANGING
        ct_local           TYPE zif_abapgit_definitions=>ty_files_item_tt
        ct_remote          TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        iv_config_blob TYPE xstring
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_config TYPE ty_config.

    CLASS-METHODS find_strip_config
      IMPORTING
        it_remote             TYPE zif_abapgit_git_definitions=>ty_files_tt
        iv_config_file_name   TYPE string OPTIONAL
      RETURNING
        VALUE(rv_config_blob) TYPE xstring.

    CLASS-METHODS identify_object_main_xml
      IMPORTING
        iv_filename    TYPE string
      RETURNING
        VALUE(rs_item) TYPE zif_abapgit_definitions=>ty_item.

    METHODS _process_files
      CHANGING
        ct_local  TYPE zif_abapgit_definitions=>ty_files_item_tt
        ct_remote TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception.

    METHODS get_strip_paths_for_item
      IMPORTING
        is_item         TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rt_paths) TYPE string_table.

    METHODS strip_file
      IMPORTING
        is_item      TYPE zif_abapgit_definitions=>ty_item
      CHANGING
        cv_file_blob TYPE xstring
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_xml_stripper IMPLEMENTATION.


  METHOD constructor.
    ms_config = lcl_config_parser=>parse_config( cl_abap_codepage=>convert_from( iv_config_blob ) ).
  ENDMETHOD.


  METHOD find_strip_config.

    FIELD-SYMBOLS <file> LIKE LINE OF it_remote.

    READ TABLE it_remote ASSIGNING <file> WITH KEY path = '/' filename = iv_config_file_name.
    IF sy-subrc = 0 AND <file>-data IS NOT INITIAL.
      rv_config_blob = <file>-data.
    ENDIF.

  ENDMETHOD.


  METHOD get_strip_paths_for_item.

    FIELD-SYMBOLS <rule> LIKE LINE OF ms_config-rules.

    READ TABLE ms_config-rules ASSIGNING <rule> WITH KEY obj_type = is_item-obj_type obj_name = is_item-obj_name.
    IF sy-subrc = 0.
      APPEND LINES OF <rule>-paths TO rt_paths.
    ENDIF.

    READ TABLE ms_config-rules ASSIGNING <rule> WITH KEY obj_type = is_item-obj_type.
    IF sy-subrc = 0.
      APPEND LINES OF <rule>-paths TO rt_paths.
    ENDIF.

  ENDMETHOD.


  METHOD identify_object_main_xml.

    DATA lv_name TYPE string.
    DATA lv_type TYPE string.
    DATA lv_ext  TYPE string.

    " Guess object type and name
    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    " Handle namespaces
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_type WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_ext WITH '/'.

    " Get original object name
    lv_name = cl_http_utility=>unescape_url( lv_name ).

    IF lv_ext = 'XML' AND strlen( lv_type ) = 4.
      rs_item-obj_type = lv_type.
      rs_item-obj_name = lv_name.
    ENDIF.

  ENDMETHOD.


  METHOD process_files.

    DATA lv_config_blob TYPE xstring.
    DATA lo_stripper TYPE REF TO zcl_abapgit_xml_stripper.

    IF boolc( iv_config IS INITIAL ) = boolc( iv_config_filename IS INITIAL ).
      zcx_abapgit_exception=>raise( 'XML_STRIPPER: config or config filename must be provided' ).
    ENDIF.

    IF iv_config_filename IS NOT INITIAL.
      lv_config_blob = find_strip_config(
        it_remote           = ct_remote
        iv_config_file_name = iv_config_filename ).
      IF lv_config_blob IS INITIAL.
        RETURN.
      ENDIF.
    ELSE.
      lv_config_blob = cl_abap_codepage=>convert_to( iv_config ).
    ENDIF.

    CREATE OBJECT lo_stripper EXPORTING iv_config_blob = lv_config_blob.

    lo_stripper->_process_files(
      CHANGING
        ct_local  = ct_local
        ct_remote = ct_remote ).

  ENDMETHOD.


  METHOD strip_file.

    DATA lt_paths TYPE string_table.

    lt_paths = get_strip_paths_for_item( is_item ).

    IF lines( lt_paths ) > 0.
      lcl_stripper=>process_file(
        EXPORTING
          it_paths = lt_paths
        CHANGING
          cv_blob  = cv_file_blob ).
    ENDIF.

  ENDMETHOD.


  METHOD _process_files.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <rfile> LIKE LINE OF ct_remote.
    FIELD-SYMBOLS <lfile> LIKE LINE OF ct_local.

    IF ms_config IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_remote ASSIGNING <rfile>.
      ls_item = identify_object_main_xml( <rfile>-filename ).
      CHECK ls_item IS NOT INITIAL. " Not xml -> skip
      strip_file(
        EXPORTING
          is_item      = ls_item
        CHANGING
          cv_file_blob = <rfile>-data ).

      <rfile>-sha1 = zcl_abapgit_hash=>sha1_blob( <rfile>-data ).
    ENDLOOP.

    LOOP AT ct_local ASSIGNING <lfile>.
      ls_item = identify_object_main_xml( <lfile>-file-filename ).
      CHECK ls_item IS NOT INITIAL. " Not xml -> skip
      strip_file(
        EXPORTING
          is_item      = ls_item
        CHANGING
          cv_file_blob = <lfile>-file-data ).

      <lfile>-file-sha1 = zcl_abapgit_hash=>sha1_blob( <lfile>-file-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
