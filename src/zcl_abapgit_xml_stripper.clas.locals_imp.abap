CLASS lcl_utils DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS open_cur_node_copy_with_attrs
      IMPORTING
        ii_reader TYPE REF TO if_sxml_reader
        ii_writer TYPE REF TO if_sxml_writer.

    CLASS-METHODS join_path
      IMPORTING
        it_stack       TYPE string_table
      RETURNING
        VALUE(rv_path) TYPE string.

ENDCLASS.

CLASS lcl_utils IMPLEMENTATION.
  METHOD open_cur_node_copy_with_attrs.

    ii_writer->open_element(
      prefix = ii_reader->prefix
      nsuri  = ii_reader->nsuri
      name   = ii_reader->name ).
    DO.
      ii_reader->next_attribute( ).
      IF ii_reader->node_type <> if_sxml_node=>co_nt_attribute.
        EXIT.
      ENDIF.
      ii_writer->write_attribute(
        name  = ii_reader->name
        value = ii_reader->value ).
    ENDDO.

  ENDMETHOD.

  METHOD join_path.
    LOOP AT it_stack ASSIGNING FIELD-SYMBOL(<seg>).
      rv_path = '/' && <seg> && rv_path.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

CLASS lcl_stripper DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_blob  TYPE xstring
        it_paths TYPE string_table.

    CLASS-METHODS process_file
      IMPORTING
        it_paths TYPE string_table
      CHANGING
        !cv_blob TYPE xstring
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mi_reader TYPE REF TO if_sxml_reader.
    DATA mi_writer TYPE REF TO if_sxml_writer.
    DATA mt_paths TYPE string_table.
    DATA mo_xml_writer TYPE REF TO cl_sxml_string_writer.

    METHODS render_blob
      RETURNING
        VALUE(rv_blob) TYPE xstring.
    METHODS do_processing
      RAISING
        zcx_abapgit_exception cx_sxml_parse_error.
    METHODS validate_and_copy
      IMPORTING
        iv_tag_name  TYPE string
        iv_node_type TYPE if_sxml_node=>node_type
        iv_read_next TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_abapgit_exception cx_sxml_parse_error.
    METHODS do_remove
      RAISING
        zcx_abapgit_exception cx_sxml_parse_error.
    METHODS is_to_remove
      IMPORTING
        iv_cur_path   TYPE string
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

ENDCLASS.

CLASS lcl_stripper IMPLEMENTATION.

  METHOD process_file.
    DATA lo_processor TYPE REF TO lcl_stripper.
    DATA lx_parse_error TYPE REF TO cx_root.

    CREATE OBJECT lo_processor
      EXPORTING
        it_paths = it_paths
        iv_blob  = cv_blob.

    TRY.
        lo_processor->do_processing(  ).
      CATCH cx_sxml_parse_error INTO lx_parse_error.
        zcx_abapgit_exception=>raise( |XML_STRIPPER: Parsing failed { lx_parse_error->get_text( ) }| ).
    ENDTRY.

    cv_blob = lo_processor->render_blob( ).
  ENDMETHOD.

  METHOD constructor.
    mt_paths      = it_paths.
    mi_reader     = cl_sxml_string_reader=>create( iv_blob ).
    mo_xml_writer = cl_sxml_string_writer=>create( encoding = 'utf-8' ).
    mi_writer     = mo_xml_writer.
    mi_writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
    mi_writer->set_option( option = if_sxml_writer=>co_opt_indent ).
    mi_writer->set_option( option = if_sxml_writer=>co_opt_normalizing ).
  ENDMETHOD.

  METHOD render_blob.
    rv_blob = mo_xml_writer->get_output( ).
  ENDMETHOD.

  METHOD do_processing.

    " Expect asx:abap XML structure
    validate_and_copy(
      iv_tag_name  = 'abapGit'
      iv_node_type = if_sxml_node=>co_nt_element_open ).
    validate_and_copy(
      iv_tag_name  = 'asx:abap'
      iv_node_type = if_sxml_node=>co_nt_element_open ).
    validate_and_copy(
      iv_tag_name  = 'asx:values'
      iv_node_type = if_sxml_node=>co_nt_element_open ).

    do_remove( ).

    validate_and_copy(
      iv_read_next = abap_false " already read in do_remove
      iv_tag_name  = 'asx:values'
      iv_node_type = if_sxml_node=>co_nt_element_close ).

    mi_reader->next_node( ).

    IF  mi_reader->node_type = if_sxml_node=>co_nt_element_open AND mi_reader->prefix = 'asx' AND mi_reader->name = 'heap'.
      validate_and_copy(
       iv_read_next = abap_false " already read before
       iv_tag_name  = 'asx:heap'
       iv_node_type = if_sxml_node=>co_nt_element_open ).

      do_remove( ).

      validate_and_copy(
         iv_read_next = abap_false " already read in do_remove
         iv_tag_name  = 'asx:heap'
         iv_node_type = if_sxml_node=>co_nt_element_close ).

      validate_and_copy(
        iv_tag_name  = 'asx:abap'
        iv_node_type = if_sxml_node=>co_nt_element_close ).
    ELSE.
      validate_and_copy(
        iv_read_next = abap_false " already read before
        iv_tag_name  = 'asx:abap'
        iv_node_type = if_sxml_node=>co_nt_element_close ).
    ENDIF.

    validate_and_copy(
      iv_tag_name  = 'abapGit'
      iv_node_type = if_sxml_node=>co_nt_element_close ).

    mi_reader->next_node( ).
    IF NOT ( mi_reader->node_type = if_sxml_node=>co_nt_final ).
      zcx_abapgit_exception=>raise( |XML_STRIPPER: Unexpected XML structure: EOF expected| ).
    ENDIF.

  ENDMETHOD.

  METHOD validate_and_copy.

    DATA lv_prefix TYPE string.
    DATA lv_name TYPE string.
    DATA lv_real_tag_name TYPE string.

    SPLIT iv_tag_name AT ':' INTO lv_prefix lv_name.
    IF lv_name IS INITIAL.
      lv_name = lv_prefix.
      CLEAR lv_prefix.
    ENDIF.

    IF iv_read_next = abap_true.
      mi_reader->next_node( ).
    ENDIF.

    IF NOT ( mi_reader->node_type = iv_node_type AND mi_reader->prefix = lv_prefix AND mi_reader->name = lv_name ).
      IF mi_reader->prefix IS NOT INITIAL.
        lv_real_tag_name = |{ mi_reader->prefix }:{ mi_reader->name }|.
      ELSE.
        lv_real_tag_name = mi_reader->name.
      ENDIF.
      zcx_abapgit_exception=>raise( |XML_STRIPPER: Unexpected XML structure [{ iv_node_type }]: { lv_real_tag_name } instead of { iv_tag_name }| ).
    ENDIF.

    IF mi_reader->node_type = if_sxml_node=>co_nt_element_open.
      lcl_utils=>open_cur_node_copy_with_attrs(
        ii_reader = mi_reader
        ii_writer = mi_writer ).
    ELSEIF mi_reader->node_type = if_sxml_node=>co_nt_element_close.
      mi_writer->close_element( ).
    ELSE.
      zcx_abapgit_exception=>raise( |XML_STRIPPER: Unexpected node type [{ iv_node_type }] in validate_and_copy| ).
    ENDIF.

  ENDMETHOD.

  METHOD do_remove.

    DATA lt_stack TYPE string_table.
    DATA lv_cur_path TYPE string.
    DATA lv_stack_top TYPE string.
    DATA lv_elem_depth TYPE i.
    DATA lv_start_skip_at TYPE i.

    DO.
      mi_reader->next_node( ).
      IF mi_reader->node_type = if_sxml_node=>co_nt_final.
        zcx_abapgit_exception=>raise( |XML_STRIPPER: Unexpected EOF| ).
      ENDIF.

      CASE mi_reader->node_type.
        WHEN if_sxml_node=>co_nt_element_open.
          INSERT to_upper( mi_reader->name ) INTO lt_stack INDEX 1.
          lv_elem_depth = lv_elem_depth + 1.
          lv_cur_path   = to_upper( lcl_utils=>join_path( lt_stack ) ).

          IF lv_start_skip_at = 0 AND is_to_remove( lv_cur_path ) = abap_true.
            lv_start_skip_at = lv_elem_depth.
          ENDIF.

          IF lv_start_skip_at = 0.
            mi_writer->open_element( name = mi_reader->name ).
          ENDIF.

        WHEN if_sxml_node=>co_nt_element_close.
          lv_elem_depth = lv_elem_depth - 1.
          IF lv_elem_depth < 0. " wrapping tag closes
            EXIT.
          ENDIF.

          READ TABLE lt_stack INDEX 1 INTO lv_stack_top.
          ASSERT sy-subrc = 0.

          IF to_upper( mi_reader->name ) <> lv_stack_top.
            zcx_abapgit_exception=>raise( |XML_STRIPPER: Unexpected closing node type { lv_stack_top }| ).
          ENDIF.

          IF lv_start_skip_at = 0.
            mi_writer->close_element( ).
          ENDIF.

          DELETE lt_stack INDEX 1.
          lv_cur_path = to_upper( lcl_utils=>join_path( lt_stack ) ).

          IF lv_start_skip_at > 0 AND lv_elem_depth < lv_start_skip_at.
            lv_start_skip_at = 0.
          ENDIF.

        WHEN if_sxml_node=>co_nt_value.

          IF lv_start_skip_at = 0.
            mi_writer->write_value( mi_reader->value ).
          ENDIF.

        WHEN OTHERS.
          zcx_abapgit_exception=>raise( 'Unexpected node type' ).
      ENDCASE.

    ENDDO.

  ENDMETHOD.

  METHOD is_to_remove.

    DATA lv_path_len TYPE i.
    FIELD-SYMBOLS <path> LIKE LINE OF mt_paths.

    LOOP AT mt_paths ASSIGNING <path>.
      lv_path_len = strlen( <path> ).
      IF strlen( iv_cur_path ) >= lv_path_len AND find( val = iv_cur_path sub = <path> len = lv_path_len ) = 0.
        rv_yes = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_config_parser DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS parse_config
      IMPORTING
        iv_config        TYPE string
      RETURNING
        VALUE(rs_config) TYPE zcl_abapgit_xml_stripper=>ty_config
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS parse_remove_args
      IMPORTING
        iv_args   TYPE string
      CHANGING
        cs_config TYPE zcl_abapgit_xml_stripper=>ty_config
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS lcl_config_parser IMPLEMENTATION.

  METHOD parse_config.

    DATA lt_lines  TYPE string_table.
    DATA lv_config LIKE iv_config.
    DATA lv_cmd    TYPE string.
    DATA lv_rest   TYPE string.
    FIELD-SYMBOLS <line> TYPE string.

    lv_config = replace(
      val  = iv_config
      sub  = cl_abap_char_utilities=>cr_lf
      with = cl_abap_char_utilities=>newline ).

    SPLIT lv_config AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines ASSIGNING <line> WHERE table_line IS NOT INITIAL.

      SPLIT <line> AT ` ` INTO lv_cmd lv_rest.
      CONDENSE: lv_cmd, lv_rest.
      lv_cmd = to_upper( lv_cmd ).

      CASE lv_cmd.
        WHEN 'REMOVE'.
          parse_remove_args(
            EXPORTING
              iv_args = lv_rest
            CHANGING
              cs_config = rs_config ).

        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |XML_STRIPPER: Unexpected procesing command { lv_cmd }| ).
      ENDCASE.

    ENDLOOP.


  ENDMETHOD.


  METHOD parse_remove_args.

    DATA lv_obj TYPE string.
    DATA lv_obj_name_pre TYPE string.
    DATA lv_obj_type TYPE zcl_abapgit_xml_stripper=>ty_rule-obj_type.
    DATA lv_obj_name TYPE zcl_abapgit_xml_stripper=>ty_rule-obj_name.
    DATA lv_path TYPE string.
    DATA lv_len TYPE i.
    DATA ls_rule LIKE LINE OF cs_config-rules.

    FIELD-SYMBOLS <rule> LIKE ls_rule.

    SPLIT iv_args AT ':' INTO lv_obj lv_path.
    CONDENSE: lv_obj, lv_path.

    IF find( val = lv_obj sub = '(' ) >= 0.
      SPLIT lv_obj AT '(' INTO lv_obj_type lv_obj_name_pre.
      lv_len = strlen( lv_obj_name_pre ).
      IF substring( val = lv_obj_name_pre off = lv_len - 1 ) <> ')'.
        zcx_abapgit_exception=>raise( |XML_STRIPPER: Incorrect obj name delimiters "{ lv_obj }"| ).
      ENDIF.
      lv_obj_name = substring( val = lv_obj_name_pre len = lv_len - 1 ). " Buf overflow check ?
    ELSE.
      lv_obj_type = lv_obj.
      CLEAR lv_obj_name.
    ENDIF.

    IF lv_obj_type IS INITIAL.
      zcx_abapgit_exception=>raise( |XML_STRIPPER: Object type cannot be empty "{ lv_obj }"| ).
    ENDIF.

    IF lv_path IS INITIAL.
      zcx_abapgit_exception=>raise( |XML_STRIPPER: Path cannot be empty "{ lv_obj }"| ).
    ENDIF.

    READ TABLE cs_config-rules ASSIGNING <rule>
      WITH KEY
        obj_name = lv_obj_name
        obj_type = lv_obj_type
        command  = zcl_abapgit_xml_stripper=>c_command-remove.
    IF sy-subrc <> 0.
      ls_rule-command  = zcl_abapgit_xml_stripper=>c_command-remove.
      ls_rule-obj_type = lv_obj_type.
      ls_rule-obj_name = lv_obj_name.
      INSERT ls_rule INTO TABLE cs_config-rules ASSIGNING <rule>.
      ASSERT sy-subrc = 0.
    ENDIF.

    lv_path = to_upper( lv_path ).
    APPEND lv_path TO <rule>-paths.

  ENDMETHOD.
ENDCLASS.
