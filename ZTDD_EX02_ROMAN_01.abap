*&---------------------------------------------------------------------*
*& Report ztdd_ex02_roman_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztdd_ex02_roman_01.

*----------------------------------------------------------------------*
* PRODUCTION CODE
*----------------------------------------------------------------------*
CLASS lcl_roman DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_roman_map,
             arabic TYPE i,
             roman  TYPE string,
           END OF ts_roman_map,
           tt_roman_map TYPE STANDARD TABLE OF ts_roman_map WITH EMPTY KEY.

    METHODS convert
      IMPORTING iv_input         TYPE i
      RETURNING VALUE(rv_output) TYPE string.

ENDCLASS.

CLASS lcl_roman IMPLEMENTATION.

  METHOD convert.
    DATA(lv_number) = iv_input.
    CLEAR rv_output.

    DATA(lt_mapping) = VALUE tt_roman_map( ( arabic = 5 roman = `V` )
                                           ( arabic = 4 roman = `IV` )
                                           ( arabic = 1 roman = `I` ) ).

    LOOP AT lt_mapping INTO DATA(ls_mapping).
      WHILE ( lv_number >= ls_mapping-arabic ).
        rv_output = rv_output && ls_mapping-roman.
        lv_number = lv_number - ls_mapping-arabic.
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* TEST CODE
*----------------------------------------------------------------------*
CLASS ltcl_roman01 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      get_i_when_1 FOR TESTING RAISING cx_static_check,
      get_ii_when_2 FOR TESTING RAISING cx_static_check,
      get_iii_when_3 FOR TESTING RAISING cx_static_check,
      get_iv_when_4 FOR TESTING RAISING cx_static_check,
      get_v_when_5 FOR TESTING RAISING cx_static_check.
    METHODS get_new_roman
      RETURNING
        VALUE(r_result) TYPE REF TO lcl_roman.
ENDCLASS.


CLASS ltcl_roman01 IMPLEMENTATION.

  METHOD get_i_when_1.
    DATA(lo_roman) = get_new_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'I'
            act = lo_roman->convert( 1 ) ).
  ENDMETHOD.

  METHOD get_new_roman.
    r_result = NEW lcl_roman( ).
  ENDMETHOD.

  METHOD get_iii_when_3.
    DATA(lo_roman) = get_new_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'III'
            act = lo_roman->convert( 3 ) ).
  ENDMETHOD.

  METHOD get_ii_when_2.
    DATA(lo_roman) = get_new_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'II'
            act = lo_roman->convert( 2 ) ).
  ENDMETHOD.

  METHOD get_iv_when_4.
    DATA(lo_roman) = get_new_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'IV'
            act = lo_roman->convert( 4 ) ).
  ENDMETHOD.

  METHOD get_v_when_5.
    DATA(lo_roman) = get_new_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'V'
            act = lo_roman->convert( 5 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_roman02 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      data_driven_testing FOR TESTING RAISING cx_static_check,
      run_variants
        IMPORTING
          iv_container_name TYPE etobj_name.
ENDCLASS.


CLASS ltcl_roman02 IMPLEMENTATION.

  METHOD data_driven_testing.
    run_variants( 'ZTDD_EX02_ROMAN_01' ).
  ENDMETHOD.

  METHOD run_variants.

    DATA: lt_variants TYPE etvar_name_tabtype,
          lo_ex       TYPE REF TO cx_root,
          lo_roman    TYPE REF TO lcl_roman,
          lv_arabic   TYPE i,
          lv_roman    TYPE string.

    "SECATT Test Data Container
    TRY .
        DATA(lo_tdc_api) = cl_apl_ecatt_tdc_api=>get_instance( iv_container_name ).

        " Get all variants from test data container
        lt_variants = lo_tdc_api->get_variant_list( ).

      CATCH cx_ecatt_tdc_access INTO lo_ex.
        cl_aunit_assert=>fail(
            msg  = |Container { iv_container_name } failed: { lo_ex->get_text( ) }|
            quit = if_aunit_constants=>no ).
        RETURN.

    ENDTRY.

    "Skip default variant
    DELETE lt_variants WHERE table_line = 'ECATTDEFAULT'.

    "Execute test method for all data variants
    lo_roman = NEW #( ).
    LOOP AT lt_variants INTO DATA(lv_variant).

      TRY .
          lo_tdc_api->get_value(
            EXPORTING
              i_param_name = 'ARABIC'
              i_variant_name = lv_variant
            CHANGING
              e_param_value = lv_arabic ).
          lo_tdc_api->get_value(
            EXPORTING
              i_param_name = 'ROMAN'
              i_variant_name = lv_variant
            CHANGING
              e_param_value = lv_roman ).

          DATA(lv_actual) = lo_roman->convert( lv_arabic ).
          cl_abap_unit_assert=>assert_equals(
                  msg = |Variant { lv_variant } failed: Actual [{ lv_actual }] Expect [{ lv_roman }]|
                  exp = lv_roman
                  act = lv_actual ).

        CATCH cx_root INTO lo_ex.
          cl_aunit_assert=>fail(
              msg  = |Variant { lv_variant } failed: { lo_ex->get_text( ) }|
              quit = if_aunit_constants=>no ).
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.