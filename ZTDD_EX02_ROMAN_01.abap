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

    DATA(lt_mapping) = VALUE tt_roman_map( ( arabic = 4 roman = `IV` )
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
      get_iv_when_4 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_roman01 IMPLEMENTATION.

  METHOD get_i_when_1.
    DATA(lo_roman) = NEW lcl_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'I'
            act = lo_roman->convert( 1 ) ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'II'
            act = lo_roman->convert( 2 ) ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'III'
            act = lo_roman->convert( 3 ) ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'IV'
            act = lo_roman->convert( 4 ) ).
  ENDMETHOD.

  METHOD get_ii_when_2.
    DATA(lo_roman) = NEW lcl_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'II'
            act = lo_roman->convert( 2 ) ).
  ENDMETHOD.

  METHOD get_iii_when_3.
    DATA(lo_roman) = NEW lcl_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'III'
            act = lo_roman->convert( 3 ) ).
  ENDMETHOD.

  METHOD get_iv_when_4.
    DATA(lo_roman) = NEW lcl_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'IV'
            act = lo_roman->convert( 4 ) ).
  ENDMETHOD.

ENDCLASS.