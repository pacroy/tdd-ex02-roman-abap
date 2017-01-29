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
    METHODS convert
      IMPORTING iv_input         TYPE i
      RETURNING VALUE(rv_output) TYPE string.

ENDCLASS.

CLASS lcl_roman IMPLEMENTATION.

  METHOD convert.
    rv_output = 'I'.
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
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_roman01 IMPLEMENTATION.

  METHOD first_test.
    DATA(lo_roman) = NEW lcl_roman( ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'I'
            act = lo_roman->convert( 1 ) ).
    cl_abap_unit_assert=>assert_equals(
            exp = 'II'
            act = lo_roman->convert( 2 ) ).
  ENDMETHOD.

ENDCLASS.