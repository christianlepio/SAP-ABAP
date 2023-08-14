*&---------------------------------------------------------------------*
*& Report ZABAP_EX7_PDC6
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_ex7_pdc6.

TYPE-POOLS: truxs, slis.

TYPES: BEGIN OF ty_final,
         cust_no TYPE kunnr,
         name    TYPE name1_gp,
         city    TYPE ort01_gp,
         pstlz   TYPE pstlz,
         land1   TYPE land1_gp,
       END OF ty_final,

       BEGIN OF ty_line,
         line TYPE string,
       END OF ty_line.

DATA: gt_final    TYPE STANDARD TABLE OF ty_final,
      gs_final    TYPE ty_final,
      gt_line     TYPE STANDARD TABLE OF ty_line,
      gs_line     TYPE ty_line,

      gv_fln      TYPE rlgrap-filename,
      gv_path     TYPE char100,
      gv_al_file  TYPE char100,
      gt_raw      TYPE truxs_t_text_data,

      gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv.

INITIALIZATION.
  REFRESH: gt_final, gt_line, gt_raw, gt_fieldcat.
  CLEAR: gs_final, gs_line, gs_fieldcat, gv_fln, gv_path, gv_al_file.

  SELECTION-SCREEN BEGIN OF BLOCK frame0 WITH FRAME TITLE TEXT-0f0.
  PARAMETERS: rbup RADIOBUTTON GROUP dset USER-COMMAND test DEFAULT 'X',
              rbad RADIOBUTTON GROUP dset,
              rbrr RADIOBUTTON GROUP dset.
  SELECTION-SCREEN END OF BLOCK frame0.

  SELECTION-SCREEN BEGIN OF BLOCK frame3 WITH FRAME TITLE TEXT-0f3.
  PARAMETERS: p_alfl TYPE rlgrap-filename MODIF ID alp.
  SELECTION-SCREEN END OF BLOCK frame3.

  SELECTION-SCREEN BEGIN OF BLOCK frame1 WITH FRAME TITLE TEXT-0f1.
  PARAMETERS: p_custno TYPE kunnr MODIF ID adn,
              p_name   TYPE name1_gp MODIF ID adn,
              p_city   TYPE ort01_gp MODIF ID adn,
              p_pstlz  TYPE pstlz MODIF ID adn,
              p_cntry  TYPE land1_gp MODIF ID adn.
  SELECTION-SCREEN END OF BLOCK frame1.

  SELECTION-SCREEN BEGIN OF BLOCK frame2 WITH FRAME TITLE TEXT-0f2.
  PARAMETERS: p_upld TYPE rlgrap-filename MODIF ID upn.
  SELECTION-SCREEN END OF BLOCK frame2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF rbup EQ 'X' AND screen-group1 = 'UPN'.
      screen-active = 1.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rbup EQ 'X' AND screen-group1 = 'ALP'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rbad EQ 'X' AND screen-group1 = 'ADN'.
      screen-active = 1.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rbup NE 'X' AND screen-group1 = 'UPN'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE .
    ELSEIF rbad NE 'X' AND screen-group1 = 'ADN'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE.
    ELSEIF rbrr EQ 'X' AND screen-group1 = 'UPN'.
      screen-active = 1.
      MODIFY SCREEN.
      CONTINUE .
    ELSEIF rbad EQ 'X' AND screen-group1 = 'ADN'.
      screen-active = 1.
      MODIFY SCREEN.
      CONTINUE .
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upld.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_upld.

START-OF-SELECTION.

END-OF-SELECTION.

  CASE 'X'.
    WHEN rbup.
      IF p_upld IS NOT INITIAL.
        CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP' "to pass data inside the xlsx file into itab.
          EXPORTING
            i_line_header        = 'X' "to eliminate header in uploaded xlsx file.
            i_tab_raw_data       = gt_raw
            i_filename           = p_upld
          TABLES
            i_tab_converted_data = gt_final "to handle data inside the uploaded xlsx file.
          EXCEPTIONS
            conversion_failed    = 1
            OTHERS               = 2.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH' "FM to split file path to file name.
            EXPORTING
              full_name     = p_upld
            IMPORTING
              stripped_name = gv_fln "this is for filename.
              file_path     = gv_path "this is for file path
            EXCEPTIONS
              x_error       = 1
              OTHERS        = 2.
          IF sy-subrc EQ 0.
            CONCATENATE '/tmp/' gv_fln INTO gv_al_file.

            IF gt_final[] IS NOT INITIAL.
              LOOP AT gt_final INTO gs_final.
                PERFORM concat_data USING gs_final-cust_no gs_final-name gs_final-city gs_final-pstlz gs_final-land1. "concatinate values

                APPEND gs_line TO gt_line.
                CLEAR: gs_final, gs_line.
              ENDLOOP.

              IF gt_line[] IS NOT INITIAL.
                OPEN DATASET gv_al_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT. "to open dataset and upload file in app. server file.
                IF sy-subrc EQ 0.
                  LOOP AT gt_line INTO gs_line.
                    TRANSFER gs_line-line TO gv_al_file. "to transfer structure to app. server file.
                    CLEAR gs_line.
                  ENDLOOP.

                  CLOSE DATASET gv_al_file. "close dataset

                  MESSAGE: 'File has been uploaded successfully to application server file.'  TYPE 'S'.
                ELSE.
                  MESSAGE: 'Filepath not found in TCODE: AL11.' TYPE 'S'.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE: 'Failed to split file path and name.' TYPE 'S'.
          ENDIF.
        ELSE.
          MESSAGE: 'Failed to convert xls / xls file to text.' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE: 'Please specify xls/xlsx file.' TYPE 'S'.
      ENDIF.

    WHEN rbrr.
      IF p_alfl IS NOT INITIAL.
        REFRESH: gt_line, gt_final.

        OPEN DATASET p_alfl FOR INPUT IN TEXT MODE ENCODING DEFAULT. "to open dataset and read file in app. server file.
        IF sy-subrc EQ 0.
          DO.
            READ DATASET p_alfl INTO gs_line-line. "read file
            IF sy-subrc EQ 0.
              APPEND gs_line TO gt_line.
              CLEAR gs_line.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.

          CLOSE DATASET p_alfl. "close dataset.
          IF gt_line[] IS NOT INITIAL.
            LOOP AT gt_line INTO gs_line.
              SPLIT gs_line-line AT cl_abap_char_utilities=>horizontal_tab
              INTO gs_final-cust_no
                   gs_final-name
                   gs_final-city
                   gs_final-pstlz
                   gs_final-land1.

              APPEND gs_final TO gt_final.
              CLEAR: gs_final, gs_line.
            ENDLOOP.
            IF gt_final[] IS NOT INITIAL.
              PERFORM print_alv USING 'cust_no' 'Customer No.'.
              PERFORM print_alv USING 'Name' 'Name'.
              PERFORM print_alv USING 'city' 'City'.
              PERFORM print_alv USING 'pstlz' 'Postal Code'.
              PERFORM print_alv USING 'land1' 'Country'.

              CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
                EXPORTING
                  i_callback_program = sy-repid
                  it_fieldcat        = gt_fieldcat
                TABLES
                  t_outtab           = gt_final
                EXCEPTIONS
                  program_error      = 1
                  OTHERS             = 2.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE: 'File path not found in application server file.' TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE: 'Please specify AL11-FILE PATH.' TYPE 'S'.
      ENDIF.
    WHEN OTHERS.
      IF p_alfl IS NOT INITIAL.
        REFRESH: gt_line, gt_final.
        CLEAR gs_line.
        PERFORM concat_data USING p_custno p_name p_city p_pstlz p_cntry. "concatinate values.

        IF gs_line IS NOT INITIAL.
          OPEN DATASET p_alfl FOR APPENDING IN TEXT MODE ENCODING DEFAULT. "to open dataset and append data in app. server file.
          IF sy-subrc EQ 0.
            TRANSFER gs_line-line TO p_alfl. "to transfer structure to app. server file.
            CLOSE DATASET p_alfl. "close dataset.

            CLEAR: gs_line.

            MESSAGE: 'Data has been added to application server file.'  TYPE 'S'.
          ELSE.
            MESSAGE: 'File path not found in application server file.' TYPE 'S'.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE: 'Please specify AL11-FILE PATH.' TYPE 'S'.
      ENDIF.
  ENDCASE.

FORM print_alv USING fldname TYPE slis_fieldname
                     sltext  TYPE string.
  gs_fieldcat-fieldname = fldname.
  gs_fieldcat-seltext_l = sltext.
  gs_fieldcat-outputlen = '20'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
ENDFORM.

FORM concat_data USING cust_no TYPE kunnr
                       name TYPE name1_gp
                       city TYPE ort01_gp
                       pstlz TYPE pstlz
                       land1 TYPE land1_gp.
  CONCATENATE cust_no name city pstlz land1
  INTO gs_line-line
  SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
ENDFORM.
