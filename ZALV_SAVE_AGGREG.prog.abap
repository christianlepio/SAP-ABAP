*&---------------------------------------------------------------------*
*& Report ZTEST_LEPS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_leps.
*&---------------------------------------------------------------------*
*& DATA DECLARATIONS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_output,
soldto TYPE kunag,
contrct_no TYPE vbelv,
budget TYPE netwr,
year TYPE char4,
yrmos TYPE string,
expns_amnt TYPE netwr,
jan TYPE netwr,
feb TYPE netwr,
mar TYPE netwr,
apr TYPE netwr,
may TYPE netwr,
jun TYPE netwr,
jul TYPE netwr,
aug TYPE netwr,
sep TYPE netwr,
oct TYPE netwr,
nov TYPE netwr,
dec TYPE netwr,
total TYPE netwr,
net_avail TYPE netwr,
END OF ty_output.
DATA: gt_output TYPE STANDARD TABLE OF ty_output,
gt_final TYPE STANDARD TABLE OF ty_output,
gt_moname TYPE STANDARD TABLE OF t247,
gt_vbak TYPE STANDARD TABLE OF vbak,
gt_vbfa TYPE STANDARD TABLE OF vbfa,
gt_vbrk TYPE STANDARD TABLE OF vbrk,
gs_vbak TYPE vbak,
gs_vbrk TYPE vbrk,
gv_retcode TYPE sy-subrc
.
DATA: go_salv_tbl TYPE REF TO cl_salv_table,
gv_msg1 TYPE REF TO cx_salv_msg ##NEEDED,
gv_msg2 TYPE REF TO cx_salv_not_found ##NEEDED,
gr_layout TYPE REF TO cl_salv_layout, "cl_salv_table,
gr_columns TYPE REF TO cl_salv_columns_table,
gr_sorts TYPE REF TO cl_salv_sorts,
gr_agg TYPE REF TO cl_salv_aggregations,
gv_key TYPE salv_s_layout_key,
gv_set_lay TYPE slis_vari.

INITIALIZATION.
FREE: gt_output, gt_moname, gt_vbak, gt_vbfa, gt_vbrk, gs_vbak, gs_vbrk, gv_retcode.
*&---------------------------------------------------------------------*
*& SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK frame1 WITH FRAME TITLE TEXT-sdh. "VBAK – Sales
Document Data
SELECT-OPTIONS: so_contr FOR gs_vbak-vbeln, "Document
so_dctyp FOR gs_vbak-auart DEFAULT 'ZWK1', "Contract type
so_soldt FOR gs_vbak-kunnr. "Sold-to Party
SELECTION-SCREEN END OF BLOCK frame1.
SELECTION-SCREEN BEGIN OF BLOCK frame2 WITH FRAME TITLE TEXT-vtd. "VBAK – Validity
Date
PARAMETERS: p_date1 TYPE vbak-guebg, "valid from
p_date2 TYPE vbak-gueen DEFAULT sy-datum. "valid to
SELECTION-SCREEN END OF BLOCK frame2.
SELECTION-SCREEN BEGIN OF BLOCK frame3 WITH FRAME TITLE TEXT-ord. "VBAK –
Organizational Data
SELECT-OPTIONS: so_slorg FOR gs_vbak-vkorg DEFAULT 'CA10', "Sales Organization
so_dchnl FOR gs_vbak-vtweg, "Distribution Channel
so_divsn FOR gs_vbak-spart. "Division
SELECTION-SCREEN END OF BLOCK frame3.
AT SELECTION-SCREEN OUTPUT.
AT SELECTION-SCREEN.
START-OF-SELECTION.
PERFORM f_get_data.
END-OF-SELECTION.
IF gt_vbrk[] IS NOT INITIAL.
PERFORM f_process_data.
IF gt_final[] IS NOT INITIAL.
PERFORM f_disp_alv.
ELSE.
MESSAGE 'No entries found!' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.
ELSE.
MESSAGE 'No entries found!' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.

*&---------------------------------------------------------------------*
*& SUB-ROUTINES
*&---------------------------------------------------------------------*

FORM f_get_data.
SELECT FROM vbak
FIELDS *
WHERE vbeln IN @so_contr "Document
AND vbtyp EQ 'G' "Contracts only
AND auart IN @so_dctyp "Contract type
AND kunnr IN @so_soldt "sold-to party
AND ( guebg LE @p_date2
AND gueen GE @p_date1 ) "valid-to, valid-from
AND vkorg IN @so_slorg "Sales Organization
AND vtweg IN @so_dchnl "Distribution Channel
AND spart IN @so_divsn "Division
INTO TABLE @gt_vbak.
IF sy-subrc EQ 0.
SORT gt_vbak BY vbeln.
SELECT FROM vbfa
FIELDS *
FOR ALL ENTRIES IN @gt_vbak
WHERE vbelv EQ @gt_vbak-vbeln
AND vbtyp_n EQ 'O'
INTO TABLE @gt_vbfa.
IF sy-subrc EQ 0.
SORT gt_vbfa BY vbeln.
SELECT FROM vbrk
FIELDS *
FOR ALL ENTRIES IN @gt_vbfa
WHERE vbeln EQ @gt_vbfa-vbeln
INTO TABLE @gt_vbrk.
IF sy-subrc EQ 0.
SORT gt_vbrk BY vbeln.
SELECT FROM bseg
FIELDS h_monat,
gjahr
FOR ALL ENTRIES IN @gt_vbrk
WHERE vbeln EQ @gt_vbrk-vbeln
INTO TABLE @DATA(gt_bseg).
ENDIF.
ENDIF.
ENDIF.
ENDFORM.
FORM f_process_data.
CALL FUNCTION 'MONTH_NAMES_GET'
EXPORTING
language = sy-langu
IMPORTING
return_code = gv_retcode
TABLES
month_names = gt_moname
EXCEPTIONS
month_names_not_found = 1
OTHERS = 2.
IF sy-subrc <> 0.

MESSAGE 'Unknown Month name.' TYPE 'E'.
ELSE.
LOOP AT gt_vbrk INTO gs_vbrk.
DATA(gv_contract) = VALUE #( gt_vbfa[ vbeln = gs_vbrk-vbeln ]-vbelv OPTIONAL ).
DATA(gv_yrmos) = |{ VALUE #( gt_moname[ mnr = gs_vbrk-fkdat+4(2) ]-ktx OPTIONAL
) }/{ gs_vbrk-fkdat(4) }|.
APPEND VALUE #(
soldto = gs_vbrk-kunag
contrct_no = gv_contract
budget = space
year = gs_vbrk-fkdat(4)
yrmos = gv_yrmos
expns_amnt = gs_vbrk-netwr
) TO gt_output.
FREE: gs_vbrk.
ENDLOOP.
IF gt_output[] IS NOT INITIAL.
SORT gt_output BY soldto contrct_no yrmos.
DATA(gt_temp) = gt_output.
DELETE ADJACENT DUPLICATES FROM gt_temp COMPARING soldto contrct_no yrmos.
LOOP AT gt_temp ASSIGNING FIELD-SYMBOL(<fs_final>).
<fs_final>-expns_amnt = REDUCE netwr( INIT lv_val TYPE netwr
FOR ls_output IN gt_output
WHERE ( soldto = <fs_final>-soldto
AND contrct_no = <fs_final>-
contrct_no
AND yrmos = <fs_final>-yrmos )

NEXT lv_val = lv_val + ls_output-
expns_amnt ).

ENDLOOP.
UNASSIGN <fs_final>.
LOOP AT gt_temp INTO DATA(gs_temp) GROUP BY ( key1 = gs_temp-soldto
key2 = gs_temp-contrct_no
key3 = gs_temp-year ).

APPEND VALUE #(
soldto = gs_temp-soldto
contrct_no = gs_temp-contrct_no
year = gs_temp-year
) TO gt_final.
FREE: gs_temp.
ENDLOOP.
LOOP AT gt_final ASSIGNING <fs_final>.
<fs_final>-budget = abs( VALUE #( gt_vbak[ vbeln = <fs_final>-contrct_no ]-
netwr OPTIONAL ) ). "budget
LOOP AT gt_temp INTO gs_temp WHERE soldto EQ <fs_final>-soldto
AND contrct_no = <fs_final>-contrct_no

AND year = <fs_final>-year.

PERFORM f_get_expns_amnt USING gs_temp 'JAN' CHANGING <fs_final>-jan.

PERFORM f_get_expns_amnt USING gs_temp 'FEB' CHANGING <fs_final>-feb.
PERFORM f_get_expns_amnt USING gs_temp 'MAR' CHANGING <fs_final>-mar.
PERFORM f_get_expns_amnt USING gs_temp 'APR' CHANGING <fs_final>-apr.
PERFORM f_get_expns_amnt USING gs_temp 'MAY' CHANGING <fs_final>-may.
PERFORM f_get_expns_amnt USING gs_temp 'JUN' CHANGING <fs_final>-jun.
PERFORM f_get_expns_amnt USING gs_temp 'JUL' CHANGING <fs_final>-jul.
PERFORM f_get_expns_amnt USING gs_temp 'AUG' CHANGING <fs_final>-aug.
PERFORM f_get_expns_amnt USING gs_temp 'SEP' CHANGING <fs_final>-sep.
PERFORM f_get_expns_amnt USING gs_temp 'OCT' CHANGING <fs_final>-oct.
PERFORM f_get_expns_amnt USING gs_temp 'NOV' CHANGING <fs_final>-nov.
PERFORM f_get_expns_amnt USING gs_temp 'DEC' CHANGING <fs_final>-dec.
FREE gs_temp.
ENDLOOP.
"get total
<fs_final>-total = <fs_final>-jan + <fs_final>-feb + <fs_final>-mar +
<fs_final>-apr + <fs_final>-may + <fs_final>-jun + <fs_final>-jul + <fs_final>-aug +
<fs_final>-sep + <fs_final>-oct + <fs_final>-nov + <fs_final>-dec.
<fs_final>-net_avail = <fs_final>-budget - <fs_final>-total. "get net avail
ENDLOOP.
UNASSIGN <fs_final>.
ENDIF.
ENDIF.
ENDFORM.
FORM f_disp_alv.
TRY .
cl_salv_table=>factory(
IMPORTING
r_salv_table = go_salv_tbl
CHANGING
t_table = gt_final
).
IF sy-subrc EQ 0.
go_salv_tbl->get_functions( )->set_all( abap_true ).
go_salv_tbl->get_functions( )->set_view_excel( abap_false ).
go_salv_tbl->get_functions( )->set_export_localfile( abap_false ).
go_salv_tbl->get_columns( )->get_column( 'YRMOS' )->set_visible( space ).
go_salv_tbl->get_columns( )->get_column( 'EXPNS_AMNT' )->set_visible( space ).
PERFORM f_set_colname USING 'SOLDTO' 'Sold-To' 'Sold-To' 'Sold-To'.
PERFORM f_set_colname USING 'CONTRCT_NO' 'Contract' 'Contract No.' 'Contract
No.'.
PERFORM f_set_colname USING 'BUDGET' 'Budget' 'Budget Amount' 'Budget Amount'.
PERFORM f_set_colname USING 'YEAR' 'Year' 'Year' 'Year'.
PERFORM f_set_colname USING 'JAN' 'JAN' 'JAN' 'JAN'.
PERFORM f_set_colname USING 'FEB' 'FEB' 'FEB' 'FEB'.
PERFORM f_set_colname USING 'MAR' 'MAR' 'MAR' 'MAR'.
PERFORM f_set_colname USING 'APR' 'APR' 'APR' 'APR'.
PERFORM f_set_colname USING 'MAY' 'MAY' 'MAY' 'MAY'.
PERFORM f_set_colname USING 'JUN' 'JUN' 'JUN' 'JUN'.
PERFORM f_set_colname USING 'JUL' 'JUL' 'JUL' 'JUL'.

PERFORM f_set_colname USING 'AUG' 'AUG' 'AUG' 'AUG'.
PERFORM f_set_colname USING 'SEP' 'SEP' 'SEP' 'SEP'.
PERFORM f_set_colname USING 'OCT' 'OCT' 'OCT' 'OCT'.
PERFORM f_set_colname USING 'NOV' 'NOV' 'NOV' 'NOV'.
PERFORM f_set_colname USING 'DEC' 'DEC' 'DEC' 'DEC'.
PERFORM f_set_colname USING 'TOTAL' 'Total' 'Total' 'Total'.
PERFORM f_set_colname USING 'NET_AVAIL' 'Net Avail' 'Net Avail' 'Net Avail'.
go_salv_tbl->get_columns( )->set_optimize( abap_true ).
go_salv_tbl->get_display_settings( )->set_striped_pattern( abap_true ).
"Layout changes...
gr_columns = go_salv_tbl->get_columns( ).
gr_sorts = go_salv_tbl->get_sorts( ).
gr_agg = go_salv_tbl->get_aggregations( ).
gr_layout = go_salv_tbl->get_layout( ).
"Set sub totals...
gr_agg->clear( ).
gr_sorts->clear( ).
TRY .
gr_sorts->add_sort(
columnname = 'SOLDTO'
position = 1
subtotal = abap_false
sequence = if_salv_c_sort=>sort_up
).
gr_sorts->add_sort(
columnname = 'CONTRCT_NO'
position = 2
subtotal = abap_true
sequence = if_salv_c_sort=>sort_up
).
CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
ENDTRY.
TRY .
gr_agg->add_aggregation(
columnname = 'TOTAL'
).
CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
ENDTRY.
* Saving Layouts
gv_key-report = sy-repid.
gr_layout->set_key( gv_key ).
gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
* Setting Layout Screen from selection screen.
gr_layout->set_initial_layout( value = gv_set_lay ).

go_salv_tbl->display( ).
ENDIF.
*
CATCH cx_salv_msg INTO gv_msg1 ##NO_HANDLER.
CATCH cx_salv_not_found INTO gv_msg2.
ENDTRY.
ENDFORM.
FORM f_set_colname USING i_colname TYPE lvc_fname
i_stxt TYPE scrtext_s
i_mtxt TYPE scrtext_m
i_ltxt TYPE scrtext_l.
TRY .
go_salv_tbl->get_columns( )->get_column( i_colname )->set_short_text( i_stxt ).
go_salv_tbl->get_columns( )->get_column( i_colname )->set_medium_text( i_mtxt ).
go_salv_tbl->get_columns( )->get_column( i_colname )->set_long_text( i_ltxt ).
CATCH cx_salv_msg INTO gv_msg1 ##NO_HANDLER.
CATCH cx_salv_not_found INTO gv_msg2.
ENDTRY.
ENDFORM.
FORM f_get_expns_amnt USING i_ls_temp TYPE ty_output
i_lv_mos TYPE char3
CHANGING c_lv_expns TYPE netwr.

c_lv_expns = COND netwr( WHEN i_ls_temp-yrmos(3) EQ i_lv_mos THEN i_ls_temp-
expns_amnt ELSE 0 ).

ENDFORM.