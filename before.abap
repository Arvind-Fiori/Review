*&---------------------------------------------------------------------*
*&  Include           ZMM_VEN_CREATE_EMAIL_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

FORM SEND_MAIL_VEND .

  DATA : LT_LIFNR TYPE TABLE OF LIFNR,
         LW_LIFNR TYPE LIFNR.

  DATA : LW_NX_ADRNR TYPE ADRNR .

  DATA : LT_ADRNR TYPE TABLE OF ADRNR,
         LW_ADRNR TYPE ADRNR.

  IF P_VCRE_S = 'X'.

    SELECT LIFNR ,
           NAME1 ,
           ERDAT ,
           ERNAM ,
           ADRNR ,
           STCD3 ,
           STCD5,
           TELF1,
           TELF2 ,
           REGIO  FROM LFA1
           INTO TABLE @DATA(LT_LFA1)
           WHERE ERDAT IN @S_DATE .
    IF SY-SUBRC EQ 0.
      SORT LT_LFA1 BY LIFNR .
    ENDIF.

  ELSEIF P_VCHG_S = 'X' .

    SELECT OBJECTCLAS,
           OBJECTID ,
           CHANGENR,
           UDATE ,
           UTIME ,
           TCODE ,
           CHANGE_IND ,
           LANGU FROM CDHDR
           INTO TABLE @DATA(LT_CDHDR)
           WHERE OBJECTCLAS IN ( 'ADRESSE' , 'MKK_BPTAX' , 'BUPA_BUP' , 'DEBI' , 'KRED' )
           AND UDATE IN @S_DATE
           AND TCODE IN ( 'XK01' , 'XK02' , 'XK03' )
           AND CHANGE_IND = 'U'
           AND LANGU = 'E' .
    IF SY-SUBRC EQ 0.
      SORT LT_CDHDR BY OBJECTCLAS OBJECTID CHANGE_IND .
    ENDIF.

    IF LT_CDHDR IS NOT INITIAL.

      SELECT OBJECTCLAS,
             OBJECTID  ,
             CHANGENR  ,
             TABNAME   ,
             FNAME     ,
             CHNGIND   ,
             VALUE_NEW ,
             VALUE_OLD  FROM CDPOS
             INTO TABLE @DATA(LT_CDPOS)
             FOR ALL ENTRIES IN @LT_CDHDR
             WHERE OBJECTCLAS = @LT_CDHDR-OBJECTCLAS
             AND OBJECTID = @LT_CDHDR-OBJECTID
            AND CHANGENR = @LT_CDHDR-CHANGENR .
      IF SY-SUBRC EQ 0.
        LOOP AT LT_CDPOS ASSIGNING FIELD-SYMBOL(<LFS_CDPOS>) WHERE TABNAME = 'ADRC' OR TABNAME = 'ADR6' .
          <LFS_CDPOS>-OBJECTID = <LFS_CDPOS>-OBJECTID+4(10) .
        ENDLOOP.
        SORT LT_CDPOS BY OBJECTCLAS OBJECTID CHANGENR TABNAME FNAME .
        DELETE ADJACENT DUPLICATES FROM LT_CDPOS COMPARING OBJECTCLAS OBJECTID CHANGENR TABNAME FNAME .
        SORT LT_CDPOS BY OBJECTID TABNAME FNAME .
      ENDIF.
    ENDIF.

    LOOP AT LT_CDHDR INTO DATA(LW_CDHDR) .
      IF LW_CDHDR-OBJECTCLAS = 'ADRESSE'.
        LW_CDHDR-OBJECTID = LW_CDHDR-OBJECTID+4(10) .
        IF SY-SYSID = 'ATQ' .
          CONDENSE LW_CDHDR-OBJECTID .
          LW_ADRNR = LW_CDHDR-OBJECTID - 1 .
        ELSE.
          CONDENSE LW_CDHDR-OBJECTID .
          LW_ADRNR = LW_CDHDR-OBJECTID .
        ENDIF.

        CONDENSE : LW_ADRNR .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LW_ADRNR
          IMPORTING
            OUTPUT = LW_ADRNR.

        APPEND LW_ADRNR TO LT_ADRNR .
        CLEAR : LW_ADRNR .
      ENDIF.
      LW_LIFNR = LW_CDHDR-OBJECTID .
      APPEND LW_LIFNR TO LT_LIFNR .
      CLEAR : LW_LIFNR , LW_CDHDR .
    ENDLOOP.

    IF LT_ADRNR IS NOT INITIAL.
      REFRESH LT_LFA1 .
      SELECT LIFNR ,
             NAME1 ,
             ERDAT ,
             ERNAM ,
             ADRNR ,
             STCD3 ,
             STCD5 ,
             TELF1,
             TELF2 ,
             REGIO FROM LFA1
             INTO TABLE @LT_LFA1
             FOR ALL ENTRIES IN @LT_ADRNR
             WHERE ADRNR = @LT_ADRNR-TABLE_LINE .
      IF SY-SUBRC EQ 0.
        SORT LT_LFA1 BY LIFNR .
      ENDIF.
    ENDIF.
  ENDIF.

  IF LT_LFA1 IS NOT INITIAL.
    SELECT SPRAS,
           LAND1,
           BLAND,
           BEZEI
           FROM T005U
           INTO TABLE @DATA(LT_T005U)
           FOR ALL ENTRIES IN @LT_LFA1
           WHERE SPRAS = 'E'
           AND LAND1 = 'IN'
           AND BLAND = @LT_LFA1-REGIO .
    IF SY-SUBRC EQ 0.
      SORT LT_T005U BY BLAND .
    ENDIF.

    SELECT ADDRNUMBER ,
           CITY1 ,
           POST_CODE1,
           STR_SUPPL1 ,
           STR_SUPPL2 ,
           COUNTRY ,
           REGION FROM ADRC
           INTO TABLE @DATA(LT_ADRC)
           FOR ALL ENTRIES IN @LT_LFA1
           WHERE ADDRNUMBER = @LT_LFA1-ADRNR .
    IF SY-SUBRC EQ 0.
      SORT LT_ADRC BY ADDRNUMBER .
    ENDIF.

    SELECT ADDRNUMBER ,
           TELNR_LONG ,
           TEL_NUMBER FROM ADR2
           INTO TABLE @DATA(LT_ADR2)
           FOR ALL ENTRIES IN @LT_LFA1
           WHERE ADDRNUMBER = @LT_LFA1-ADRNR .
    IF SY-SUBRC EQ 0.
      SORT LT_ADR2 BY ADDRNUMBER .
    ENDIF.

    SELECT ADDRNUMBER ,
           SMTP_ADDR FROM ADR6
           INTO TABLE @DATA(LT_ADR6)
           FOR ALL ENTRIES IN @LT_LFA1
           WHERE ADDRNUMBER = @LT_LFA1-ADRNR .
    IF SY-SUBRC EQ 0.
      SORT LT_ADR6 BY ADDRNUMBER .
    ENDIF.

    SELECT LIFNR,
           J_1IPANNO FROM J_1IMOVEND
           INTO TABLE @DATA(LT_J_1IMOVEND)
           FOR ALL ENTRIES IN @LT_LFA1
           WHERE LIFNR = @LT_LFA1-LIFNR .
    IF SY-SUBRC EQ 0.
      SORT LT_J_1IMOVEND BY LIFNR .
    ENDIF.

    SELECT LIFNR,
           BANKS,
           BANKL,
           BANKN,
           KOINH FROM LFBK
           INTO TABLE @DATA(LT_LFBK)
           FOR ALL ENTRIES IN @LT_LFA1
           WHERE LIFNR = @LT_LFA1-LIFNR .
    IF SY-SUBRC EQ 0.
      SORT LT_LFBK BY LIFNR .
    ENDIF.

    SELECT LIFNR,
           BUKRS FROM LFB1
           INTO TABLE @DATA(LT_LFB1)
           FOR ALL ENTRIES IN @LT_LFA1
           WHERE LIFNR = @LT_LFA1-LIFNR
           AND BUKRS IN @S_BUKRS .
    IF SY-SUBRC EQ 0.
      SORT LT_LFB1 BY LIFNR BUKRS .
    ENDIF.

  ENDIF.

  IF LT_LFB1 IS NOT INITIAL.
    SELECT LIFNR,
           BUKRS,
           WITHT FROM LFBW
           INTO TABLE @DATA(LT_LFBW)
           FOR ALL ENTRIES IN @LT_LFB1
           WHERE LIFNR = @LT_LFB1-LIFNR
           AND BUKRS = @LT_LFB1-BUKRS .
    IF SY-SUBRC EQ 0.
      SORT LT_LFBW BY LIFNR BUKRS .
    ENDIF.
  ENDIF.

  CLEAR : GW_COL_CONT .

  LOOP AT LT_LFA1 INTO DATA(LW_LFA1).
    DATA(LW_ADRC) = VALUE #( LT_ADRC[ ADDRNUMBER = LW_LFA1-ADRNR ] OPTIONAL ) .
    DATA(LW_ADR2) = VALUE #( LT_ADR2[ ADDRNUMBER = LW_LFA1-ADRNR ] OPTIONAL ) .
    DATA(LW_ADR6) = VALUE #( LT_ADR6[ ADDRNUMBER = LW_LFA1-ADRNR ] OPTIONAL ) .
    DATA(LW_J_1IMOVEND) = VALUE #( LT_J_1IMOVEND[ LIFNR = LW_LFA1-LIFNR ] OPTIONAL ) .
    DATA(LW_LFBK) = VALUE #( LT_LFBK[ LIFNR = LW_LFA1-LIFNR ] OPTIONAL ) .
    DATA(LW_LFB1) = VALUE #( LT_LFB1[ LIFNR = LW_LFA1-LIFNR ] OPTIONAL ) .
    DATA(LW_LFBW) = VALUE #( LT_LFBW[ LIFNR = LW_LFB1-LIFNR BUKRS = LW_LFB1-BUKRS ] OPTIONAL ) .
    DATA(LW_S_ADR) = VALUE #( LT_T005U[ BLAND = LW_LFA1-REGIO ]-BEZEI  OPTIONAL ) .

    LW_TEXT-LINE = '<HTML>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<head>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    CONCATENATE '<P>' 'Please find the below vendor details.' '</P>'  INTO LW_TEXT-LINE.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '</head>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<body>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<table border="1" >'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    CLEAR : LW_P3 .
    LW_P3 = LW_LFA1-LIFNR .
    PERFORM BUILD_ROW USING 'Vendor Code' LW_P3 .
    CLEAR : LW_P3 .
    LW_P3 = LW_LFA1-NAME1 .
    PERFORM BUILD_ROW USING  'Name1' LW_P3 .

    CLEAR : LW_NX_ADRNR.
    IF SY-SYSID = 'ATQ'.
      LW_NX_ADRNR = LW_LFA1-ADRNR + 1 .
    ELSE.
      LW_NX_ADRNR = LW_NX_ADRNR .
    ENDIF.

    IF LW_NX_ADRNR IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = LW_NX_ADRNR
        IMPORTING
          OUTPUT = LW_NX_ADRNR.
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'STR_SUPPL1' ] ) .
        DATA(GW_ADRC_ACT) = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'STR_SUPPL2' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'POST_CODE1' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'CITY1'] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'REGION' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'COUNTRY' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF GW_ADRC_ACT = 'X' .
        CONCATENATE LW_ADRC-STR_SUPPL1 LW_ADRC-STR_SUPPL2 LW_ADRC-POST_CODE1
                          LW_ADRC-CITY1 LW_S_ADR LW_ADRC-COUNTRY INTO DATA(LW_ADR) SEPARATED BY ',' .
        CLEAR : LW_P3 .
        LW_P3 = LW_ADR .
        CLEAR : LW_ADR .
        PERFORM BUILD_ROW USING  'Address' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_ADR .
      CONCATENATE LW_ADRC-STR_SUPPL1 LW_ADRC-STR_SUPPL2 LW_ADRC-POST_CODE1
                       LW_ADRC-CITY1 LW_S_ADR LW_ADRC-COUNTRY INTO LW_ADR SEPARATED BY ',' .
      CLEAR : LW_P3 .
      LW_P3 = LW_ADR .
      CLEAR : LW_ADR .
      PERFORM BUILD_ROW USING  'Address' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFA1' FNAME = 'STCD3' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_J_1IMOVEND-J_1IPANNO  .
        PERFORM BUILD_ROW USING 'PAN Number' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_J_1IMOVEND-J_1IPANNO  .
      PERFORM BUILD_ROW USING  'PAN Number' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFA1' FNAME = 'TELF2' ] ).
        CLEAR : LW_P3 .
        LW_P3 =  LW_LFA1-TELF2  .
        PERFORM BUILD_ROW USING 'Mob. No.' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 =  LW_LFA1-TELF2  .
      PERFORM BUILD_ROW USING 'Mob. No.' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFA1' FNAME = 'TELF1' ] ).
        CLEAR : LW_P3 .
        LW_P3 =  LW_LFA1-TELF1  .
        PERFORM BUILD_ROW USING 'Telephone no' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 =  LW_LFA1-TELF1  .
      PERFORM BUILD_ROW USING 'Telephone no' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADR6' FNAME = 'SMTP_ADDR' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_ADR6-SMTP_ADDR  .
        PERFORM BUILD_ROW USING 'Email Id' LW_P3  .
      ENDIF.

    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_ADR6-SMTP_ADDR  .
      PERFORM BUILD_ROW USING 'Email Id' LW_P3  .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFA1' FNAME = 'STCD3' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_LFA1-STCD3  .
        PERFORM BUILD_ROW USING  'TAX Number' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_LFA1-STCD3   .
      PERFORM BUILD_ROW USING 'TAX Number' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFBK' FNAME = 'BANKN' ] ) .
        DATA(GW_B_DET) = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFBK' FNAME = 'KOINH' ] ) .
        CLEAR : GW_B_DET .
        GW_B_DET = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFBK' FNAME = 'BANKL' ] ) .
        CLEAR : GW_B_DET .
        GW_B_DET = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFBK' FNAME = 'BANKS' ] ) .
        CLEAR : GW_B_DET .
        GW_B_DET = 'X' .
      ENDIF.

      IF GW_B_DET = 'X'.
        PERFORM BUILD_ROW USING 'Bank Account Details' ' ' .

        CLEAR : LW_P3 .
        LW_P3 = LW_LFBK-BANKN  .
        PERFORM BUILD_ROW USING 'Bank Account' LW_P3  .

        CLEAR : LW_P3 .
        LW_P3 = LW_LFBK-KOINH .
        PERFORM BUILD_ROW USING  'Bank Account Name' LW_P3 .

        CLEAR : LW_P3 .
        LW_P3 = LW_LFBK-BANKL  .
        PERFORM BUILD_ROW USING  'Bank Key' LW_P3 .

        CLEAR : LW_P3 .
        LW_P3 = LW_LFBK-BANKS  .
        PERFORM BUILD_ROW USING  'Bank Country' LW_P3 .
      ENDIF.

    ELSE.

      PERFORM BUILD_ROW USING  'Bank Account Details' ' ' .
      CLEAR : LW_P3 .
      LW_P3 = LW_LFBK-BANKN  .
      PERFORM BUILD_ROW USING 'Bank Account' LW_P3  .
      CLEAR : LW_P3 .
      LW_P3 = LW_LFBK-KOINH .
      PERFORM BUILD_ROW USING 'Bank Account Name' LW_P3 .
      CLEAR : LW_P3 .
      LW_P3 = LW_LFBK-BANKL  .
      PERFORM BUILD_ROW USING  'Bank Key' LW_P3 .
      CLEAR : LW_P3 .
      LW_P3 = LW_LFBK-BANKS  .
      PERFORM BUILD_ROW USING  'Bank Country' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFB1' FNAME = 'BUKRS' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_LFB1-BUKRS  .
        PERFORM BUILD_ROW USING 'Company Code' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_LFB1-BUKRS  .
      PERFORM BUILD_ROW USING  'Company Code' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_LFA1-LIFNR TABNAME = 'LFA1' FNAME = 'STCD5' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_LFA1-STCD5  .
        PERFORM BUILD_ROW USING  'Tax number 5' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_LFA1-STCD5  .
      PERFORM BUILD_ROW USING  'Tax number 5' LW_P3 .
    ENDIF.

    LW_TEXT-LINE = '</table>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT .

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT .

    LW_TEXT-LINE = 'Thanks'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT .

    LW_TEXT-LINE = 'This is system generated mail so kindly do not reply.'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '</body>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '</HTML>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    IF LT_TEXT IS NOT INITIAL.
      TRY .
          GO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

          LW_LFA1-LIFNR = | { LW_LFA1-LIFNR ALPHA = OUT } | .

          CLEAR : LV_SUB , LV_SUBJJ .
          IF P_VCRE_S = 'X'.
            CONCATENATE 'Vendor code-' LW_LFA1-LIFNR 'created In SAP System' INTO LV_SUB SEPARATED BY SPACE .
            LV_SUBJJ = LV_SUB .
          ELSEIF P_VCHG_S = 'X' .
            CONCATENATE 'Vendor code-' LW_LFA1-LIFNR 'changed In SAP System' INTO LV_SUB SEPARATED BY SPACE .
            LV_SUBJJ = LV_SUB .
          ENDIF.

          GO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                I_TYPE    = 'HTM'
                I_TEXT    = LT_TEXT
                I_SUBJECT  = LV_SUBJJ ).

          CALL METHOD GO_SEND_REQUEST->SET_MESSAGE_SUBJECT
            EXPORTING
              IP_SUBJECT = LV_STRING.

          IF GO_DOCUMENT IS NOT INITIAL.

            CALL METHOD GO_SEND_REQUEST->SET_DOCUMENT( GO_DOCUMENT ).

            LO_REF_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = 'donotreply@arvind.in'
                                                                           I_ADDRESS_NAME   = 'Do not reply' ).
            CALL METHOD GO_SEND_REQUEST->SET_SENDER
              EXPORTING
                I_SENDER = LO_REF_SENDER.

            LV_MAIL1 = LW_ADR6-SMTP_ADDR .
            IF LV_MAIL1 IS NOT INITIAL.
              GI_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( LV_MAIL1 ).
              CALL METHOD GO_SEND_REQUEST->ADD_RECIPIENT(
                EXPORTING
                  I_RECIPIENT = GI_RECIPIENT
                  I_EXPRESS   = 'X' ).
              CLEAR : LV_MAIL1 .
            ENDIF.
          ENDIF.

          CALL METHOD GO_SEND_REQUEST->SET_SEND_IMMEDIATELY
            EXPORTING
              I_SEND_IMMEDIATELY = 'X'.

          CALL METHOD GO_SEND_REQUEST->SEND( ).
          IF SY-SUBRC EQ 0.
            DATA(LW_S_MSG) = 'X' .
            COMMIT WORK AND WAIT.
          ENDIF.

          FREE:  GI_RECIPIENT ,GO_DOCUMENT.
        CATCH CX_SY_REF_IS_INITIAL INTO GX_SY_REF_IS_INITIAL.
        CATCH CX_SEND_REQ_BCS INTO GX_SEND_REQ_BCS.
        CATCH CX_ADDRESS_BCS  INTO GX_ADDR_EXCEPTION.
        CATCH CX_DOCUMENT_BCS INTO GX_BCS_EXCEPTION.
      ENDTRY.
      REFRESH LT_TEXT .
    ENDIF.

    CLEAR : LW_LFA1 , LW_ADRC , LW_ADR2 , LW_ADR6 , LW_J_1IMOVEND , LW_LFBK, LW_LFB1 , LW_LFBW , GW_COL_CONT , GW_B_DET , LW_NX_ADRNR , LW_S_ADR.
  ENDLOOP.

  IF LW_S_MSG = 'X'.
    CLEAR : LW_S_MSG .
    MESSAGE 'Mail Sent Successfully' TYPE 'I' .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL_CUST
*&---------------------------------------------------------------------*
FORM SEND_MAIL_CUST .

  DATA : LT_KUNNR TYPE TABLE OF KUNNR,
         LW_KUNNR TYPE KUNNR.

  DATA : LW_NX_ADRNR TYPE ADRNR .

  DATA : LT_ADRNR TYPE TABLE OF ADRNR,
         LW_ADRNR TYPE ADRNR.

  IF P_VCRE_S = 'X'.

    SELECT KUNNR,
        BUKRS,
        ERDAT,
        AKONT FROM KNB1
        INTO TABLE @DATA(LT_KNB1)
        WHERE ERDAT IN @S_DATE
        AND BUKRS IN @S_BUKRS .
    IF SY-SUBRC EQ 0.
      SORT LT_KNB1 BY KUNNR .
    ENDIF.

  ELSEIF P_VCHG_S = 'X' .

    SELECT OBJECTCLAS,
           OBJECTID ,
           CHANGENR,
           UDATE ,
           UTIME ,
           TCODE ,
           CHANGE_IND ,
           LANGU FROM CDHDR
           INTO TABLE @DATA(LT_CDHDR)
           WHERE OBJECTCLAS IN ( 'ADRESSE' , 'MKK_BPTAX' , 'BUPA_BUP' , 'DEBI' , 'KRED' )
           AND UDATE IN @S_DATE
           AND TCODE IN ( 'XD01' , 'XD02' , 'XD03' )
           AND CHANGE_IND = 'U'
           AND LANGU = 'E' .
    IF SY-SUBRC EQ 0.
      SORT LT_CDHDR BY OBJECTCLAS OBJECTID CHANGE_IND .
    ENDIF.

    IF LT_CDHDR IS NOT INITIAL.

      SELECT OBJECTCLAS,
             OBJECTID  ,
             CHANGENR  ,
             TABNAME   ,
             FNAME     ,
             CHNGIND   ,
             VALUE_NEW ,
             VALUE_OLD  FROM CDPOS
             INTO TABLE @DATA(LT_CDPOS)
             FOR ALL ENTRIES IN @LT_CDHDR
             WHERE OBJECTCLAS = @LT_CDHDR-OBJECTCLAS
             AND OBJECTID = @LT_CDHDR-OBJECTID
             AND CHANGENR = @LT_CDHDR-CHANGENR .
      IF SY-SUBRC EQ 0.
        LOOP AT LT_CDPOS ASSIGNING FIELD-SYMBOL(<LFS_CDPOS>) WHERE TABNAME = 'ADRC' OR TABNAME = 'ADR6' OR TABNAME = 'ADR2'.
          <LFS_CDPOS>-OBJECTID = <LFS_CDPOS>-OBJECTID+4(10) .
        ENDLOOP.
        SORT LT_CDPOS BY OBJECTCLAS OBJECTID CHANGENR TABNAME FNAME .
        DELETE ADJACENT DUPLICATES FROM LT_CDPOS COMPARING OBJECTCLAS OBJECTID CHANGENR TABNAME FNAME .
        SORT LT_CDPOS BY OBJECTID TABNAME FNAME .
      ENDIF.
    ENDIF.

    LOOP AT LT_CDHDR INTO DATA(LW_CDHDR) .
      IF LW_CDHDR-OBJECTCLAS = 'ADRESSE'.
        LW_CDHDR-OBJECTID = LW_CDHDR-OBJECTID+4(10) .
        IF SY-SYSID = 'ATQ' .
          CONDENSE LW_CDHDR-OBJECTID .
          LW_ADRNR = LW_CDHDR-OBJECTID - 1 .
        ELSE.
          CONDENSE LW_CDHDR-OBJECTID .
          LW_ADRNR = LW_CDHDR-OBJECTID .
        ENDIF.

        CONDENSE : LW_ADRNR .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = LW_ADRNR
          IMPORTING
            OUTPUT = LW_ADRNR.

        APPEND LW_ADRNR TO LT_ADRNR .
        CLEAR : LW_ADRNR .
      ENDIF.
      LW_KUNNR = LW_CDHDR-OBJECTID .
      APPEND LW_KUNNR TO LT_KUNNR.
      CLEAR : LW_KUNNR , LW_CDHDR .
    ENDLOOP.

    IF LT_ADRNR IS NOT INITIAL.
      SELECT KUNNR,
             ADRNR
             FROM KNA1
             INTO TABLE @DATA(LT_KNA1_A)
             FOR ALL ENTRIES IN @LT_ADRNR
             WHERE ADRNR = @LT_ADRNR-TABLE_LINE .
      IF SY-SUBRC EQ 0.
        SORT LT_KNA1_A BY ADRNR .
      ENDIF.
    ENDIF.

    LOOP AT LT_KNA1_A INTO DATA(LW_KNA1_A).
      LW_KUNNR = LW_KNA1_A-KUNNR .
      APPEND LW_KUNNR TO LT_KUNNR.
      CLEAR : LW_KUNNR , LW_KNA1_A .
    ENDLOOP.

    IF LT_KUNNR IS NOT INITIAL.
      SORT LT_KUNNR .
      DELETE ADJACENT DUPLICATES FROM LT_KUNNR COMPARING ALL FIELDS .
    ENDIF.

    IF LT_KUNNR IS NOT INITIAL.
      SELECT KUNNR,
          BUKRS,
          ERDAT,
          AKONT FROM KNB1
          INTO TABLE @LT_KNB1
          FOR ALL ENTRIES IN @LT_KUNNR
          WHERE KUNNR = @LT_KUNNR-TABLE_LINE
          AND BUKRS IN @S_BUKRS .
      IF SY-SUBRC EQ 0.
        SORT LT_KNB1 BY KUNNR .
      ENDIF.
    ENDIF.
  ENDIF.

  IF LT_KNB1 IS NOT INITIAL.
    SELECT KUNNR ,
           NAME1 ,
           ORT01 ,
           PSTLZ ,
           SORTL ,
           ADRNR ,
           KTOKD ,
           STCD3 ,
           STCD5,
           TELF1,
           TELF2,
           REGIO FROM KNA1
           INTO TABLE @DATA(LT_KNA1)
           FOR ALL ENTRIES IN @LT_KNB1
           WHERE KUNNR = @LT_KNB1-KUNNR .
    IF SY-SUBRC EQ 0.
      SORT LT_KNA1 BY KUNNR .
    ENDIF.
  ENDIF.

  IF LT_KNA1 IS NOT INITIAL.
    SELECT SPRAS,
       LAND1,
       BLAND,
       BEZEI
       FROM T005U
       INTO TABLE @DATA(LT_T005U)
       FOR ALL ENTRIES IN @LT_KNA1
       WHERE SPRAS = 'E'
       AND LAND1 = 'IN'
       AND BLAND = @LT_KNA1-REGIO .
    IF SY-SUBRC EQ 0.
      SORT LT_T005U BY BLAND .
    ENDIF.

    SELECT ADDRNUMBER ,
           CITY1      ,
           CITY2      ,
           POST_CODE1 ,
           STR_SUPPL1 ,
           STR_SUPPL2 ,
           COUNTRY    ,
           REGION     ,
           SORT1      FROM ADRC
           INTO TABLE @DATA(LT_ADRC_1)
           FOR ALL ENTRIES IN @LT_KNA1
           WHERE ADDRNUMBER = @LT_KNA1-ADRNR .
    IF SY-SUBRC EQ 0.
      SORT LT_ADRC_1 BY ADDRNUMBER .
    ENDIF.

    SELECT ADDRNUMBER ,
          TELNR_LONG ,
          TEL_NUMBER FROM ADR2
          INTO TABLE @DATA(LT_ADR2_1)
          FOR ALL ENTRIES IN @LT_KNA1
          WHERE ADDRNUMBER = @LT_KNA1-ADRNR .
    IF SY-SUBRC EQ 0.
      SORT LT_ADR2_1 BY ADDRNUMBER .
    ENDIF.

    SELECT ADDRNUMBER ,
           SMTP_ADDR FROM ADR6
           INTO TABLE @DATA(LT_ADR6_1)
           FOR ALL ENTRIES IN @LT_KNA1
          WHERE ADDRNUMBER = @LT_KNA1-ADRNR .
    IF SY-SUBRC EQ 0.
      SORT LT_ADR6_1 BY ADDRNUMBER .
    ENDIF.

    SELECT KUNNR,
           J_1IPANNO FROM J_1IMOCUST
           INTO TABLE @DATA(LT_J_1IMOCUST)
           FOR ALL ENTRIES IN @LT_KNA1
           WHERE KUNNR = @LT_KNA1-KUNNR .
    IF SY-SUBRC EQ 0.
      SORT LT_J_1IMOCUST BY KUNNR .
    ENDIF.

    SELECT KUNNR ,
           BANKS ,
           BANKL ,
           BANKN ,
           KOINH FROM KNBK
           INTO TABLE @DATA(LT_KNBK)
           FOR ALL ENTRIES IN @LT_KNA1
           WHERE KUNNR = @LT_KNA1-KUNNR .
    IF SY-SUBRC EQ 0.
      SORT LT_KNBK BY KUNNR .
    ENDIF.

    IF LT_KNB1 IS NOT INITIAL.
      SELECT KUNNR,
         BUKRS,
         WITHT FROM KNBW
         INTO TABLE @DATA(LT_KNBW)
         FOR ALL ENTRIES IN @LT_KNB1
         WHERE KUNNR = @LT_KNB1-KUNNR
         AND BUKRS =  @LT_KNB1-BUKRS .
      IF SY-SUBRC EQ 0.
        SORT LT_KNBW BY KUNNR BUKRS .
      ENDIF.
    ENDIF.

    IF LT_KNBW IS NOT INITIAL.
      SELECT SPRAS ,
             WITHT ,
             TEXT40 FROM T059ZT
             INTO TABLE @DATA(LT_T059ZT)
             FOR ALL ENTRIES IN @LT_KNBW
             WHERE WITHT = @LT_KNBW-WITHT
             AND SPRAS = 'E' .
      IF SY-SUBRC EQ 0.
        SORT LT_T059ZT BY WITHT .
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR : GW_COL_CONT .

  LOOP AT LT_KNB1 INTO DATA(LW_KNB1).
    DATA(LW_KNA1) = VALUE #( LT_KNA1[ KUNNR = LW_KNB1-KUNNR ] OPTIONAL ) .
    DATA(LW_ADRC_1) = VALUE #( LT_ADRC_1[ ADDRNUMBER = LW_KNA1-ADRNR  ] OPTIONAL ) .
    DATA(LW_ADR2_1) = VALUE #( LT_ADR2_1[ ADDRNUMBER = LW_KNA1-ADRNR  ] OPTIONAL ) .
    DATA(LW_ADR6_1) = VALUE #( LT_ADR6_1[ ADDRNUMBER = LW_KNA1-ADRNR  ] OPTIONAL ) .
    DATA(LW_J_1IMOCUST) = VALUE #( LT_J_1IMOCUST[ KUNNR = LW_KNA1-KUNNR  ] OPTIONAL ) .
    DATA(LW_KNBK) = VALUE #( LT_KNBK[ KUNNR = LW_KNA1-KUNNR  ] OPTIONAL ) .
    DATA(LW_KNBW) = VALUE #( LT_KNBW[ KUNNR = LW_KNB1-KUNNR BUKRS = LW_KNB1-BUKRS ] OPTIONAL ) .
    DATA(LW_T059ZT) = VALUE #( LT_T059ZT[ WITHT = LW_KNBW-WITHT ] OPTIONAL ) .
    DATA(LW_S_ADR) = VALUE #( LT_T005U[ BLAND = LW_KNA1-REGIO ]-BEZEI  OPTIONAL ) .

    LW_TEXT-LINE = '<HTML>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<head>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    CONCATENATE '<P>' 'Please find the below customer details.' '</P>'  INTO LW_TEXT-LINE.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '</head>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<body>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<table border="1" >'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    CLEAR : LW_NX_ADRNR.
    LW_NX_ADRNR = LW_KNA1-ADRNR.

    IF SY-SYSID = 'ATQ'.
      LW_NX_ADRNR = LW_NX_ADRNR + 1 .
    ELSE.
      LW_NX_ADRNR = LW_NX_ADRNR .
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = LW_NX_ADRNR
      IMPORTING
        OUTPUT = LW_NX_ADRNR.

    CLEAR : LW_P3 .
    LW_P3 = LW_KNA1-KUNNR .
    PERFORM BUILD_ROW USING 'Customer Code' LW_P3 .

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNA1' FNAME = 'KTOKD' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_KNA1-KTOKD .
        PERFORM BUILD_ROW USING 'Account Group' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_KNA1-KTOKD .
      PERFORM BUILD_ROW USING 'Account Group' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR  TABNAME = 'ADRC' FNAME = 'NAME1' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_KNA1-NAME1 .
        PERFORM BUILD_ROW USING 'Name 1' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_KNA1-NAME1 .
      PERFORM BUILD_ROW USING 'Name 1' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'SORT1' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_KNA1-SORTL .
        PERFORM BUILD_ROW USING ' Search Item' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_KNA1-SORTL .
      PERFORM BUILD_ROW USING ' Search Item' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'STR_SUPPL1' ] ) .
        DATA(GW_ADRC_ACT) = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'STR_SUPPL2' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR  TABNAME = 'ADRC' FNAME = 'POST_CODE1' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'CITY1'] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'REGION' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADRC' FNAME = 'COUNTRY' ] ) .
        GW_ADRC_ACT = 'X' .
      ENDIF.

      IF GW_ADRC_ACT = 'X' .
        CONCATENATE LW_ADRC_1-STR_SUPPL1 LW_ADRC_1-STR_SUPPL2 LW_ADRC_1-POST_CODE1
                              LW_ADRC_1-CITY1 LW_S_ADR LW_ADRC_1-COUNTRY INTO DATA(LW_ADR) SEPARATED BY ',' .
        CLEAR : LW_P3 .
        LW_P3 = LW_ADR .
        CLEAR : LW_ADR .
        PERFORM BUILD_ROW USING 'Address' LW_P3 .
      ENDIF.

    ELSE.
      CONCATENATE LW_ADRC_1-STR_SUPPL1 LW_ADRC_1-STR_SUPPL2 LW_ADRC_1-POST_CODE1
                            LW_ADRC_1-CITY1 LW_S_ADR LW_ADRC_1-COUNTRY INTO LW_ADR SEPARATED BY ',' .
      CLEAR : LW_P3 .
      LW_P3 = LW_ADR .
      CLEAR : LW_ADR .
      PERFORM BUILD_ROW USING 'Address' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNA1' FNAME = 'STCD3' ]  ).
        CLEAR : LW_P3 .
        LW_P3 = LW_J_1IMOCUST-J_1IPANNO.
        PERFORM BUILD_ROW USING 'PAN Number' LW_P3.
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_J_1IMOCUST-J_1IPANNO.
      PERFORM BUILD_ROW USING 'PAN Number' LW_P3.
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNA1' FNAME = 'TELF2' ] ).
        CLEAR : LW_P3 .
*        LW_P3 = LW_ADR2_1-TEL_NUMBER.
        LW_P3 = LW_KNA1-TELF2.
        PERFORM BUILD_ROW USING 'Mobile no' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_KNA1-TELF2.
      PERFORM BUILD_ROW USING 'Mobile no' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNA1' FNAME = 'TELF1' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_KNA1-TELF1.
        PERFORM BUILD_ROW USING 'Telephone no' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_KNA1-TELF1.
      PERFORM BUILD_ROW USING 'Telephone no' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_NX_ADRNR TABNAME = 'ADR6' FNAME = 'SMTP_ADDR' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_ADR6_1-SMTP_ADDR .
        PERFORM BUILD_ROW USING 'Email Id' LW_P3 .
      ENDIF.

    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_ADR6_1-SMTP_ADDR .
      PERFORM BUILD_ROW USING 'Email Id' LW_P3 .
    ENDIF.


    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNBK' FNAME = 'BANKN' ] ).
        DATA(GW_B_DET) = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNBK' FNAME = 'KOINH' ] ).
        CLEAR : GW_B_DET .
        GW_B_DET = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNBK' FNAME = 'BANKL' ] ).
        CLEAR : GW_B_DET .
        GW_B_DET = 'X' .
      ENDIF.

      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNBK' FNAME = 'BANKS' ] ).
        CLEAR : GW_B_DET .
        GW_B_DET = 'X' .
      ENDIF.


      IF GW_B_DET = 'X'.
        PERFORM BUILD_ROW USING 'Bank Account Details' ' ' .

        CLEAR : LW_P3 .
        LW_P3 = LW_KNBK-BANKN  .
        PERFORM BUILD_ROW USING 'Bank Account' LW_P3  .

        CLEAR : LW_P3 .
        LW_P3 = LW_KNBK-KOINH .
        PERFORM BUILD_ROW USING  'Bank Account Name' LW_P3 .

        CLEAR : LW_P3 .
        LW_P3 = LW_KNBK-BANKL  .
        PERFORM BUILD_ROW USING  'Bank Key' LW_P3 .

        CLEAR : LW_P3 .
        LW_P3 = LW_KNBK-BANKS  .
        PERFORM BUILD_ROW USING  'Bank Country' LW_P3 .
      ENDIF.
    ELSE.
      PERFORM BUILD_ROW USING 'Bank Account Details' ' ' .
      CLEAR : LW_P3 .
      LW_P3 = LW_KNBK-BANKN  .
      PERFORM BUILD_ROW USING 'Bank Account' LW_P3  .
      CLEAR : LW_P3 .
      LW_P3 = LW_KNBK-KOINH .
      PERFORM BUILD_ROW USING  'Bank Account Name' LW_P3 .
      CLEAR : LW_P3 .
      LW_P3 = LW_KNBK-BANKL  .
      PERFORM BUILD_ROW USING  'Bank Key' LW_P3 .
      CLEAR : LW_P3 .
      LW_P3 = LW_KNBK-BANKS  .
      PERFORM BUILD_ROW USING  'Bank Country' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNB1' FNAME = 'BUKRS' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_KNB1-BUKRS .
        PERFORM BUILD_ROW USING 'Company Code' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_KNB1-BUKRS .
      PERFORM BUILD_ROW USING 'Company Code' LW_P3 .
    ENDIF.

    IF P_VCHG_S = 'X'.
      IF LINE_EXISTS( LT_CDPOS[ OBJECTID = LW_KNB1-KUNNR TABNAME = 'KNB1' FNAME = 'AKONT' ] ).
        CLEAR : LW_P3 .
        LW_P3 = LW_KNB1-AKONT .
        PERFORM BUILD_ROW USING 'Reconciliation Account' LW_P3 .
      ENDIF.
    ELSE.
      CLEAR : LW_P3 .
      LW_P3 = LW_KNB1-AKONT .
      PERFORM BUILD_ROW USING 'Reconciliation Account' LW_P3 .
    ENDIF.

    LW_TEXT-LINE = '</table>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT .

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT .

    LW_TEXT-LINE = 'Thanks'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '<BR>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT .

    LW_TEXT-LINE = 'This is system generated mail so kindly do not reply.'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '</body>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    LW_TEXT-LINE = '</HTML>'.
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.

    IF LT_TEXT IS NOT INITIAL.
      TRY .
          GO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

          LW_KNB1-KUNNR = |{ LW_KNB1-KUNNR ALPHA = OUT }|.

          CLEAR : LV_SUB , LV_SUBJJ .
          IF P_VCRE_S = 'X'.
            CONCATENATE 'Customer code-' LW_KNB1-KUNNR 'created In SAP System' INTO LV_SUB SEPARATED BY SPACE .
            LV_SUBJJ = LV_SUB .
          ELSEIF P_VCHG_S = 'X' .
            CONCATENATE 'Customer code-' LW_KNB1-KUNNR 'changed In SAP System' INTO LV_SUB SEPARATED BY SPACE .
            LV_SUBJJ = LV_SUB .
          ENDIF.

          GO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                I_TYPE    = 'HTM'
                I_TEXT    = LT_TEXT
                I_SUBJECT  = LV_SUBJJ ).

          CALL METHOD GO_SEND_REQUEST->SET_MESSAGE_SUBJECT
            EXPORTING
              IP_SUBJECT = LV_STRING.

          IF GO_DOCUMENT IS NOT INITIAL.

            CALL METHOD GO_SEND_REQUEST->SET_DOCUMENT( GO_DOCUMENT ).

            LO_REF_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( I_ADDRESS_STRING = 'donotreply@arvind.in'
                                                                           I_ADDRESS_NAME   = 'Do not reply' ).
            CALL METHOD GO_SEND_REQUEST->SET_SENDER
              EXPORTING
                I_SENDER = LO_REF_SENDER.

            LV_MAIL1 = LW_ADR6_1-SMTP_ADDR .
            IF LV_MAIL1 IS NOT INITIAL.
              GI_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( LV_MAIL1 ).
              CALL METHOD GO_SEND_REQUEST->ADD_RECIPIENT(
                EXPORTING
                  I_RECIPIENT = GI_RECIPIENT
                  I_EXPRESS   = 'X' ).
              CLEAR : LV_MAIL1 .
            ENDIF.
          ENDIF.

          CALL METHOD GO_SEND_REQUEST->SET_SEND_IMMEDIATELY
            EXPORTING
              I_SEND_IMMEDIATELY = 'X'.

          CALL METHOD GO_SEND_REQUEST->SEND( ).
          IF SY-SUBRC EQ 0.
            DATA(LW_S_MSG) = 'X' .
            COMMIT WORK AND WAIT.
          ENDIF.

          FREE:  GI_RECIPIENT ,GO_DOCUMENT.
        CATCH CX_SY_REF_IS_INITIAL INTO GX_SY_REF_IS_INITIAL.
        CATCH CX_SEND_REQ_BCS INTO GX_SEND_REQ_BCS.
        CATCH CX_ADDRESS_BCS  INTO GX_ADDR_EXCEPTION.
        CATCH CX_DOCUMENT_BCS INTO GX_BCS_EXCEPTION.
      ENDTRY.
      REFRESH LT_TEXT .
    ENDIF.
    CLEAR : LW_KNB1 , LW_KNA1 , LW_ADRC_1 , LW_ADR2_1 , LW_ADR6_1 , LW_J_1IMOCUST , LW_KNBK , LW_KNBW , LW_T059ZT , GW_B_DET  , GW_COL_CONT , LW_NX_ADRNR, LW_S_ADR.
  ENDLOOP.

  IF LW_S_MSG = 'X'.
    CLEAR : LW_S_MSG .
    MESSAGE 'Mail Sent Successfully' TYPE 'I' .
  ENDIF.

ENDFORM .

*&---------------------------------------------------------------------*
*&      Form  BUILD_ROW
*&---------------------------------------------------------------------*

FORM BUILD_ROW  USING P_CO1_2 TYPE SO_TEXT255
                      P_CO1_3 TYPE SO_TEXT255.

  LW_TEXT-LINE = '<tr>'.
  APPEND LW_TEXT TO LT_TEXT.
  CLEAR LW_TEXT .

  IF P_CO1_2 = 'Bank Account' OR P_CO1_2 = 'Bank Account Name' OR P_CO1_2 = 'Bank Key' OR P_CO1_2 = 'Bank Country' .
    CONCATENATE '<td>' ' ' '</td>' INTO LW_TEXT-LINE .
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.
  ELSE.
    GW_COL_CONT = GW_COL_CONT + 1 .

    CONCATENATE '<td>' GW_COL_CONT '.' '</td>' INTO LW_TEXT-LINE .
    APPEND LW_TEXT TO LT_TEXT.
    CLEAR LW_TEXT.
  ENDIF.

  CONCATENATE '<td>' P_CO1_2 '</td>' INTO LW_TEXT-LINE .
  APPEND LW_TEXT TO LT_TEXT.
  CLEAR LW_TEXT.

  CONCATENATE '<td>' P_CO1_3 '</td>' INTO LW_TEXT-LINE .
  APPEND LW_TEXT TO LT_TEXT.
  CLEAR LW_TEXT.

  LW_TEXT-LINE = '</tr>'.
  APPEND LW_TEXT TO LT_TEXT.
  CLEAR LW_TEXT .

ENDFORM.
