       SUBROUTINE SEQMAX( EYY, IK, IA, EYYV, EYYI )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: SEQMAX( EYY, IK, IA, EYYV, EYYI )
C   ------------
C   HA�HA�EH�E:        O�PAH���BAET �HA�EH�� EYY - AM���T���
C   ----------         ��PAB����EE�O C��HA�A:
C
C                      "A" �P���A B��P�M��ET:
C                       =====================
C                        �P� IA > 0  � EYY > 0 --> O�PAH��EH�E "CBEPX�"
C                              EC�� EYY > EYYV, TO EYY=EYYV;
C
C
C                      "K" �P���A B��P�M��ET:
C                       =====================
C                        �P� IK > 0  � EYY < 0 --> O�PAH��EH�E "CBEPX�"
C                            EC��  -EYY > EYYV, TO EYY=-EYYV;
C
C
C                      "A" �P���A �HBEPT�P�ET:
C                       =====================
C                        �P� IA > 0  � EYY < 0 --> O�PAH��EH�E "CH���"
C                              EC�� -EYY > EYYI, TO EYY=-EYYI;
C
C
C                      "K" �P���A �HBEPT�P�ET:
C                       =====================
C                        �P� IK > 0  � EYY > 0 --> O�PAH��EH�E "CH���"
C                              EC�� EYY > EYYI, TO EYY=EYYI;
C
C   METO�:             �CTAHOB�T� PE��M � ��MEH�T� EYY
C   ------             CO��ACHO EYYV ��� EYYI
C
C   �APAMETP�:         EYY - ��MEH�EMOE HA�P��EH�E ��PAB�EH��,
C   ---------          IK - TOK "K" �P����,
C                      IA - TOK "A" �P����,
C                      EYYV - O�PAH��EH�E "CBEPX�" (B��P�M�EH�E),
C                      EYYI - O�PAH��EH�E "CH���"  (�HBEPT�POBAH�E).
C
C   �O��PO�PAMM�:   HET
C   -------------
C   �P�ME�AH�E:
C   -----------
C
CZZZ-------------------------------------------------------------------
       REAL IA, IK
C
       IF ( IA .GT. 0.0 .AND. EYY .GT. 0.0 .AND. EYY .GT. EYYV )
     *            EYY = EYYV
       IF ( IA .GT. 0.0 .AND. EYY .LT. 0.0 .AND. -EYY .GT. EYYI )
     *            EYY = -EYYI
       IF ( IK .GT. 0.0 .AND. EYY .LT. 0.0 .AND. -EYY .GT. EYYV )
     *            EYY = -EYYV
       IF ( IK .GT. 0.0 .AND. EYY .GT. 0.0 .AND. EYY .GT. EYYI )
     *            EYY = EYYI
       RETURN
       END
