         SUBROUTINE SEQMS0( MSWALL, NEL, IMSW, IRC, NSM, ITOK )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   SEQMS0( MSWALL, NEL, IMSW, IRC, NSM, ITOK )
C   ------------
C   HA�HA�EH�E:    �POCMOTP MSWALL C��CKA, �O�CK ��EMEHTA C HOMEPOM NEL;
C   -----------    �P� ��A�E: O�PE�E�EH�E NSM - MAX BO�MO�HO�O
C                  "CME�EH��" (KO�-BO B�B. �HA�EH��), ITOK - A�PECA
C                  B MACC�BAX TOKN � TOKN1 XPAHEH�� �T�X �EPEMEHH�X,
C                  A TAK �E A�PECA IMSW �A��C� ��EMEHTA B MSWALL;
C   METO�:         �O�CK.
C   -----
C   �APAMETP�:     MSWALL - C�OBA COCTO�H�� ��EMEHTOB;
C   ---------      NEL - HOMEP ��EMEHTA;
C                  IMSW - A�PEC �A��C� O� ��EMEHTE B MSWALL;
C                  NSM - "CME�EH�E" MAX;
C                  ITOK - A�PEC "TOK" B TOKN1 � TOKN;
C                  IRC = 1 - HA��� ��EMEHT;
C                      = 2 - ��EMEHTA HET B MSWALL;
C   �O��PO�PAMM�:  HET
C   ------------
C   �P�ME�AH�E:
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION MSWALL(1)
         INTEGER ADR
         IRC = 2
         ADR = 6
900      I1 = MSWALL(ADR)
         IF ( I1 .EQ. 0 ) RETURN
         IMSW = ADR + 1
         ADR = I1
C---> KTO TAM?
         IF ( MSWALL(IMSW) .NE. NEL ) GO TO 900
C--->   HA���...
         IRC = 1
         NSM = MSWALL(IMSW+5)
         ITOK = MSWALL(IMSW+6)
         RETURN
         END
