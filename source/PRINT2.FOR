         SUBROUTINE PRINT2( KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C    �O��PO�PAMMA:  PRINT2( KOD )
C   -------------
C    HA�HA�EH�E: KOHTPO��HA� �E�AT� MACC�BOB CO��ACHO �A�POCA KOD;
C    ----------
C   �APAMETP�:     KOD = 1 �E�AT� MASPRT B B��E 16-�O �AM�A �
C   ----------             B B��E "BE�ECTBEHH�X" C�OB REAL*4;
C                  KOD = 2 �E�AT� MASTAB B B��E INTEGER*4;
C   �P�ME�AH�E: 1. PA�O��� BAP�AHT--> OT�A�O�HA� �E�AT�;
C   ----------  2. �E�AT� C���E�H�M� �/� FORTRAN C�CTEM�;
C               3. ��� MASPRT --> MAX_M1 = 80;
C                  ��� MACTAB --> MAX_M2 = 128;
C
CZZZ-------------------------------------------------------------------
        DIMENSION BA0(1), KNOT(1), PAR0(1)
         COMMON/PROF1/ MASPRT(1)
         COMMON/PROF2/ MACTAB(1)
         COMMON/PROF3/ FUNTAB(1)
         DATA M1/80/, M2/128/
         GO TO (1,2), KOD
C======================================================================
         WRITE(6,10) KOD
10       FORMAT(' ***  PRINT2:  KOD = ', I10, ' ***' )
C######################################################################
1        CONTINUE
         WRITE(6,11)
11       FORMAT(1X, 128('*')/'     MACC�B MASPRT  [ 16-� B�� ]' )
         CALL PDUMP(MASPRT(1), MASPRT(M1), 0 )
         WRITE(6,12)
12       FORMAT(1X, 128('*') /'    MACC�B MASPRT [ REAL*4 ]')
         CALL PDUMP(MASPRT(1), MASPRT(M1), 5 )
         WRITE(6,13)
13       FORMAT(1X, 128('*') )
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
2        CONTINUE
         WRITE(6,14)
14       FORMAT(1X, 128('*')/'    MACC�B MACTAB  [ INTEGER*4 ]')
         CALL PDUMP(MACTAB(1), MACTAB(M2), 4 )
         WRITE(6,13)
         RETURN
         END
