         SUBROUTINE DRCIRC( KAR, DAT, MSWALL, IRCC)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)---IRCC !!!
C   �O�PO�PAMMA:    DRCIRC( KAR, DAT, MSWALL)
C   ------------
C   HA�HA�EH�E:    ##O���� MOH�TOP O�PA�OTK� #�E�� ***
C   ----------
C   METO�:         CM. �/� DRCIR0, DRCIR1, DRCIR2;
C   -----           B V.M = 2.0  -  PEA���OBAHO CTAT��ECKOE B��E�EH�E
C                   O.�. �O� �H�OPMA��OHH�E MACC�B�:
C                  MACC�B� B��E���T�, �X ��CKP��TOP� �H���A����P��TC�;
C                  B COOTBETCTB�� C B��E�EHHO� O.�. �E�AETC� �A��C� B
C                  MASPRT C��COK �KA�ATE�E�;
C                  (HEO�XO��MO �EPE��CAT�)
C
C   �APAMETP�:     CM. DRCIR0, DRCIR1, DRCIR2;
C   ---------
C   �O��PO�PAMM�:  1. COMMON PROF1, [ MEMORY, PONTER, PRINTN ]
C   ------------   2. DRCIR0, DRCIR1, DRCIR2  [REGCPU];
C   �P�ME�AH�E:    1. REGION "A��APAT" �O��EH ��T� �H����POBA;
C   ----------     2. B KOH�E �/� CO��PAETC� "M�COP";
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/MEMORY/ NREG, REGION(1)
         COMMON/PONTER/ NPRT, PRT(1)
         COMMON/PRINTN/ NW6
         INTEGER PRT, REGION, FRCPU, BA0, DMSWAL, FMSWAL
         DIMENSION KAR(1), DAT(1)
C--->
         EQUIVALENCE ( MASPRT(73), FRCPU )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(13), NWTR  )
         EQUIVALENCE ( MASPRT(1), KBA0  )
         EQUIVALENCE ( MASPRT(2), NMBA0 )
         EQUIVALENCE ( MASPRT(4), N1BA0 )
         EQUIVALENCE ( MASPRT(5), KKNOT )
         EQUIVALENCE ( MASPRT(6), NDKNOT )
         EQUIVALENCE ( MASPRT(8), KPAR0  )
         EQUIVALENCE ( MASPRT(9), NDPAR0 )
         EQUIVALENCE ( MASPRT(19), KMSWAL )
         EQUIVALENCE ( MASPRT(20), DMSWAL )
         EQUIVALENCE ( MASPRT(74), FMSWAL )
C######################################################################
         DIMENSION BA0(800)
         DIMENSION KNOT(650)
         DIMENSION PAR0(1500)
         DIMENSION MSWALL(2000)
         FMSWAL = 0
         NW6 = NWTRE
         IRCC = 0
C----------------------------------------------------------------------
C  C T A T � � E C K O E    B � � E � E H � E   � A M � T �.
C----------------------------------------------------------------------
         NDKNOT = 650
         NDPAR0 = 1500
         NDBA0 = 800
         DMSWAL = 2000
C--->
         NMBA0 = NDBA0 / N1BA0
C--->
C--> ��CK� � �H����POBAH�E MACC�BOB:
C
         DO 1 I = 1, NDKNOT
1        KNOT(I) = -1
         DO 2 I = 1, NDPAR0
2        PAR0(I) = -1.0
         DO 3 I = 1, NDBA0
3        BA0(I) = -1
         DO 4 I = 1, DMSWAL
4        MSWALL(I) = -1
C-->  MAX PA�MEPHOCT�:
         BA0(2) = NDBA0
         KNOT(2) = NDKNOT
         PAR0(2) = NDPAR0
         MSWALL(2) = DMSWAL
C--->  �KA�ATE�� HA CBO�O�H�E
         BA0(3) = 6
         KNOT(3) = 6
         PAR0(3) = 6.0
         MSWALL(3) = 6
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  O��CTKA NXBA0[ MASPRT(3) ]
C  T.K INPUT0 MO�ET �OBBO��T�
       MASPRT(3) = 0
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C######################################################################
C -->  B��O�HEH�E ��PEKT�B�  #�E��
C######################################################################
         CALL DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRC )
         IF ( IRC .EQ. 2 ) RETURN
C######################################################################
C        CALL PRINT0( BA0, KNOT, PAR0)
C######################################################################
         CALL DRCIR1( BA0, KNOT, PAR0,      IRC )
         IF ( IRC .EQ. 2 ) RETURN
C######################################################################
C        CALL PRINT0( BA0, KNOT, PAR0)
         CALL DRCIR2(BA0, KNOT, PAR0, MSWALL )
C        CALL PRINT0( BA0, KNOT, PAR0)
         RETURN
         END
