         SUBROUTINE DRCIR2( BA0, KNOT, PAR0, MSWALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRCIR2( BA0, KNOT, PAR0, MSWALL )
C   ------------
C   HA�HA�EH�E:    �OPM�POBAH�E C��CKA MSWALL
C  ------------
C                  ( 3-� �ACT� ��PEKT�B� #�E�� )
C   METO�:         CM. �/� GETNEW;
C   -----
C   �APAMETP�:     CM. GETNEW;
C   ---------
C
C   �O��PO�PAMM�:  1. COMMON PROF1
C   -------------  2. GETMSW
C   �P�ME�AH�E:    1. B C���AE �C�E�HO�O �OPM�POBAH�� MSWALL
C   ----------        BO�����AETC� ��A� FMSWAL
C                  2. �O�PO�HEE CM. INPUT0
C                  3. MACC�B MSWALL �O��EH ��T�  �H����POBAHH�;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         DIMENSION BA0(1), PAR0(1), KNOT(1), MSWALL(1)
         INTEGER BA0, FMSWAL
         EQUIVALENCE ( MASPRT(13),  NWTR )
         EQUIVALENCE ( MASPRT(14),  NWTRE )
         EQUIVALENCE ( MASPRT(74),  FMSWAL )
         FMSWAL = 0
         CALL GETMSW( BA0, KNOT, PAR0, MSWALL, IER )
         FMSWAL = 1
         IF ( IER    .NE. 0   ) WRITE( NWTR, 100 )
         IF ( IER    .NE. 0   ) FMSWAL = 0
100     FORMAT(//' *** �O���ECK�E O���K� O��CAH�� �E�� - �E�� C�PACABAET
     =C� ***'//)
C23456
         RETURN
         END
