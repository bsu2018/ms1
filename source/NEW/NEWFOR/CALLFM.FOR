       SUBROUTINE CALLFM( NFM, BA0T, KNOTT, PAR0T, MSWT, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: CALLFM( NFM, BA0T, KNOTT, PAR0T, MSWT, LM )
C   ------------
C   HA�HA�EH�E:   �O HOMEP� NFM B���BAET COOTBETCTB����� MOH�TOP
C   ----------    HACTPO�K� MO�E�� FM...<NFM>....;
C   METO�:        O�EB��EH;
C   -----
C   �APAMETP�:    NFM - HOMEP B���BAEMO�O MOH�TOPA;
C   ---------     BA0T, KNOTT, PAR0T, MSWT -"OTPE�K�" MACC�BOB
C                 BA0, KNOT, PAR0, MSWALL -> �O�PO�HEE CM. GETNEW
C                                            INPUT0;
C                 LM - ���HA �EPEMEHHO� �ACT� MSW ��EMEHTA,
C                    *** �P� BO�����EH�� ��A�A FGMSW [ MASPRT(87) ]:
C                        LM - �TO ���HA BCE�O  MSW ��� ��EMEHTA C
C                                       *****
C                        �P��HAKOM ���H� B MACTAB -->  +1(VAR)
C
C
C   �O��P�PAMM�:   1. FM... MOH�TOP�;
C   ------------    *****   PEA���OABH B��OB OT FM0001 �O FM0035 ******
C
C                  2.  PROF1
C   �P�ME�AH�E:    1.  KOHTPO�� COOTBETCTB�� FM..@... � FGMSW ��A�A
C   ----------         HE �POBO��TC�;
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(1), KNOTT(1), PAR0T(1), MSWT(1)
       INTEGER BA0T
       NWTRE = MASPRT(14)
C----------------------------------------------------------------------
         GO TO ( 1001, 1002, 1003, 1004, 1005, 1006,
     =           1007, 1008, 1009, 1010, 1011, 1012,
     =           1013, 1014, 1015, 1016, 1017, 1018,
     =           1019, 1020, 1021, 1022, 1023, 1024,
     =           1025, 1026, 1027, 1028, 1029, 1030,
     =           1031, 1032, 1033, 1034, 1035, 1036,
     =           1037, 1038, 1039                       ), NFM
        NFMM = 39
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 503)     NFM, NFMM
503      FORMAT(' *** �O�. C�O� B CALLFM - B��OB MOH�TOPA # ',I4, '***'/
     =       ' *** PEA���OBAHO BCE�O ', I4, ' B��OBOB MOH�TOPOB ***'//)
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C######################################################################
C1************* B��OB O�PA�OT��KA  "FM 0 0 0 1" ***********************
1001   CALL FM0001(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C2************* B��OB O�PA�OT��KA  "FM 0 0 0 2" ***********************
1002   CALL FM0002(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C3************* B��OB O�PA�OT��KA  "FM 0 0 0 3" ***********************
1003   CALL FM0003(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C4************* B��OB O�PA�OT��KA  "FM 0 0 0 4" ***********************
1004   CALL FM0004(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C5************* B��OB O�PA�OT��KA  "FM 0 0 0 5" ***********************
1005   CALL FM0005(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C6************* B��OB O�PA�OT��KA  "FM 0 0 0 6" ***********************
1006   CALL FM0006(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C7************* B��OB O�PA�OT��KA  "FM 0 0 0 7" ***********************
1007   CALL FM0007(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C8************* B��OB O�PA�OT��KA  "FM 0 0 0 8" ***********************
1008   CALL FM0008(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C9************* B��OB O�PA�OT��KA  "FM 0 0 0 9" ***********************
1009   CALL FM0009(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C10************ B��OB O�PA�OT��KA  "FM 0 0 1 0" ***********************
1010   CALL FM0010(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C11************ B��OB O�PA�OT��KA  "FM 0 0 1 1" ***********************
1011   CALL FM0011(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C12************ B��OB O�PA�OT��KA  "FM 0 0 1 2" ***********************
1012   CALL FM0012(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C13************ B��OB O�PA�OT��KA  "FM 0 0 1 3" ***********************
1013   CALL FM0013(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C14************ B��OB O�PA�OT��KA  "FM 0 0 1 4" ***********************
1014   CALL FM0014(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C15************ B��OB O�PA�OT��KA  "FM 0 0 1 5" ***********************
1015   CALL FM0015(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C16************ B��OB O�PA�OT��KA  "FM 0 0 1 6" ***********************
1016   CALL FM0016(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C17************ B��OB O�PA�OT��KA  "FM 0 0 1 7" ***********************
1017   CALL FM0017(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C18************ B��OB O�PA�OT��KA  "FM 0 0 1 8" ***********************
1018   CALL FM0018(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C19************ B��OB O�PA�OT��KA  "FM 0 0 1 9" ***********************
1019   CALL FM0019(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C20************ B��OB O�PA�OT��KA  "FM 0 0 2 0" ***********************
1020   CALL FM0020(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C21************ B��OB O�PA�OT��KA  "FM 0 0 2 1" ***********************
1021   CALL FM0021(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C22************ B��OB O�PA�OT��KA  "FM 0 0 2 2" ***********************
1022   CALL FM0022(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C23************ B��OB O�PA�OT��KA  "FM 0 0 2 3" ***********************
1023   CALL FM0023(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C24************ B��OB O�PA�OT��KA  "FM 0 0 2 4" ***********************
1024   CALL FM0024(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C25************ B��OB O�PA�OT��KA  "FM 0 0 2 5" ***********************
1025   CALL FM0025(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C26************ B��OB O�PA�OT��KA  "FM 0 0 2 6" ***********************
1026   CALL FM0026(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C27************ B��OB O�PA�OT��KA  "FM 0 0 2 7" ***********************
1027   CALL FM0027(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C28************ B��OB O�PA�OT��KA  "FM 0 0 2 8" ***********************
1028   CALL FM0028(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C29************ B��OB O�PA�OT��KA  "FM 0 0 2 9" ***********************
1029   CALL FM0029(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C30************ B��OB O�PA�OT��KA  "FM 0 0 3 0" ***********************
1030   CALL FM0030(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C31************ B��OB O�PA�OT��KA  "FM 0 0 3 1" ***********************
1031   CALL FM0031(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C32************ B��OB O�PA�OT��KA  "FM 0 0 3 2" ***********************
1032   CALL FM0032(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C33************ B��OB O�PA�OT��KA  "FM 0 0 3 3" ***********************
1033   CALL FM0033(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C34************ B��OB O�PA�OT��KA  "FM 0 0 3 4" ***********************
1034   CALL FM0034(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C35************ B��OB O�PA�OT��KA  "FM 0 0 3 5" ***********************
1035   CALL FM0035(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C36************ B��OB O�PA�OT��KA  "FM 0 0 3 6" ***********************
1036   CALL FM0036(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C37************ B��OB O�PA�OT��KA  "FM 0 0 3 7" ***********************
1037   CALL FM0037(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C38************ B��OB O�PA�OT��KA  "FM 0 0 3 8" ***********************
1038   CALL FM0038(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
C38************ B��OB O�PA�OT��KA  "FM 0 0 3 8" ***********************
1039   CALL FM0039(BA0T,KNOTT,PAR0T,MSWT,LM)
       RETURN
C######################################################################
         END
