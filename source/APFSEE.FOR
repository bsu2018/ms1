       SUBROUTINE APFSEE ( II , R , INDEX ,DATAR , KINDEX )
CXXX****KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*  �O��PO�PAMMA :  APFSEE ( II , R , INDEX ,DATAR , KINDEX )          *
C*  ------------                                                       *
C*                                                                     *
C*  HA�HA�EH�E :    O�PE�E�EH�E ��C�EHHO�O �HA�EH�� ��EHT���KATOPA.    *
C*  ----------                                                         *
C*                                                                     *
C*  METO� :         �O�CK COOTBETCTB�� B TA����E �MEH ��EHT���KATOPOB  *
C*  -----           C �OMO��� �OC�E�OBATE��HO�O �EPE�OPA.              *
C*                                                                     *
C*  �APAMETP� :     II - �M� ��EHT���KATOPA .                          *
C*  ---------       R - �HA�EH�E ��EHT���KATOPA .                      *
C*                  INDEX - MACC�B �MEH ��EHT���KATOPOB .              *
C*                  DATAR - MACC�B ��C�EHH�X �HA�EH�� ��EHT���KATOPOB .*
C*                  KINDEX - PA�MEPHOCT�  INDEX � DATAR .              *
C*                                                                     *
C*  �O��PO�PAMMA :  HET , ( SUBERR(1) , COMMON : PRINTN )              *
C*  ------------                                                       *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION INDEX(1) , DATAR(1)
C!!!-------------------------------------------------------------------
       COMMON / PRINTN / NW6
C!!!-------------------------------------------------------------------
       DO 1 J=1,KINDEX
       IF ( II .NE. INDEX(J) ) GO TO 1
       R = DATAR(J)
       RETURN
 1     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NW6,10)
 10    FORMAT(1X,'***** C�O� B APFSEE.�HA�EH�E ��EHT. HE HA��EHO.')
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
       RETURN
       END
