      SUBROUTINE APFNML(NRDR,NWTR,TAB,RAB,KTAB,KST,KODEOJ)
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0                                   
C    �O��PO�PAMMA:  APFNML(NRDR,NWTR,TAB,KTAB,KST,KODEOJ)
C    ------------
C       HA�HA�EH�E : �EC�OPMATH�� BBO� �H�OPMA��� B��A
C       ----------   <��EHT��.>=<�HA�EH�E> [, <��EHT��.>=<�HA�EH�E> ]
C
C       �APAMETP�  : NRDR - HOMEP KAHA�A C�CTEMHO�O BBO�A
C       ---------    NWTR - HOMEP KAHA�A C�CTEMHO�O B�BO�A
C                    TAB  - TA����A �MEH � �HA�EH��
C
C   CTPOKA TA����� CO�EP��T C�E�. �H�OPMA���:
C
C C�OBO:  1    2    3  <-->  10   11   12   13
C -----   A1   A2   I<--A3-->I    A4   A5   A6
C
C  ��E:  A1 - T�� �EPEMEHHO� 0 -> INTEGER, 1 -> REAL;
C  ---
C        A2 - ���HA ��EHT���KATOPA �MEH� �EPEMEHHO� (HE �O�EE 8 );
C
C        A3 - �M� �EPEMEHHO� ( '*','*',....,),
C             ( �M� KOPO�E 8-M� C�MBO�OB �O�O�H�ETC� �PO�E�AM� );
C        A4 -  �HA�EH�E "�O �MO��AH��";
C
C        A5 -  �P��HAK :=  0 - HE ��MEH��� �P� BBO�E,
C                          1 - BBE�EHO HOBOE �HA�EH�E;
C
C        A6 -  HOMEP ��EMEHTA B MASPRT
C              KST - KO���ECTBO �A�T B O�HO� CTPOKE TAB (���HA C�OBA)
C              KTAB - KO���ECTBO CTPOK TA����� TAB
C                    KODEOJ - KO� BBO�A
C                        KODEOJ=1   �C�E�H�� BBO� �H�OPMA���
C                              =2   �O���EH� O���K� �P� BBO�E
C                              =3   OTC�TCTB�E BBO�A �H�OPMA���
C                              =4   KOHE� NRDR
C
C       �O��PO�PAMM�  :  APFTRT,APFSQE,APFSQI
C       ------------     SUBERR;
C                                  STOPP(16)
C       �P�ME�AH�E:
C       -----------
C                     
C
C
CZZZ--------------------------------------------------------------------
      COMMON/PROF1/MASPRT(200)
       EQUIVALENCE (MASPRT(127), NZRDR)
       EQUIVALENCE (MASPRT(12), NRDROO)
      INTEGER SIM,RAV,ZAP,PROB,TAB,EOF,ER,ERR,KAR
      DIMENSION  TAB(1),KAR(80),SIM(8),ERR(80),EOF(4),FKAR(80),RAB(1)
      DIMENSION H0DATA(6),HOUTT(5),DTMCIU(6),SETKA(5)
      INTEGER H0DATA,HOUTT,DTMCIU,SETKA
      COMMON /PRINTN/NW6
      DATA RAV/'='/,ZAP/','/,PROB/' '/,ER/'$'/,EOF/'#','B','C','E'/
      DATA K72/72/
C----------------------------------------------------------------------
      NW6=6
C>>>>>>>
20    DO 7777 I=1,K72
7777  KAR(I)=PROB
C>>20 READ(NRDR,2,END=666  ) KAR
      READ(NRDR,2,END=666  ) KAR
      IF ( NRDR .EQ. NRDROO ) NZRDR = NZRDR + 1
    2 FORMAT(80A1)
C     WRITE(NWTR,22) KAR
   22 FORMAT(2X,72A1      )
      IF (KAR(1).EQ.EOF(1)) GO TO 100
      CALL EQMAS(KAR,1,EOF,1,4,KOD)
      IF (KOD.EQ.1) GO TO 100
C++++++++++++++++++++++
       CALL APF CAS(1,K72,KAR,FKAR)
C++++++++++++++++++++++
      CALL APFTRT(1,K72,FKAR,FKAR)
      IP=1
    4 IS=0
      DO 1 IE=1,80
    1 ERR(IE)=PROB
    5 IF (KAR(IP).NE.PROB) GO TO 3
      IP=IP+1
      IF (IP.LT.K72) GO TO 5
      KODEOJ=3
C*****WRITE(NWTR,55) KODEOJ
C**55 FORMAT(5X,'--- KODEOJ=',I2,' ---')
      GO TO 20
    3 IF (KAR(IP).EQ.RAV) GO TO 6
C  �TEH�E ��EHT���KATOPA
      IS=IS+1
      IF (IS.GT.8) GO TO 7
      SIM(IS)=KAR(IP)
      IP=IP+1
      IF(IP.LT.K72) GO TO 3
    7 IF (KAR(IP).NE.PROB) GO TO 66
      IP=IP+1
      IF (IP.LT.K72) GO TO 7
C O���KA : ��C�O C�MBO�OB B ��EHT���KATOPE �O���E 8
   66 IF (KAR(IP).NE.RAV) GO TO 77
      IS=IS-1
      GO TO 6
   77 ERR(IP)=ER
      WRITE(NWTR,22) KAR
      WRITE(NWTR,22) ERR
      IF (IP.LT.K72) GO TO 88
      WRITE(NWTR,67)
   67 FORMAT(5X,'*** O訡��: ��᫥ ����� ��� ���祭�� ***')
      GO TO 99
   88 WRITE(NWTR,31)
   31 FORMAT(5X,'*** O訡��: �᫮ ᨬ����� � ����� ��६����� > 8 ***')
   99 KODEOJ=2
C*****WRITE(NWTR,55) KODEOJ
      GO TO 20
    6 K=KTAB*KST
      KODS=0
C ��EHT���KATOP C��TAH, �O�CK B TAB COB�A�A��E�O  �MEH�
      DO 15 ITAB=1,K,KST
      IF(IS.NE.TAB(ITAB+1)) GO TO 15
      CALL EQMAS(TAB,ITAB+2,SIM,1,IS,KODS)
      IF (KODS.EQ.0) GO TO 15
C �M� HA��EHO, �TEH�E �HA�EH��
      IP=IP+1
      IF (IP.LT.K72) GO TO 9
      ERR(IP)=ER
C O���KA : �OC�E H�EHT���KATOPA HET �HA�EH��
      WRITE(NWTR,22) KAR
      WRITE(NWTR,22) ERR
      WRITE(NWTR,33)
   33 FORMAT(5X,'*** O訡��: ��᫥ ����� ��� ���祭�� ***')
      KODEOJ=2
C*****WRITE(NWTR,55) KODEOJ
      GO TO 20
    9 IEND=K72-IP+1
      IF (TAB(ITAB).EQ.1) GO TO 10
C T�� �MEH�-I
      CALL APFSQI(FKAR,I0,IP,IEND,IRC)
C �HA�EH�E C��TAHO B I0, �A��C� �HA�EH�� B TAB
      TAB(ITAB+10)=I0
      TAB(ITAB+11)=TAB(ITAB+11)+1
      GO TO 12
C T�� �MEH�-E
   10 CALL APFSQE(FKAR,E0,IP,IEND,IRC)
C �HA�EH�E C��TAHO B E0, �A��C� �HA�EH�� B RAB
C----------------------------------------------------------------------
      RAB(ITAB+10)=E0
      TAB(ITAB+11)=TAB(ITAB+11)+1
   12 IF(IRC.NE.2) GO TO 13
      IF (KAR(IP).EQ.ZAP) GO TO 18
   13 IF (KAR(IP).NE.PROB) GO TO 4
      IP=IP+1
      IF (IP.LE.K72) GO TO 13
      GO TO 20
   15 CONTINUE
      IF (KODS.EQ.1) GO TO 18
C O���KA : HET COB�A�A��E�O �MEH� B TAB                                0
      ERR(IP)=ER
      WRITE(NWTR,22) KAR
      WRITE(NWTR,22) ERR
      WRITE(NWTR,38)
   38 FORMAT(5X,'*** O訡��: ��� ��६����� �� �।�ᬮ�७�� ***')
      KODEOJ=2
C*****WRITE(NWTR,55) KODEOJ
      GO TO 20
   18 IP=IP+1
      IF(IP.LT.K72) GO TO 4
      GO TO 20
  666 KODEOJ=4
C*****WRITE(NWTR,55) KODEOJ
       WRITE(NWTR, 55055) NRDR
55055  FORMAT(//' *** ����� ������ �� ������ ', I3, ' ***'/
     =         ' *** P���� ���뢠����  [ APFNML ] ***'//)
       CALL STOPP(16) 
  100 RETURN
      END
