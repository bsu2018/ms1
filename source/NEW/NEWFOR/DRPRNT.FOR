         SUBROUTINE DRPRNT(KAR, DATDIR, MSWALL, TOKN, VALN )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.0  (OT ДEKAБPЯ 1986 Г.)------------
C   ПOДПPOГPAMMA:   DRPRNT(KAR, DATDIR, MSWALL )
C   ------------
C   HAЗHAЧEHИE :   *** OБPAБOTKA ДИPEKTИBЫ  #ПEЧ:
C   -----------
C                    #ПEЧ T0 TM KOUT  PPVENT
C
C            ГДE:    T0 - BPEMЯ HAЧAЛA BЫBOДA;
C                    TM - BPEMЯ KOHEЦA BЫBOДA;
C                    KOUT - "KAЖДУЮ KOUT - УЮ TOЧKУ ИЗ PЯДA COXPAHЯEMЫX
C                            BЫBOДHЫX ЗHAЧEHИЙ PEШEHИЯ (ШAГ COXPAHEHИЯ
C                            HOUTT) BЫBOДИTЬ HA ПEЧATЬ ";              0
C                      PPVENT - ПPИ = 0 HE ПEЧATAETCЯ ИHФOPMAЦИЯ B
C                                       MOMEHT KOMMУTAЦИИ;
C                               ПPИ = 1 - ПEЧATAETCЯ;
C                                 ( УMOЛЧAHИE = 0 );
C
C
C
C                  ПPИ УCПEШHOM ПPИHЯTИИ ПAPAMETPOB ДИPEKTИBЫ И
C                   CПИCKA "ЗAKAЗ BЫBOДA" --> ЗAПOЛHЯET ПOЛЯ
C                   COMMON/SERVZ6/ И BOЗБУЖДAET ФЛAГ "PON",
C                    + BOЗБУЖДEHИE MASPRT(106) <-> PPVENT;
C   METOД:         OЧEBИДEH;
C   ------
C
C   ПAPAMETPЫ:     KAR - CИMBOЛЬHAЯ CTPOKA TEKCTA ДИPEKTИBЫ #ПEЧ;
C   ---------            (ПOCTABЛЯETCЯ MOHИTOPOM - CKAHEPOM );
C                  DATDIR - MACCИB ПPИHЯTЫX ЧИCEЛ ИЗ KAR->(CTPOKA ДИP.)
C                  MSWALL - CПИCOK COCTOЯHИЯ ЭЛEMEHTOB ЦEПИ;
C   ПOДПPOГPAMMЫ:  1. GETSPS
C   ------------   2. COMMON PROF1, SERVZ6
C   ПPИMEЧAHИE:    1. B COMMON SERVZ6 ФИKCИPУЮTCЯ:
C   ----------        TBEGON, TENDON, KOUTON, SPISON
C                  2.  ПO УMOЛЧAHИЮ T0=0.0, TM=666.0, KOUT=1;
C
C               От 02.04.1992 г. Дополнены 6 элементов,являющих 
C                масштабами для динамического вывода 6 графиков
C                в массиве MASPRT(130 -141) .
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
         REAL TOKN(*),VALN(*)
         INTEGER SPISON
         DIMENSION KAR(*), DATDIR(*), MSWALL(*)
         EQUIVALENCE ( TMAX1,MASPRT(130))
         EQUIVALENCE ( TMAX2,MASPRT(131))
         EQUIVALENCE ( TMAX3,MASPRT(132))
         EQUIVALENCE ( TMAX4,MASPRT(133))
         EQUIVALENCE ( TMAX5,MASPRT(134))
         EQUIVALENCE ( TMAX6,MASPRT(135))
         NRDR = MASPRT(12)
         NWTR = MASPRT(13)
C--->  ECTЬ ЛИ MSWALL
       IF ( MASPRT(74) .EQ. 1 ) GO TO 200
       WRITE(NWTR,201)
201    FORMAT(/' *** HET OПИCAHИЯ ЦEПИ - ДИPEKTИBA ИГHOPИPУETCЯ ***'/)
        RETURN
200    CONTINUE
C        ***************************
         KOUT = 1
         IPP = 0
C********READ(NRDR, PON)
       IF ( DATDIR(1) .EQ. 0.0 ) GO TO 100
C--> ЧTO-TO ECTЬ
       T0 = DATDIR(2)
       TM = DATDIR(3)
c**************************************
         DO 141  I = 6,8
         IF (DATDIR(I).EQ.0.)  DATDIR(I) = 1.
141      CONTINUE
         TMAX1 = DATDIR(6)
         TMAX2 = DATDIR(7)
         TMAX3 = DATDIR(8)
         TMAX4 = DATDIR(9)
         TMAX5 = DATDIR(10)
         TMAX6 = DATDIR(11)
c         PRINT *,'*********************************'
c         PRINT *,'*********************************'
c         PRINT *,'*********************************'
c         PRINT *,'*********************************'
c         PRINT *,TMAX1,TMAX2
c         pause ' press any key'
C****************************************
       KOUT = IFIX( DATDIR(4) )
       IPP = IFIX( DATDIR(5) )
       IF ( IPP .EQ. 0 ) GO  TO   101
       IPP = 1
       GO TO 101
100    CONTINUE
C-->  У M O Л Ч A H И E
       T0 = 0.0
       TM = +666.0
       KOUT = 1
       IPP = 0
101    CONTINUE
C********WRITE(NWTR, PON)
         IF ( KOUT .LE. 0 ) KOUT = 1
         TBEGON = T0
         TENDON = TM
         KOUTON = KOUT
         WRITE(NWTR, 50) T0, TM, KOUT, IPP
50       FORMAT(/' *** #ПEЧ ПPИHЯTA:==> T0=',G12.5,' TM=',G12.5,
     =  ' KOUT=',I5, ' PPVENT=', I2, ' ***'/)
C--------
         CALL GETSPS(MSWALL, SPISON, IRC )
         GO TO (1, 2, 3 ), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR,10)
10       FORMAT(/' *** KOHEЦ ФAЙЛA "RDR" - PAБOTA ПPEPЫBAETCЯ ***'/
     = ' *** [ DRPRNT ] ***'/)
         STOP 666
C######################################################################
2        CONTINUE
C--->  SPIS ПУCT....   CБPOC PON....
         MASPRT(56) = 0
         MASPRT(106) = 0
         RETURN
C######################################################################
1        CONTINUE
C--> CПИCOK ECTЬ.....  PON  "BЗДEPHEM"
         MASPRT(56) = 1
         MASPRT(106) = IPP
         RETURN
         END
