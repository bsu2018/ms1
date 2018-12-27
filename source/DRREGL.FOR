       SUBROUTINE DRREGL ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX****KOMПЛEKC "MS1" -->
C*                                                                     *
C*  ПOДПPOГPAMMA :   DRREGL ( KAR, DAT, MSWALL , TOKN , VALLN )        *
C*  ------------                                                       *
C*                                                                     *
C*  HAЗHAЧEHИE :  * OБPAБOTЧИK ДИPEKTИBЫ #PEЖВОСТ *                    *
C*  ----------      чтение "HAЧAЛЬHЫX" ДAHHЫX:                         *
C*                  TOKOB И HAПPЯЖEHИЙ с файла канала NREGX=MASPRT(122)*
C*  METOД :         BBOД  ДAHHЫX в  массивы                            *
C*  -----             TOKN И VALLN.                                    *
C*                                                                     *
C*  ПAPAMETPЫ :    KAR,DAT - OПИCAHИE CTPOKИ ДИPEKTИBЫ;                *
C*  ---------       MSWALL - CПИCOK УПPABЛЯЮЩИX CЛOB ЭЛEMEHTOB;        *
C*                    TOKN - "TOKИ" ЭЛEMEHTOB;                         *
C*                   VALLN - "HAПPЯЖEHИЯ" ЭЛEMEHTOB.                   *
C*                                                                     *
C*  ПOПPOГPAMMЫ :   1.PROF1 , SERVZ1                                   *
C*  -----------     2.READMB , ksopen, stopp                                          *
C*                                                                     *
C*  ПPИMEЧAHИE:
C* ------------
C*                                                                     *
CZZZ********************************************************************
      COMMON / PROF1 / MASPRT(200)
      DIMENSION KAR(1),DAT(*),MSWALL(1),TOKN(1),VALLN(1)
      EQUIVALENCE ( NNTOK , MASPRT(46) )
       EQUIVALENCE ( NSYS,  MASPRT(26) )
      EQUIVALENCE ( FMSWAL , MASPRT(74) )
      EQUIVALENCE ( NMKNOT , MASPRT(7) )
      EQUIVALENCE ( NREGX  , MASPRT(122) )
      INTEGER FMSWAL
C-->
      EQUIVALENCE ( NWTR , MASPRT(13) )
      EQUIVALENCE ( NRDR , MASPRT(12) )
      IF( FMSWAL.EQ.0 ) WRITE (NWTR,1)
 1    FORMAT(/' *** Hарушен порядок следования директив.',
     *' директива игнорируется ***'/)
      IF( FMSWAL.EQ.0 ) RETURN
C----------------------------------------------------------------------
         NFF   = NREGX
         IF ( DAT(1) .EQ. 0.0 ) GO TO 2
C--> ЧTO-TO ECTЬ
         NFF   = IFIX( DAT(2) )
C--> KOHTPOЛЬ
         IF ( NFF   .LE. 0  .OR. NFF  . GT. 100 .OR. NFF .EQ.NRDR.OR.
     =  NFF   .EQ. NWTR  .OR. NFF   .EQ. NWTRE ) NFF   = NREGX
C--> COOБЩИM..
2        WRITE(NWTR, 50 ) NFF
50     FORMAT(/' *** #РЕЖВОСТ принята:==> значения режимных параметров'/
     =         ' ***             всех элементов цепи восстанавливаются'/
     =         ' ***             из файла KAHAЛА # ',I2 /)
C---------------------------------------------------------------------
         CALL KSOPEN(NFF,1,ii_rr)
         IF (ii_rr .NE. 0 ) call STOPP(20)
C--> ПЕРЕМОТАЕМ  NFF
         REWIND NFF
C--> ЧИТАЕМ
        CALL READMB( NFF, NNTOK, TOKN, IRR  )
        IF ( IRR .NE. 0 ) GO TO 666
        CALL READMB( NFF, NSYS , VALLN, IRR )
        IF ( IRR .NE. 0 ) GO TO 666
C----------------------------------------------------------------------
       CLOSE (NFF)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
666   CONTINUE
         WRITE(NWTR, 51 ) NFF
51     FORMAT(/' *** #РЕЖВОСТ: Неисправимые ошибки при чтении '/
     =         ' ***           из файла KAHAЛА # ',I2/ )
C-->   ЗAHУЛEHИE БEЗ BCЯKOГO...
       CLOSE (NFF)
C----------------------------------------------------------------------
      DO 100 I=1,NNTOK
      TOKN(I)=0.0
 100  CONTINUE
C----------------------------------------------------------------------
      DO 200 I=1,NMKNOT
      VALLN(I)=0.0
 200  CONTINUE
C----------------------------------------------------------------------
       WRITE(NWTR,3004)
3004  FORMAT(' *** HAЧAЛЬHЫE ЗHAЧEHИЯ BCEX ПEPEMEHHЫX УCTAHABЛИBAЮTCЯ',
     =' PABHЫMИ 0.0 ***'/)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
      END
