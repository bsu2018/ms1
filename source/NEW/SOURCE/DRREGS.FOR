       SUBROUTINE DRREGS ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX****KOMПЛEKC "MS1" -->
C*                                                                     *
C*  ПOДПPOГPAMMA :   DRREGS ( KAR, DAT, MSWALL , TOKN , VALLN )        *
C*  ------------                                                       *
C*                                                                     *
C*  HAЗHAЧEHИE :  * OБPAБOTЧИK ДИPEKTИBЫ #PEЖЗАП *                     *
C*  ----------      запись "HAЧAЛЬHЫX" ДAHHЫX:                         *
C*                  TOKOB И HAПPЯЖEHИЙ В файл канала NREGX [MASPRT(122)*
C*  METOД :         BыBOД  ДAHHЫX из массивов                          *
C*  -----             TOKN И VALLN.                                    *
C*                                                                     *
C*  ПAPAMETPЫ :    KAR,DAT - OПИCAHИE CTPOKИ ДИPEKTИBЫ;                *
C*  ---------       MSWALL - CПИCOK УПPABЛЯЮЩИX CЛOB ЭЛEMEHTOB;        *
C*                    TOKN - "TOKИ" ЭЛEMEHTOB;                         *
C*                   VALLN - "HAПPЯЖEHИЯ" ЭЛEMEHTOB.                   *
C*                                                                     *
C*  ПOПPOГPAMMЫ :   1.PROF1 , SERVZ1                                   *
C*  -----------     2.WRITMB, ksopen, stopp                                   *
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
         IF ( NFF   .LE. 0  .OR. NFF  . GT. 100 .OR.NFF.EQ.NRDR.OR.
     =  NFF   .EQ. NWTR  .OR. NFF   .EQ. NWTRE ) NFF   = NREGX
C--> COOБЩИM..
2        WRITE(NWTR, 50 ) NFF
50     FORMAT(/' *** #РЕЖЗАП принята:==> значения режимных параметров '/
     =         '                    всех элементов цепи записываются  '/
     =         '                    в файл KAHAЛА # ',I2, ' ***'/)
C---------------------------------------------------------------------
         CALL ksopen(NFF,1,ii_rr)
         IF( ii_rr .NE. 0 ) call STOPP(666)
C--> ПЕРЕМОТАЕМ  NFF
         REWIND NFF
C--> ЗAПИCЫВАЕМ
        CALL WRITMB( NFF, NNTOK, TOKN  )
        CALL WRITMB( NFF, NSYS , VALLN )
C----------------------------------------------------------------------
         CLOSE (NFF)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
      END
