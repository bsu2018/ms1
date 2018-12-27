       SUBROUTINE DRREG0 ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX****KOMПЛEKC "MS1" --> BEPCИЯ 1.0  (OT ДEKAБPЯ 1986 Г.)*************
C*                                                                     *
C*  ПOДПPOГPAMMA :   DRREG0 ( KAR, DAT, MSWALL , TOKN , VALLN )        *
C*  ------------                                                       *
C*                                                                     *
C*  HAЗHAЧEHИE :  * OБPAБOTЧИK ДИPEKTИBЫ #PEЖИM *                      *
C*  ----------      ПPИEM CПИCKA И ЗAДAHИE "HAЧAЛЬHЫX" ДAHHЫX          *
C*                  TOKOB И HAПPЯЖEHИЙ COГЛACHO CПИCKA:                *
C*                  #PEЖИM                                             *
C*                  "V" Z "У" K1 K2....;                               *
C*                  "V" Z "У" "BCE";                                   *
C*                  "I" Z "Э" N1[.S1]...N2...N3[.BCE];                 *
C*                  "GAMMA" Z "Э" N1 N2...;                            *
C*                  "OMEGA" Z "Э" N1 N2...;                            *
C*                  "I" Z "Э" "BCE";                                   *
C*                   #BCE                                              *
C*           *** ЭTO HE PEAЛИЗOBAHO - TOЛЬKO OБHУЛEHИE -               *
C*                                                                     *
C*  METOД :         BBOД И PAЗБOP ДAHHЫX , KOHTPOЛЬ И ЗAПИCЬ           *
C*  -----           B TOKN И VALLN COГЛACHO HAЛИЧИЯ B MSWALL.          *
C*                                                                     *
C*  ПAPAMETPЫ :    KAR,DAT - OПИCAHИE CTPOKИ ДИPEKTИBЫ;                *
C*  ---------       MSWALL - CПИCOK УПPABЛЯЮЩИX CЛOB ЭЛEMEHTOB;        *
C*                    TOKN - "TOKИ" ЭЛEMEHTOB;                         *
C*                   VALLN - "HAПPЯЖEHИЯ" ЭЛEMEHTOB.                   *
C*                                                                     *
C*  ПOПPOГPAMMЫ :   1.PROF1 , SERVZ1                                   *
C*  -----------     2.INPUT2                                           *
C*                                                                     *
C*  ПPИMEЧAHИE:     ЧACTИЧHAЯ PEAЛИЗAЦИЯ                               *
C* ------------     ( MAKET ПPOГPAMMЫ ).                               *
C*                                                                     *
CZZZ********************************************************************
      COMMON / PROF1 / MASPRT(200)
      DIMENSION KAR(1),DAT(1),MSWALL(1),TOKN(1),VALLN(1)
      EQUIVALENCE ( NNTOK , MASPRT(46) )
      EQUIVALENCE ( FMSWAL , MASPRT(74) )
      EQUIVALENCE ( NMKNOT , MASPRT(7) )
      INTEGER FMSWAL
C-->
      EQUIVALENCE ( NWTR , MASPRT(13) )
      EQUIVALENCE ( NRDR , MASPRT(12) )
      IF( FMSWAL.EQ.0 ) WRITE (NWTR,1)
 1    FORMAT(/' *** HAPУШEH ПOPЯДOK CЛEДOBAHИЯ ДИPEKTИB.',
     *' ДИPEKTИBA ИГHOPИPУETCЯ ***'/)
      IF( FMSWAL.EQ.0 ) RETURN
C-->   ЗAHУЛEHИE БEЗ BCЯKOГO...
C----------------------------------------------------------------------
      DO 100 I=1,NNTOK
      TOKN(I)=0.0
 100  CONTINUE
C----------------------------------------------------------------------
      DO 200 I=1,NMKNOT
      VALLN(I)=0.0
 200  CONTINUE
C----------------------------------------------------------------------
C
       WRITE(NWTR,3000)
3000   FORMAT(/' *** Директива #PEЖИM - пока  не реализована ***'  )
       WRITE(NWTR,3004)
3004  FORMAT(' *** начальные значения всех переменных устанавливаются',
     =' равными 0.0 ***'/)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
      END
