         SUBROUTINE DRCIR2( BA0, KNOT, PAR0, MSWALL )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.0  (OT ДEKAБPЯ 1986 Г.)------------
C   ПOДПPOГPAMMA:   DRCIR2( BA0, KNOT, PAR0, MSWALL )
C   ------------
C   HAЗHAЧEHИE:    ФOPMИPOBAHИE CПИCKA MSWALL
C  ------------
C                  ( 3-Я ЧACTЬ ДИPEKTИBЫ #ЦEПЬ )
C   METOД:         CM. П/П GETNEW;
C   -----
C   ПAPAMETPЫ:     CM. GETNEW;
C   ---------
C
C   ПOДПPOГPAMMЫ:  1. COMMON PROF1
C   -------------  2. GETMSW
C   ПPИMEЧAHИE:    1. B CЛУЧAE УCПEШHOГO ФOPMИPOBAHИЯ MSWALL
C   ----------        BOЗБУЖДAETCЯ ФЛAГ FMSWAL
C                  2. ПOДPOБHEE CM. INPUT0
C                  3. MACCИB MSWALL ДOЛЖEH БЫTЬ  ИHИЦИИPOBAHHЫ;
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
100     FORMAT(//' *** Логические ошибки описания цепи - цепь сбрасывает
     =ся ***'//)
C23456
         RETURN
         END
