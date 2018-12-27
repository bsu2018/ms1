       SUBROUTINE INTPRT
CXXXX---KOMèãEKC "MS1" --> BEPCàü 1.5  (OT ABÉìCTA 1989 É.)------------
C   èOÑèPOÉPAMMA:  INTPRT
C   -------------
C   HAáHAóEHàE:    áAÑAHàE HAóAãúHõX áHAóEHàâ Ñãü MASPRT........
C   ----------
C   METOÑ:
C   -----
C   èAPAMETPõ:
C   ---------
C   èOÑèPOÉPAMMõ: COMMON /PROF1/
C   ------------
C   èPàMEóAHàE:
C   ----------
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         CHARACTER*8 JNAME
         DIMENSION R(200)
         EQUIVALENCE( MASPRT(1), R(1) )
         EQUIVALENCE( MASPRT(125), JNAME)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         DATA NVER/' 2.0'/
1        CONTINUE
C-->  èO ìMOãóAHàû....
         DO 1000 I = 1, 200
1000     MASPRT(I) = 0
         MASPRT(2) = 100
         MASPRT(4) = 8
         MASPRT(6) = 650
         MASPRT(9) = 500
C       KOã-BO MAKPOMOÑEãEâ
         MASPRT(10) = 110
         MASPRT(11) = 0
         MASPRT(12) = 5
         MASPRT(13) = 3
         MASPRT(14) = 6
         MASPRT(15) = 0
         MASPRT(16) = 0
         MASPRT(17) = 8
         MASPRT(18) = 9
         MASPRT(20) = 2000
         MASPRT(21) = NVER
         MASPRT(23) = 50
         R(29) = 0.0
         R(30) = 5.0/ 1.8E+4
         R(31) = 0.5
         R(39) = 1E-9
         R(40) = 5.0 / 1.8E+4
         R(48) = R(40)
         R(50) = 1E-32
C **** PAáMEP MASPRT  =================================================
         MASPRT(54) = 200
         R(59) = 1E-5
         MASPRT(60) = 0
         R(62) = 0.0
         R(63)=90.0
         MASPRT(64) = 1
         MASPRT(66) = 10
         R(67) = 1E-3
         MASPRT(68) = 0
         R(69) = 1E-3
         MASPRT(70) = 10
         MASPRT(71) = 10
         R(72) = 1E-6
         MASPRT(73) = 0
         MASPRT(76) = 10000
         R(77) = 1.E-2
         R(78) = 1.E-2
         MASPRT(83) = 4
         MASPRT(86) = 0
         MASPRT(89) = 1
         R(90) = 0.5
         R(94) = 1.0 / 1.8E+4
         MASPRT(95) = 10
         MASPRT(97) = 11
         R(100) = 50.0
         R(101) = 25.0
         R(102) = 0.0
         R(103) = 975.0
         R(104) = 0.5
         R(105) = 1.0E-1
         MASPRT(106) = 0
         MASPRT(107) = 0
         MASPRT(108) = 0
         MASPRT(109) = 2
         R(110) = 1.0E-6
         MASPRT(113) = 0
         MASPRT(114) = 1
         R(115) = 0.0
         R(116) = 0.0
         R(117) = 0.0
         MASPRT(118) = 0
         MASPRT(119) = 0
         MASPRT(120) = 0
         MASPRT(121) = 0
         MASPRT(122) = 12
         MASPRT(123) = 13
         MASPRT(124) = 14
C        MASPRT(125) =....
C        MASPRT(126) =....
         JNAME = 'MS1-1989'
         MASPRT(127) = 0
         MASPRT(128) = 0
         MASPRT(129) = 0
C
C
       RETURN
       END
