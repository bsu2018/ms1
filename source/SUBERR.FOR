         SUBROUTINE SUBERR(IDUMP)
CXXX ---KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)-------------
C      O„POƒPAMMA SUBERR(IDUMP)
C      ------------
C      HA‡HA—EHˆE: OTPAOTKA COŸ POƒPAMMHOƒO KOM‹EKCA.
C      ----------
C      METO„:      ˆ‡MEHŸETCŸ B PO–ECCE OT‹A„Kˆ.
C      -----
C      APAMETP›:  DUMP - POCœA B›„A—ˆ O. ˆ T..
C      ---------
C                   Pˆ IDUMP = 0 "„AM" HE B›BO„ˆTCŸ!
C
C   O„POƒPAMM›: ERRTRA - C‹“. ”OPT. ƒM.
C   ------------
C
C
C      PˆME—AHˆE: OT‹A„O—HAŸ POƒAMMA;
C      ----------
CZZZ -------------------------------------------------------------------
C   V.M.=2.0
C
       I = 10
       J=0
       WRITE(6,666) IDUMP
666    FORMAT(/1X,'***> "SUBERR" :=> IDUMP = ', I10,'       <***' )
       WRITE(6,667)
667    FORMAT( ' ***> ‡APOC K "SUBERR" O –EO—KE B›‡OBOB: <***'/)
C----------------------------------------------------------------------
C      CALL ERRTRA
        I=I/J
C----------------------------------------------------------------------
16      CALL STOPP(666)
        END
