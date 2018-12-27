         SUBROUTINE DRCIR1(BA0, KNOT, PAR0, IRC)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT ABƒ“CTA 1987 ƒ.)------------
C   O„POƒPAMMA:   DRCIR1(BA0, KNOT, PAR0, IRC)
C   ------------
C   HA‡HA—EHˆE:    COPTˆPOBKA BA0 ˆ OPE„E‹EHˆE PA‡MEPA MSW_ALL
C   ----------
C   METO„:         COPTˆPOBKA - CM. / TRIARB, O Pˆ‡HAK“ TˆA MO„E‹ˆ;
C   -----          PA‡MEP MSWALL OPE„E‹ŸETCŸ O O‹ŸM MACTAB(8)
C                  Pˆ TOM:   +5 C‹OB HA „ˆCKPˆTOP,
C                              +1*NXBA0 - HA “KA‡ATE‹ˆ C‹E„. MSW OˆCAHˆ
C                               (NXBA0 - KO‹ˆ—ECTBO ‹EMEHTOB –Eˆ)
C                               +1 C‹OBO HA “KA‡ATE‹œ KOH–A CˆCKA;
C                              O„POHEE CM. CTP“KT“P“ BA0 B / INPUT0;
C                 + LM = MACTAB(...+7) C‹OB HA KA†„›‰ ‹EMEHT ˆ‡ BA0;
C                   ***  Pˆ  MACTAB(...+7) = +1(VAR) - ‡APOC
C                        —EPE‡ CALLFM Pˆ BO‡“†„EHHOM ”‹AƒE FGMSW
C                        HA “TO—HEHˆE BCE‰ „‹ˆH› MSW_ALL „AHHOƒO
C                        ‹EMEHTA.    ****
C
C
C   APAMETP›:     BA0, KNOT, PAR0 - MACCˆB› OˆCAHˆŸ ‹EMEHTOB;
C   ---------      IRC = 1 - ‡A„AHˆE B›O‹Hˆ‹ˆ;
C                      = 2 - BO‡MO†HO TO‹œKO Pˆ FRCPU = 0
C                            "AAPAT" REGION OTK‹ž—EH (CM. PˆME—AHˆE);
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2
C   ------------   2. *** SORTBA ******  ( OT ABƒ“CTA 1987 ƒ. )
C                  3.  CALLFM.
C   PˆME—AHˆE:    1. Pˆ FRCPU = 1 [MASPRT(73)] OPE„E‹EHH›‰
C  -----------        PA‡MEP MSWALL ‡AˆC›BAETCŸ B DMSWAL [MASPRT(20)]
C                     Pˆ FRCPU = 0 (CTATˆ—ECKOE B›„E‹EHˆE O..) -
C                     POBO„ˆTCŸ CPABHEHˆE HEOXO„ˆMOƒO „‹Ÿ „A‹œHE‰˜E‰
C                     PAOT› PA‡MEPA MSWALL ˆ ‡HA—EHˆŸ DMSWAL ˆ‡ MASPRT:
C                     B C‹“—AE HE XBATKˆ O.. “CTAHAB‹ˆBAEˆCŸ IRC=2 ˆ
C                     ƒEHEPˆP“ETCŸ COO™EHˆE;
C                   2.  !! CTPAXOBKA !! -->  O‹ž NSYS PˆCBAˆBAETCŸ
C                                            ‡HA—EHˆE NMKNOT
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/PROF2/ MACTAB(800)
         INTEGER BA0, BA0CPL, AKKK, DMSWAL, FRCPU, FGMSW, TIP
         DIMENSION BA0(*), MSWT(50), KNOT(*), PAR0(*)
         EXTERNAL BA0CPL, BA0CNG
         EQUIVALENCE ( DMSWAL, MASPRT(20) )
         EQUIVALENCE ( FGMSW,  MASPRT(87) )
         EQUIVALENCE ( NMKNOT, MASPRT(7) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NXBA0,   MASPRT(3)  )
         EQUIVALENCE ( N1BA0,    MASPRT(4)  )
         EQUIVALENCE ( FRCPU,    MASPRT(73)  )
         EQUIVALENCE ( N1MACT,   MASPRT(17)  )
         EQUIVALENCE ( NSYS,     MASPRT(26)  )
         NSYS = NMKNOT
         IRC = 1
C--> SORT....
C!!!-------------------------------------------------------------------
C      CALL PRINT0( BA0, KNOT, PAR0)
C!!!-------------------------------------------------------------------
         CALL SORTBA(  BA0  )
C!!!-------------------------------------------------------------------
C      CALL PRINT0( BA0, KNOT, PAR0)
C!!!-------------------------------------------------------------------
C--->  OPE„E‹ŸEM PA‡MEP MSW_ALL
         MMSW = 5 + 1 * NXBA0 + 1
C-->  OˆCK O MACTAB „‹Ÿ BCEX ‹EMEHTOB:
         I0 = 6
         IMAX=BA0(3)
         DO 1 KKK = 1, NXBA0
C->  A„PEC ‹EMEHTA KKK B BA0:
         AKKK = (KKK-1)*N1BA0 + I0
C-->  KOHTPO‹œ
         IF( AKKK+N1BA0  .GT.  IMAX ) CALL SUBERR(AKKK)
C-->  CMOTPˆM MACTAB
         TIP = BA0(AKKK+1)
         IX = 1
4        CONTINUE
         IF(MACTAB(IX) .EQ. 0 ) GO TO 2
         IF(MACTAB(IX) .EQ. TIP ) GO TO 3
         IX = IX + N1MACT
         GO TO 4
2        CONTINUE
C-->  ‹Oƒ. O˜ˆKA, TˆA HET...
         CALL SUBERR(TIP)
3        CONTINUE
C--> IX HA˜A CTPOKA
C-->  KOHTPO‹œ
         IF ( MACTAB(IX+7) .LT. 0 ) CALL SUBERR(IX)
         IF ( MACTAB(IX+7) .EQ. +1 ) GO TO 60
C--->    ‹EMEHT HE C LM  -->  +1(VAR)
C ==> C—ˆTAEM
         MMSW = MMSW + MACTAB(IX+7)
       GO TO 1
C######################################################################
60     CONTINUE
C-->  “TO—HˆM LM.....
       FGMSW = 1
C      *********
C  AKKK - A„PEC OˆCAHˆŸ ‹EMEHTA B BA0, IKNOT ˆ IPAR0 -> B KNOT ˆ PAR0:
       IKNOT = BA0( AKKK+2 )
       IPAR0 = BA0( AKKK+3 )
C--->  HOMEP FM MOHˆTOPA:
       NFM = MACTAB( IX+6 ) / 1000
C-->  “TO—HˆM
       LM = 10
       CALL CALLFM( NFM, BA0(AKKK), KNOT(IKNOT), PAR0(IPAR0), MSWT, LM )
C--> CPOCˆM ”‹A†EK...
       FGMSW = 0
C-->  C—ˆTAEM
       MMSW = MMSW + LM
C######################################################################
1        CONTINUE
C######################################################################
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 51) MMSW
51       FORMAT(' *** „‹Ÿ CˆCKA MSW_ALL HEOXO„ˆMO ', I5, ' C‹OB O.. *
     =**')
C!!!-------------------------------------------------------------------
C--->  —TO “ HAC ?
         IF ( FRCPU .EQ. 0 ) GO TO 5
C--> "APPARAT" REGION BKž—EH....
         DMSWAL = MMSW
         IRC = 1
         RETURN
C**********************************************************************
5        CONTINUE
         IRC = 1
C-->  CTATˆ—ECKˆ B›„E‹ˆ‹ˆ „OCTATO—HO ??
         IF ( DMSWAL.GE. MMSW ) RETURN
         IRC = 2
C-->  HET O..
C!!!-------------------------------------------------------------------
         IZZ = MMSW - DMSWAL
         WRITE(NWTRE, 50) IZZ, DMSWAL
50      FORMAT(/' *** „‹Ÿ MSW_ALL HEXBATAET ', I5, ' C‹OB O.. ***'/
     =  ' *** ˆ‡HA—A‹œHO O„ MSW_ALL B›„E‹EHO ', I5, ' C‹OB O.. ***'/
     = ' *** PAOTA Bˆ„ˆMO “„ET PEPBAHA ***'///)
C!!!-------------------------------------------------------------------
         RETURN
         END
