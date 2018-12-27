       SUBROUTINE SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
     *                    IFXK, IFXA, EYYN, EYYN1 )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
C  --------------         IFXK, IFXA, EYYN, EYYN1 )
C
C   HA‡HA—EHˆE : “PAB‹EHˆE PA‡„E‹œHO‰ PAOTO‰ BEHTˆ‹œH›X ƒP“,
C   -----------   *** BEPTˆKA‹œHOE “PAB‹EHˆE ***
C
C   METO„:  OO‡HA—EHˆE  IH(T) = IA(T) - IK(T);
C   -----
C              Pˆ  ABS( IH(TN) ) <= IYSTAV ---> "PA‡PE˜EHˆŸ":
C                      EYYN1 > 0  „O‹†HA "A" ƒP“A;
C                      EYYN1 < 0  „O‹†HA "K" ƒP“A;
C                      EYYN1 = 0  „O‹†HA "A" ƒP“A ˆ "K" ƒP“A;
C
C              Pˆ ABS( IH(TN+H0) ) <= IYSTAV ---> "‡PET›":
C                      EYYN1 > 0  HE „O‹†HA "A" ƒP“A;
C                      EYYN1 < 0  HE „O‹†HA "K" ƒP“A;
C                      EYYN1 = 0  HE „O‹†HA "A" ƒP“A ˆ "K" ƒP“A;
C                                 (????????????????????????????????)
C
C  !!!  TXA/K ˆ‡MEHŸETCŸ B C‹“—AE „E‰CTBˆTE‹œHO‰ CMEH› CXA/K   !!!
C
C   APAMETP›: CXK - ”‹Aƒ ‡APE™EHˆŸ ( =0 ) / PA‡PE˜EHˆŸ ( =1 ) "O„A—ˆ"
C   ---------        "Cˆ“" HA K-ƒP““;
C              TXK - BPEMŸ, HA—ˆHAŸ C KOTOPOƒO „E‰CTB“ET CXK ”‹Aƒ;
C              IFXK - ”‹Aƒ PA‡PE˜EHˆŸ ˆ‡MEHEHˆŸ ‡HA—EHˆŸ TX B RMSW_50
C                     OˆCAHˆŸ BEHTˆ‹Ÿ HA ‡HA—EHˆE TXK ( Pˆ =1 MO†HO );
C                 „ ‹ Ÿ  A-ƒP“›  --> AHA‹Oƒˆ—HO;
C              EYYN, EYYN1 - ‡HA—EHˆŸ “PAB‹Ÿž™ˆX CˆƒHA‹OB B TN, TN+H0;
C              IKN, IKN1 - TOKˆ K-ƒP“› B TN, TN+H0;
C              IAN, IAN1 - TOKˆ A-ƒP“› B TN, TN+H0;
C                                                                      ;
C
C
C   O„POƒPAMM›:   PROF1;
C   ------------
C   PˆME—AHˆE:    IYSTAV ˆ‡ MASPRT(105);
C   ----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( ICLOSE, MASPRT(78) )
         EQUIVALENCE ( IYSTAV, MASPRT(105) )
         REAL ICLOSE, IH, IH1
C----------------------------------------------------------------------
       REAL IYSTAV,IKN, IKN1, IAN, IAN1
       INTEGER CXA, CXK
C
       IFXK = 0
       IFXA = 0
       IH=IAN-IKN
       IH1=IAN1-IKN1
C**********************************************************************
       AIH1 = ABS( IH1 )
       AIH  = ABS( IH  )
C----------------------------------------------------------------------
       IF ( AIH .LE. IYSTAV ) GO TO 1
10     CONTINUE
       IF ( AIH1 .LE. IYSTAV ) GO TO 100
C--->  Hˆ —EƒO HE MEHŸTœ B TOM C‹“—AE....
20     CONTINUE
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  "PA‡PE˜EHˆŸ" COƒ‹ACHO ‡HAKA EYYN1......
       IF ( EYYN1 ) 2,3,4
C******
2      CONTINUE
C-->   < 0  „O‹†EH K.
       IF ( CXK .EQ. 1 ) GO TO 10
       CXK = 1
       IFXK = 1
       TXK = TN + H0
       GO TO 10
3      CONTINUE
C   = 0,  „O‹†EH ˆ K ˆ A..
       IF ( CXK .EQ. 1 ) GO TO 30
       CXK = 1
       IFXK = 1
       TXK = TN + H0
30     CONTINUE
       IF ( CXA .EQ. 1 ) GO TO 10
       CXA = 1
       IFXA = 1
       TXA = TN + H0
       GO TO 10
4      CONTINUE
C  >  0  „O‹†EH A....
      IF ( CXA .EQ. 1 ) GO TO 10
      CXA = 1
      IFXA = 1
      TXA = TN + H0
      GO TO 10
C----------------------------------------------------------------------
100    CONTINUE
C-->  ‡APET  COƒ‹ACHO EYYN1......
       IF ( EYYN1 ) 5, 6, 7
5      CONTINUE
C  < 0  HE „O‹†EH A
       IF ( CXA .EQ. 0 ) GO TO 20
       CXA = 0
       IFXA = 1
       TXA = TN + H0
       GO TO 20
6      CONTINUE
C   = 0  HE „O‹†EH Hˆ K Hˆ A ( ????)
C@@@@@GO TO 20
       IF ( CXA .EQ . 0 ) GO TO  60
       CXA = 0
       IFXA = 1
       TXA = TN + H0
60     CONTINUE
       IF ( CXK .EQ. 0 ) GO TO 20
       CXK = 0
       IFXK = 1
       TXK = TN + H0
       GO TO 20
7      CONTINUE
C  > 0 HE „O‹†EH K...
       IF ( CXK .EQ. 0 ) GO TO 20
       CXK = 0
       IFXK = 1
       TXK = TN + H0
       GO TO 20
C**********************************************************************
C**********************************************************************
       END
