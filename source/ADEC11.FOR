       SUBROUTINE ADEC11( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (AT OCTOBER  1989  ƒ.)-----------
C   O„POƒPAMMA: ADEC11( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )

C   HA‡HA—EHˆE:  OPAOT—ˆK „B“XA„PECH›X KOMAH„
C   ----------   MˆKPOPO–ECCOPA  K1801BM1(2,3)  ( PAOTA C A‰TAMˆ )
C              *------------------------------------------------------*
C              * KOMAH„A  "MOVB"( 11SSDD Q ):                          *
C              * OEPAH„ ˆCTO—HˆK   <SS> EPEC›‹AETCŸ O A„PEC“       *
C              * OEPAH„A PˆEMHˆKA <DD> .                            *
C              *------------------------------------------------------*
C              * KOMAH„A  "CMPB"( 12SSDD Q ):                          *
C              * CPABHˆBAETCŸ OEPAH„ ˆCTO—HˆKA <SS> ˆ OEPAH„  Pˆ-  *
C              * EMHˆKA <DD> ˆ ˆ‡MEHŸžTCŸ Pˆ‡HAKˆ. OA OEPAH„A HE   *
C              * ˆ‡MEHŸžTCŸ.                                          *
C              *------------------------------------------------------*
C              * KOMAH„A  "BITB"( 13SSDD Q ):                          *
C              * B›O‹HŸET ‹Oƒˆ—ECK“ž ”“HK–ˆž  "ˆ" HA„ OEPAH„OM      *
C              * ˆCTO—HˆKA <SS> ˆ OEPAH„OM  PˆEMHˆKA <DD>, ˆ‡MEHŸŸ  *
C              * COOTBETCTB“ž™ˆM OPA‡OM Pˆ‡HAKˆ. OA OEPAH„A HE    *
C              * ˆ‡MEHŸžTCŸ.                                          *
C              *------------------------------------------------------*
C              * KOMAH„A  "BICB"( 14SSDD Q ):                          *
C              * O—ˆ™AET KA†„›‰ PA‡PŸ„ OEPAH„A PˆEMHˆKA  <DD>,      *
C              * COOTBETCTB“ž™ˆ‰ “CTAHOB‹EHHOM“ PA‡PŸ„“ OEPAH„A      *
C              * ˆCTO—HˆKA <SS>. CO„EP†ˆMOE  <SS>  HE ˆ‡MEHŸETCŸ.     *
C              *------------------------------------------------------*
C              * KOMAH„A  "BISB"( 15SSDD Q ):                          *
C              * B›O‹HŸET ‹Oƒˆ—ECK“ž OEPA–ˆž "ˆ‹ˆ" HA„ CO„EP†ˆM›M   *
C              * OEPAH„A ˆCTO—HˆKA <SS> ˆ OEPAH„A PˆEMHˆKA <DD> ˆ  *
C              * ‡AˆC›BAET PE‡“‹œTAT O A„PECC“  <DD>.               *
C              * CO„EP†ˆMOE  <SS>  HE ˆ‡MEHŸETCŸ.                     *
C              *------------------------------------------------------*
C
C   METO„:      ‹Oƒˆ—ECKOE MO„E‹ˆPOBAHˆE.
C   -----
C
C   APAMETP›:  BXO„H›E :
C  ----------      KD     - KO„ KOMAH„›  ;
C                  REG(8) - MACCˆB PEƒˆCTPOB MˆKPOPO–ECCOPA ;
C                           REG(1) --> R0
C                           REG(2) --> R1
C                           REG(3) --> R2
C                           REG(4) --> R3
C                           REG(5) --> R4
C                           REG(6) --> R5
C                           REG(7) --> R6 ( “KA‡ATE‹œ CTEKA )
C                           REG(8) --> R7 ( C—ET—ˆK KOMAH„  )
C                  FLAG(6)- MACCˆB ”‹AƒOB CC MˆKPOPO–ECCOPA ;
C                           FLAG(1) --> ”‹Aƒ  C (EPEHOC)
C                           FLAG(2) --> ”‹Aƒ  V (APˆ”M. EPEO‹HEHˆE)
C                           FLAG(3) --> ”‹Aƒ  Z (H“‹EBO‰ PE‡“‹œTAT)
C                           FLAG(4) --> ”‹Aƒ  N (OTPˆ–AT.PE‡“‹œTAT)
C                           FLAG(5) --> ”‹Aƒ  T
C                           FLAG(6) --> ”‹Aƒ I/O(PˆOPˆTET)
C                  ADPRTO - A„PEC OPTA B›BO„A ;
C                  ADPRTI - A„PEC OPTA BBO„A ;
C                  ADPROG - MAKCˆMA‹œH›‰ A„PEC O‡“ ,B PE„E‹AX KOTOPOƒO
C                           PAOTAET “PAB‹Ÿž™AŸ POƒPAMMA ;
C                  IPORT  - CO„EP†ˆMOE OPTA B›BO„A ;
C                  IPORT1 - CO„EP†ˆMOE OPTA BBO„A ;
C                  IBAI1 -  CTAP˜ˆ‰ PA‡PŸ„
C                  IBAI2 -  M‹A„˜ˆ‰ PA‡PŸ„
C
C               B›XO„H›E :
C                  T      - BPEMŸ B›O‹HEHˆŸ KOMAH„› ;
C                  IER    - “KA‡ATE‹œ O˜ˆKˆ:
C                           IER=0  -  O˜ˆOK HE OHAP“†EHO ;
C                           IER=1  -  O˜ˆO—HAŸ ‡AˆCœ KOMAH„› ‹ˆO
C                                     B›XO„ ‡A PE„E‹› O‡“ ;
C                           IER=2  -  POˆ‡O˜‹O OPA™EHˆE K Ÿ—E‰KE
C                                     AMŸTˆ, ˆMEž™E‰ HE—ETH›‰ A„PEC.
C                           IER=3  -  HE HA˜A KOMAH„A ;
C                           IER=4  -  OPA™EHˆE K OPT“ B/B
C                                     HE O TOM“ A„PEC“ ;
C                           IER=5  -  KOMAH„A OKA HE PEA‹ˆ‡OBAHA ;
C
C
C   O„POƒPAMM›:  1. ADECIS, ADECDB, ADECBD ;
C   ------------
C
C   PˆME—AHˆE:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6),  ADPROG, KB(16), KB1(16)
       INTEGER  ADPRTO, ADPRTI
       IER = 0
       KD = KD - 32768
       KOM = KD / 4096
C----------------------------------------------------------------------
C--> ˆ™EM OEPAH„ ˆCTO—HˆK ( A„PEC--> IADRS,  —ˆC‹O--> IDATA )
       KD1 = ( KD - KOM*4096 ) / 64
       CALL ADECIS( KD1, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .EQ. 1 ) RETURN
       IER0 = IER
       IF( IADRS .GE.-8 ) GO TO 10
C--> ‡APOC HA BBO„ „AHH›X ˆ‡ OPTA BBO„A
CCCC   WRITE (6,*) ADPRTI,ADPRTO
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT1
10     CONTINUE
C----------------------------------------------------------------------
C--> ˆ™EM OEPAH„ PˆEMHˆK ( A„PEC--> IADRS1,  —ˆC‹O--> IOLD  )
       KD1 = KD - KOM*4096 - KD1*64
       CALL ADECIS( KD1, ADPROG, REG, IADRS1, IOLD, TO, IER )
       IF( IER   .EQ. 1 ) RETURN
       IER1 = IER
       IF( IADRS1.GE.-8 ) GO TO 20
C--> ‡APOC HA B›BO„ „AHH›X B OPT  B›BO„A
       IF( (-IADRS1) .NE. ADPRTO ) GO TO 9904
       IOLD = IPORT
20     CONTINUE
C----------------------------------------------------------------------
C -->  PA‡OœEM C‹OBO HA A‰T›
       IBAI1 = IDATA / 256
       IBAI2 = IDATA - IBAI1*256
C--> KAKO‰ A‰T OPABAT›BAEM ?
       IDATA = IBAI2
       IF ( IER0.EQ.2 ) IDATA = IBAI1
C -->  PA‡OœEM C‹OBO HA A‰T›
       IBAI3 = IOLD / 256
       IBAI4 = IOLD - IBAI3*256
C--> KAKO‰ A‰T OPABAT›BAEM ?
       IOLD = IBAI4
       IF ( IER1.EQ.2 ) IOLD = IBAI3
C--> PA‡O‰„EMCŸ O KOMAH„AM :
       GO TO ( 100,  200,  300,  400,  500, 9903, 9903, 9903, 9903,
     *        9903, 9903, 9903, 9903,9903, 9903 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAH„A  "MOVB"
C----------------------------------------------------------------------
100    CONTINUE
       IF( IADRS1 .LT. 0 ) GO TO 110
       IF( IER1.EQ. 2 ) GO TO 105
       MEM(IADRS1+1)=(MEM(IADRS1+1)/256)*256 + IDATA
       GO TO 120
105    CONTINUE
       MEM(IADRS1+1)= MEM(IADRS1+1) + (IDATA -
     * MEM(IADRS1+2))*256
       MEM(IADRS1+2)= IDATA
       GO TO  120
110    CONTINUE
       IF( IADRS1 .LT.-8 ) GO TO 115
       IREG1 = REG(-IADRS1)/32768
       TREG2 = REG(-IADRS1) - IREG1*32768
       REG(-IADRS1) = (IREG2 / 256)*256 + IDATA
       IF (IDATA.GT.128) REG(-IADRS1) = REG(-IADRS1)+32768
       GO TO 120
115    CONTINUE
       IPORT = (IPORT / 256) + IDATA
120    CONTINUE
       IER = 0
C--> OC—ˆTAEM BPEMŸ
       TB = 4.4E-6
       T = TB + TI + TO
C--> ‡A‰MEMCŸ Pˆ‡HAKAMˆ.
       FLAG(2) = 0
       FLAG(3) = 0
       IF( IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( IDATA .GE. 32768 ) FLAG(4) = 1
       RETURN
C######################################################################
C----------------------------------------------------------------------
C              KOMAH„A  "CMPB"
C----------------------------------------------------------------------
200    CONTINUE
        IDATA = IDATA  - IOLD
C--> OC—ˆTAEM BPEMŸ
       TB = 3.2E-6
       IF( TI .EQ. 0 .AND. TO .EQ. 0 ) TB = 3.6E-6
       T = TB + TI + TO
C--> ‡A‰MEMCŸ Pˆ‡HAKAMˆ.
       FLAG(1) = 0
       IF(IDATA .GE. 0 ) GO TO 205
       FLAG(1) = 1
       IDATA = 256 + IDATA
 205   CONTINUE
       FLAG(2) = 0
C      IF( IDATA.GE.32768.AND.IOLD.LT.
C    *32768.AND.INEW.LT.32768)    FLAG(2)=1
C      IF( IDATA.LT.32768.AND.IOLD.GE.
C    *32768.AND.INEW.GE.32768)    FLAG(2)=1
       FLAG(3) = 0
       IF(IDATA.EQ.0)  FLAG(3) = 1
       FLAG(4) = 0
C      IF(INEW.GE.32768)      FLAG(4)=1
       IER = 0
       RETURN
C######################################################################
C----------------------------------------------------------------------
C              KOMAH„A  "BITB"
C----------------------------------------------------------------------
300    CONTINUE
C--> PA‡OœEM OEPAH„ ˆCTO—HˆK O CTEEHŸM „BO‰Kˆ
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
C--> PA‡OœEM OEPAH„ PˆEMHˆK O CTEEHŸM „BO‰Kˆ
       CALL ADECDB( IOLD, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C----------------------------------------------------------------------
       ISUM = 0
       DO 310 I=1,8
       KB(I) = KB(I) * KB1(I)
       ISUM  = KB(I) + ISUM
310    CONTINUE
C--> OC—ˆTAEM BPEMŸ
       TB = 4.4E-6
       T = TB + TI + TO
C--> ‡A‰MEMCŸ Pˆ‡HAKAMˆ.
       FLAG(2) = 0
       FLAG(3) = 0
       IF( ISUM .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = KB(8)
       IER = 0
       RETURN
C######################################################################
C______________________________________________________________________
C                    KOMAH„A "BICB"
C______________________________________________________________________
400    CONTINUE
C-->  PA‡OœEM OEPAH„ ˆCTO—HˆK O CTEEHŸM „BO‰Kˆ
       CALL  ADECDB(IDATA,KB,IER)
       IF(IER.NE.0)  RETURN
C-->  PA‡OœEM OEPAH„ PˆEMHˆK O CTEEHŸM „BO‰Kˆ
       CALL ADECDB(IOLD,KB1,IER)
       IF( IER. NE. 0 )  RETURN
C----------------------------------------------------------------------
       ISUM = 0
       DO 410  I=1,8
       IF( KB(I) .EQ. 1 ) KB1(I) = 0
       ISUM = ISUM + KB1(I)
410    CONTINUE
C-->   PEOPA‡“EM PE‡“‹œTAT B „ECŸTˆ—HOM Bˆ„E
       CALL ADECBD(IDATA,KB1,IER)
C-->  ‡AˆC›BAEM PE‡“‹œTAT
       GO TO 100
C######################################################################
C______________________________________________________________________
C                    KOMAH„A "BISB"
C______________________________________________________________________
500    CONTINUE
C-->  PA‡OœEM OEPAH„ ˆCTO—HˆK O CTEEHŸM „BO‰Kˆ
       CALL  ADECDB(IDATA,KB,IER)
       IF(IER.NE.0)  RETURN
C-->  PA‡OœEM OEPAH„ PˆEMHˆK O CTEEHŸM „BO‰Kˆ
       CALL ADECDB(IOLD,KB1,IER)
       IF( IER. NE. 0 )  RETURN
C----------------------------------------------------------------------
       ISUM = 0
       DO 510  I=1,8
       IF( KB(I). EQ. 1 ) KB1(I) = 1
       ISUM = ISUM + KB1(I)
510    CONTINUE
C-->   PEOPA‡“EM PE‡“‹œTAT B „ECŸTˆ—HOM Bˆ„E
       CALL ADECBD(IDATA,KB1,IER)
C-->  ‡AˆC›BAEM PE‡“‹œTAT
       GO TO 100
C######################################################################
C----------------------------------------------------------------------
9903   CONTINUE
       IER = 3
       RETURN
C----------------------------------------------------------------------
9904   CONTINUE
       IER = 4
       RETURN
C----------------------------------------------------------------------
       END
