       SUBROUTINE FM0035(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (AT OCTOBER   1989 ƒ.)-----------
C   O„POƒPAMMA: FM0035(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹ˆ œE‡OKOMEHCATOPA ,
C   ----------   OCTPOEHHOƒO KAK CˆCTEMA C PACPE„E‹EHH›Mˆ APAMETPAMˆ
C                CO‡„AHA CˆHTETˆ—ECKAŸ CXEMA
C                ******************************************************
C                Pˆ‡HAK TIP # 1 -> ‹ETPˆ—. MAKPOMO„E‹œ.
C                ( DM0039,LM0014 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C
C
C
C
C                  BBO„ O OˆCAHˆž:
C
C            # T 800 “ BX B›X 0   #‹ #CM L S M K Y KM D E ;
C
C           ƒ„E :
C
C        #‹ - HOMEP ‹EMEHTA ,B›XO„HO‰ EƒO CˆƒHA‹  ŸB‹ŸETCŸ BXO„H›M
C               HAPŸ†EHˆEM K
CC       #CM - HOMEP B›X. CˆƒHA‹ ‹EMEHTA #‹   ( “„ET B BO‹œTAX )
C          L - „‹ˆHA œE‡O-˜A‰›                ( METP )
C          S - OEPE—HAŸ ‹O™A„œ               ( M**2 )
C          M - MACCA œE‡O-˜A‰›                ( Kƒ   )
CC         K - KˆHEMATˆ—ECKAŸ BŸ‡KOCTœ          ( M**2 / C )
C          Y - MO„“‹œ žHƒA                      (A )
C        KM - KO””. ‹EKTPOMEXAH. CBŸ‡œ
C          D - œE‡O-MO„“‹œ                     ( K‹ / H )
C          E - OTHOCˆTE‹œHAŸ „ˆ‹EKTPˆ—ECKAŸ POHˆ–AEMOCTœ
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹ˆ.
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹ˆ;
C   -----
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW;
C
C   O„POƒPAMM›:  1.COMMON PROF1  , CONDUC
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HE POˆ‡BO„ˆTCŸ;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C
C                  RMSW TˆA  # 800
C                  --------------------
C
C
C  OCT.  I EPEMEHHAŸ RMSW-->
C +----------+----+---+----+---+----+---+------+------+---+---+---+...>
C I “‡‹›I    I    I   I RI I   I    I   I      I      I   I   I   I...>
C I     I G1 I #‹I#CMI    I GLI GC I KOI  KJP I G(20)IG11IG12IG22I...>
C I     I    I    I   I    I   I    I   I      I      I   I   I   I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I   3 I 1  I 1  I 1 I  1 I 1 I  1 I 1 I  1   I  20  I 1 I 1 I 1 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I 9-11I 12 I 13 I 14I 15 I16 I 17 I18 I 19   I 20-39I 40I41 I42 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I     I 1  I 2  I 3 I 4  I 5 I 6  I 7 I 8    I 9-28 I29 I30 I 31I
C
C
C
C ...>+---------+--------+------+
C ...>I G MAT-  I PE‡EPB I      I
C ...>I   Pˆ–A  I        I ATOKMI
C ...>I BCOMOƒ.I        I      I
C ...>+---------+--------+------+
C ...>I   16    I   1    I   1  I
C ...>+---------+--------+------+
C ...>I 43 - 58 I  59    I  60  I
C ...>+---------+--------+------+
C     I 32 - 47 I  48    I  49  I
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 60
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 49
C
C   BHˆMAHˆE           —ˆC‹O œE‡O-˜A‰ CTPOƒO PABHO  198
C   ----------
C               „‹Ÿ KA†„O‰ œE‡O-˜A‰›  —ˆC‹O ‡BEHœEB PABHO 1 (M)
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PIEZO/ TNL(200),TNC(200),UN0(200),UN1(200),
     * TJNL(200),TJNC(200),D11(200),D21(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       DIMENSION G(20)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(40) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW, ADPRTI,ADPRTO, ADPROG
       REAL K, KEM, M, L0, LP, LI, L1, KY, KYD, KO, KP,KJP
C!!!-------------------------------------------------------------------
C!!!-------------------------------------------------------------------
       NEL = BA0T(1)
       TIP = BA0T(2)
       IF (  TIP .EQ. 800 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0035 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
       RETURN
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->       œE‡OKOMEHCATOP, CˆCTEMA C PACPE„E‹EHH›Mˆ APAMETPAMˆ
C --->
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 49
       DO 10 I=1,LM
10     RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> POBEPˆM HA O‹O†ˆTE‹œHOCTœ
       DO 15 I =1,10
       IF (PAR0T(I).LT.0.0) GO TO 190
15     CONTINUE
C---> BBE„EM „AHH›E
       N  =  IFIX( PAR0T(1))
       NCM =  IFIX( PAR0T(2))
       L0 =  PAR0T(3)
       S0 =  PAR0T(4)
       M  =  PAR0T(5)
       K  =  PAR0T(6)
       Y  =  PAR0T(7)
       KEM=  PAR0T(8)
       D  =  PAR0T(9)
       E  =  PAR0T(10)
C----------------------------------------------------------------------
C--> KO”. “P“ƒOCTˆ
       KY = S0*Y/ L0
       KYD = KY/ (1 - KEM**2)
C--> CKOPOCTœ ‡B“KA B K
       CZB = (SQRT( KY/ M))*L0
       CZBD = CZB/ SQRT(1 - KEM**2)
C--> OƒOHH›E APAMETP› K
       RP  = L0/ (M*K)
       LP = 1./ (KYD*L0)
       CP  = M/ L0
C--> APAMETP› KA†„OƒO ‡BEHA CˆTETˆ—ECKO‰ CXEM›
       L1 = L0
       RI = RP*L1
       LI = LP*L1
       CI = CP*L1
C--> KO”. PŸMOƒO ˆ OPATHOƒO œE‡O””EKTA
       KP = S0*Y*D/ L0
       KO = KP
C-->  ‹EKTPˆ—ECKAŸ OCTOŸHHAŸ
       E0 = 8.85E-12
C-->  ‹EKTPˆ—ECKAŸ EMKOCTœ
       C0 = E*E0*(1 - KEM**2)*S0/ L0
C!!!-------------------------------------------------------------------
C  B›—ˆC‹ŸžTCŸ POBO„ˆMOCTˆ KA†„OƒO ‡BEHA CC
C   ######### >  Cˆ‹A TOKA B Kˆ‹OHœžTOH !!!!!!!!!!!
C----------------------------------------------------------------------
       GL = (H0*RI + 2.*LI)*(1.E-3)/( 2.*LI*RI)
       GC = 2.E-3*CI/ H0
       WRITE (6,*) GL ,GC,H0
       G1 = H0*(1.E-3)/ (2.*LI)
       KO = KO*1.E-3
       KJP = KP*KO/ C0
C--> ‡AOMHˆM..
       RMSW(1) = G1
       RMSW(2) = RN
       N = NCM
       RMSW(3) = RN
       RMSW(4) = RI
       RMSW(5) = GL
       RMSW(6) = GC
       RMSW(7) = KO
       RMSW(8) = KJP
C!!!-------------------------------------------------------------------
       DO 20 I =1,20
       G(I) = 0.
20     CONTINUE
C!!!-------------------------------------------------------------------
C  B›—ˆC‹ŸEM POBO„ˆMOCTœ O CTEEHŸM „BO‰Kˆ 198= 2**7+2**6+2**2+2**1
C----------------------------------------------------------------------
       G(1) = GL + GC/2.
       G(2) = -GL
       DO 30 I =3,15,2
       G(I+1) = - (G(I-1)**2)/ (2.*G(I-2))
       G(I)   = G(I-2) + G(I + 1)
30     CONTINUE
CCCC   WRITE (6,*) G(1),G(2),G(3),G(4),G(5),G(6)
C--> ‡AOMHˆM.....
       DO 40 I =1,20
       RMSW(I + 8) = G(I)
40     CONTINUE
C--> O™AŸ POBO„ˆMOCTœ
       G11 = G(15)
       G22 = G11
       G12 = G(16)
       CALL CONDUC(G11,G12,G22,G(13),G(14),RMSW(32),RMSW(33))
       CALL CONDUC(G11,G12,G22,G(5),G(6),RMSW(34),RMSW(35))
       CALL CONDUC(G11,G12,G22,G(3),G(4),RMSW(36),RMSW(37))
       G11 = G11 - GC/2.
       G22 = G22 + GC/2.
C--> ‡AOMHˆM..
       RMSW(29) = G11
       RMSW(30) = G12
       RMSW(31) = G22
C--> OH“‹ŸEM MACCˆB› ‹OKA PIEZO
       DO 60 I = 1,200
       TNL(I) = 0.
       TNC(I) = 0.
       UN0(I) = 0.
       UN1(I) = 0.
       TJNL(I) = 0.
       TJNC(I) = 0.
       D11(I)  = 0.
       D21(I)  = 0.
60     CONTINUE
C----------------------------------------------------------------------
C-->  METO„OM PA‡‹O†EHˆŸ XO‹E–KOƒO PA‡‹O†ˆM MATPˆ–“ G = D*DT
C----------------------------------------------------------------------
       D11(2) = SQRT(2*GL + GC)
       DO 50 I = 3,198
       D21(I-1)= - GL/ D11(I-1)
       D11(I)  = SQRT(D11(2)**2 - D21(I-1)**2)
C      WRITE (6,*) D21(I-1),D11(I)
50     CONTINUE
C----------------------------------------------------------------------
CCCC   WRITE (6,*) RMSW(15),RMSW(3)
       RETURN
190    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 551 ) NEL, TIP
551    FORMAT(/' *** O˜ˆKA B FM0035 --> APAMETP HE O‹O†ˆTE‹EH   ***'/
     =         ' *** ‹EMEHT  ',   I5,'      HOMEP TˆA ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
       RETURN
CCCCC  DEBUG INIT
       END
