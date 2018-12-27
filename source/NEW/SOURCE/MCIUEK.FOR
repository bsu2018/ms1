       INTEGER FUNCTION MCIUEK( TN, H0, HX, F1, F2, TS, DTAU )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA_”“HK–ˆŸ:  MCIUEK( TN, H0, HX, F1, F2, TS, DTAU )
C   --------------------
C   HA‡HA—EHˆE:  "ƒEHEPATOP" - "BEPˆ”ˆKATOP" ˆM“‹œCOB “PAB‹EHˆŸ Cˆ“:
C   ----------
C               BAPˆAHT "ECTECTBEHHAŸ KOMM“TA–ˆŸ" ( EK ):
C               -----------------------------------------
C
C  “PAB‹EHˆE ‡A„AETCŸ:
C  -------------------
C
C  F1(T) - OOPHOE HAPŸ†EHˆE ( MO„“‹ˆP“EMOE ) -> BHE˜HŸŸ ”“HK–ˆŸ;
C  F2(T) - HAPŸ†EHˆE “PAB‹EHˆŸ ( MO„“‹ˆP“ž™EE ) -> BHE˜. ”“H-Ÿ;
C               OO‡HA—EHˆŸ:
C               ------------
C   DF12(T) :=: F1(T) - F2(T)
C   MCIUEK := (‡HA—EHˆE ”“HK–ˆˆ) --> "‹AH ˆM“‹œCA “PAB‹EHˆŸ:
C
C            = 00 -->  HA [TN, TN+H0] ˆM“‹œCA HET;
C            = 01 -->  HA [TN, TN+H0] ˆM“‹œC OŸBˆ‹CŸ;
C            = 11 -->  HA [TN, TN+H0] ˆM“‹œC HE ˆC—E‡A‹;
C            = 10 -->  HA [TN, TN+H0] ˆM“‹œC ˆC—E‡;
C
C   ( M1,M2 ) -->  B A‹ƒOPˆTME COOTBETCTB“žT TˆM „AHH›M;
C
C   DTAU - „‹ˆTE‹œHOCTœ ˆM“‹œCA;
C   TN -  BPEMŸ HA—A‹A N-‰ TO—Kˆ;
C   H0 - ˜Aƒ, OPE„E‹Ÿž™ˆ‰ C‹E„. BPEMEHH“ž TO—K“;
C   TN1 = TN + H0;
C   HX - ˜Aƒ OT TN  „O  TX MOMEHTA BPEMEHˆ, ƒ„E
C   TX - BPEMŸ CMEH› COCTOŸHˆŸ ˆM“‹œCA;
C   TS - MOMEHT BPEMEHˆ O—EPE„HOƒO "‡A“CKA" ˆM“‹œCA Cˆ“;
C
C !!!!! Pˆ EPBOM OPA™EHˆˆ K MCIUEK --> ‡A„ATœ TS = -2*DTAU !!!!!!!!!!
C
C   METO„:  KOHTPO‹œ ‡A“CK/CHŸTˆE ˆM“‹œCA Cˆ“ O C‹E„. A‹ƒOPˆTM“:
C   -----   A) Pˆ DF12(TN) <= 0.0 ˆ DF12(TN+H0) >= 0.0
C              Cˆ“ ‡A“CKAETCŸ, TS ”ˆKCˆP“ETCŸ, HX OPE„E‹ŸETCŸ;
C           B) Pˆ (TN+H0) > (TS+DTAU) --> ˆM“‹œC CHˆMAETCŸ,
C              HX OPE„E‹ŸETCŸ;
C   APAMETP›: TN - BPEMŸ OPE„E‹EHˆŸ CˆƒHA‹A;
C   ---------  H0 - OKPECHOCTœ OPE„E‹EHˆŸ CˆƒHA‹A;
C              HX - ˜Aƒ OT TN „O TX - BPEMEHˆ POXO†„EHˆŸ O—EPE„HOƒO
C                   ”POHTA ˆM“‹œCA;
C              F1, F2 - BHE˜HˆE ”“HK–ˆˆ OOPHOƒO ˆ “PAB‹Ÿž™EƒO
C                       CˆƒHA‹OB;
C              TS - PAHEEE OPE„E‹EHH›‰ MOMEHT BPEMEHˆ CTAPTA ˆM“‹œCA;
C              DTAU - ‡A„AHHAŸ „‹ˆTE‹œHOCTœ ˆM“‹œCA;
C
C
C  ‡HA—EHˆE ”“HK–ˆˆ:  MCIUEK = 00  --> "Cˆ“" HET B [TN, TN+H0];
C  ****************                     HX = 0.0;
C                     MCIUEK = 01  --> B [TN, TN+H0] -> EPE„Hˆ‰ ”POHT
C                                      ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹ OT
C                                      TN „O EPE„HEƒO ”POHTA "Cˆ“";
C                     MCIUEK = 10 -->  B [TN, TN+H0] -> ‡A„Hˆ‰ ”POHT
C                                      ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹
C                                      OT TN „O ‡A„HEƒO ”POHTA ˆM“‹œCA;
C                     MCIUEK = 11 -->  B [TN, TN+H0] "Cˆ“" BK‹ž—EH,
C                                      COCTOŸHˆE HE MEHŸ‹, HX=0.0;
C
C                !!!!!  Pˆ H0 > TIMP --->  BO†MO†HA OPEPŸ "Cˆ“" !!!!!
C                      PE„O‹OƒAETCŸ, —TO H0 < TIMP
C
C
C
C   O„POƒPAMM›:  F1  ˆ   F2  -  BHE˜HˆE, ‡A„AHH›E Pˆ B›‡OBE;
C   ------------
C   PˆME—AHˆE:
C   ----------
CZZZ-------------------------------------------------------------------
       EXTERNAL F1, F2
C-->  BPEMŸ KOH–A...
       TK = TS + DTAU
C-->  BPEMŸ C‹. TO—Kˆ
       TN1 = TN + H0
C-->  HX -->  CTAH„APTHO = 0.0
       HX = 0.0
C   A) ----------------------------------------------------------------
C-->  OCTPOEHˆE [M1, M2]-->
       M1 = 0
       IF ( TS .LE. TN  .AND.  TN .LE. TK ) GO TO 1
C   B) ----------------------------------------------------------------
C-->  AHA‹ˆ‡ ˆ‡MEHEHˆŸ ( M1=0 ), OˆCK M2 = ?
C--->  ‡A“CK CIU BO‡MO†EH ?
       DF12N = F1(TN) - F2(TN)
       DF12N1 = F1(TN1) - F2(TN1)
       IF ( DF12N .LE. 0.0  .AND. DF12N1 .GE. 0.0 ) GO TO 2
C---> COCTOŸHˆE HE ˆ‡MEHˆ‹OCœ, M2 = 0, ‡A“CKA Cˆ“ HET...
       MCIUEK = 00
       RETURN
C======================================================================
2      CONTINUE
C-->  ‡A“CK Cˆ“ ECTœ(M1=0, M2=1), KOƒ„A ?
       HX = -H0 * ( DF12N / (DF12N1 - DF12N) )
C-->  TS HOBOE...
       TS = TN + HX
       MCIUEK = 01
       RETURN
C======================================================================
1      CONTINUE
C--> M1=1, HA‰Tˆ M2 = ?
       IF ( TN1 .LE. TK ) GO TO 3
C-->  ˆM“‹œC CHˆMAETCŸ...  HA [TN, TN+H0], KOƒ„A  ?
       HX = TK - TN
       MCIUEK = 10
       RETURN
C======================================================================
3      CONTINUE
C-->  M1  ˆ   M2  = 1, ˆ‡MEHEHˆ‰ HET, ˆM“‹œC O„AH...
       MCIUEK = 11
       RETURN
       END
