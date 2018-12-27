       SUBROUTINE MCIU50( TN, H0, UOPN, UOPN1, UYPN, UYPN1,
     *                    DTCIU, DTMCIU, MSW50, RMSW50 )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: MCIU50( TN, H0, UOPN, UOPN1, UYPN, UYPN1,
C                         DTCIU, DTMCIU, MSW50, RMSW50 )
C   HA‡HA—EHˆE: "ƒEHEPATOP CˆƒHA‹OB Cˆ“" O A‹ƒOPˆTM“
C   -----------  "BEPTˆKA‹œHOƒO “PAB‹EHˆŸ" C “—ETOM ‡APETA-PA‡PE˜EHˆŸ
C                 O„A—ˆ CˆƒHA‹OB "Cˆ“" ( OT 5.08.86 );
C
C   METO„:    CPABHEHˆE „B“X CˆƒHA‹OB, ”ˆKCA–ˆŸ EPEXO„A ˆX
C   -----      "PA‡HOCTˆ" —EPE‡ "HO‹œ" OT OTPˆ–ATE‹œHOƒO ‡HA—EHˆŸ;
C              O TˆM „AHH›M -> B›—ˆC‹EHˆE TS - BPEMEHˆ CTAPTA,
C              OCTA‹œHOE O—EBˆ„HO;
C
C      !!! "Cˆ“" CTAPT“žT B TO—KE EPECE—EHˆŸ U_“PAB‹EHˆŸ C
C                         "HˆCA„Až–ˆMˆ" “—ACTKAMˆ U_OOPHOE.
C      !!!
C
C
C   APAMETP›-OO‡HA—EHˆŸ:
C   ----------------------
C              UOPN, UOPN1 - OOPH›E HAPŸ†EHˆŸ,
C              UYPN, UYPN1 - “PAB‹Ÿž–ˆE HAPŸ†EHˆŸ,
C                              B T=TN ˆ T=TN+H0
C              TN - BPEMŸ N-ƒO ˜AƒA, H0 - ˜Aƒ ˆHTEƒP.
C              DTCIU - „‹ˆTE‹œHOCTœ ˆM“‹œCA;
C              DIMCIU - MˆHˆMA‹œHAŸ „‹ˆTE‹œHOCTœ ˆM“‹œCA,
C                       KOTOP“ž "—“BCTB“žT" BETˆ‹ˆ;
C              MSW50 (RMSW50) - MACCˆBA OˆCAHˆŸ BEHTˆ‹Ÿ:
C              *************
C
C
C       ”OPMAT MSWV_EHTˆ‹Ÿ „‹Ÿ DM00B9:
C       -------------------------------
C
C      C‹OBO                   ‡HA—EHˆE
C
C      1       KD - KO„ COCTOŸHˆŸ: =0 - ‡AKP›T, =1 - OTKP›T;
C      2       HX - ˜Aƒ OT TN, BO‡MO†HOE BPEMŸ EPEK‹ž—EHˆŸ;
C
C ( B [*] HOMEPA MSW_VEHTˆ‹Ÿ „‹Ÿ / "MCIU50" )
C
C  [1] 3       TS - BPEMŸ CTAPTA ˆM“‹œCA( "E" KOMM“TA–ˆŸ );
C  [2] 4       TK - BPEMŸ ”ˆHˆ˜A ˆM“‹œCA( "E" KOMM“TA–ˆŸ );
C  [3] 5       CX - KO„ PA‡PE˜EHˆŸ/‡APETA "Cˆ“":
C                   =0 - Cˆ“ ‡APE™EHO O„ABATœ HA BEHTˆ‹œ,
C                   =1 - Cˆ“ PA‡PE˜EHO O„ABATœ HA BEHTˆ‹œ;
C [4] 6        TX - BPEMŸ, C KOTOPOƒO „E‰CTB“ET KO„ "CX";
C
C
C [5] 7        MCIU - KO„ ˆM“‹œCA --->
C                     MCIU   = 00  --> "Cˆ“" HET B [TN, TN+H0];
C                                       HXCIU = 0.0;
C                     MCIU   = 01  --> B [TN, TN+H0] -> EPE„Hˆ‰ ”POHT
C                                   ˆM“‹œCA "Cˆ“", HXCIU = ˆHTEPBA‹ OT
C                                      TN „O EPE„HEƒO ”POHTA "Cˆ“";
C                     MCIU   = 10 -->  B [TN, TN+H0] -> ‡A„Hˆ‰ ”POHT
C                                   ˆM“‹œCA "Cˆ“", HXCIU = ˆHTEPBA‹
C                                      OT TN „O ‡A„HEƒO ”POHTA ˆM“‹œCA;
C                     MCIU   = 11 -->  B [TN, TN+H0] "Cˆ“" BK‹ž—EH,
C                                      COCTOŸHˆE HE MEHŸ‹, HXCIU=0.0;
C
C
C [6] 8        HXCIU - ˜Aƒ OT TN, „O O—EPE„HOƒO ”POHTA "Cˆ“";
C (---)
C     9              - ”‹Aƒ KD COCTOŸHˆŸ
C    10              - PE‡EPB
C********************************************************************
C
C   O„POƒPAMM›:   HET;
C   ------------
C   PˆME—AHˆE:   1.  H0  „O‹†EH ›Tœ MEHœ˜E DTCIU;
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION MSW50(*), RMSW50(*)
       INTEGER CX
C
       TX = RMSW50(4)
       CX = MSW50(3)
C
C-->  "ECTECTBEHH›‰" CTAPT.., ECTœ ‹ˆ OH B [TN, TN+H0 ] ?
       TN1 = TN+H0
       DUN = UOPN - UYPN
       DUN1 = UOPN1 - UYPN1
C-->  TS - O—EPE„HO‰ HE "XO—ET" ??
       IF ( DUN.LT.0.0 .AND. DUN1.GE.0.0 .AND. UOPN.GT.UOPN1 )  GO TO 1
       IF ( DUN.GT.0.0 .AND. DUN1.LE.0.0 .AND. UOPN.GT.UOPN1 )  GO TO 1
C-->  O—EPE„HOƒO CTAPTA HE›‹O, PE„O‹OƒAEM TS ˆ TK OPE„E‹EHH›Mˆ
C  PA‡OP CX, TX APAMETPOB
       GO TO 2
1      CONTINUE
C-->  "E" CTAPT HOBOƒO ˆM.
       HXCIUE = -( DUN / ( DUN1 - DUN ) ) * H0
       TSE = TN + HXCIUE
       TKE = TSE + DTCIU
C-->  ”ˆKCˆP“EM TO..
       RMSW50(1) = TSE
       RMSW50(2) = TKE
2      CONTINUE
C-->  OC‹E„Hˆ‰ B›„AHH›‰ ˆM. "Cˆ“" -->
       TSE = RMSW50(1)
       TKE = RMSW50(2)
C-->  TS  ˆ  TK -- ˆM. “PAB‹EHˆŸ C “—ETOM CX,TX
C-->  CX=0 -> ‡APET, CX=1 - PA‡PE˜EHˆE HA "Cˆ“", TX - KOƒ„A
C
       IF ( CX .EQ. 1 ) GO TO 3
C-->  „E‰CTB“ET ‡APET...
C-->  ˆ‡MEHˆM, EC‹ˆ HA„O  BPEMEHA CTAPTA ˆ‹ˆ ”ˆHˆ˜A...
C-->  E„O‹.  ˆM. HET..
       TS = -DTCIU
       TK = - DTCIU
       IF ( TX .LT. TSE ) GO TO 100
C-->  PE„. Cˆ“ ECTœ...
       TS = TSE
       TK = TKE
       IF ( TKE .LT. TX ) GO TO 100
C-->  Bˆ„ˆMO Cˆ“  OP›BAETCŸ O CPE„ˆHE ˆM“‹œCA
C     TSE <= TX <= TKE
       TS = TSE
       TK = TX
       GO TO 100
C----------------------------------------------------------------------
3      CONTINUE
C-->  Cˆ“  PA‡PE˜ˆ‹ˆ...
C  PE„  Cˆ“ ECTœ
       TS = TSE
       TK = TKE
       IF ( TX .LE. TSE ) GO TO 100
C-->  PE„. Cˆ“ HET
       TS = -DTCIU
       TK = -DTCIU
       IF ( TKE .LE. TX ) GO TO 100
C-->  Bˆ„ˆMO PA‡PE˜ˆ‹ˆ "O CPE„ˆHE"
       TS = TX
       TK = TKE
C**********************************************************************
C**********************************************************************
100    CONTINUE
C-->  TS  ˆ TK  C “—ETOM  KODA CX  ˆ TX BPEMEHˆ
C-->  KAPTA Cˆ“--->
       M1 = 0
       M2 = 0
C      MCIU -->  M1.M2
       IF ( TS .LE. TN   .AND.  TN  .LT. TK )   M1 = 1
       IF ( TS .LE. TN1  .AND.  TN1 .LE. TK )   M2 = 1
C-->  EC‹ˆ M1=0 ˆ M2=0 -->  Cˆ“ HET.....
       IF ( M1 .EQ. 0  .AND.  M2 .EQ. 0 ) GO TO 200
C-->  M1 ˆ M2  =1 Cˆ“  HE KOH—AET!!
       IF ( M1 .EQ. 1  .AND.  M2 .EQ. 1 ) GO TO 201
C-->  „O. M1 = 1 (A M2=0 )  ->  ‡A„Hˆ‰ ”POHT..
       IF ( M1 .EQ. 1 ) GO TO 202
C--->  HE TAK,  M1=0, A M2 = 1       -->  EPE„Hˆ‰ ”POHT
C -->  „‹ˆT. Cˆ“, KAKAŸ O‹“—ˆ‹ACœ..
       DTXXX = TK - TS
C-->  MO†ET "Cˆ“" O—EHœ KOPOTKˆ‰ ?
       IF ( DTXXX .LE. DTMCIU ) GO TO 200
C-->  Cˆ“ BEHTˆ‹œ O—“BCTB“ET..
       MCIU = 01
       HXCIU = TS - TN
       GO TO 300
C######################################################################
200    CONTINUE
C-->  M1=0, M2=0
       MCIU = 0
       HXCIU = 0
       GO TO 300
201    CONTINUE
C-->  M1, M2 = 1
       MCIU = 11
       HXCIU = 0.0
       GO TO 300
202    CONTINUE
C-->  M1 = 1, M2 = 0 -->  ‡A„OK..
       MCIU = 10
       HXCIU = TK - TN
C**********************************************************************
C**********************************************************************
300    CONTINUE
C-->  ”ˆKCˆP“EM
       RMSW50(6) = HXCIU
       MSW50(5) = MCIU
       RETURN
       END
