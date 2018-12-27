       SUBROUTINE FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA  FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HAáHAóEHàE:  1) HACTPOâKA MAKPOMOÑEãà ùãEKTPàóECKOâ  MAòàHõ 3X3X0:
C   ----------      ***************************************************
C                  Tàè # 11  --> "Pìó. BAP.", MB(_T_)
C                  ****************************************************
C                            Ñãü TàèOB # 11 à 110:
C                            ---------------------
C                  ùãEMEHT OèàCõBAETCü:
C           ù ?? T 11 ì #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                     è RS RF LS  MS  LR  MR  MSR  PTAU GD**2
C                       MAS < T1 Y1 ....   > ;
C
C                     [ "PìóHOâ" B A P à A H T ]
C
C
C                            Ñãü TàèOB # 14 à 140:
C                            ---------------------
C                  ùãEMEHT OèàCõBAETCü:
C           ù ?? T 11 ì #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                     è RS RF LS  MS  LR  MR  MSR  PTAU GD**2  #ù
C                       MAS < T1 Y1 ....   > ;
C
C      #ù -- àá ùTOÉO ùãEMEHTA BáüTú M_FT à BõóECTú àá M_BHEò.
C                                           *******
C                     [ "PìóHOâ" B A P à A H T ]
C
C                            Ñãü TàèOB # 12 à 120:
C                            ---------------------
C                  ùãEMEHT OèàCõBAETCü:
C           ù ?? T 11 ì #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                     è RS RF LVS LVF MSS MFF     PTAU GD**2
C                       MAS < T1 Y1 ....   > ;
C
C@
C
C
C            2) èPà  FGMSW = 1 -> ìTOóHüETCü áHAóEHàE LM -
C               OÅôEâ ÑãàHõ MSW ÑAHHOÉO TàèA ( C ìóETOM MAS èAP-OB );
C
C   METOÑ:      îOPMàPOBAHàE èEPEMEHHOâ óACTà MSW_ALL OèàCAHàü ùã-TA,
C   -----       BõóàCãEHàE èO AHAãàTàóECKàM îOPMìãAM KOùî-OB OÅPATHõX
C               MATPàñ à ÑP. BCèOMOÉATEãúHõX ÑAHHõX, HEOÅXOÑàMõX Ñãü
C               ÑAãúHEâòEâ PAÅOTõ MAKPOMOÑEãà;
C                èEPEBOÑ B CàCTEMì Cà JMOM = GD**2/(4*GGRAV)
C
C   èAPAMETPõ:  BA0T, KNOT, PAR0T - "OTPEáKà" MACCàBOB BA0, KNOT, PAR0
C   ---------   BBEÑEHHõX óEPEá è/è INPUT0;
C               RMSW - èEPEMEHHAü óACTú MSW_ALL OèàCAHàü ùãEMEHTA;
C               LM - èPà FGMSW = 0 => ÑãàHA èEPEMEHHOâ óACTà MSW_ALL;
C                    èPà FGMSW = 1 => ÑãàHA BCEÉO MACCàBA    MSW_ALL;
C   èPàMEóAHàE: 1. BCE èOãü  áAHìãüûTCü
C   ----------  2. îOPMìãõ Ñãü MACCàBA PARM -> èOãìóEHõ óEPEá REDUCE;
C                  à B PìóHìû;
C               3. îOPMAT MSW TàèA # 11, 110, 12, 120:
C
C
C
C               "èOCTOüHHAü" óACTú MSW TàèA # 11:
C                ----------------------------------------------
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I èPàáHAKI KOãà I KOã. IAÑP.   IPE I
C  I ùãEME I TàèA   I è/è    I TàèA   I óECT I BõB. IHAó.   IáE I
C  I HTA   I MOÑEãà I PACóETAI MOÑEãà I BO   I áHAó I"TOKN" IPB I
C  I       I        I DM.....I       .I ìáãOBI EHàâ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C              @ - OèPEÑEãüETCü PAáMEPOM MAS <...> ==> ...+ NMAS + 1
C            *** - 22 - AÑPEC B TOKN/N1 áHAóEHàü MFT Ñãü #ù (Tàè 14,140)
C
C
C
C
C
C      PACòàîPOBKA MACCàBOB PARD à T.Ñ. CM. B è/è ME@N13 .....
C
C
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       INTEGER BA0T, FGMSW, TIP
       EQUIVALENCE ( FGMSW,  MASPRT(87) )
       EQUIVALENCE ( N    ,  RN         )
       REAL L0, LSD, LSQ, LF, LD, LQ, MF, MD, MQ, MR, R,
     =      RF, RD, RQ, OMEGA
C----------------------------------------------------------------------
       INTEGER D1
       INTEGER DR1
C
       DATA D1/   17  /
C
C--->  D@ -> HOMEPA èOãEâ B MSW_ALL MACCàBA;
       DATA DR1/   1  /
C
       DATA FK/1.2247448 /
C
       TIP = BA0T(2)
         NEL = BA0T(1)
         IF ( TIP .EQ. 11  )   GO TO 99999
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ãOÉ. CÅOâ B FM0008: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
99999  CONTINUE
C-->  óTO èPOCüT?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  ìTOóHàTú MSW_ALL PAáMEP
       LM = 110
       RETURN
C######################################################################
1000   CONTINUE
       TIP = BA0T(2)
C-->  îOPMàPOBATú RMSW....
C-->  LM - ÑãàHA "èEPEMEHHOâ" óACTà RMSW...
C
       LM = 95
C-> óàCTKA
       DO 1 I=1,LM
       RMSW(I) = 0.0
1      CONTINUE
C
C
13130     CONTINUE
C    P ì ó H O â  B A P à A H T..............
C-->  èPàEM èOCTOüHHõX èAPAMETPOB...
C--------------------------------------------------------------------
          L0     = PAR0T(1)
          LSD    = PAR0T(2)
          LSQ    = PAR0T(3)
          LF     = PAR0T(4)
          LD     = PAR0T(5)
          LQ     = PAR0T(6)
          MF     = PAR0T(7)
          MD     = PAR0T(8)
          MQ     = PAR0T(9)
          MR     = PAR0T(10)
          R      = PAR0T(11)
          RF     = PAR0T(12)
          RD     = PAR0T(13)
          RQ     = PAR0T(14)
          OMEGA  = PAR0T(15)
C--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
C  *** BOáMOÜEH KOHTPOãú, èOKA HET   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C       NWTR = MASPRT(13)
C!!!-------------------------------------------------------------------
C--->  áAèOãHEHàE PARD....(DR2 - HAóAãO )
C----------------------------------------------------------------------
       RMSW(DR1 +0)  =   L0
       RMSW(DR1 +1)  =   LSD
       RMSW(DR1 +2)  =   LSQ
       RMSW(DR1 +3)  =   LF
       RMSW(DR1 +4)  =   LD
       RMSW(DR1 +5)  =   LQ
       RMSW(DR1 +6)  =   MF
       RMSW(DR1 +7)  =   MD
       RMSW(DR1 +8)  =   MQ
       RMSW(DR1 +9)  =   MR
       RMSW(DR1 +10) =   R
       RMSW(DR1 +11) =   RF
       RMSW(DR1 +12) =   RD
       RMSW(DR1 +13) =   RQ
       RMSW(DR1 +14) =   OMEGA
C----------------------------------------------------------------------
       RETURN
C......................................................................
       END
