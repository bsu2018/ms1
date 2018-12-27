       SUBROUTINE FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA  FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HAáHAóEHàE:  1) HACTPOâKA MAKPOMOÑEãà ùãEKTPàóECKOâ  MAòàHõ 3X3X0:
C   ----------      ***************************************************
C                  Tàè # 11  --> "Pìó. BAP.", MB(_T_)
C                  Tàè # 110 --> "Pìó. BAP.", MB(_OMEÉA_)
C                  Tàè # 14  --> "Pìó. BAP.", MB(_T_) - M_FT( *#ù* )
C                  Tàè # 140 --> "Pìó. BAP.", MB(_OMEÉA_) - M_FT( *#ù*)
C                  Tàè # 12  --> "REDUCE."  , MB(_T_)
C                  Tàè # 120 --> "REDUCE."  , MB(_OMEÉA_)
C                  ****************************************************
C                            Ñãü TàèOB # 11 , # 12 à # 14:
C                            -----------------------------
C                   M_BHEò(T) - áAÑAETCü KìCOóHO-ãàHEâHOâ îìHKñàEâ
C                               óEPEá MACCàB MAS < T1 Y1 .... >
C                                ( CM. è/è KMAS02 );
C
C                      Ñãü TàèOB # 110 , # 120 à # 140:
C                     ---------------------------------
C              M_BHEò(_OMEGA_) áAÑAETCü KOùî. èOãàHOMA:
C              MAS < A0 A1 A2 ...>;
C
C                      MB := A0 + A1 * OMEGA + A2 * OMEGA**2 + ....
C
C
C                      Ñãü TàèOB 14 à 140 àá OèPEÑEãEHHOÉO M_BH
C                      ------------------     BõóàTAETCü M_FT #ù-TA;
C
C
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
C              RMSW Tàè # 11, 110, 12, 120, 14, 140:
C              **************************************
C
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PEáEPB I"D0X"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 3,4,5  I Ñãü I      I      I       I       I       I       I     I
C  I 1,2 ÑãüIMEX. I 9-Tú I 9-Tú I 36-Tú I 36-Tú I 6-Tú  I 6-Tú  I NMASI
C  IT-13,130IìP-ü I CãOB I CãOB I CãOB  I CãOB  I CãOB  I CãOB  I CãOBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I  1 - 5 I  6  I7 - 15I16-24 I25-60  I61-96  I97-102 I103-108I109-@I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I ****** I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - OèPEÑEãüETCü PAáMEPOM MAS <...> ==> ...+ NMAS + 1
C              ** - 1-> HOMEP #ù, 2--> AÑPEC MFT ==> Ñãü T. 14 à 140
C
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
       REAL LS, MS, LR, MR, MSR, JMOM, LS1, MS1, LR1, MR1, MSR1,
     =      LMSP, LMSM, LMRM, LMRP, MSR2D3
       REAL MSSSS0, MFFFF0, MSSFF0, LVSS0, LVFF0
C----------------------------------------------------------------------
       INTEGER D1,D2,D3,D4,D5,D6,D7,D8
       INTEGER DR1,DR2,DR3,DR4,DR5,DR6,DR7,DR8
C
       DATA D1, D2, D3, D4, D5, D6,  D7,  D8
     =  /   21, 27, 36, 45, 81, 117, 123, 129 /
C
C--->  D@ -> HOMEPA èOãEâ B MSW_ALL MACCàBA;
       DATA DR1,  DR2,  DR3,  DR4,  DR5,  DR6,  DR7,  DR8
     =  /   1,    7,    16,   25,   61,   97,  103,   109 /
C
C--->  DR@ -> HOMEPA èOãEâ B "OTPEáKE" èEPEMEHHOâ óACTà RMSW;
C
C      ÑãàHA MSW_ALL ==> D8 + NMAS + 1; (+1 - HA ÑãàHì TMAS );
C
C      ÑãàHA "èEPEMEHHOâ" óACTà ==> DR8 + NMAS + 1;
C----------------------------------------------------------------------
       DATA KALL/6/, GGRAV / 9.81 /, SQRT3/1.732050808/
C
       TIP = BA0T(2)
         NEL = BA0T(1)
         IF ( TIP .EQ. 11 .OR. TIP .EQ. 110  .OR. TIP .EQ. 12
     =        .OR.  TIP .EQ. 120 .OR. TIP .EQ. 14  .OR. TIP .EQ. 140 )
     =      GO TO 99999
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
       IF ( TIP .EQ. 11  .OR.  TIP .EQ. 110 )
     = NMAS = IFIX( PAR0T(10) ) + 1
C             *********************
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     =   NMAS = IFIX ( PAR0T(9) ) + 1
       IF ( TIP .EQ. 14  .OR.  TIP .EQ. 140 )
     = NMAS = IFIX( PAR0T(11) ) + 1
C             *********************
       LM = D8 + NMAS
       RETURN
C######################################################################
1000   CONTINUE
       TIP = BA0T(2)
C-->  îOPMàPOBATú RMSW....
C-->  LM - ÑãàHA "èEPEMEHHOâ" óACTà RMSW...
       IF ( TIP .EQ. 11  .OR. TIP .EQ. 110 )
     =   NMAS = IFIX ( PAR0T(10) ) + 1
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     =   NMAS = IFIX ( PAR0T(9) ) + 1
       IF ( TIP .EQ. 14  .OR. TIP .EQ. 140 )
     =   NMAS = IFIX ( PAR0T(11) ) + 1
C***********************************
C
       LM = DR8 + NMAS
C-> óàCTKA
       DO 1 I=1,LM
       RMSW(I) = 0.0
1      CONTINUE
C
       IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 ) GO TO 1001
C
       IF ( TIP .EQ. 14  .OR.  TIP .EQ. 140 ) GO TO 1013
C
C
13130     CONTINUE
C    P ì ó H O â  B A P à A H T..............
C-->  èPàEM èOCTOüHHõX èAPAMETPOB...
C--------------------------------------------------------------------
          RS     = PAR0T(1)
C-
          RR     = PAR0T(2)
C-
          LS     = PAR0T(3)
          MS     = PAR0T(4)
          LR     = PAR0T(5)
          MR     = PAR0T(6)
          MSR    = PAR0T(7)
          PTAU   = PAR0T(8)
          JMOM   = PAR0T(9) / ( 4.0 * GGRAV )
C--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
C  *** BOáMOÜEH KOHTPOãú, èOKA HET   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C       NWTR = MASPRT(13)
C!!!-------------------------------------------------------------------
C--->  áAèOãHEHàE PARD....(DR2 - HAóAãO )
C----------------------------------------------------------------------
       RMSW(DR2 +0) = RS
       RMSW(DR2 +1) = RR
       RMSW(DR2 +2) = LS
       RMSW(DR2 +3) = MS
       RMSW(DR2 +4) = LR
       RMSW(DR2 +5) = MR
       RMSW(DR2 +6) = MSR
       RMSW(DR2 +7) = PTAU
       RMSW(DR2 +8) = JMOM
C----------------------------------------------------------------------
C--> áAèOãHEHàE PARM---> KOùî. OÅPATHOâ MATPàñõ L(GAMMA):
C -->  COÉãACHO "PìóHOÉO" BAPàAHTA !!!!!!!!
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C****** ùãEMEHTõ OÅPATHOâ ******
       LMSP = LS + MS
       LMSM = LS - MS
       LMRP = LR + MR
       LMRM = LR - MR
       MSR2D3 = 3.0 * MSR * MSR
C------->  OèPEÑEãàTEãú....
       ZLM = 4.0 * LMSM * LMRM  -  3.0 *MSR2D3
       LS1 = ( 4.0 * LMSP * LMRM  -  MSR2D3 ) / (ZLM*(LS+2.0*MS))
       MS1 =-( 4.0 *  MS  * LMRM  +  MSR2D3 ) / (ZLM*(LS+2.0*MS))
       LR1 = ( 4.0 * LMRP * LMSM  - MSR2D3  ) / (ZLM*(LR+2.0*MR))
       MR1 =-( 4.0 *  MR  * LMSM  + MSR2D3  ) / (ZLM*(LR+2.0*MR))
       MSR1 = -4.0 * MSR / ZLM
C--->  MACCàB PARM......
       RMSW(DR3+0) = LS1
       RMSW(DR3+1) = MS1
       RMSW(DR3+2) = LR1
       RMSW(DR3+3) = MR1
       RMSW(DR3+4) = MSR1
       RMSW(DR3+5) = LMSP
       RMSW(DR3+6) = LMSM
       RMSW(DR3+7) = MSR2D3
       RMSW(DR3+8) = PTAU / JMOM
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( TIP .EQ. 14  .OR. TIP .EQ. 140 ) RETURN
C      ---------------------------------------------
C--->  áAèOãHEHàE TMAS--->  MAS <....>
       DO 2 I=1, NMAS
       RMSW(DR8-1 + I ) = PAR0T(10-1 + I )
2      CONTINUE
       RETURN
1001   CONTINUE
C
C-->  èPàEM èOCTOüHHõX èAPAMETPOB...   " R E D U C E ..."
C--------------------------------------------------------------------
          MSSSS0 = PAR0T(5)
          MFFFF0 = PAR0T(6)
          LVSS0  = PAR0T(3)
          LVFF0  = PAR0T(4)
          RSS0   = PAR0T(1)
          RFF0   = PAR0T(2)
          PTAU   = PAR0T(7)
          JMOM   = PAR0T(8) / ( 4.0 * GGRAV )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         MSSFF0 = SQRT( MSSSS0 * MFFFF0 )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
C  *** BOáMOÜEH KOHTPOãú, èOKA HET   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        NWTR = MASPRT(13)
       WRITE(NWTR,100) BA0T(1),  RSS0, RFF0, LVSS0, LVFF0,
     =                 MSSSS0, MFFFF0, MSSFF0, PTAU, JMOM
 100    FORMAT(/ ' *** ùãEMEHT # ', I4, ' - ùãEKTPàóECKAü MAòàHA',
     = ' 3X_3X_0X ***'/
     = ' *** CèàCOK BBEÑEHHõX èAPAMETPOB OèàCAHàü MAKPOMOÑEãà: ***'/
     = ' *** ', ' R_SS=',G12.5,' R_FF=', G12.5,' LV_SS=' ,G12.5, '***'/
     = ' *** ', 'LV_FF=',G12.5,' M_SS=', G12.5,'  M_FF=',G12.5,' ***'/
     = ' *** ', ' M_SF=',G12.5,' P_TAU=',G11.4,' J_MOM=',G12.5,' ***'/)
C
C
C
C!!!-------------------------------------------------------------------
C--->  áAèOãHEHàE PARD....(DR2 - HAóAãO )
C----------------------------------------------------------------------
       RMSW(DR2 +0) = RSS0
       RMSW(DR2 +1) = RFF0
       RMSW(DR2 +2) = LVSS0
       RMSW(DR2 +3) = LVFF0
       RMSW(DR2 +4) = MSSSS0
       RMSW(DR2 +5) = MFFFF0
       RMSW(DR2 +6) = MSSFF0
       RMSW(DR2 +7) = PTAU
       RMSW(DR2 +8) = JMOM
C----------------------------------------------------------------------
C--> áAèOãHEHàE PARM---> KOùî. OÅPATHOâ MATPàñõ L(GAMMA):
C----------------------------------------------------------------------
C-->  OèPEÑEãàTEãú:
      DEN      =4 /(LVFF0*LVSS0*(9 *MFFFF0**2*LVSS0**2+12 *MFFFF0*LVFF0
     X *LVSS0**2+18 *MSSFF0**2*LVFF0*LVSS0+9 *MSSSS0**2*LVFF0**2+12 *
     X MSSSS0*LVFF0**2*LVSS0+4 *LVFF0**2*LVSS0**2))
      RMSW(DR3)=(  DEN    *LVFF0*(9 *MFFFF0**2*LVSS0**2+12 *MFFFF0*
     X LVFF0*LVSS0**2+12 *MSSFF0**2*LVFF0*LVSS0+3 *MSSSS0**2*LVFF0**2+
     X 8 *MSSSS0*LVFF0**2*LVSS0+4 *LVFF0**2*LVSS0**2))/4
      RMSW(DR3+1)=(  DEN    *LVFF0**2*(3 *MSSFF0**2*LVSS0+3 *MSSSS0**2*
     X LVFF0+2 *MSSSS0*LVFF0*LVSS0))/4
      RMSW(DR3+2)=(DEN*LVSS0*(3 *MFFFF0**2*LVSS0**2+8 *MFFFF0*LVFF0
     X *LVSS0**2+12 *MSSFF0**2*LVFF0*LVSS0+9 *MSSSS0**2*LVFF0**2+12 *
     X MSSSS0*LVFF0**2*LVSS0+4 *LVFF0**2*LVSS0**2))/4
      RMSW(DR3+3)=(DEN*LVSS0**2*(3 *MFFFF0**2*LVSS0+2 *MFFFF0*LVFF0
     X *LVSS0+3 *MSSFF0**2*LVFF0))/4
      RMSW(DR3+4)=(DEN*MSSFF0*LVFF0*LVSS0*(-3 *MFFFF0*LVSS0-3 *
     X MSSSS0*LVFF0-2 *LVFF0*LVSS0))/2
      RMSW(DR3+5)=(DEN*MSSFF0*LVFF0*LVSS0*(3 *MFFFF0*LVSS0+3 *
     X MSSSS0*LVFF0+2 *LVFF0*LVSS0))/4
      RMSW(DR3+6)=(DEN *MSSFF0*LVFF0*LVSS0*SQRT3*(3 *MFFFF0*LVSS0+3
     X *MSSSS0*LVFF0+2 *LVFF0*LVSS0))/4
      RMSW(DR3+7)=(DEN*MSSFF0*LVFF0*LVSS0*SQRT3*(-3 *MFFFF0*LVSS0-
     X 3 *MSSSS0*LVFF0-2 *LVFF0*LVSS0))/4
      RMSW(DR3+8 )=PTAU/JMOM
C********************************************************************
C     #_13:  STATOR_3, ROTOR_3, DEMPF_0  G * 31.01.86 *
C*********************************************************************
C--->  áAèOãHEHàE TMAS--->  MAS <....>
       DO 20 I=1, NMAS
       RMSW(DR8-1 + I ) = PAR0T(09-1 + I )
20     CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C             NWTRE = MASPRT(14)
C           WRITE(NWTRE,667) DEN
C667      FORMAT(10X,' DEN=', G12.5 )
C       WRITE(NWTRE, 666 )   ( I,RMSW(I), I=1,LM)
C666     FORMAT(1X, 5( 'R(',I3,')=',G12.5, 1X ) )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       RETURN
C----------------------------------------------------------------------
1013   CONTINUE
C--->  áAèOãHEHàE TMAS--->  MAS <....>
       DO 30 I=1, NMAS
       RMSW(DR8-1 + I ) = PAR0T(11-1 + I )
30     CONTINUE
C****              HOMEP ùã-TA CBüáà....
            N = PAR0T(10)
       RMSW ( 1)  =  RN
C-->  ÑAãEE TAK ÜE KAK Ñãü 11 àãà 110
       GO TO 13130
C......................................................................
       END
