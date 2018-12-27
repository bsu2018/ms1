         SUBROUTINE FM0013( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  FM0013( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹ˆ:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  667      ˆ„. K‹ž— ( PE‹E )         KL
C
C
C      MO„E‹œ KO„ˆP“ETCŸ:
C
C       # T 667 “ "K" "M"  MAS < T(1) T(2) T(3) ... T(KT) >;
C
C   Pˆ TOM K‹ž— BK‹ž—EH -->
C
C      T(J)  <=  TN  <=  T(J+1)    Pˆ J HE —ETHOE
C                                  ****************
C
C
C      !!! Pˆ KT=0 ( T.E. Pˆ " MAS <  >;" ) K‹ž— BCEƒ„A ‡AKP›T !!!
C
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹ˆ.
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ "K‹ž—",
C   -----
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW( Pˆ FGMSW = 0 )
C                  LM - „‹ˆHA BCEƒO MSW OˆCAHˆŸ     ( Pˆ FGMSW = 1 )
C
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ B MˆHˆMA‹œHOM O"EME;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3.  BCE BPEMEHA KO„ˆP“žTCŸ B CEK“H„AX.
C
C
C
C
C                           K‹ž—
C                           BK‹ž—.
C                            I
C       *********       **********               *********
C       *********       **********               *********
C       *********       **********               *********
C  T0*--*********-------**********---------------*********---------*--->
C       T(1)    T(2)    T(3)     T(4)            T(J)    T(J+1)    T(KT)
C
C
C            K‹ž— ‡AKP›T ( B›K‹. ) -->
C            -----------
C            TN < T(1)
C            TN > T(KT)  --> KT —ETHOE;
C
C            K‹ž— OTKP›T ( BK‹. ) -->
C            -----------
C            TN >=T(KT) --> KT HE —ETHOE;
C            T(J) <= TN <= T(J+1) --> J HE —ETHOE;
C
C
C
C                  4. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C                  RMSW TˆA # 667 [ ˆ„. K‹ž—     ]
C                  --------------------------------
C                                                      LM<O™>=15+KT
C
C                                                      LM<‹OK>= 5+KT
C                    SPM                      SRAB
C +--+--+----+----+----+------+------+---+----------+
C IKDIHXI MM I HX I KT I T(1) I T(2) I...I T(KT)    I
C I  I  I CIUI CIUI    I      I      I   I          I
C I  I  I    I    I    I      I      I   I          I
C +--+--+----+----+----+------+------+---+----------+
C I1 I2 I 3  I 4  I 5  I  6   I  7   I...I 5 + KT   I
C +--+--+----+----+----+------+------+---+----------+
C I11I12I 13 I 14 I 15 I  16  I 17   I...I 15 + KT  I
C +--+--+----+----+----+------+------+---+----------+
C
C      KT -  KO‹ˆ—ECTBO TO—EK OPE„E‹EHˆŸ "KAPT›" BK‹ž—EHˆŸ KO‹ž—A;
C
C      K‹ž— OTKP›T -> KD=1;    K‹ž— ‡AKP›T ->  KD=0;
C      HX - ˜Aƒ O BPEMEHˆ, BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ;
C
C        MCIU ˆ HXCIU - APAMETP› "BOCTAHOB‹EHHOƒO" ˆM“‹œCA BK‹ž—EHˆŸ;
C                            ( CM. MCIU01 );
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         EQUIVALENCE ( RKT, KT )
         EQUIVALENCE ( FGMSW, MASPRT(87) )
C
         INTEGER FGMSW
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 667 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0013: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1      CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  O™AŸ „‹ˆHA
       KT = IFIX( PAR0T(1) )
       LM = 15 + KT
       RETURN
C----------------------------------------------------------------------
1000   CONTINUE
       KT = IFIX( PAR0T(1) )
       LM = 5 + KT
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C-->  ‡AˆCœ KT-->
       RMSW(5) = RKT
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( KT .LE. 0 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  ‡AˆCœ T(J) -->
       DO 3 I=1,KT
       RMSW(5+I) = PAR0T(1+I)
3      CONTINUE
       RETURN
       END
