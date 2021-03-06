         SUBROUTINE FM0013( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  FM0013( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E��:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  667      ��. K��� ( PE�E )         KL
C
C
C      MO�E�� KO��P�ETC�:
C
C      � # T 667 � "K" "M" � MAS < T(1) T(2) T(3) ... T(KT) >;
C
C   �P� �TOM K��� BK���EH -->
C
C      T(J)  <=  TN  <=  T(J+1)    �P� J HE �ETHOE
C                                  ****************
C
C
C      !!! �P� KT=0 ( T.E. �P� " MAS <  >;" ) K��� BCE��A �AKP�T !!!
C
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E�� "K���",
C   -----
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW( �P� FGMSW = 0 )
C                  LM - ���HA BCE�O MSW O��CAH��     ( �P� FGMSW = 1 )
C
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3.  BCE BPEMEHA KO��P��TC� B CEK�H�AX.
C
C
C
C
C                           K���
C                           BK���.
C                            I
C       *********       **********               *********
C       *********       **********               *********
C       *********       **********               *********
C  T0*--*********-------**********---------------*********---------*--->
C       T(1)    T(2)    T(3)     T(4)            T(J)    T(J+1)    T(KT)
C
C
C            K��� �AKP�T ( B�K�. ) -->
C            -----------
C            TN < T(1)
C            TN > T(KT)  --> KT �ETHOE;
C
C            K��� OTKP�T ( BK�. ) -->
C            -----------
C            TN >=T(KT) --> KT HE �ETHOE;
C            T(J) <= TN <= T(J+1) --> J HE �ETHOE;
C
C
C
C                  4. H��E �P�BO��TC� �OPMAT� RMSW:
C
C                  RMSW T��A # 667 [ ��. K���     ]
C                  --------------------------------
C                                                      LM<O��>=15+KT
C
C                                                      LM<�OK>= 5+KT
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
C      KT -  KO���ECTBO TO�EK O�PE�E�EH�� "KAPT�" BK���EH�� KO���A;
C
C      K��� OTKP�T -> KD=1;    K��� �AKP�T ->  KD=0;
C      HX - �A� �O BPEMEH�, BPEM� BO�MO�HO�O �EPEK���EH��;
C
C        MCIU � HXCIU - �APAMETP� "BOCTAHOB�EHHO�O" �M����CA BK���EH��;
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
500      FORMAT(' *** �O�. C�O� B FM0013: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1      CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  O��A� ���HA
       KT = IFIX( PAR0T(1) )
       LM = 15 + KT
       RETURN
C----------------------------------------------------------------------
1000   CONTINUE
       KT = IFIX( PAR0T(1) )
       LM = 5 + KT
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C-->  �A��C� KT-->
       RMSW(5) = RKT
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( KT .LE. 0 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  �A��C� T(J) -->
       DO 3 I=1,KT
       RMSW(5+I) = PAR0T(1+I)
3      CONTINUE
       RETURN
       END
