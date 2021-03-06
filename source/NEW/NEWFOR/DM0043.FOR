         SUBROUTINE DM0043 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT �������   1992 �.)------------
C   �O��PO�PAMMA:  DM0043 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C
C                  A) ZAPROS = 1
C                  -------------
C                    C�POC ��A�A FQ - BE����H� ��P. �APAM. O�PE�E�EH�;
C                  === H� �E�O HE PAC��T�BAEM ==
C
C                  B) ZAPROS = 2
C                     ----------
C                      == B C���AE H��EBO�O �HA�EH�� ��A�A FQ -->
C                  B���C�EH�E
C
C
C              FQ - PAC�ET ��P. HA�P��EH�� B��O�HEH -->
C                  MEXAH��M �TO�O ��A�A �O�BO��ET C�HXPOH���POBAT�
C                  B��O�HEH�� PA����H�X DM_MOH�TOPOB C �.T��A # 4;
C
C                  C) ZAPROS = 3
C                     **********
C                     HE �O��EP��BAETC� ( PE�EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE �O��EP��BAETC�;
C
C----------------------------------------------------------------------
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0043, LM000? )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C
C                      ���HA:  T�� 351:==>+1<VAR>
C----------------------------------------------------------------------
C              ��EMEHT O��C�BAETC�:
C
C      � # T 351 � 0 �  OMEG EDM E XG XD #��1 #��1
C              MAS < NT  �1  I1  T2  I2 ... >
C
C      ��E
C          OMEG - ����� ����饩 ��                      
C                  
C          EDM    -   ������㤠 ���筨��� ����殮���
C          E      -   ��⨢����               ;
C          XG         ����⨢��� ᮯ�⨢����� �� ��஭� ��६������ ⮪�
C          X      -   ����⨢��� ᮯ�⨢����� �� ��஭� ����ﭭ��� ⮪�
C          NT     -   ������⢮ �祪
C          T1 I1  -   COOTBETCTBEHHO ������ �६��� � ���祭�� ⮪�
C          � T.�.     � ��� ������       
C          ------------------------------------------------------------
C
C----------------------------------------------------------------------
C
C   METO�:     O�PE�E�EH�E B�XO�H�X C��HA�OB ��HE�H�X ��HAM��ECK�X
C   -----      �BEH�EB B COOTBETCTB�� C �X �EPE�ATO�H�M� ��HK���M�.
C
C   �APAMETP�:     MSW, RMSW - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------      ( "OTPE�OK" MACC�B� MSWALL -> C.M. �/� GETMSW );
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          B T = TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          B T = TN + H0;
C                  TOKN  �  TOKN1  -  �HA�EH�� "TOKOB" �P� T :=
C                                     TN  �  TN + H0;
C                                   PA�MEPHOCT� CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 5;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C                   ( =1 �P� �O���O� �OK. �O�PE�HOCT� );
C                 MSWALL( (RMSWAL ) - BEC� C��COK MSW_ALL,
C                 ADRMSW - A�PEC TEK. C�OAB MSW ( RMSW );
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C
C
C          "�OCTO�HHA�" �ACT� MSW T��OB # 320,321,322,323,325:
C           --------------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C                      �OPMAT RMSW T�� # 351
C                 ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I  NMIN - HOMEP ����A��E� MEH��E�
C    12      I     3        I  DM  I  TIMEU - �६� �뤠� ᨣ���� ��.
C    13      I     4        I  DM  I  ALFOLD 
C    14      I     5        I  DM  I  GAMMA  
C    15      I     6        I  DM  I  TOKOLD 
C    16      I     7        I      I  R       
C    17      I     8        I      I  BETAOLD -        
C    18      I     9        I      I  UN       
C    19      I     10       I      I         ������
C    20      I     11       I      I  #��2       
C    21      I     12       I      I  #��2       
C    22      I     13       I      I  #�����2       
C    23      I     14       I  FM  I  #��1-        
C    24      I     15       I  FM  I  #��1       
C    25      I     16       I  FM  I  #�����1 - ��ࠢ�塞� ⮪      
C    26      I     17       I  FM  I  OMEG - frequenc;
C    27      I     18       I  FM  I  EDM  - ������㤠 �����. ����.;
C    28      I     19       I  LM  I  TIMEPER
C    29      I     20       I  FM  I  XG  - ����⨢��� ᮯ�⨢.
C    30      I     21       I  FM  I  X   - ����⨢��� ᮯ�⨢.
C    31      I     22       I  FM  I  NT  - ������⢮ ���祭��
C    32      I     23       I  FM  I  T1 
C    33      I     24       I  FM  I  T2 
C    31+2*nt I    22+2*NT   I  FM  I  I(NT/2)
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 31 + 2*NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 22+2*NT
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C     **********************************
C     TOKN/N1(1) - �HA�EH�E B�XO�HO�O C��HA�A;
c
c     DM0043 ��� �ࠢ����� ⮪�� �१ ����⨢����� (LC//R - ����㧪�)
c
c
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(100)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER, TIP, ZAPROS 
         DATA PI / 3.1415292653 /         
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       NTIP = TIP 
       IF ( NTIP .EQ. 351                   ) GO TO 9991
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0025 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9991   CONTINUE
C--->  B��OP PE��MA �A�POCA..
6666   GOTO ( 100, 200) , ZAPROS
C----------------------------------------------------------------------
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C==> C�POC ��A�A       OCHOBHO� PAC�ET B��O�HEH;
       MSW(10) = 0
       RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-----ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  H��HO �� PA�OTAT� ?
       IF ( MSW(10) .EQ. 1 ) RETURN
C----------------------------------------------------------------------
C-->  �P��ETC� .............................
C-->  ZAPROS = 2 --> PAC��TAT� �HA�EH��....
C######################################################################
       ITOK = MSW(7)
C----------------------------------------------------------------------
C-->   ������� ��� ���� ��ਮ���� ( T�� 351 )
C----------------------------------------------------------------------
C  ���� �� ���᫨�� ���� ᨣ��� �ࠢ����� ?
       NT   = MSW(31)        
       do  300 i = 1,nt
       if(tn .LT. rmsw( 31 + i) ) goto 400
300    continue       
       uy = rmsw( 31 + 2*nt)
       goto 500
400    continue
       uy = rmsw( 31 + nt + i -1 )
500    continue              
       TOKN1(ITOK) = uy
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
       END
