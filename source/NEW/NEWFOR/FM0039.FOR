       SUBROUTINE FM0039(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   ������� 1992 �.)-----------
C   �O��PO�PAMMA: FM0039(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�� ���������� ����. ���������������:
C   ----------
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0025, LM000? )
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
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E��
C   -----
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW �P� FGMSW = 0;
C                  LM - ���HA BCE�O MSW O��CAH��      �P� FGMSW = 1;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
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
C    13      I     4        I  DM  I  SIGOLD 
C    14      I     5        I  DM  I  GAMMA  
C    15      I     6        I  DM  I  TOKOLD 
C    16      I     7        I      I         
C    17      I     8        I      I         
C    18      I     9        I      I         
C    19      I     10       I      I         ������
C    20      I     11       I      I         
C    21      I     12       I      I         
C    22      I     13       I      I         
C    23      I     14       I  FM  I  #��1-        
C    24      I     15       I  FM  I  #��1       
C    25      I     16       I  FM  I  #�����1 - ��ࠢ�塞� ⮪      
C    26      I     17       I  FM  I  OMEG - frequenc;
C    27      I     18       I  FM  I  EDM  - ������㤠 �����. ����.;
C    28      I     19       I  LM  I  E    - ��⨢����
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP 
       IF ( NTIP .EQ. 351                   ) GO TO 100
500    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0039 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
100   CONTINUE
C----------------------------------------------------------------------
C--->  ( T��  # 351 )
C----------------------------------------------------------------------
C--> O�PE�E�EH�E KOHCTAHT
       NT = IFIX ( PAR0T(9) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 31 + 2*NT
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 22 + 2*NT
       DO 102 I=1,LM
102    RMSW(I) = 0.0
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NT1 = (IFIX ( PAR0T(8) ) - 1 ) / 2
       IF ( NT .EQ. NT1 ) GO TO 103
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O���KA B FM0039 -->HE�PAB���HO �A�AHO KO���ECTBO TO
     =�EK ***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
103    CONTINUE
C      NELBX = IFIX ( PAR0T(1) )
C      IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  �A�OMH�M OMEG EDM E XG X
        N = 1
       RMSW(2) = RN
       RMSW(3) = 0.011667
       RMSW(4) = 3.14
       RMSW(17)= PAR0T(1)
       RMSW(18)= PAR0T(2)
       RMSW(19)= PAR0T(3)
       RMSW(20)= PAR0T(4)
       RMSW(21)= PAR0T(5)
        N      = IFIX(PAR0T(6))
       rmsw(14)= RN
           N   = IFIX( PAR0T(7) )
       RMSW(15) = RN    
C---->  �A�OMH�M ��C�O TO�EK �EPE�ATO�HO� XAPAKTEP�CT�K� ---------
       N = NT
       RMSW(22) = RN
       IF( NT.EQ. 0 ) GO TO 105
C---->  �A�OMH�M KOOP��HAT� TO�EK �EPE�ATO�HO� XAPAKTEP�CT�K� ---------
          DO 104 I = 1 , NT
          RMSW(22+I)   = PAR0T( 8 + 2*I )
          RMSW(22 + NT+I)= PAR0T( 9 + 2*I )
104    CONTINUE
105    CONTINUE
C----> �POBEP�M,�PAB��HO �� BBE�EH� �AHH�E ?
       DO 106 I=2,NT
       IF ( RMSW(21+I) .LE. RMSW(22+I) ) GO TO 106
C----------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** O���KA B FM0039-->OP��HAT� �A�AH� HE B BO�PACA��EM
     =�OP��KE ***'/' *** HOMEP ��EMEHTA',I5,' HOMEP T��A',I5,' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
106    CONTINUE
       RETURN
       END
