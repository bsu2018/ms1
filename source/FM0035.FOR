       SUBROUTINE FM0035(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (AT OCTOBER   1989 �.)-----------
C   �O��PO�PAMMA: FM0035(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�� ��E�OKOM�EHCATOPA ,
C   ----------   �OCTPOEHHO�O KAK C�CTEMA C PAC�PE�E�EHH�M� �APAMETPAM�
C                CO��AHA C�HTET��ECKA� CXEMA
C                ******************************************************
C                �P��HAK TIP # 1 -> ��ETP��. MAKPOMO�E��.
C                ( DM0039,LM0014 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C
C
C
C
C                  BBO� �O O��CAH��:
C
C           � # T 800 � BX B�X 0  � #�� #CM L S M K Y K�M D E ;
C
C           ��E :
C
C        #�� - HOMEP ��EMEHTA ,B�XO�HO� E�O C��HA�  �B��ETC� BXO�H�M
C               HA�P��EH�EM �K
CC       #CM - HOMEP B�X. C��HA� ��EMEHTA #��   ( ���ET B BO��TAX )
C          L - ���HA ��E�O-�A���                ( METP )
C          S - �O�EPE�HA� ��O�A��               ( M**2 )
C          M - MACCA ��E�O-�A���                ( K�   )
CC         K - K�HEMAT��ECKA� B��KOCT�          ( M**2 / C )
C          Y - MO���� �H�A                      (�A )
C        K�M - KO���. ��EKTPOMEXAH. CB���
C          D - ��E�O-MO����                     ( K� / H )
C          E - OTHOC�TE��HA� ����EKTP��ECKA� �POH��AEMOCT�
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E��;
C   -----
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW;
C
C   �O��PO�PAMM�:  1.COMMON PROF1  , CONDUC
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� HE �PO��BO��TC�;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C
C                  RMSW T��A  # 800
C                  --------------------
C
C
C  �OCT.  I �EPEMEHHA� RMSW-->
C +----------+----+---+----+---+----+---+------+------+---+---+---+...>
C I ����I    I    I   I RI I   I    I   I      I      I   I   I   I...>
C I     I G1 I #��I#CMI    I GLI GC I KOI  KJP I G(20)IG11IG12IG22I...>
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
C ...>I G MAT-  I PE�EPB I      I
C ...>I   P��A  I        I ATOKMI
C ...>I BC�OMO�.I        I      I
C ...>+---------+--------+------+
C ...>I   16    I   1    I   1  I
C ...>+---------+--------+------+
C ...>I 43 - 58 I  59    I  60  I
C ...>+---------+--------+------+
C     I 32 - 47 I  48    I  49  I
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 60
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 49
C
C   BH�MAH�E           ��C�O ��E�O-�A�� CTPO�O PABHO  198
C   ----------
C               ��� KA��O� ��E�O-�A���  ��C�O �BEH�EB PABHO 1 (M)
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
501    FORMAT(/' *** �O�. C�O� B FM0035 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
       RETURN
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->       ��E�OKOM�EHCATOP, C�CTEMA C PAC�PE�E�EHH�M� �APAMETPAM�
C --->
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 49
       DO 10 I=1,LM
10     RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> �POBEP�M HA �O�O��TE��HOCT�
       DO 15 I =1,10
       IF (PAR0T(I).LT.0.0) GO TO 190
15     CONTINUE
C---> BBE�EM �AHH�E
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
C--> KO��. ��P��OCT�
       KY = S0*Y/ L0
       KYD = KY/ (1 - KEM**2)
C--> CKOPOCT� �B�KA B �K
       CZB = (SQRT( KY/ M))*L0
       CZBD = CZB/ SQRT(1 - KEM**2)
C--> �O�OHH�E �APAMETP� �K
       RP  = L0/ (M*K)
       LP = 1./ (KYD*L0)
       CP  = M/ L0
C--> �APAMETP� KA��O�O �BEHA C�TET��ECKO� CXEM�
       L1 = L0
       RI = RP*L1
       LI = LP*L1
       CI = CP*L1
C--> KO��. �P�MO�O � O�PATHO�O ��E�O���EKTA
       KP = S0*Y*D/ L0
       KO = KP
C-->  ��EKTP��ECKA� �OCTO�HHA�
       E0 = 8.85E-12
C-->  ��EKTP��ECKA� EMKOCT�
       C0 = E*E0*(1 - KEM**2)*S0/ L0
C!!!-------------------------------------------------------------------
C  B���C���TC� �POBO��MOCT� KA��O�O �BEHA CC
C   ######### >  C��A TOKA B K��OH��TOH !!!!!!!!!!!
C----------------------------------------------------------------------
       GL = (H0*RI + 2.*LI)*(1.E-3)/( 2.*LI*RI)
       GC = 2.E-3*CI/ H0
       WRITE (6,*) GL ,GC,H0
       G1 = H0*(1.E-3)/ (2.*LI)
       KO = KO*1.E-3
       KJP = KP*KO/ C0
C--> �A�OMH�M..
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
C  B���C��EM �POBO��MOCT� �O CTE�EH�M �BO�K� 198= 2**7+2**6+2**2+2**1
C----------------------------------------------------------------------
       G(1) = GL + GC/2.
       G(2) = -GL
       DO 30 I =3,15,2
       G(I+1) = - (G(I-1)**2)/ (2.*G(I-2))
       G(I)   = G(I-2) + G(I + 1)
30     CONTINUE
CCCC   WRITE (6,*) G(1),G(2),G(3),G(4),G(5),G(6)
C--> �A�OMH�M.....
       DO 40 I =1,20
       RMSW(I + 8) = G(I)
40     CONTINUE
C--> O��A� �POBO��MOCT�
       G11 = G(15)
       G22 = G11
       G12 = G(16)
       CALL CONDUC(G11,G12,G22,G(13),G(14),RMSW(32),RMSW(33))
       CALL CONDUC(G11,G12,G22,G(5),G(6),RMSW(34),RMSW(35))
       CALL CONDUC(G11,G12,G22,G(3),G(4),RMSW(36),RMSW(37))
       G11 = G11 - GC/2.
       G22 = G22 + GC/2.
C--> �A�OMH�M..
       RMSW(29) = G11
       RMSW(30) = G12
       RMSW(31) = G22
C--> O�H���EM MACC�B� ��OKA PIEZO
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
C-->  METO�OM PA��O�EH�� XO�E�KO�O PA��O��M MATP��� G = D*DT
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
551    FORMAT(/' *** O���KA B FM0035 --> �APAMETP HE �O�O��TE�EH   ***'/
     =         ' *** ��EMEHT  ',   I5,'      HOMEP T��A ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
       RETURN
CCCCC  DEBUG INIT
       END
