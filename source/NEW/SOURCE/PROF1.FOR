         BLOCK DATA
C====== HA�OP PROF1 ===================================================
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C
C                      BH�TPEHH�� CTP�KT�PA
C                   TA����� �KA�ATE�E� MASPRT.
C                   --------------------------
C                       (COMMON /PROF1/)
C+-------+---------+------------+-------------------------------------+
CI HOMEP I �C�O��- I �HA�EH�E   I  �O���ECKOE �HA�EH�E �KA�ATE��      I
CI ��E-  I ��EMOE  I(KTO �A�AET)I                                     I
CI MEH-  I O�O�HA- IU - USER    I  ( [* - A��APAT PA�OT� C "REGCPU" ) I
CI TA    I �EH�E   IS - PA�PA�. I                                     I
CI MAC-  I         IPGM - �PO�. I                                     I
CI C�BA  I         IU* - U B DR.I                                     I
C+-------+---------+------------+-------------------------------------+
CI  1    I KOD_BA0 I   PGM      I  KO� MACC�BA BA0 B REGION; [*       I
CI       I         I            I  (PA�MEPHOCT�:= NMBA0 * N1BA0)      I
CI  2    I NMBA0   I U(100)*S   I  MAX KO���ECTBO ��EMEHTOB B �E��;   I
CI  3    I NXBA0   I   PGM      I  KO���ECTBO ��EMEHTOB B �E��;       I
CI  4    I N1BA0   I  S(8)      I  KO�-BO C�OB MACC�BA BA0 ��� O��CA- I
CI       I         I            I          H�� O�HO�O ��EMEHTA;       I
CI  5    I KOD_KNOTI    PGM     I  KO� MACC�BA KNOT B REGCPU;  [*     I
CI  6    I NDKNOT  I  U(650)*S  I  PA�MEPHOCT� MACC�BA KNOT;          I
CI  7    I NMKNOT  I   PGM      I  KO�-BO ���OB B PAC��T�BAEMO� �E��  I
CI       I         I            I  ( HOMEP MAX ���A �E�� );           I
CI  8    I KOD_PAR0I   PGM      I  KO� MACC�BA PAR0 B REGION;  [*     I
CI  9    I NDPAR0  I U(500)*S   I  PA�MEPHOCT� MACC�BA PAR0;          I
CI  10   I MODMAX  I S(100)     I  KO�-BO PEA���OBAHH�X MAKPOMO�E�E�; I
CI  11   I FUNMAX  I S(0)       I  KO�-BO PEA���OBAHH�X ��HK��OHA��-  I
CI       I         I            I         H�X �AB�C�MOCTE� FUN(T);    I
CI  12   I  NRDR   I U(5)       I  HOMEP KAHA�A C�CTEMHO�O BBO�A;     I
CI  13   I  NWTR   I U(3)       I  HOMEP KAHA�A C�CTEMHO�O B�BO�A;    I
CI  14   I  NWTRE  I U(6)       I  HOMEP KAHA�A ABAP��H�X B�BO�OB;    I
CI  15   I  DIAL   I U(0)*      I  0 - �AKETHA� O�PA�OTKA;            I
CI       I         I            I  1 - �HTEPAKT�BH�� PE��M;           I
CI  16   I  NWTRD  I U(8)       I  HOMEP KAHA�A B�BO�A TBEP��X KO���; I
CI  17   I  N1MACT I S(8)       I  KO-BO C�OB MACC�BA MACTAB, HEO�XO- I
CI       I         I            I    ��M�X ��� O��CAH�� O�HO� MO�E��; I
CI  18   I  NSPOOL I U(9)       I  HOMEP KAHA�A �A��OB HAKO��EH��;    I
CI  19   I KOD_    I   PGM      I  KO� MACC�BA MSWALL B REGION; [*    I
CI       I  MSWALL I            I                                     I
CI  20   I DMSWAL  I S(2000)PGM I  PA�MEPHOCT� MSWALL;                I
CI  21   I  NVER   I S(' 1.5')  I  HOMEP BEPC�� KOM��EKCA ( 1A4 )     I
CI  22   I KOD_    I   PGM      I  KO� MACC�BA YALL B REGION  [*      I
CI       I   YALL  I            I   - MATP��A �POBO��MOCTE� YALL(*,*) I
CI  23   I  NDALL  I  S(50)PGM  I  ��HE�H�E "PA�MEPHOCT�" �P���� "ALL"I
CI  24   I KOD_    I   PGM      I  KO� MACC�BA JALL B REGION  [*      I
CI       I   JALL  I            I   - BEKTOP �CTO�H�KOB TOKA          I
CI  25   I KOD_    I   PGM      I  KO� MACC�BA VALLN  B REGION  [*    I
CI       I   VALLN I            I   - BEKTOP ���. HA�P��EH�� B T(N)   I
CI  26   I  NSYS   I   PGM      I  TEK��A� PA�MEPHOCT� PE�. C�CTEM�   I
CI  27   I  KOD_   I   PGM      I  KO� MACC�BA LOGYTR B REGION  [*    I
CI       I  LOGYTR I            I   - BEKTOP �P��HAKOB K.�. ���OB     I
CI  28   I  ZAPROS I   PGM      I     - �A�POC K �P���E DM.... HA     I
CI       I         I            I = 1   �OPM�POBAH�E YALL � JALL      I
CI       I         I            I = 2   B���C�EH�E "HOB�X" TOKOB � �P.I
CI       I         I            I = 3   AHA��� CMEH� COCTO�H�� T�P-OB.I
CI       I         I            I = 4   ��MEHEH�E COCTO�H�� BEHT��E�..I
CI       I         I            I = 10 � 20 - C�E�. B��OB ��� BEHT��E�I
CI  29   I  TN     I   PGM      I "TEK��EE" BPEM� HA�A�A HOBO�O �A�A  I
CI  30   I  H0     I   PGM      I "TEK����" �A� PAC�ETA [ 5 ��. �PA�.]I
CI  31   I  MU     I U(1/2)S    I  CONST METO�A ��HE��EPA-B�O�����    I
CI  32   I  H1     I H0*(1-MU)  I  HE �BH�� �A� METO�A �HTE�P�POBAH�� I
CI  33   I  H2     I   H0*MU    I  �BH�� �A� METO�A �HTE�P�POBAH��    I
CI  34   I  FKOM   I    PGM     I  ��A� HA����� KOMM�TA��� B TEK. �A�EI
CI  35   I  TX_G   I  PGM(-->   I  BPEM� ��MEHEH�� TO�O�O��� ��� COC- I
CI       I         I--> TN+H0 ) I   TO�H�� �AP-OB �E�� := TN + HXG;   I
CI  36   I  NTX_G  I  PGM       I  HOMEP �OC�E�HE�O �AKP�B�E�OC� BEHT.I
CI  37   I  FLOG   I  PGM       I  ��A� BK���EH�� A��APATA "LOGYTR".  I
CI  38   I  FSYS   I  PGM       I  ��A� "C�CTEMA C�OPM�POBAHA"(1-�A)  I
CI  39   I  EPSKOM I U(1E-6)    I  �HTEPBA� "PABH�E BPEMEHA" ���      I
CI       I         I            I   BPEMEH� KOMM�TA��� BEHT��E�;      I
CI  40   I  H0DATA I U(5 ��.�P.)I  �A�AHH�� HA�. �A� �HTE�P�POBAH��.  I
CI  41   I  T5MSW  I  PGM       I  "A�P" B MSW_ALL 1-O ��. �.T��A #5; I
CI  42   I   T0    I U(?)*      I  BPEM� HA�A�A PAC�ETA-MO�E��POBAH��.I
CI  43   I   TMAX  I U(?)*      I  BPEM� KOH�A  PAC�ETA-MO�E��POBAH��.I
CI  44   I  TOUT   I  PGM       I  BPEM� B�BO�A C�E�. TO�K� B NSPOOL;.I
CI  45   I  FREST  I  PGM       I  ��A� �A�POCA "�PO�O���T� PE�EH�E"; I
CI  46   I  NNTOK  I  PGM       I  KO�-BO �EPEMEHH�X, �P���� TOK...   I
CI  47   I  NTGRAF I  PGM       I  KO�-BO TO�EK �PA�. B�BO�A;         I
CI  48   I  HOUTT  I U(H0DATA)* I  �A� B�BO�A KO��MO�   �H�OPMA���.   I
CI  49   I  T6MSW  I  PGM       I  "A�P" B MSW_ALL 1-O ��. �.T��A #6; I
CI  50   I  ZEROCP I U(1E-32)   I  "MA��HH��" HO��...;                I
CI  51   I  KOD_   I  PGM       I  KO� MACC�BA VALLN1 B REGION;  [*   I
CI       I  VALLN1 I            I  -BEKTOP ���OB�X HA�P��EH�� B TN+H0;I
CI  52   I  KOD_   I  PGM       I  KO� MACC�BA TOKN1 B REGION;   [*   I
CI       I   TOKN  I            I  -BEKTOP TOKOB � C�. �EP. B TN;     I
CI  53   I  KOD_   I  PGM       I  KO� MACC�BA TOKN1 B REGION;   [*   I
CI       I   TOKN1 I            I  -BEKTOP TOKOB � C�. �EP. B TN + H0;I
CI  54   I  LPRTM  I  S(200)    I  ���HA B C�OBAX MACC�BA MASPRT;     I
CI  55   I  FEND   I  PGM       I  ��A� "�PO�ECC PAC�ETA �ABEP�EH";   I
CI  56   I  PON    I U(0)*      I  ��A� "�E�AT� ON-LINE" 1 - �A;      I
CI  57   I  OPEN6  I  PGM       I  KO�-BO �H���EHTH�X BETBE� B T = TN.I
CI  58   I  KNOTM6 I  PGM       I  HOMEP MIN ���A B �P���AX �H���EHT. I
CI  59   I  GVENT6 I U(1E-5)    I  �POBO��MOCT� "�AKP�TO�O" BEHT���,  I
CI       I         I            I  ��� "�ATA��H�X" B�PO��EH�� C�CTEM�;I
CI  60   I  FTEST  I U(0)       I  ��A� TECT�POBAH��;                 I
CI  61   I  HXG    I  PGM       I  PEA��HO B��O�HEHH�� �A�:= TXG - TN;I
CI  62   I  HXMIN  I U( 0 )     I  MIN �A� BPEMEH� BO�MO�HO� KOM�T-���I
CI       I         I            I  �P� = 0 -> "BBO�" BEHT. B HA�. HET!I
CI  63   I  SETKA  I U(90.0)*   I  CETKA �O BPEMEH� ��� "�PA�"(��.�P.)I
CI  64   I  SEQTRX I U(1)       I  ��A� "KOHTPO��" B�PO��EHHOCT� C�CT.I
CI  65   I  FITER  I  PGM       I  ��A� �A�POC HA �TEPA��� ��� #2 � #3I
CI  66   I  MITER  I U(10)      I  MAX KO�-BO �TEPA��� HA �A�E,       I
CI       I         I            I  �P� = 0, KOHTPO�� HE �POBO��TC�;   I
CI  67   I  EITER  I U(1E-3)    I  "�OKA��HA�" �O��CT�MA� O���KA->ITERI
CI  68   I  FRUNGE I U(0)       I  ��A� "TECT P�H�E"-> "1"/"2" ->TEST;I
CI  69   I  ERUNGE I U(1E-3)    I  "��O�A��HA�" TO�HOCT� PAC�ETA;     I
CI  70   I  NRUNGE I U(10)      I  MAX KO�-BO �O�. "�PO��EH��" H0DATA I
CI  71   I  SEQTIM I U(10)      I  MAX �O�. KO�-BO "C�POCOB" B TN.    I
CI  72   I  H0MIN  I U(1E-6)    I  "�O�PE�HOCT" CETK� B�BO�A PE����T. I
CI  73   I  FRCPU  I  S(0)      I  ��A� "A��APAT" REGCPU BK��EH;      I
CI  74   I  FMSWAL I  PGM       I  ��A� "MSWALL" C�OPM�POBAH;         I
CI  75   I  KOD_   I  PGM       I  KO� MACC�BA XRGAF B REGION [*;     I
CI       I  XGRAF  I            I  ( C����H� ��� B�B. �PA��KOB );     I
CI  76   I  NDX    IS(10000)PGM I  PA�MEPHOCT� C����H�A XGRAF;        I
CI  77   I  ICLOSE I U(1E-2)    I  TOK "�A��PAH��" BEHT��E�;          I
CI  78   I  UOPEN  I U(1E-2)    I  HA�P��EH�E "OTKP�T��" BEHT���;     I
CI  79   I  NLITER I   PGM      I  HOMEP ��-TA �.T��A 2 ��� 3, ��� K-OI
CI  80   I  MAXITR I   PGM      I  �A��KC�POBAHA MAXITR "�OK" O���KA; I
CI  81   I  H0OLD  I   PGM(0)   I  "CTAP��" TEK. �A� �HTE�. ��� FRUNGEI
CI  82   I  FRUN0  I   PGM      I  �A�POC K GETNEW ��� B��. T.P�H�E;  I
CI  83   I  NFILER I  U(4)      I  HOMEP KAHA�A ��� PA�. �A��A;       I
CI  84   I  NDIVH0 I   PGM(0)   I  KO�-BO "�PO��EH��" ��� H0DATA;     I
CI  85   I  KOD_   I   PGM      I  KO� PA�. MACC�BA (PA�-T� NSYS)     I
CI       I   WORKX I            I   B REGION                    [*    I
CI  86   I  PONRUN I  U(0)      I  ��A�  �E�AT� �POTOKO�A #TECT_P�H�E#I
CI  87   I  FGMSW  I   PGM      I  "�A�POC K FM@" - �TO�H�T� ���H� MSWI
CI  88   I  NSITER I   PGM      I  "CME�EH�E" ��� NLITER_A HOMEPA ��.;I
CI  89   I  NMAK   I   U(1)     I  HOMEP KAHA�A ��� "MAKPO_PAC��PEH��"I
CI  90   I  DTCHEK I   U(0.5)*  I  �A� �O TN CO��AH�� K.T. (B CEK.)   I
CI       I         I            I   - �O��HO ��T� KPATHOE HOUTT;      I
CI  91   I  FCHEK  I   PGM      I  ��A� �A�POCA HA ABTO_COXPAHEH�E    I
CI  92   I NZAPSP  I   PGM      I  HOMEP �OC�E�HE� �A��C� B NSPOOL;   I
CI  93   I T4MSW   I   PGM      I  "A�P." B MSW_ALL 1-O ��. �.T��A #4.I
CI  94   I DTMCIU  I  U(1 ��.�) I  MIN ���T. "C��", �A��CKA��E�O BEHT.I
CI  95   I NFCHEK  I  U(10)*    I  KAHA� ��� #COXP/#BOCT ->"�MO��AH�E"I
CI  96   I KKCHEK  I  PGM       I  �EPE� KK.. TO�EK B�BO�A �E�AT� K.T.I
CI  97   I NFACHK  I  U(11)*    I  KAHA� ��� #ACOXP -> ABTO_COXPAHEH�EI
CI  98   I FRESTE  I   PGM      I  ��A� #BOCT B��O�HEHO, BCE BOCT-HO  I
CI  99   I KXXX    I   PGM      I  C.�.K. ��� �ETEPM�H�POBAHH�X B�B.  I
CI 100   I FSTD    I U(50.0)*   I  OCHOBHA� �ACTOTA ��� #C�EKTP ( �� )I
CI 101   I DFFUR   I U(25.0)*   I  �A� C�EKTPA��H�X XAP-K ��� #C�EKTP I
CI 102   I F0FUR   I U(0)*      I  HA�A��HA� �ACTOTA ��� #C�EKTP (�� )I
CI 103   I FMFUR   I U(975)*    I  KOHE�HA�  �ACTOTA ��� #C�EKTP (�� )I
CI 104   I PPFUR   I U(0.5)     I  % ��ETA B(F) � C(F) KO��. B #C�EKTPI
CI 105   I IYSTAV  I U(1E-1)    I  TOK �CTABK� BEHT. �P���� "H��E"-3; I
CI 106   I PPVENT  I U(0)*      I  ��A� �E�AT� B MOMEHT KOMM�TA���;   I
CI 107   I METVET  I U(0)       I  # METO�A PAC�ETA TXG (HXG):        I
CI       I         I            I =0 - �HTEP�O����� �O "TN" <-> "TN1";I
CI       I         I            I =1 - �EPEC�ET �� "TN" C H0=HXG,     I
CI       I         I            I    ( HXMIN^=0 - MIN �A� PAC�ETA );  I
CI       I         I            I =2 - "C���AH�E" "TN1" C "TXG",      I
CI       I         I            I    (EPSKOM �P� �TOM = ABS(TX-TN1) );I
CI       I         I            I = > 2 - TO �E, �TO � =2, HO EPSKOM  I
CI       I         I            I         HE ��MEH�ETC�;              I
CI 108   I IPOL    I  U(0)      I ��A� METO�A �HTEP�O�����, =1 - C�E�.I
CI 109   I KODSYS  I  U(2)      I KO� METO�A PE�EH�� C�A� [Y]*[V]=[J]:I
CI       I         I            I =1 - �E� �TEP. �TO�HEH��;           I
CI       I         I            I =2 - C �TEP. �TO�HEH��M�->(+2 P.M.):I
CI 110   I EPSSYS  I  U(1E-6)   I TO�HOCT� PE�EH�� "C�A�" (KODSYS=2)  I
CI 111   I KOD_    I  PGM       I KO� PA�. MACC�BA B REGION     [*    I
CI       I  YALLHP I            I  KO��� YALL (KODSYS=2);             I
CI 112   I KOD_    I  PGM       I KO� PA�. MACC�BA B REGION     [*    I
CI       I  WORKY  I            I  PA�-T� NSYS (KODSYS=2);            I
CI 113   I KODMU   I  U(0)      I KO� "PA��OHA" �OC�E KOMM�TA���:     I
CI       I         I            I =0 - "HE�BH�� ���EP";               I
CI       I         I            I =1 - METO� HE MEH�ETC� ( �A�AH MU=);I
CI 114   I KODMUM  I  U(1)      I KO� "PA��OHA" TO��KO ��� ��. MA�.   I
CI 115   I T0OUT   I  U(0.0)    I ��稭�� � T0OUT ��࠭��� � SPOOL   I
CI 116   I H0OUTT  I  U(0.0)    I � T0OUT<>0.0 ��⠭���� HOUTT=H0OUTT I
CI 117   I H0DATX  I  PGM       I "���� 蠣 ����" ��᫥ T0OUT<>0. I
CI 118   I OUTFLG  I  PGM       I   ����७��� 䫠� ��� T0OUT-a       I
CI 119   I INTIM   I  U(0)      I   ��㦥��� 䫠� ��� ����� MSW     I
CI 120   I NSXEM   I  U(0)      I ����� ����� (��� ������ �SW � ����) I
CI 121   I FTRSPL  I  U(0)*     I ���� "����稥 䨫��� �� ���.SPOOL" I
CI 122   I NREGX   I  U(12)*    I ����� ������ ��� #������+#�������   I
CI 123   I NEXTR   I  U(13)     I ����� ������ ��� #���� ���-�� �祪 I
CI 124   I NEXTRD  I  U(14)     I ����� ������ ��� #���� ���祭��     I
CI 125   I JNAME   IU(MS1-1989) I ��� 䠩�� ࠡ���: JNAME.Kxx         I
CI 126   Ichar*8   I            I xx - ����� ������ �����/�뢮��      I
CI 127   I NZRDR   I    PGM(0)  I ����쪮 ࠧ �⠫� ���. �� NRDR     I
CI 128   I FLGDIG  I   PGM(0)   I =0 - #����:  = 1 - #�����           I
CI 129   I FLGCIR  I   PGM(0)   I =0 - #����:  = 1 - #�����           I
CI       I         I            I                                     I
CI       I         I            I                                     I
CI       I         I            I                                     I
C+-------+---------+------------+-------------------------------------+
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
C----------------------------------------------------------------------
         DATA MASPRT/ 200 * 0 /
C----------------------------------------------------------------------
       END
