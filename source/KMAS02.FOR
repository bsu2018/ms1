       REAL FUNCTION KMAS02( T, TMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA-��HK���  KMAS02( T, TMAS )
C   --------------------
C   HA�HA�EH�E:        B���C�EH�E �HA�EH�� F(T), �A�AHHO� TA����HO
C   ----------         �O O�OPH�M TO�KAM (T1,Y1), ...., (T_N,Y_N)
C                      �POBO�� K�CO�HO-��HE�H�� �HTEP�O�����;
C                      �P� T <= T1, BCE��A F(T) = Y1;
C                      �P� T >= T_N, BCE��A F(T) = Y_N;
C                      �O��CKAETC� �A�AH�E ���O�O KO���ECTBA TO�EK.
C                      �P� N=0 ��� N=1 - BCE��A F(T)=0.0
C
C                      *** KO��POBKA B O��CAH�� �APAMETPOB ***
C
C                      MAS < T1 Y1 ..... T_N Y_N >
C
C                      �P� MAS < 0 > ��� MAS < T_���OE >
C                      �P�H�MAETC� F(T) = 0.0
C
C   METO�:     K�CO�HO-��HE�HA� �HTEP�O�����;
C   -----      �BO��H�� �O�CK �HTEPBA�A;
C   �APAMETP�:  KMAS02 - �HA�EH�E F(T) �P�
C   ---------   T      - BPEM� O�PE�E�EH�� F(T);
C               TMAS - MACC�B �AHH�X ( PA�MEPHOCT�  N C�OB ):
C   MACC�B  --> N   T1 Y1   T2 Y2   T3 Y3   T4 Y4   T5 Y5  � T.�.
C   C�OBO # --> 1    2  3   4   5    6  7    8  9   10  11 � T.�.
C
C    B MACC�BE O��C�BAETC� [N/2] - TO�EK (T_K, Y_K), O�PE�E�����X
C                                   ��HK��� F(T);
C
C   �O��PO�PAMM�:      HET
C   ------------
C   �P�ME�AH�E:     1.  �P� K - HE �ETHOE, ���T�BAETC� TO��KO
C   ----------          �ETHOE �HA�EH�E K;
C                   2. �HTEPBA�� B���PA�TC� BCE��A �O �P�H����:
C                        T#K#    <   T   <=    T#K+1#
C
CZZZ-------------------------------------------------------------------
       DIMENSION TMAS(*)
CVVVVV REAL KMAS02
       N2 = IFIX( TMAS(1) )
C---> CKO��KO TO�EK..
       N = N2/2
       IF ( N .NE. 0 ) GO TO 1
C-->  B�PA��EH�E..
       KMAS02 = 0.0
       RETURN
1      CONTINUE
C--> HE HA �PAH��AX �� M�....
       T1 = TMAS(2)
       TN = TMAS( 2*N )
       IF ( T .LE. T1 ) GO TO 2
       IF ( T .GE. TN ) GO TO 3
C--->  �O�CK �HTEPBA�A...
C-->   T(K) -> TMAS(2*K);   Y(K) -> TMAS( 2*K+1 )
       I = 1
       J = N + 1
20     K = (I+J)/2
       TK = TMAS(2*K)
       IF ( T .LT. TK ) J = K
       IF ( T .GE. TK ) I = K
       IF ( J .GT. (I+1) ) GO TO 20
C--> �HTEPBA� HA��� -->  ME��� I  �  (I+1) TO�KO�;
       I2 =  2*I
       TI =  TMAS(I2)
       YI =  TMAS(I2+1)
       TI1 = TMAS(I2+2)
       YI1 = TMAS(I2+3)
       DTI = TI1 - TI
       DT  = T - TI
       D   = DT/DTI
       KMAS02 = YI + (YI1-YI) * D
       RETURN
C######################################################################
2      CONTINUE
       KMAS02 = TMAS(3)
       RETURN
3      CONTINUE
       KMAS02 = TMAS(2*N+1)
       RETURN
       END
