       SUBROUTINE SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
     *                    IFXK, IFXA, EYYN, EYYN1 )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
C  --------------         IFXK, IFXA, EYYN, EYYN1 )
C
C   HA�HA�EH�E : ��PAB�EH�E PA��E��HO� PA�OTO� BEHT���H�X �P���,
C   -----------   *** BEPT�KA��HOE ��PAB�EH�E ***
C
C   METO�:  O�O�HA�EH�E  IH(T) = IA(T) - IK(T);
C   -----
C              �P�  ABS( IH(TN) ) <= IYSTAV ---> "PA�PE�EH��":
C                      EYYN1 > 0  �O��HA "A" �P���A;
C                      EYYN1 < 0  �O��HA "K" �P���A;
C                      EYYN1 = 0  �O��HA "A" �P���A � "K" �P���A;
C
C              �P� ABS( IH(TN+H0) ) <= IYSTAV ---> "��PET�":
C                      EYYN1 > 0  HE �O��HA "A" �P���A;
C                      EYYN1 < 0  HE �O��HA "K" �P���A;
C                      EYYN1 = 0  HE �O��HA "A" �P���A � "K" �P���A;
C                                 (????????????????????????????????)
C
C  !!!  TXA/K ��MEH�ETC� B C���AE �E�CTB�TE��HO� CMEH� CXA/K   !!!
C
C   �APAMETP�: CXK - ��A� �A�PE�EH�� ( =0 ) / PA�PE�EH�� ( =1 ) "�O�A��"
C   ---------        "C��" HA K-�P����;
C              TXK - BPEM�, HA��HA� C KOTOPO�O �E�CTB�ET CXK ��A�;
C              IFXK - ��A� PA�PE�EH�� ��MEHEH�� �HA�EH�� TX B RMSW_50
C                     O��CAH�� BEHT��� HA �HA�EH�E TXK ( �P� =1 MO�HO );
C                 � � �  A-�P����  --> AHA�O���HO;
C              EYYN, EYYN1 - �HA�EH�� ��PAB�����X C��HA�OB B TN, TN+H0;
C              IKN, IKN1 - TOK� K-�P���� B TN, TN+H0;
C              IAN, IAN1 - TOK� A-�P���� B TN, TN+H0;
C                                                                      ;
C
C
C   �O��PO�PAMM�:   PROF1;
C   ------------
C   �P�ME�AH�E:    IYSTAV �� MASPRT(105);
C   ----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( ICLOSE, MASPRT(78) )
         EQUIVALENCE ( IYSTAV, MASPRT(105) )
         REAL ICLOSE, IH, IH1
C----------------------------------------------------------------------
       REAL IYSTAV,IKN, IKN1, IAN, IAN1
       INTEGER CXA, CXK
C
       IFXK = 0
       IFXA = 0
       IH=IAN-IKN
       IH1=IAN1-IKN1
C**********************************************************************
       AIH1 = ABS( IH1 )
       AIH  = ABS( IH  )
C----------------------------------------------------------------------
       IF ( AIH .LE. IYSTAV ) GO TO 1
10     CONTINUE
       IF ( AIH1 .LE. IYSTAV ) GO TO 100
C--->  H� �E�O HE MEH�T� B �TOM C���AE....
20     CONTINUE
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  "PA�PE�EH��" CO��ACHO �HAKA EYYN1......
       IF ( EYYN1 ) 2,3,4
C******
2      CONTINUE
C-->   < 0  �O��EH K.
       IF ( CXK .EQ. 1 ) GO TO 10
       CXK = 1
       IFXK = 1
       TXK = TN + H0
       GO TO 10
3      CONTINUE
C   = 0,  �O��EH � K � A..
       IF ( CXK .EQ. 1 ) GO TO 30
       CXK = 1
       IFXK = 1
       TXK = TN + H0
30     CONTINUE
       IF ( CXA .EQ. 1 ) GO TO 10
       CXA = 1
       IFXA = 1
       TXA = TN + H0
       GO TO 10
4      CONTINUE
C  >  0  �O��EH A....
      IF ( CXA .EQ. 1 ) GO TO 10
      CXA = 1
      IFXA = 1
      TXA = TN + H0
      GO TO 10
C----------------------------------------------------------------------
100    CONTINUE
C-->  �A�PET  CO��ACHO EYYN1......
       IF ( EYYN1 ) 5, 6, 7
5      CONTINUE
C  < 0  HE �O��EH A
       IF ( CXA .EQ. 0 ) GO TO 20
       CXA = 0
       IFXA = 1
       TXA = TN + H0
       GO TO 20
6      CONTINUE
C   = 0  HE �O��EH H� K H� A ( ????)
C@@@@@GO TO 20
       IF ( CXA .EQ . 0 ) GO TO  60
       CXA = 0
       IFXA = 1
       TXA = TN + H0
60     CONTINUE
       IF ( CXK .EQ. 0 ) GO TO 20
       CXK = 0
       IFXK = 1
       TXK = TN + H0
       GO TO 20
7      CONTINUE
C  > 0 HE �O��EH K...
       IF ( CXK .EQ. 0 ) GO TO 20
       CXK = 0
       IFXK = 1
       TXK = TN + H0
       GO TO 20
C**********************************************************************
C**********************************************************************
       END
