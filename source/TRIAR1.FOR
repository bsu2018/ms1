         SUBROUTINE TRIAR1(N, ECHANG, COMPCL,   BA0)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   TRIAR1(N, ECHANG, COMPCL,   BA0)
C   -------------
C   HA�HA�EH�E:    "�PEBECHA�" BOCXO���A� COPT�POBKA
C  -----------      ( OPEEHT�POBAHHA� HA COPT�POBK� BA0 C��CKA )
C
C   METO�:         ��O��A - �����MCA ( CM.  ME�EP �., �O���H K.,
C   -----           METO�� �PO�PAMM�POBAH��, M. "M�P" T2, CTP. 181 )
C
C   �APAMETP�:     N - KO���ECTBO O'EKTOB ( ��EMEHTOB �E�� );
C   ---------      ECHANG (I,J) - �M� �/� B��O�H���E� �EPECTAHOBK�
C                                  I  �  J  O�'EKTA;
C                  COMPCL (I,J) - �M� FUNCTION CPABHEH�� K���E�:
C                         = -1  K���� B I  MEH��E  K���A  B J
C                         =  0  K���� B I  PABEH   K����  B J
C                         = +1  K���� B I  �O���E  K���A  B J
C
C
C   �O��PO�PAMM�:  1. REORG1 - "PEOP�AH��A���"
C   -------------
C
C   �P�ME�AH�E:    1. MACC�B BA0 BBE�EH B �OPM. �APAMETP� T.K. OH HE
C  ------------       �B��ETC� �OCTO�HHO ( CTAT��ECK� ) B��E�EHH�M;
C                  2. ����� �PO�PAMM� TRIARB ( �/� REORG1 ����� REORG)
CZZZ-------------------------------------------------------------------
         EXTERNAL ECHANG, COMPCL
         INTEGER COMPCL
         DIMENSION BA0(1)
C--> "�OCA�KA" �EPEBA
         I = N/2
100      CONTINUE
         IF ( I .LE. 0 ) GO TO 200
C-->  PEOP�AH��A���
         CALL REORG1(I, N, ECHANG, COMPCL, BA0 )
         I = I - 1
         GO TO 100
C-->  COPT�POBKA MAX-��E�O �EPEBA
200      CONTINUE
         I = N
300      CONTINUE
         IF ( I .LE. 1 ) GO TO 1000
C--> �EPECTAB��EM..
         CALL ECHANG( 1,  I,  BA0)
         I = I- 1
         CALL REORG1(1, I, ECHANG, COMPCL, BA0 )
         GO TO 300
C--->
1000     RETURN
         END
