      SUBROUTINE EQMAS(M1,I1,M2,I2,N,KODS)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C    �O��PO�PAMMA:  EQMAS(M1,I1,M2,I2,N,KODS)
C    ------------
C      HA�HA�EH�E: �O��PO�PAMMA CPABHEH�� N-��EMEHTOB MACC�BOB M1 � M2,
C      ----------   HA��HA�  C ��EMEHTOB I1 � I2 COOTBETCTBEHHO
C
C      METO�:             O�EB��EH;
C      -----
C      �APAMETP�:   M1 - MACC�B
C      ---------    M2 - MACC�B
C                   I1 - HOMEP �EPBO�O ��EMEHTA MACC�BA M1 C KOTOPO�O
C                        HA��HAETC� CPABHEH�E
C                   I2 - -//- MACC�BA M2 -//-
C                   N - KO���ECTBO ��EMEHTOB CPABHEH��
C                   KOD - KO� CPABHEH��:
C                         KOD=1 CPABHEH�E �C�E�HO
C                         KOD=0 CPABHEH�E HE �C�E�HO
C                                         **
C      �O��PO�PAMM�:  HET
C      ------------
C
C      �P�ME�AH�E:
C      -----------
CZZZ-------------------------------------------------------------------
      DIMENSION M1(1),M2(1)
      KODS=1
      DO 1 K=1,N
      K1=K-1
      IF (M1(I1+K1).EQ.M2(I2+K1)) GO TO 1
      KODS=0
      GO TO 2
    1 CONTINUE
    2 RETURN
      END
