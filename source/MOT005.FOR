         SUBROUTINE MOT005(G6,G12,J6,J12)
CXXXX   KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)-------------
C
C     �O��PO�PAMMA: MOT005(G6,G12,J6,J12)
C    -------------
C
C     HA�HA�EH�E: �O����T� PAC��PEHH�� MATP��� �POBO��MOCTE� ���
C     ----------  MO�E�� ��. MA��H�, �� MATP��� G6 - TO��KO BXO��;
C                 TO �E ��� J12 - �CTO�H�KOB TOKA <-- �� J6;
C
C    METO�: �P�MOE PAC��PEH�E MATP�� G6 � J6 CO��ACHO MAT. MO�E��;
C    -----
C
C    �APAMETP�: G6 - C����OK ��� G12;
C    ---------
C               G12 - B�XO�HA� PAC��PEHHA� MATP��A �POBO��MOCTE�;
C               J6  - C����OK ��� J12;
C               J12 - B�XO�HO� MACC�B �CTO�H�KOB TOKA;
C
C     �C�����EM�E �O��PO�PAMM�: HET
C     -------------------------
C
CZZZZ    MOT005    -----------------------------------------------------
C
C
         DIMENSION G6(6,6),G12(12,12),J6(6),J12(12)
         REAL*4 G6,G12,J6,J12
         INTEGER*4 I,J,I7,J7
C------
         DO 1 I=1,6
         DO 1 J=1,6
1        G12(I,J)=G6(I,J)
C------
         DO 2 I=7,12
         I7=I-6
         DO 2 J=7,12
         J7=J-6
2        G12(I,J)=G6(I7,J7)
C-------
         DO 3 I=1,6
         DO 3 J=7,12
         J7=J-6
3        G12(I,J)=-G6(I,J7)
C-------
         DO 4 I=7,12
         I7=I-6
         DO 4 J=1,6
4        G12(I,J)=-G6(I7,J)
C-------
         DO 5 I=1,12
         IF ( I .LE. 6 ) J12(I)=J6(I)
         IF ( I .GT. 6 ) J12(I)=-J6(I-6)
5        CONTINUE
         RETURN
         END
