         SUBROUTINE GETNDM( MSWALL     )
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0
C   �O��PO�PAMMA:   GETNDM( MSWALL     )
C   ------------
C   HA�HA�EH�E:    �POCMOTP MSWALL C��CKA,
C   -----------    � ����஥��� 䠩�� ����� 96, ᮤ�ঠ饣�
C                  ����� DM �����஢ - ����ଠ�� �����᪨�
C                  MAS(i) = 0 = .FALSE. -  ��� ������ � MSW
C                  MAS(i) = 1 = .TRUE.  -  ���� ������ � MSW
C                  ���� ����ᠭ � �ଥ 'DIRECT' � ������ �����
C                  100 ����.
C   METO�:         �O�CK.
C   -----
C   �APAMETP�:     MSWALL - C�OBA COCTO�H�� ��EMEHTOB;
C   ---------
C @@@@             IRC = 0 - 䠩� ᮧ����;
C @@@@             IRC = 1 - 䠩� ᮧ���� �� 㤠����;
C   �O��PO�PAMM�:  HET
C   ------------
C   �P�ME�AH�E:
C   ----------
C
C                  CTP�KT�PA MACC�BA  MSWALL:
C                  --------------------------
C
C  +-------------...+------+------------+------------+...+----------+
C  I            I   I A�P. I"�OCTO�HHA�"I"�EPEMEHHA�"I   IA�P. C�E�.I
C  I ��CKP��TOP I   I C�E�.I �ACT� MSW  I �ACT� MSW  I   I   ���    I
C  I            I   I MSW  I (INTEGER*4)I            I   I  HO�� [0]I
C  +------------+...+------+------------+------------+...+----X-----+
C  I1 2 3 4 5   I   I  1   I   8 C�OB   I    ????    I        I
C  +------------+   +------+------------+------------+        I
C                      I                                      I
C                      V                                      I
C                      +>------------------------------------>+
C
C
C               "�OCTO�HHA�" �ACT� MSW:
C                ----------------------
C
C  +-------+--------+--------+--------+------+------+----------+------+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. I A�P. B�B.I PE   I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I �HA�. B  I �E   I
C  I HTA   I MO�E�� I PAC�ETAI        I BO   I �HA� I "TOKN" � I PB   I
C  I       I        I DM.....I        I ���OBI EH��.I B "TOKN1"I      I
C  +-------+--------+--------+--------+------+------+----------+------+
C  I  1    I   2    I   3    I    4   I   5  I  6   I    7     I  8   I
C  +-------+--------+--------+--------+------+------+----------+------+
C
C
C               "�EPEMEHHA�" �ACT� MSW:
C               -----------------------
C
C  +----------++-------------+-------------+
C  I C��COK   II C��COK      I PA�O�A�     I
C  I ���OB    II BH�TPEHH�X  I O��ACT�     I
C  I �O�K��-  II �APAMETPOB  I MAKPOMO�E�� I
C  I �EH��    II MAKPOMO�E�� I             I
C  I ��EMEHTA II             I             I
C  I          II             I             I
C  +----------++-------------+-------------I
C  I 9+K"���OBII   SPM       I  SRAB       I
C  +----------++-------------+-------------+
C
CZZZ-------------------------------------------------------------------
         PARAMETER (LLMAS=100)
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE( MASPRT(13), NWTR )
         EQUIVALENCE( MASPRT(14), NWTRE )
         DIMENSION MSWALL(*)
         LOGICAL*1 MAS(LLMAS)
         INTEGER ADR
         DATA nf/96/
         DO 100 i=1,LLMAS
100      mas(i) = .FALSE.
         IRC = 1
         ADR = 6
900      I1 = MSWALL(ADR)
         IF ( I1 .EQ. 0 ) GO TO 555
         IMSW = ADR + 1
         ADR = I1
C---> KTO TAM?
         IF ( MSWALL(IMSW+2) .LE. 0    .OR.
     =        MSWALL(IMSW+2) .GT.LLMAS        ) GO TO 777
C--->
         MAS( MSWALL(IMSW+2) ) = .TRUE.
         GO TO 900
555      CONTINUE
CCCCC*         ACCESS='DIRECT',RECL=LLMAS,ACTION='WRITE',STATUS='NEW',
         OPEN (UNIT=nf,file='MS1_DATA.DMN',
     *         ACCESS='DIRECT',RECL=LLMAS,
     *         ERR=666,IOSTAT=irc)
        WRITE(UNIT=nf,REC=1,IOSTAT=irc,ERR=888) (MAS(iii),iii=1,LLMAS)
        CLOSE(UNIT=nf)
        IRC = 0
         RETURN
C************
666      WRITE(NWTRE,667) irc
         WRITE(NWTR ,667) irc
667    FORMAT(/' *** �訡�� �� ����⨨ 䠩�� �裡 MS1_DATA.DMN ***'/
     *        ' *** ��� �訡�� {IOSTAT} = ',I8, '                ***'/)
C        RETURN
        CALL STOPP(20)
C************
888      WRITE(NWTRE,887) irc
         WRITE(NWTR ,887) irc
887    FORMAT(/' *** �訡�� �� �����   䠩�� �裡 MS1_DATA.DMN ***'/
     *        ' *** ��� �訡�� {IOSTAT} = ',I8, '                ***'/)
        CALL STOPP(24)
C        RETURN
C************
777      WRITE(NWTRE,778) MSWALL(IMSW+2),MSWALL(IMSW+0),MSWALL(IMSW+1)
         WRITE(NWTR ,778) MSWALL(IMSW+2),MSWALL(IMSW+0),MSWALL(IMSW+1)
778    FORMAT(/' *** �訡�� �� ��ᬮ�� ᯨ᪠  MSW '/
     *         ' *** �� �����⨬� ����� DM ��ࠡ��稪�:'/
     *         ' *** DM',I4.4, ' ��� ������� ',I5,' ⨯ ', I5/)
        CALL STOPP(28)
C        RETURN
         END
