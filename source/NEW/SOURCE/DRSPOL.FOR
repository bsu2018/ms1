          SUBROUTINE DRSPOL ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   DRSPOL ( KAR, DAT, MSWALL , TOKN , VALLN )
C   ------------
C   HA�HA�EH�E:    ��ࠡ��稪 ��४⨢� #SPOOL
C   -----------     ��ନ��� ᯨ᪨ � COMMON/SERVZ8/ SPISNN, ...
C                    㪠�뢠�騥 ����� ���祭�� �����뢠�� � �㫨��
C                    ����㦤��� 䫠� FTRSPL==MASPRT(121)
C   METO�:         BBO� �AHH�X ( CM. INPUT1 )
C   -----
C*  �APAMETP� :    KAR,DAT - O��CAH�E CTPOK� ��PEKT�B�;                *
C*  ---------       MSWALL - C��COK ��PAB�����X C�OB ��EMEHTOB;        *
C*                    TOKN - "TOK�" ��EMEHTOB;                         *
C*                   VALLN - "HA�P��EH��" ��EMEHTOB.                   *
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, SERVZ1, 2, 3, 8
C   -------------  2. GETSPO
C   �P�ME�AH�E:    1. B C��CKE HE �O�EE 15-T� �HA�EH��;
C   ----------     2. �OPMAT SPISVL �  SPISTK
C
C                  �OPMAT  SPISxx:
C                  -------------
C
C   +------------+----------+----------+    *   ��� VL - ����� 㧫�
C   I KO���ECTBO I ���祭�� I ���祭�� I        ��� TK - ���� ⮪�
C   I ���祭��   I    *     I    *     I ........        � ��ᨢ�� TOKN
C   +------------+----------+----------+
C   I 1 C�OBO    I 1 C�OB�  I 1- C�OB� I .......
C   +------------+----------+----------+
C
C     P A � M E P H O C T �    SPIS  ==>  1 + 15 = 16  C�OB;
C
C
CZZZ-------------------------------------------------------------------
      COMMON / PROF1 / MASPRT(200)
      COMMON/SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
      INTEGER        SPISNN, SPISVL,     SPISTK
      EQUIVALENCE ( NNTOK , MASPRT(46) )
      EQUIVALENCE ( FMSWAL , MASPRT(74) )
      EQUIVALENCE ( NMKNOT , MASPRT(7) )
      EQUIVALENCE ( MASPRT(15),  DIAL )
      EQUIVALENCE ( MASPRT(14),  NWTRE )
      EQUIVALENCE ( MASPRT(13),  NWTR )
      EQUIVALENCE ( MASPRT(121), FTRSPL )
      INTEGER FMSWAL,FTRSPL
C-->
      EQUIVALENCE ( NWTR , MASPRT(13) )
      EQUIVALENCE ( NRDR , MASPRT(12) )
      DIMENSION KAR(1),DAT(1),MSWALL(1),TOKN(1),VALLN(1)
      DIMENSION VT(16), UT(31), IT(31), SPIS(91)
      INTEGER VT, UT, SUM, PRT, SPIS, TABSPC, TIP, DIAL
C-->
      IF( FMSWAL.EQ.0 ) WRITE (NWTR,201)
201   FORMAT(/' *** HAP��EH �OP��OK C�E�OBAH�� ��PEKT�B.',
     *' ��PEKT�BA ��HOP�P�ETC� ***'/)
      IF( FMSWAL.EQ.0 ) RETURN
         WRITE(NWTR, 202)
202   FORMAT(/' *** #SPOOL �P�H�TA:==>    ������⢮ ��࠭塞�� ***'/
     *        ' *** ���祭�� ᮪�頥��� �� ������ ���짮��⥫� ***')
C
         FTRSPL = 0
C
         CALL GETSPO( MSWALL, SPIS,IRC )
         IF (IRC .EQ. 3 ) RETURN
         IF (IRC .EQ. 2 ) RETURN
C+++++++++++++++++++++++++++++++
         DO 1 i=1,SPISNN
             SPISVL(i) = 0
1            SPISTK(i) = 0
C**
         jv = 2
         jt = 2
         DO 2 ii=1, SPIS(1)
           i = (ii-1)*6+2
*  ���� ******
           IF ( SPIS(i+0) .EQ. 1 ) THEN
               IF ( SPISVL(1) .EQ. 0 ) THEN
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               ELSE
                   DO 3 js = 2, SPISVL(jv-1)
                      IF ( SPISVL(js) .EQ. SPIS(i+1) ) GO TO 2
3                  CONTINUE
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               END IF
               GO TO 2
           ENDIF
*  ࠧ����� 㧫��
           IF ( SPIS(i+0) .EQ. 2 ) THEN
*              ���� 㧥�
               IF ( SPISVL(1) .EQ. 0 ) THEN
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               ELSE
                   DO 31 js = 2, SPISVL(jv-1)
                      IF ( SPISVL(js) .EQ. SPIS(i+1) ) GO TO 2
31                 CONTINUE
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               END IF
*              ��ன 㧥�
               IF ( SPISVL(1) .EQ. 0 ) THEN
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+2)
                   jv = jv + 1
               ELSE
                   DO 32 js = 2, SPISVL(jv-1)
                      IF ( SPISVL(js) .EQ. SPIS(i+2) ) GO TO 2
32                 CONTINUE
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+2)
                   jv = jv + 1
               END IF
               GO TO 2
           ENDIF
*  ⮪� ******
           IF ( SPIS(i+0) .EQ. 3 ) THEN
               IF ( SPISTK(1) .EQ. 0 ) THEN
                   SPISTK(1) = SPISTK(1) + 1
                   SPISTK(jt) = SPIS(i+1)
                   jt = jt + 1
               ELSE
                   DO 33 js = 2, SPISTK(jt-1)
                      IF ( SPISTK(js) .EQ. SPIS(i+1) ) GO TO 2
33                 CONTINUE
                   SPISTK(1) = SPISTK(1) + 1
                   SPISTK(jt) = SPIS(i+1)
                   jt = jt + 1
               END IF
               GO TO 2
           ENDIF
C********************
           WRITE(NWTRE,*) ' ����襭�� �������� SPIS: �ணࠬ�� DRSPOL'
C********************
2        CONTINUE
C*********************
         IF ( (SPISVL(1) + SPISTK(1)) .NE. 0 ) THEN
C
              FTRSPL = 1
C
              RETURN
         ELSE
              WRITE(NWTR,*) ' #SPOOL - ������������ !!!'
              RETURN
         ENDIF
         END
