       SUBROUTINE DRREGS ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX****KOM��EKC "MS1" -->
C*                                                                     *
C*  �O��PO�PAMMA :   DRREGS ( KAR, DAT, MSWALL , TOKN , VALLN )        *
C*  ------------                                                       *
C*                                                                     *
C*  HA�HA�EH�E :  * O�PA�OT��K ��PEKT�B� #PE���� *                     *
C*  ----------      ������ "HA�A��H�X" �AHH�X:                         *
C*                  TOKOB � HA�P��EH�� � 䠩� ������ NREGX [MASPRT(122)*
C*  METO� :         B�BO�  �AHH�X �� ���ᨢ��                          *
C*  -----             TOKN � VALLN.                                    *
C*                                                                     *
C*  �APAMETP� :    KAR,DAT - O��CAH�E CTPOK� ��PEKT�B�;                *
C*  ---------       MSWALL - C��COK ��PAB�����X C�OB ��EMEHTOB;        *
C*                    TOKN - "TOK�" ��EMEHTOB;                         *
C*                   VALLN - "HA�P��EH��" ��EMEHTOB.                   *
C*                                                                     *
C*  �O�PO�PAMM� :   1.PROF1 , SERVZ1                                   *
C*  -----------     2.WRITMB, ksopen, stopp                                   *
C*                                                                     *
C*  �P�ME�AH�E:
C* ------------
C*                                                                     *
CZZZ********************************************************************
      COMMON / PROF1 / MASPRT(200)
      DIMENSION KAR(1),DAT(*),MSWALL(1),TOKN(1),VALLN(1)
      EQUIVALENCE ( NNTOK , MASPRT(46) )
       EQUIVALENCE ( NSYS,  MASPRT(26) )
      EQUIVALENCE ( FMSWAL , MASPRT(74) )
      EQUIVALENCE ( NMKNOT , MASPRT(7) )
      EQUIVALENCE ( NREGX  , MASPRT(122) )
      INTEGER FMSWAL
C-->
      EQUIVALENCE ( NWTR , MASPRT(13) )
      EQUIVALENCE ( NRDR , MASPRT(12) )
      IF( FMSWAL.EQ.0 ) WRITE (NWTR,1)
 1    FORMAT(/' *** H���襭 ���冷� ᫥������� ��४⨢.',
     *' ��४⨢� ���������� ***'/)
      IF( FMSWAL.EQ.0 ) RETURN
C----------------------------------------------------------------------
         NFF   = NREGX
         IF ( DAT(1) .EQ. 0.0 ) GO TO 2
C--> �TO-TO ECT�
         NFF   = IFIX( DAT(2) )
C--> KOHTPO��
         IF ( NFF   .LE. 0  .OR. NFF  . GT. 100 .OR.NFF.EQ.NRDR.OR.
     =  NFF   .EQ. NWTR  .OR. NFF   .EQ. NWTRE ) NFF   = NREGX
C--> COO���M..
2        WRITE(NWTR, 50 ) NFF
50     FORMAT(/' *** #������ �ਭ��:==> ���祭�� ०����� ��ࠬ��஢ '/
     =         '                    ��� ������⮢ 楯� �����뢠����  '/
     =         '                    � 䠩� KAHA�� # ',I2, ' ***'/)
C---------------------------------------------------------------------
         CALL ksopen(NFF,1,ii_rr)
         IF( ii_rr .NE. 0 ) call STOPP(20)
C--> ����������  NFF
         REWIND NFF
C--> �A��C�����
        CALL WRITMB( NFF, NNTOK, TOKN  )
        CALL WRITMB( NFF, NSYS , VALLN )
C----------------------------------------------------------------------
         CLOSE (NFF)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
      END
