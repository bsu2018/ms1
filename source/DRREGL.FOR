       SUBROUTINE DRREGL ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX****KOM��EKC "MS1" -->
C*                                                                     *
C*  �O��PO�PAMMA :   DRREGL ( KAR, DAT, MSWALL , TOKN , VALLN )        *
C*  ------------                                                       *
C*                                                                     *
C*  HA�HA�EH�E :  * O�PA�OT��K ��PEKT�B� #PE����� *                    *
C*  ----------      �⥭�� "HA�A��H�X" �AHH�X:                         *
C*                  TOKOB � HA�P��EH�� � 䠩�� ������ NREGX=MASPRT(122)*
C*  METO� :         BBO�  �AHH�X �  ���ᨢ�                            *
C*  -----             TOKN � VALLN.                                    *
C*                                                                     *
C*  �APAMETP� :    KAR,DAT - O��CAH�E CTPOK� ��PEKT�B�;                *
C*  ---------       MSWALL - C��COK ��PAB�����X C�OB ��EMEHTOB;        *
C*                    TOKN - "TOK�" ��EMEHTOB;                         *
C*                   VALLN - "HA�P��EH��" ��EMEHTOB.                   *
C*                                                                     *
C*  �O�PO�PAMM� :   1.PROF1 , SERVZ1                                   *
C*  -----------     2.READMB , ksopen, stopp                                          *
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
         IF ( NFF   .LE. 0  .OR. NFF  . GT. 100 .OR. NFF .EQ.NRDR.OR.
     =  NFF   .EQ. NWTR  .OR. NFF   .EQ. NWTRE ) NFF   = NREGX
C--> COO���M..
2        WRITE(NWTR, 50 ) NFF
50     FORMAT(/' *** #������� �ਭ��:==> ���祭�� ०����� ��ࠬ��஢'/
     =         ' ***             ��� ������⮢ 楯� ����⠭����������'/
     =         ' ***             �� 䠩�� KAHA�� # ',I2 /)
C---------------------------------------------------------------------
         CALL KSOPEN(NFF,1,ii_rr)
         IF (ii_rr .NE. 0 ) call STOPP(20)
C--> ����������  NFF
         REWIND NFF
C--> ������
        CALL READMB( NFF, NNTOK, TOKN, IRR  )
        IF ( IRR .NE. 0 ) GO TO 666
        CALL READMB( NFF, NSYS , VALLN, IRR )
        IF ( IRR .NE. 0 ) GO TO 666
C----------------------------------------------------------------------
       CLOSE (NFF)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
666   CONTINUE
         WRITE(NWTR, 51 ) NFF
51     FORMAT(/' *** #�������: ����ࠢ��� �訡�� �� �⥭�� '/
     =         ' ***           �� 䠩�� KAHA�� # ',I2/ )
C-->   �AH��EH�E �E� BC�KO�O...
       CLOSE (NFF)
C----------------------------------------------------------------------
      DO 100 I=1,NNTOK
      TOKN(I)=0.0
 100  CONTINUE
C----------------------------------------------------------------------
      DO 200 I=1,NMKNOT
      VALLN(I)=0.0
 200  CONTINUE
C----------------------------------------------------------------------
       WRITE(NWTR,3004)
3004  FORMAT(' *** HA�A��H�E �HA�EH�� BCEX �EPEMEHH�X �CTAHAB��BA�TC�',
     =' PABH�M� 0.0 ***'/)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
      END
