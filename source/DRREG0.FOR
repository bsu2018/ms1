       SUBROUTINE DRREG0 ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX****KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*  �O��PO�PAMMA :   DRREG0 ( KAR, DAT, MSWALL , TOKN , VALLN )        *
C*  ------------                                                       *
C*                                                                     *
C*  HA�HA�EH�E :  * O�PA�OT��K ��PEKT�B� #PE��M *                      *
C*  ----------      �P�EM C��CKA � �A�AH�E "HA�A��H�X" �AHH�X          *
C*                  TOKOB � HA�P��EH�� CO��ACHO C��CKA:                *
C*                  #PE��M                                             *
C*                  "V" Z "�" K1 K2....;                               *
C*                  "V" Z "�" "BCE";                                   *
C*                  "I" Z "�" N1[.S1]...N2...N3[.BCE];                 *
C*                  "GAMMA" Z "�" N1 N2...;                            *
C*                  "OMEGA" Z "�" N1 N2...;                            *
C*                  "I" Z "�" "BCE";                                   *
C*                   #BCE                                              *
C*           *** �TO HE PEA���OBAHO - TO��KO O�H��EH�E -               *
C*                                                                     *
C*  METO� :         BBO� � PA��OP �AHH�X , KOHTPO�� � �A��C�           *
C*  -----           B TOKN � VALLN CO��ACHO HA����� B MSWALL.          *
C*                                                                     *
C*  �APAMETP� :    KAR,DAT - O��CAH�E CTPOK� ��PEKT�B�;                *
C*  ---------       MSWALL - C��COK ��PAB�����X C�OB ��EMEHTOB;        *
C*                    TOKN - "TOK�" ��EMEHTOB;                         *
C*                   VALLN - "HA�P��EH��" ��EMEHTOB.                   *
C*                                                                     *
C*  �O�PO�PAMM� :   1.PROF1 , SERVZ1                                   *
C*  -----------     2.INPUT2                                           *
C*                                                                     *
C*  �P�ME�AH�E:     �ACT��HA� PEA���A���                               *
C* ------------     ( MAKET �PO�PAMM� ).                               *
C*                                                                     *
CZZZ********************************************************************
      COMMON / PROF1 / MASPRT(200)
      DIMENSION KAR(1),DAT(1),MSWALL(1),TOKN(1),VALLN(1)
      EQUIVALENCE ( NNTOK , MASPRT(46) )
      EQUIVALENCE ( FMSWAL , MASPRT(74) )
      EQUIVALENCE ( NMKNOT , MASPRT(7) )
      INTEGER FMSWAL
C-->
      EQUIVALENCE ( NWTR , MASPRT(13) )
      EQUIVALENCE ( NRDR , MASPRT(12) )
      IF( FMSWAL.EQ.0 ) WRITE (NWTR,1)
 1    FORMAT(/' *** HAP��EH �OP��OK C�E�OBAH�� ��PEKT�B.',
     *' ��PEKT�BA ��HOP�P�ETC� ***'/)
      IF( FMSWAL.EQ.0 ) RETURN
C-->   �AH��EH�E �E� BC�KO�O...
C----------------------------------------------------------------------
      DO 100 I=1,NNTOK
      TOKN(I)=0.0
 100  CONTINUE
C----------------------------------------------------------------------
      DO 200 I=1,NMKNOT
      VALLN(I)=0.0
 200  CONTINUE
C----------------------------------------------------------------------
C
       WRITE(NWTR,3000)
3000   FORMAT(/' *** ��४⨢� #PE��M - ����  �� ॠ�������� ***'  )
       WRITE(NWTR,3004)
3004  FORMAT(' *** ��砫�� ���祭�� ��� ��६����� ��⠭����������',
     =' ࠢ�묨 0.0 ***'/)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
      END
