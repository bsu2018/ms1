       SUBROUTINE SAVEMS(MSWALL, TOK, VALL, KAR, DAT)
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA: SAVEMS(MSWALL, TOK, VALL, KAR, DAT)
C   ------------
C   HA�HA�EH�E:   ������ �� 97 KAHA�  ( MS1_data.MSA ) ���ᨢ�� �裡
C   -----------
C
C      ������������      �A��C� --->
C
C 1-E �A��C� -> <PROF1>, <SERVZ4>, <SERVZ5>, <SERVZ6> <SERVZ8>;
C
C 2-� �A��C� -> MSWALL, TOKN, VALLN;
C
C   METO�:    �TEH�E �AHH�X C 97 KAHA�A;
C   -----
C   �APAMETP�:  CM. HA�HA�EH�E;
C  ----------
C   �O��PO�PAMM�:  1. COMMON_�;
C   ------------
C
C  �P�ME�AH�E:     1.  �OPMAT �A��A         --> �. � readmb+writmb
C  ----------
C
CZZZ-------------------------------------------------------------------
       PARAMETER (nf=97)
       COMMON /PROF1/ MASPRT(200)
       COMMON /SERVZ4/ NAMECR(80)
       COMMON /SERVZ5/ KCOM, COMENT(100)
       COMMON /SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
       COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
       COMMON /PIEZO/ TNL(200),TNC(200),UN0(200),UN1(200),
     * TJNL(200),TJNC(200),D11(200),D21(200)
C
       INTEGER NAMECR, COMENT, SPISON, DMSWAL
C
       DIMENSION MSWALL(1), TOK(1), VALL(1), KAR(1), DAT(1)
       INTEGER   TOK, VALL
C
       EQUIVALENCE ( NNTOK, MASPRT(46) )
       EQUIVALENCE ( NSYS,  MASPRT(26) )
       EQUIVALENCE ( LPRTM, MASPRT(54) )
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       EQUIVALENCE ( NWTR,  MASPRT(13) )
       EQUIVALENCE ( DMSWAL, MASPRT(20) )
       EQUIVALENCE ( NZAPSP, MASPRT(92) )
C-------
        OPEN(UNIT=nf,file='MS1_DATA.MSA',form='UNFORMATTED',
     =       ERR=666,IOSTAT=ii_rr)
C********
C-->  �A�� B HA�A�O.....
       REWIND nf
C--> HA�A��....
C--> ��襬
       WRITE(nf       )    (MASPRT(I1), I1=1,LPRTM )
       WRITE(nf       ) (NAMECR(I2), I2=1,80  ),
     +                        KCOM,(COMENT(I3), I3=1,100 ),
     +                        TBEGON, TENDON, KOUTON,
     +                        ( SPISON(I4), I4=1,55 )
       WRITE(nf        ) SPISNN,
     +                         (SPISVL(I3), I3=1,16),
     +                         (SPISTK(I4), I4=1,16)
c     +  (TNL(J1),J1=1,200) ,(TNC(J2),J2=1,200),
c     +  (UN0(J3),J3=1,200) ,(UN1(J4),J4=1,200),
c     +  (TJNL(J5),J5=1,200) ,(TJNC(J6),J6=1,200),
c     +  (D11(J7),J7=1,200) ,(D21(J8),J8=1,200)
       CALL WRITMB( nf,   DMSWAL, MSWALL )
       CALL WRITMB( nf,   NNTOK, TOK )
       CALL WRITMB( nf,   NSYS, VALL )
       WRITE(nf        )
     +                         (KAR(I3), I3=1,80),
     +                         (DAT(I4), I4=1,21)
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       CLOSE (NF)
       RETURN
C----------------------------------------------------------------------
666    WRITE(NWTR, 104) ii_rr
       WRITE(NWTRE, 104)  ii_rr
104    FORMAT(/' *** �訡�� ������ 䠩�� MS1_DATA.MSA ',
     =  ' IOSTAT =', I8, ' ***'/)
C+---------------------------------------------------------------------
       CLOSE (NF)
       CALL STOPP(666)
       RETURN
       END
