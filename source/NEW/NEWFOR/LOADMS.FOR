       SUBROUTINE LOADMS(MSWALL, TOK, VALL,KAR,DAT, IRC)
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA: LOADMS(MSWALL, TOK, VALL,KAR,DAT, IRC)
C   ------------
C   HA�HA�EH�E:   �TEH�E C  97 KAHA�A ( MS1_data.MSA ) ���ᨢ�� �裡
C   -----------
C
C      BOCTAHAB��BA�TC�  �A��C� --->
C
C 1-E �A��C� -> <PROF1>, <SERVZ4>, <SERVZ5>, <SERVZ6> <SERVZ8>;
C
C 2-� �A��C� -> MSWALL, TOKN, VALLN;
C
C   METO�:    �TEH�E �AHH�X C 97 KAHA�A;
C   -----
C   �APAMETP�:  CM. HA�HA�EH�E;
C  ----------
C
C                      IRC = 0 -->  �C�E�HOE �ABEP�EH�E;
C                      IRC = 1 -->  �PO��TAT� HE ��A�OC� :
C                               KO�=X01  -> END _OF_FILE
C                               KO�=X02  -> ERROS_TO_READ;
C                                   X - HOMEP �A��C�     ;
C
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
       INTEGER  TOK, VALL
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
CCCCCCCWRITE(NWTRE, 100 ) NF
C
C-->  1-� �A��C�
C      KXX = 100
C      READ(NF,END=50,ERR=51)    TN, (MASPRT(I1), I1=1,LPRTM ),
C    =               (NAMECR(I2), I2=1,80    ),
C    =     KCOM,     (COMENT(I3), I3=1,100   ),
C    =    TBEGON, TENDON, KOUTON, ( SPISON(I4), I4=1,55 )
C--> 2-� �A��C�
C      KXX = 200
C      READ(NF,END=50,ERR=51) (MSWALL(I1), I1=1, DMSWAL ),
C    =           ( TOK(I2),  I2=1, NNTOK  ),
C    =           ( VALL(I3), I3=1, NSYS   )
C----------------------------------------------------------------------
C--> ��⠥�
       KXX = 100
       READ(nf, END=50,ERR=51)    (MASPRT(I1), I1=1,LPRTM )
       KXX = 200
       READ(nf, END=50,ERR=51) (NAMECR(I2), I2=1,80  ),
     +                        KCOM,(COMENT(I3), I3=1,100 ),
     +                        TBEGON, TENDON, KOUTON,
     +                        ( SPISON(I4), I4=1,55 )
       KXX = 300
       READ(nf, END=50,ERR=51) SPISNN,
     +                         (SPISVL(I3), I3=1,16),
     +                         (SPISTK(I4), I4=1,16)
c     +  (TNL(J1),J1=1,200) ,(TNC(J2),J2=1,200),
c     +  (UN0(J3),J3=1,200) ,(UN1(J4),J4=1,200),
c     +  (TJNL(J5),J5=1,200) ,(TJNC(J6),J6=1,200),
c     +  (D11(J7),J7=1,200) ,(D21(J8),J8=1,200)
       KXX = 66600
       CALL READMB( nf,   DMSWAL, MSWALL, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
       CALL READMB( nf,   NNTOK, TOK, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
       CALL READMB( nf,   NSYS, VALL, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
       READ(nf, END=50,ERR=51)
     +                         (KAR(I3), I3=1,80),
     +                         (DAT(I4), I4=1,21)
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCWRITE(NWTRE, 101 ) TN, NZAPSP
C$$
       IRC = 0
       CLOSE (NF)
       RETURN
C----------------------------------------------------------------------
50     CONTINUE
       KX = 1 + KXX
       GO TO 52
51     CONTINUE
       KX = 2 + KXX
52     WRITE(NWTR, 103)     KX
       WRITE(NWTRE, 103)     KX
103    FORMAT(/' *** �訡�� �⥭�� 䠩�� MS1_DATA.MSA ',
     =  ' ���=', I8, ' ***'/)
C+---------------------------------------------------------------------
       IRC = 1
       CLOSE (NF)
       RETURN
C-----------
666    WRITE(NWTR, 104) ii_rr
       WRITE(NWTRE, 104)  ii_rr
104    FORMAT(/' *** �訡�� ������ 䠩�� MS1_DATA.MSA ',
     =  ' IOSTAT =', I8, ' ***'/)
C+---------------------------------------------------------------------
       IRC = 1
       CLOSE (NF)
       RETURN
       END
