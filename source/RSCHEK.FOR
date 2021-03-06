       SUBROUTINE RSCHEK( NF, TN, MSWALL, TOK, VALL, IRC)
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA: RSCHEK( NF, TN, MSWALL, TOK, VALL, IRC )
C   ------------
C   HA�HA�EH�E:   �TEH�E C  NF KAHA�A ( �� HA�A�A �A��A ) K.T. PE�EH��
C   -----------
C                      �C�O�H�TE��H�� MEXAH��M ��PEKT�B� #BOCT
C                      ----------------------------------------
C
C      BOCTAHAB��BA�TC�  �A��C� --->
C
C 1-E �A��C� -> TN, <PROF1>, <SERVZ4>, <SERVZ5>, <SERVZ6> <SERVZ8>;
C
C 2-� �A��C� -> MSWALL, TOKN, VALLN;
C
C   METO�:    �TEH�E �AHH�X C NF KAHA�A;
C   -----
C   �APAMETP�:  CM. HA�HA�EH�E; TN - BPEM� MO�E��POBAH��, �P� KOTOPOM
C  ----------    �OPM�P�ETC� K.T.
C
C                      IRC = 0 -->  �C�E�HOE �ABEP�EH�E;
C                      IRC = 1 -->  �PO��TAT� HE ��A�OC� :
C                               KO�=X01  -> END _OF_FILE
C                               KO�=X02  -> ERROS_TO_READ;
C                                   X - HOMEP �A��C� K.T.;
C
C   �O��PO�PAMM�:  1. COMMON_�;
C   ------------
C
C  �P�ME�AH�E:     1.  �OPMAT �A��A ��� K.T --> �. � readmb+writmb
C  ----------
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /SERVZ4/ NAMECR(80)
       COMMON /SERVZ5/ KCOM, COMENT(100)
       COMMON /SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
       COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
C
       INTEGER NAMECR, COMENT, SPISON, DMSWAL
C
       DIMENSION MSWALL(1), TOK(1), VALL(1)
       INTEGER  TOK, VALL
C
       EQUIVALENCE ( NNTOK, MASPRT(46) )
       EQUIVALENCE ( NSYS,  MASPRT(26) )
       EQUIVALENCE ( LPRTM, MASPRT(54) )
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       EQUIVALENCE ( NWTR,  MASPRT(13) )
       EQUIVALENCE ( DMSWAL, MASPRT(20) )
       EQUIVALENCE ( NZAPSP, MASPRT(92) )
         CALL KSOPEN(NF, 1,i_rc)
         IF( i_rc .NE. 0 ) CALL STOPP(20)
C********
C-->  �A�� B HA�A�O.....
       REWIND NF
C--> HA�A��....
       WRITE(NWTRE, 100 ) NF
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
       READ(NF, END=50,ERR=51) TN,(MASPRT(I1), I1=1,LPRTM )
       KXX = 200
       READ(NF, END=50,ERR=51) (NAMECR(I2), I2=1,80  ),
     +                        KCOM,(COMENT(I3), I3=1,100 ),
     +                        TBEGON, TENDON, KOUTON,
     +                        ( SPISON(I4), I4=1,55 )
       KXX = 300
       READ(NF, END=50,ERR=51) SPISNN,
     +                         (SPISVL(I3), I3=1,16),
     +                         (SPISTK(I4), I4=1,16)
       KXX = 66600
       CALL READMB( NF,   DMSWAL, MSWALL, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
       CALL READMB( NF,   NNTOK,  TOK, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
       CALL READMB( NF,   NSYS, VALL, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       WRITE(NWTRE, 101 ) TN, NZAPSP
C$$
       IRC = 0
       CLOSE (NF)
       RETURN
100    FORMAT(/' *** C�㦡� "K.T." -> #BOCT_��������� � ������ #',
     =         I3,' ***')
101    FORMAT(' *** C����A "K.T." -> "K.T." ���⠭������  ( tn=',G12.5,
     = ' NZAPSP=', I6,' ) ***'/)
C----------------------------------------------------------------------
50     CONTINUE
       KX = 1 + KXX
       GO TO 52
51     CONTINUE
       KX = 2 + KXX
52     WRITE(NWTR, 103) NF, KX
       WRITE(NWTRE, 103) NF, KX
103    FORMAT(/' *** B��⠭������� � ������ # ', I2, ' ����������',
     =  ' KO�=', I8, ' ***'/)
C+---------------------------------------------------------------------
       IRC = 1
       CLOSE (NF)
       RETURN
       END
