       SUBROUTINE INTBSU( KXX , irest)
CXXXX------------------------------------------------------------------
C   �O��PO�PAMMA:  INTBSU(KXX)
C   -------------
C   HA�HA�EH�E:    "�APO��HA�" �A��TA
C   ----------
C   METO�:         HE PACKP�BAETC�
C   -----
C   �APAMETP�:
C   ---------
C   �O��PO�PAMM�:          "DRMET0", ERORXX, INTPRT
C   ------------
C   �P�ME�AH�E:
C   ----------
CZZZ-------------------------------------------------------------------
       DIMENSION KXX(*)
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(1)
       EQUIVALENCE ( NWTR, MASPRT(13) )
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       COMMON /SERVZ7/ MAKMOD(3),  MAKSEQ, NAMEB
       DATA IXX / ' MS1' /, NAMEB1/'�OP�'/,nnn_1/1997673236/
       DATA nnn_2/2010028909/
C----------------------------------------------------------------------
       IF ( MAKSEQ .EQ. 0 ) GO TO 1
12     CONTINUE
       NAMEB1 = nnn_1 + nnn_2
       IF ( KXX(2) .EQ. IXX .AND.  NAMEB .EQ. NAMEB1 ) RETURN
C--->
C-->
       WRITE(NWTRE, 10 )
       WRITE(NWTR,  10 )
10     FORMAT(/////' *** H� ᠭ�樮��஢����� ࠡ�� � �������ᮬ',
     = ' ***'///// )
       CALL STOPP(666)
1      CONTINUE
C--->   HA�.  �CTAHOBK�****
C******CALL  ERORXX( 0 )
C****** CALL INTPRT
       IF ( irest .EQ. 0 ) THEN
       WRITE(NWTR, 1100)    (MAKMOD(I), I=1,3), MASPRT(21)
1100   FORMAT(//1X,79('*')/25X,3A4,31X,'B���� ',1A4/1X,79('*')  /)
       WRITE(NWTRE,11)    (MAKMOD(I), I=1,3), MASPRT(21), NAMEB
11     FORMAT(//1X,79('#')/25X,3A4,31X,'B���� ',1A4/1X,79('#')  /
     =  22X, '(P��ࠡ�⪠ �믮����� ', 1A4,' C�ࣥ��)' // )
C    =  13X, '(P��ࠡ�⪠ �믮����� ', 1A4,' C�ࣥ��',
C    =       ' � ������ �������)' // )
C*******CALL  DRMET0( 1,0)
       ENDIF
       MAKSEQ = 1
       GO TO 12
       END
