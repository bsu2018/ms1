         SUBROUTINE SEQ1M( X1,  X2, K, IRC )
CXXX----KOM��EKC "MS1" --
C   �O�PO�PAMMA:    SEQ1M( X1,  X2, K, IRC )
C   ------------
C   HA�HA�EH�E:     CPABHEH�E MACC�BOB X1 � X2 ( K-C�OB);
C   -----------     IRC = 1 - COB�A�A�T, = 2 - HET;
C                 ����� �६��� ���� ����� ���. � ���⪠ 0 ���.
C
C   METO�:           (*** A�TEPHAT�BA K SEQ0 )
C   -----
C   �APAMETP�:     CM. HA�HA�EH�E;
C   ----------
C   �O��PO�PAMM�:  HET
C   -------------
C   �P�ME�AH�E:
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION X1(1),  X2(1)
         INTEGER X1, X2
         INTEGER HXX, MXX, SXX, MSXX, YYY, MYY, DYY
         WRITE(99,*) '[0;0H[K'
         IRC = 1
         DO 1 I=1,K
         IF ( X1(I) .EQ. X2(I) ) GO TO 1
         IRC = 2
         GO TO 2
1        CONTINUE
C2        RETURN
2        IF ( IRC .EQ. 1 ) THEN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C====>  ��KCA��� �AT� � BPEMEH� BBO�A �H�OPMA���
         CALL GETTIM(HXX,MXX,SXX,MSXX)
         CALL GETDAT(YYY,MYY,DYY)
         YYY = MOD ( YYY,100 )
C######################################################################
         WRITE(99, 9700) DYY,MYY,YYY,HXX,MXX,SXX, (X1(I),I=1,K)
9700     FORMAT('[11;0H', '[K',
     ='  START :    DATE ',2(I2.2,'/'),I2.2,
     ='   TIME ',2(I2.2,':'),I2.2,'.',
     ='      �믮������ ��४⨢� ',8A1 )
C######################################################################
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ENDIF
         RETURN
         END
