         SUBROUTINE PRINT1( MSWALL, RMSWALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  PRINT1( MSWALL )
C   ------------
C   HA�HA�EH�E:    KOHTPO��HA� �E�AT� MACC�BA
C   ----------     ��PAB�����X C�OB MAKPOMO�E�E�
C
C
C   METO�:         �E�AT� OT MSWALL(1) �O MSWALL( MSWALL(3)+10 )
C   -----
C
C   �O��PO�PAMM�: COMMON PROF1;
C  -------------
C
C  �P�ME�AH�E:
C  -----------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION MSWALL(*), RMSWALL(*)
         EQUIVALENCE ( NWTRE     , MASPRT(14) )
C
         IMAX = MSWALL(3)
C
         IM1 = IMAX + 10
C--------------------------------------------------------------------
C    ������ � ���� ������
C--------------------------------------------------------------------
         WRITE(NWTRE,1) IM1
1        FORMAT( 1X, 128('*')/'    MACC�B MSWALL OT [ 1 �O ',I5,' ]')
         WRITE(NWTRE,2) ( MSWALL(I), I=1,IM1 )
2        FORMAT(1X, 10I10/ )
         WRITE(NWTRE,3) IM1
3        FORMAT( 1X, 128('*')/'    MACC�B RMSWALL OT [ 1 �O ',I5,' ]')
         WRITE(NWTRE,4) ( RMSWALL(I), I=1,IM1 )
4        FORMAT(1X, 10E10.4/ )
         WRITE(NWTRE,5)
5        FORMAT( 1X, 128('*') )
         RETURN
         END
