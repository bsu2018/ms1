        SUBROUTINE WRITMB( spool, lmasi, mas )
CXXX----KOM��EKC "MS1" --> FOR DOS                          -----------
C   �O��PO�PAMMA:   WRITMB( spool, lmasi, mas )
C   -------------
C   HA�HA�EH�E:    ������  MACC�BA mas ������ lmasi c 䠩�� spool
C   ----------
C   METO�:
C   -----
C   �APAMETP�:
C   ---------
C
C                  O�HA �A��C� -> 1024   ���� ��⮬� �� 1 ������
C                  ��� �㫥��� ����� - ������ ������ ᫮��
CZZZ-------------------------------------------------------------------
         DIMENSION mas(*)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         integer spool
         data lzap/256/
         lmas = lmasi
         IF (lmasi .LE. 0 ) lmas = 1
         if (lmas .GT. lzap ) go to 100
         WRITE(SPOOL)  (mas(I), I=1,lmas)
         RETURN
C@@@@@@@
100      continue
         k = lmas/lzap
         k0 = k*lzap
         k1 = lmas - k0
         do 1 i=1,k0,lzap
1          WRITE(SPOOL)  (mas(j), j=i,lzap+i-1)
         if (k1 .NE. 0 )
     +         WRITE(SPOOL)  (mas(j), j=k0+1,lmas)
         RETURN
         END
