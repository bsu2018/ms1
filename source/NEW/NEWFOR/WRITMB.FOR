        SUBROUTINE WRITMB( spool, lmasi, mas )
CXXX----KOMПЛEKC "MS1" --> FOR DOS                          -----------
C   ПOДПPOГPAMMA:   WRITMB( spool, lmasi, mas )
C   -------------
C   HAЗHAЧEHИE:    ЗАПИСЬ  MACCИBA mas длиной lmasi c файла spool
C   ----------
C   METOД:
C   -----
C   ПAPAMETPЫ:
C   ---------
C
C                  OДHA ЗAПИCЬ -> 1024   байт потому что 1 кластер
C                  ПРИ нулевой длине - запист одного слова
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