         SUBROUTINE READMB( spool, lmasi, mas, irc )
CXXX----KOM‹EKC "MS1" --> FOR DOS                          -----------
C   O„POƒPAMMA:   READMB( spool, lmasi, mas, irc)
C   -------------
C   HA‡HA—EHˆE:    —TEHˆE  MACCˆBA mas ¤«¨­®© lmasi c ä ©«  spool
C   ----------
C   METO„:
C   -----
C   APAMETP›:     irc = 0  - ¯à®ç¨â «¨
C   ---------      irc = 1  - ¢áâà¥â¨«áï ŠŽ…– ( ¬®å­ âë©)
C                  irc = 2 -  ®è¨¡ª¨
C                  O„HA ‡AˆCœ -> 1024 ¯®â®¬ã çâ® ®¤¨­ ª« áâ¥à
C                  ¯à¨ ¤«¨­¥ = 0 ç¨â ¥¬ ®¤­® á«®¢®
CZZZ-------------------------------------------------------------------
         DIMENSION mas(*)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         integer spool
         data lzap/256/
         lmas = lmasi
         IF ( lmasi .LE. 0 ) lmas = 1
         if (lmas .GT. lzap ) go to 100
         READ(SPOOL, END=20, ERR=21)  (mas(I), I=1,lmas)
         irc = 0
         RETURN
C@@@@@@@
100      continue
         k = lmas/lzap
         k0 = k*lzap
         k1 = lmas - k0
         do 1 i=1,k0,lzap
1           READ(SPOOL, END=20, ERR=21)  (mas(j), j=i,lzap+i-1)
         if (k1 .NE. 0 )
     +          READ(SPOOL, END=20, ERR=21)  (mas(j), j=k0+1,lmas)
         irc = 0
         RETURN
C@@@@
20       irc = 1
         RETURN
21       irc = 2
         RETURN
         END
