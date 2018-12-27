         SUBROUTINE READMX( spool, lmas, mas, irc )
CXXX----KOM‹EKC "MS1" --> FOR DOS                          -----------
C   O„POƒPAMMA:   READMX( spool, lmas, mas, irc)
C   -------------  •Ž‹Ž‘’Ž…  !!!!!!!!!!!!
C   HA‡HA—EHˆE:    —TEHˆE  MACCˆBA mas ¤«¨­®© lmas c ä ©«  spool
C   ----------
C   METO„:
C   -----
C   APAMETP›:     irc = 0  - ¯à®ç¨â «¨
C   ---------      irc = 1  - ¢áâà¥â¨«áï ŠŽ…– ( ¬®å­ âë©)
C                  irc = 2 -  ®è¨¡ª¨
C                  O„HA ‡AˆCœ -> 1024 ¯®â®¬ã çâ® ä¨à¬  RMFORT-  ¬ã..
CZZZ-------------------------------------------------------------------
         DIMENSION mas(*)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         integer spool
         data lzap/256/
         if (lmas .GT. lzap ) go to 100
         READ(SPOOL, END=20, ERR=21)
         irc = 0
         RETURN
C@@@@@@@
100      continue
         k = lmas/lzap
         k0 = k*lzap
         k1 = lmas - k0
         do 1 i=1,k0,lzap
1           READ(SPOOL, END=20, ERR=21)
         if (k1 .NE. 0 )
     +          READ(SPOOL, END=20, ERR=21)
         irc = 0
         RETURN
C@@@@
20       irc = 1
         RETURN
21       irc = 2
         RETURN
         END
