         SUBROUTINE FM0000( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C
C
C   èOÑèPOÉPAMMA:  FM0000( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HAáHAóEHàE:    á†£´„Ë™†
C  ------------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
         WRITE(NWTR, 500) TIP, NEL
500      FORMAT(' *** ãOÉ. CÅOâ B FM0000: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
