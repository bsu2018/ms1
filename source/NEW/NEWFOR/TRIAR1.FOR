         SUBROUTINE TRIAR1(N, ECHANG, COMPCL,   BA0)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   TRIAR1(N, ECHANG, COMPCL,   BA0)
C   -------------
C   HA‡HA—EHˆE:    "„PEBECHAŸ" BOCXO„Ÿ™AŸ COPTˆPOBKA
C  -----------      ( OPEEHTˆPOBAHHAŸ HA COPTˆPOBK“ BA0 CˆCKA )
C
C   METO„:         ”‹O‰„A - “ˆ‹œŸMCA ( CM.  ME‰EP ., O„“H K.,
C   -----           METO„› POƒPAMMˆPOBAHˆŸ, M. "MˆP" T2, CTP. 181 )
C
C   APAMETP›:     N - KO‹ˆ—ECTBO O'EKTOB ( ‹EMEHTOB –Eˆ );
C   ---------      ECHANG (I,J) - ˆMŸ / B›O‹HŸž™E‰ EPECTAHOBK“
C                                  I  ˆ  J  O'EKTA;
C                  COMPCL (I,J) - ˆMŸ FUNCTION CPABHEHˆŸ K‹ž—E‰:
C                         = -1  K‹ž—œ B I  MEHœ˜E  K‹ž—A  B J
C                         =  0  K‹ž—œ B I  PABEH   K‹ž—“  B J
C                         = +1  K‹ž—œ B I  O‹œ˜E  K‹ž—A  B J
C
C
C   O„POƒPAMM›:  1. REORG1 - "PEOPƒAHˆ‡A–ˆŸ"
C   -------------
C
C   PˆME—AHˆE:    1. MACCˆB BA0 BBE„EH B ”OPM. APAMETP› T.K. OH HE
C  ------------       ŸB‹ŸETCŸ OCTOŸHHO ( CTATˆ—ECKˆ ) B›„E‹EHH›M;
C                  2. „“‹œ POƒPAMM› TRIARB ( / REORG1 „“‹œ REORG)
CZZZ-------------------------------------------------------------------
         EXTERNAL ECHANG, COMPCL
         INTEGER COMPCL
         DIMENSION BA0(1)
C--> "OCA„KA" „EPEBA
         I = N/2
100      CONTINUE
         IF ( I .LE. 0 ) GO TO 200
C-->  PEOPƒAHˆ‡A–ˆŸ
         CALL REORG1(I, N, ECHANG, COMPCL, BA0 )
         I = I - 1
         GO TO 100
C-->  COPTˆPOBKA MAX-“™EƒO „EPEBA
200      CONTINUE
         I = N
300      CONTINUE
         IF ( I .LE. 1 ) GO TO 1000
C--> EPECTAB‹ŸEM..
         CALL ECHANG( 1,  I,  BA0)
         I = I- 1
         CALL REORG1(1, I, ECHANG, COMPCL, BA0 )
         GO TO 300
C--->
1000     RETURN
         END
