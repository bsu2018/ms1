         SUBROUTINE DRCIR2( BA0, KNOT, PAR0, MSWALL )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DRCIR2( BA0, KNOT, PAR0, MSWALL )
C   ------------
C   HA‡HA—EHˆE:    ”OPMˆPOBAHˆE CˆCKA MSWALL
C  ------------
C                  ( 3-Ÿ —ACTœ „ˆPEKTˆB› #–Eœ )
C   METO„:         CM. / GETNEW;
C   -----
C   APAMETP›:     CM. GETNEW;
C   ---------
C
C   O„POƒPAMM›:  1. COMMON PROF1
C   -------------  2. GETMSW
C   PˆME—AHˆE:    1. B C‹“—AE “CE˜HOƒO ”OPMˆPOBAHˆŸ MSWALL
C   ----------        BO‡“†„AETCŸ ”‹Aƒ FMSWAL
C                  2. O„POHEE CM. INPUT0
C                  3. MACCˆB MSWALL „O‹†EH ›Tœ  ˆHˆ–ˆˆPOBAHH›;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         DIMENSION BA0(1), PAR0(1), KNOT(1), MSWALL(1)
         INTEGER BA0, FMSWAL
         EQUIVALENCE ( MASPRT(13),  NWTR )
         EQUIVALENCE ( MASPRT(14),  NWTRE )
         EQUIVALENCE ( MASPRT(74),  FMSWAL )
         FMSWAL = 0
         CALL GETMSW( BA0, KNOT, PAR0, MSWALL, IER )
         FMSWAL = 1
         IF ( IER    .NE. 0   ) WRITE( NWTR, 100 )
         IF ( IER    .NE. 0   ) FMSWAL = 0
100     FORMAT(//' *** ‹Oƒˆ—ECKˆE O˜ˆKˆ OˆCAHˆŸ –Eˆ - –Eœ CPACABAET
     =CŸ ***'//)
C23456
         RETURN
         END
