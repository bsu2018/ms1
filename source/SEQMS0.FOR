         SUBROUTINE SEQMS0( MSWALL, NEL, IMSW, IRC, NSM, ITOK )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   SEQMS0( MSWALL, NEL, IMSW, IRC, NSM, ITOK )
C   ------------
C   HA‡HA—EHˆE:    POCMOTP MSWALL CˆCKA, OˆCK ‹EMEHTA C HOMEPOM NEL;
C   -----------    Pˆ “„A—E: OPE„E‹EHˆE NSM - MAX BO‡MO†HOƒO
C                  "CME™EHˆŸ" (KO‹-BO B›B. ‡HA—EHˆ‰), ITOK - A„PECA
C                  B MACCˆBAX TOKN ˆ TOKN1 XPAHEHˆŸ TˆX EPEMEHH›X,
C                  A TAK †E A„PECA IMSW ‡AˆCˆ ‹EMEHTA B MSWALL;
C   METO„:         OˆCK.
C   -----
C   APAMETP›:     MSWALL - C‹OBA COCTOŸHˆŸ ‹EMEHTOB;
C   ---------      NEL - HOMEP ‹EMEHTA;
C                  IMSW - A„PEC ‡AˆCˆ O ‹EMEHTE B MSWALL;
C                  NSM - "CME™EHˆE" MAX;
C                  ITOK - A„PEC "TOK" B TOKN1 ˆ TOKN;
C                  IRC = 1 - HA˜‹ˆ ‹EMEHT;
C                      = 2 - ‹EMEHTA HET B MSWALL;
C   O„POƒPAMM›:  HET
C   ------------
C   PˆME—AHˆE:
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION MSWALL(1)
         INTEGER ADR
         IRC = 2
         ADR = 6
900      I1 = MSWALL(ADR)
         IF ( I1 .EQ. 0 ) RETURN
         IMSW = ADR + 1
         ADR = I1
C---> KTO TAM?
         IF ( MSWALL(IMSW) .NE. NEL ) GO TO 900
C--->   HA˜‹ˆ...
         IRC = 1
         NSM = MSWALL(IMSW+5)
         ITOK = MSWALL(IMSW+6)
         RETURN
         END
