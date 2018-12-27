       SUBROUTINE PLOM00( I, J, RAB )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  PLOM00( I, J, RAB )
C   -----------
C   HA‡HA—EHˆE:   EPECTAHOBKA I ˆ J ‹EMEHTA OˆCAHˆŸ MACCˆB› "RAB":
C   -----------    ( CM. PLOMXY )  ->  „PEBECHAŸ COPTˆPOBKA MACCˆABA
C                                      OTOPA†EHˆŸ;
C   METO„:        ‹EMEHT CTP“KT“P› B RAB -> 2-C‹OBA: IX,IY;
C   -----
C   APAMETP›:    I, J - HOMEPA EPECTAB‹ŸEM›X TO—EK (IX,IY) B RAB;
C   ---------      RAB - MACCˆB OTOPA†EHˆŸ „‹Ÿ PLOMXY;
C   O„POƒPAMM›:  HET;
C   -------------
C   PˆME—AHˆE:
C   ----------
CZZZ-------------------------------------------------------------------
       DIMENSION RAB(1)
       INTEGER RAB, XI, YI
       XI = RAB( 2*I - 1 )
       YI = RAB( 2*I )
C-->   "J"  B  "I":
       RAB(2*I-1) = RAB(2*J-1)
       RAB(2*I)   = RAB(2*J)
C--->   "I"   B  "J":
       RAB(2*J-1) = XI
       RAB(2*J)   = YI
       RETURN
       END
