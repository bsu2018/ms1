       INTEGER FUNCTION PLOM01( I, J, RAB )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA_”“HK–ˆŸ      PLOM01( I, J, RAB )
C   -----------
C   HA‡HA—EHˆE:   CPABHEHˆE  I ˆ J ‹EMEHTA OˆCAHˆŸ MACCˆB› "RAB":
C   -----------    ( CM. PLOMXY )  ->  „PEBECHAŸ COPTˆPOBKA MACCˆABA
C                                      OTOPA†EHˆŸ;
C   METO„:        ‹EMEHT CTP“KT“P› B RAB -> 2-C‹OBA: IX,IY;
C   -----
C   APAMETP›:    I, J - HOMEPA CPABHˆBAEM›X   TO—EK (IX,IY) B RAB;
C   ---------      RAB - MACCˆB OTOPA†EHˆŸ „‹Ÿ PLOMXY;
C   O„POƒPAMM›:  HET;
C   -------------
C   PˆME—AHˆE:   PLOM01 -> -1  (I<J); =0 (I=J);  =+1 (I>J);
C   ----------    CM.  "„PEB. COPTˆPOBKA";
CZZZ-------------------------------------------------------------------
       DIMENSION RAB(1)
       INTEGER RAB, XI, XJ
CVVVVV INTEGER  PLOM01
       XI = RAB( 2*I - 1 )
       XJ = RAB( 2*J - 1 )
        IF ( XI - XJ ) 1, 2, 3
1      PLOM01 = -1
       RETURN
2      PLOM01 = 0
       RETURN
3      PLOM01 = +1
       RETURN
       END
