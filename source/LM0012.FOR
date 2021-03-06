       SUBROUTINE LM0012( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA: LM0012( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�E� T�� 999:
C   ----------    ***  MEXAH��ECKA� C�CTEMA ***
C
C   METO�:        �O�CK B MSW_ALL O��CAH�� ��EMEHTOB , A�PECOB �A�AHH�X
C   -----         TOKOB ��EMEHTOB B MACC�AX TOKN/TOKN1.
C                 B C���AE HAXO��EH�� -> �O�OPM�POBAH�E MSW_O��CAH��;
C                 B �TOM C���AE KOD = 0, �HA�E = 1;
C
C   �APAMETP�:    MSW(RMSW) - "OTPE�OK" MSW_MO�E�� T�� # 999;
C   ----------    ADR - A�PEC B MSW_ALL MSW_O��CAH�� �TO� MO�E��;
C                 MSWALL(RMSWAL) - BEC� C��COK MSW C�OB;
C                 KOD - = 0 - C�OPM�POBA�� MSW,
C                       = 1 - �O�. O���K�;
C                      ( HA NWTR COO��EH�E B�BO��TC� );
C   �O��PO�PAMM�: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C              �OPMAT RMSW T�� # 999:
C              ----------------------
C
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II KO�-BO I KO�-BO I # �BEHAI # ��-TAI # CME- I A�PEC  I ...==>
C     II �BEH�EBI CB��E� I MEX.   I ��.    I �EH��  I   B    I ...==>
C     II        I C ��.�.I �E��   I �E��   I        I TOKN/N1I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   1    I    2   I   3    I   4    I   5    I   6    I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   9    I   10   I   11   I   12   I   13   I   14   I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM
C----------------------------------------------------------------------
C-->  HA�A��...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
       IF( TIP .NE. 999 ) GO TO 9999
C----------------------------------------------------------------------
C--> CKO��KO CB��E� C ��EKTP��ECKO� �E��� ?
C----------------------------------------------------------------------
       KELC = MSW(10)
       IF( KELC .LE. 0 ) RETURN
C----------------------------------------------------------------------
C--> ��� KA��O� CB��� ��EM...
C----------------------------------------------------------------------
          DO 15 I= 1 , KELC
          NELMOT = MSW(8 + 4*I)
          NSM    = MSW(9 + 4*I)
C----------------------------------------------------------------------
C-->  �CKAT� B MSWALL ��EMEHT C #��-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HA��� ��� HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C-->  ��EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 500) NEL, TIP, NELMOT
500    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     =  ' ( T�� # ', I3, ' ) ***'/
     =  ' *** C HE C��ECTB����M B �E�� ��EMEHTOM # ', I5,' ***'/
     =  ' *** O�PA�OTKA �E�� �PEKPA�AETC� *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA���, ECT� �� � ��EMEHTA TPE��EM�� TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 3
C----------------------------------------------------------------------
C-->  CB��� C TOKOM , KOTOPO�O HE C��ECTB�ET � �A�AHHO�O ��EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,501) NEL, TIP, NELMOT, NSM
501    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, ' ( CME�EH�E',I3, ' ) - O���O�EH',
     = ' ��EMEHT HE �MEET TPE��EMO�O TOKA ***'/
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
3      CONTINUE
          MSW(10+4*I) = ATOKM + NSM - 1
15     CONTINUE
C######################################################################
C----------------------------------------------------------------------
C--> �AKOH����!
       KOD = 0
       RETURN
C======================================================================
C!!!-------------------------------------------------------------------
9999   CONTINUE
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 555) TIP, NEL
555      FORMAT(' *** �O�. C�O� B LM0012: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
       RETURN
       END
