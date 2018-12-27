         BLOCK DATA
C====>  SERVZ2  <======================================================
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C
C  TAÅãàñA àMEH èEPEMEHHõX "TABTOK":
C   COMMON / SERVZ2 /   TABTOK(1)
C
C+------------+----------------------+
CI KOÑ BXOÑA  I    àMü èEPEMEHHOâ    I
CI            I[ PAáMEP 3 CãOBA  ]   I
CI KOÑ àMEHà  I                      I
CI            I( XPAHEHàE èãOTHOE )  I
CI èEPEMEHHOâ I                      I
C+------------+----------------------+
CI            I     1   2   3        I
C+------------+----------------------+
CI    1       I        TOK           I
CI    2       I       GAMMA          I
CI    3       I       OMEGA          I
CI    4       I  ùã.MAÉ.MOMEHT [MùM] I
CI    5       I  M_BH.               I
CI    6       I  CàÉHAã ìèPABãEHàü   I
CI    7       I  U_ ìèPAB. "PB"      I
CI    8       I  OMEÉA_CETà_OèOPHAü  I
CI    9       I  CKOãúÜEHàE          I
CI   10       I  U_ìèP. Q_CàHXPOHHOE I
CI   11       I  U_ìèP. D_CàHXPOHHOE I
CI   12       I  TOK BEHTàãü         I
CI** 13 ****  I    [??]              I   !!!  13  BCEÉÑA  "[??]" !!!
CI   14       I  HAèPüÜEHàE          I
CI   15       I  MOÑìãú CìMMõ -> ABC I
CI   16       I  ìÉOã îAáA "A"<->V_M.I
CI   17       I  V_X HA "A".         I
CI   18       I  V_Y HA "A+90 ÉPAÑ." I
CI   19       I  U_S_Q_OèOP. (ùã.MAò)I
CI   20       I  U_S_D_OèOP. (ùã.MAò)I
CI   21       I  I_S_Q_OèOP. (ùã.MAò)I
CI   22       I  I_S_D_OèOP. (ùã.MAò)I
CI   23       I  U_R_Q_OèOP. (ùã.MAò)I
CI   24       I  U_R_D_OèOP. (ùã.MAò)I
CI   25       I  I_R_Q_OèOP. (ùã.MAò)I
CI   26       I  I_R_D_OèOP. (ùã.MAò)I
CI   27       I  ù.Ñ.C. PEÉ. BOáÅìÜ. I
CI   28       I  OMEÉA MOÑìãü -> ABC.I
CI   29       I  MÉH. MOôHOCTú       I
CI   30       I  I_S_D_áAÑ._ PB.     I
CI   31       I  S_áAÑ._ PB.         I
CI   32       I  INT( ISD ) _ PB.    I
CI   33       I  INT( S ) _ PB.      I
CI   34       I  èEPEMEôEHàE         I
CI   35       I  ãàH. CKOPOCTú       I
CI   36       I  ùã. MAÉH. CàãA      I
CI   37       I  BHEòHüü HAÉP. CàãA  I
CI   38       I  KOÑ Càì             I
CI   39       I  òAÉ àáMEHEHàü Càì   I
CI   40       I  BõXOÑHOâ CàÉHAã     I
CI   41       I  AKTàBHAü MOôHOCTú   I
CI            I  KOMèEHCàPìEMOâ HAÉP.I
CI   42       I  OÅMEHHAü MOôHOCTú   I
CI            I  KOMèEHCATOPA        I
CI   43       I  BPEMü àáMEHEHàü BõX.I
CI            I  CàÉHAãA             I
CI   44       I  POH MàKPOèPOñECCOPA I
CI   45       I  CCè MàKPOèPOñECCOPA I
CI   46       I  COÑEP. èOPTA B/B    I
CI   47       I  OÅOÅôEHHAü KOOPÑ.   I
C+------------+----------------------+
C    èEóATú àá TABTOK(IX) èO îOPMATì 3A4  [ 12 CàMBOãOB ]
C
C  BXOÑ B TABTOK èO KOD_NAME:
C
C  IX = (KOD_NAME - 1) * 3  +  1
CZZZ-------------------------------------------------------------------
         COMMON /SERVZ2/   TABTOK(180)
         DIMENSION TABTO1(45), TABTO2(45), TABTO3(45)
         DIMENSION TABTO4(45)
         EQUIVALENCE( TABTOK(1) , TABTO1(1) )
         EQUIVALENCE( TABTOK(46), TABTO2(1) )
         EQUIVALENCE( TABTOK(91), TABTO3(1) )
         EQUIVALENCE( TABTOK(136), TABTO4(1) )
         INTEGER TABTOK, TABTO1, TABTO2, TABTO3, TABTO4
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C--->  HA  90 / 3  = 30  àMEH                                    <-----
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        DATA TABTO1/ ' TOK', '    ', '    ',
     =               'GAMM', 'A PO', 'TOPA',
     =               'OMEG', 'A PO', 'TOPA',
     =               ' M. ', 'ùã-M', 'AÉ. ',
     =               ' M. ', 'BHEò', 'Hàâ ',
     =               'CàÉH', 'Aã C', 'àì. ',
     =               'U ìè', 'P. "', 'PB".',
     =               'OMEG', ' CàC', 'TEMõ',
     =               'CKOã', 'úÜEH', 'àE. ',
     =               'U ìè', 'P. Q', ' Oè.',
     =               'U ìè', 'P. D', ' Oè.',
     =               'TOK ', 'BEHT', 'àãü.',
     =               '[??]', '[??]', '[??]',
     =               'HAèP', 'üÜEH', 'àE. ',
     =               'MOÑì', 'ãú->', ' ABC' /
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       DATA TABTO2/ 'ìÉOã', ' OT ', 'î. A',
     =              'V_X ', 'HA î', '."A"',
     =              'V_Y ', 'HA A', '+90É',
     =              'U_S_', 'Q_Oè', 'OP. ',
     =              'U_S_', 'D_Oè', 'OP. ',
     =              'I_S_', 'Q_Oè', 'OP. ',
     =              'I_S_', 'D_Oè', 'OP. ',
     =              'U_R_', 'Q_Oè', 'OP. ',
     =              'U_R_', 'D_Oè', 'OP. ',
     =              'I_R_', 'Q_Oè', 'OP. ',
     =              'I_R_', 'D_Oè', 'OP. ',
     =              'ù.Ñ.', 'C.->', '"PB"',
     =              'OMEG', 'A.MO', 'Ñìãü',
     =              'MÉH.', 'MOôH', 'OCTú',
     =              'ISD_', 'áAÑ.', '"PB"'   /
C######################################################################
       DATA TABTO3/ 'S_áA', 'Ñ. "', 'PB".',
     =              'INT(', 'ISD)', '"PB"',
     =              'INT(', 'S) "', 'PB".',
     =              'èEPE', 'MEôE', 'HàE.',
     =              'ãàH.', 'CKOP', 'OCTú',
     =              'ùã. ', 'MAÉ.', 'CàãA',
     =              'BH.H', 'AÉP.', 'CàãA',
     =              'KOÑ ', 'Càì ', '    ',
     =              'òAÉ ', 'àáM.', ' Càì',
     =              'BõX.', ' CàÉ', 'HAã ',
     =              'P KO', 'Mè.H', 'AÉP.',
     =              'Q OÅ', 'MEHH', 'Aü  ',
     =              'BPEM', 'ü àá', 'M.Cà',
     =              'POH ', 'MàK.', 'èPOñ',
     =              'CCè ', 'MàK.', 'èPOñ' /
C######################################################################
       DATA TABTO4/ 'COÑ.', 'èOPT', '.B/B',
     =              'OÅOÅ', 'ô.KO', 'OPÑ.',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....' /
C######################################################################
C
C
         END
