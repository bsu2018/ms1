       SUBROUTINE LM0000( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: LM0000( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA�HA�EH�E:   �����誠
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       NWTR = MASPRT(13)
       WRITE(NWTR, 100)
100    FORMAT(//' *** LM0000: ���㯠�� ���� ���  # '//)
       CALL suberr(0)
       RETURN
       END
