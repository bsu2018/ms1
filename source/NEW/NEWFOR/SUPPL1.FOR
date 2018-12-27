       SUBROUTINE  SUPPL1 (N ,TJN1,TJN2 ,TJ1 ,TJ2, G )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (AT OCTOBER   1989 É.)-----------
C   èOÑèPOÉPAMMA: SUPPL1(N ,TJN1,TJN2, TJ1 ,TJ2, G )
C   -------------
C   HAáHAóEHàE:  BõóàCãüET OÅôàâ àCTOóHàK TOKA 2**N èOCãEÑOBATEãúHO
C   ----------   èOÑCEÑàHEHHõX TPEX-èOãìCHàKAMà C àCTOóHàKAMà TOKA
C                TJN1 , TJN2
C                PEáìãúTAT B TJ1, TJ2
C                BCèOMOÉATEãúHõE áHAóEHàü G - MATPàñA èPOBOÑàMOCTà
C
C                OÅCãìÜàBAET èOÑèPOÉPAMMA DM0039
C
CZZZ--------------------------------------------------------------------
       DIMENSION TJN1(1), TJN2(1),G(1)
C--> ÉPìèàPìEM èO CTEèEHüM ÑBOâKà
       DO 10 K =1,N
       M = 2**(N-K)
       AB = G(2*K)/(2*G(2*K - 1))
       DO 10 I = 1,M
       A = AB*(TJN2(2*I-1) + TJN1(2*I))
       TJN1(I) = TJN1(2*I-1) - A
       TJN2(I) = TJN2(2*I) - A
10     CONTINUE
       TJ1 = TJN1(1)
       TJ2 = TJN2(1)
       RETURN
       END
