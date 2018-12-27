           SUBROUTINE SOLVE0( NDIM, N, A, B, IPVT, IRC)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:  SOLVE0( NDIM, N, A, B, IPVT, IRC)
C   -------------
C
C   HAáHAóEHàE:    PEòEHàE CàCTEMõ: A * X = B   C îAKTOPàáOBAHHOâ MAT-
C   ----------     PàñEâ A ( L _ U PAáãOÜEHàE èPOBEÑEHO è/è DECOM0 );
C
C   METOÑ:         èPüMOâ à OÅPATHõâ XOÑ "ÉAìCCA";
C   -----
C
C   èAPAMETPõ:     NDIM - "áAüBOóHAü" PAáMEPHOCTú MATPàñõ A;
C   ---------      N - PAáMEPHOCTú PEòAEMOâ CàCTEMõ ( OèPEÑEãüET TAK ÜE
C                   PAáMEPHOCTà BCEX àCèOãúáìEMõX MACCàBOB - BEKTOPOB )
C                  B - BEKTOP èPABõX óACTEâ CàCTEMõ; èOCãE BõèOãHEHàü
C                      SOLVE0 - BEKTOP PEòEHàü ( èPà IRC = 0 );
C                  IPVT - BEKTOP BEÑ. ùãEMEHTOB, èOãìóEHHõâ àá  DECOM0;
C                  IRC = 0 - CàCTEMì PEòàãà;
C                  IRC ^=0 = K - BõPOÜÑEHàE èPà OÅPAÅOTKE A(K,K),
C                                èPà ùTOM PEòEHàE èPEPõBAETCü -
C                                BEKTOP B - "àCèOPóEH";
C
C
C   èOÑèPOÉPAMMõ:  HET;
C   ------------
C
C  èPàMEóAHàE:     CM. è/è   DECOM0;
C  ----------
C
C
CZZZ-------------------------------------------------------------------
         DIMENSION A(1), B(1), IPVT(1)
         INTEGER NDIM, N, IPVT, KB, KM1, NM1, KP1, I, K, M
         REAL A, B, T
         ID(I,J) = NDIM * ( J - 1 ) + I
C
C---> èPüMOâ XOÑ
C
         IF ( N .EQ. 1 ) GO TO 50
         NM1 = N - 1
         DO 20 K = 1, NM1
            KP1 = K + 1
            M = IPVT(K)
            T = B(M)
            B(M) = B(K)
            B(K) = T
            DO 10 I=KP1, N
               B(I) = B(I) + A( ID(I,K) ) * T
10          CONTINUE
20       CONTINUE
C
C---->  OÅPATHAü èOÑCTAHOBKA
C
         DO 40 KB = 1, NM1
            KM1 = N - KB
            K = KM1 + 1
            AKKX = A( ID(K,K) )
            IF ( AKKX .EQ. 0.0 ) GO TO 100
            B(K) = B(K) / AKKX
            T = - B(K)
            DO 30 I = 1, KM1
               B(I) = B(I) + A( ID(I,K) ) * T
30          CONTINUE
40       CONTINUE
50       B(1) = B(1) / A(1)
         IRC = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
100      CONTINUE
         IRC = K
         RETURN
         END
