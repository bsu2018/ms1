         SUBROUTINE DECOM0( NDIM, N, A, COND, IPVT, WORK )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DECOM0( NDIM, N, A, COND, IPVT, WORK )
C   -------------
C
C   HA‡HA—EHˆE:    OPE„E‹ŸET L_U PA‡‹O†EHˆE BE™ECTBEHHO‰ MATPˆ–› A  ˆ
C   ----------      O–EHˆBAET EE O“C‹OB‹EHHOCTœ;
C
C   METO„:         ƒA“COBO ˆCK‹ž—EHˆE O CTO‹–AM C —ACTˆ—H›M B›OPOM
C   -----           BE„“™EƒO ‹EMEHTA;
C
C   APAMETP›:     NDIM - ‡AŸBO—HAŸ "CTPO—HAŸ" PA‡MEPHOCTœ MATPˆ–› A;
C   ---------      N - OPŸ„OK CˆCTEM› ( MATPˆ–› A ˆ BCEX BEKTOPOB );
C                  COND - O–EHKA O“C‹OB‹EHHOCTˆ MATPˆ–› A:
C                    EC‹ˆ  "COND .EQ. COND+1" --> TO MATPˆ–A ‹ˆ‡KA K
C                    B›PO†„EHHO‰; Pˆ TO—HOM B›PA†„EHˆˆ COND = 1.0E+32;
C                    EC‹ˆ Pˆ OPA™EHˆˆ K DECOM0 "COND=-1.0" --> O–EHKA
C                    O“C‹OB‹EHHOCTˆ HE B›—ˆC‹ŸETCŸ -> KOHTPO‹Ÿ HET!!!!!
C                  A - ˆCXO„HAŸ MATPˆ–A CˆCTEM› " A * X = B ";
C                      OC‹E B›O‹HEHˆŸ DECOM0 - CO„EP†ˆT:
C                      L _ U PA‡‹O†EHˆE A, C TO—HOCTœž „O EPECTAHOBOK;
C                  IPVT - BEKTOP BE„“™ˆX ‹EMEHTOB:
C                     IPVT(K) - ˆH„EKC K-‰ BE„“™E‰ CTPOKˆ;
C                     IPVT(N) = (-1)**<—ˆC‹O EPECTAHOBOK>;
C                  WORK - PAO—ˆ‰ BEKTOP;
C
C   O„POƒPAMM›:  HET
C  --------------
C
C
C   PˆME—AHˆE:  1. „‹Ÿ PE˜EHˆŸ CˆCTEM›, OC‹E „EKOMO‡ˆ–ˆˆ MATPˆ–› A,
C   ----------      HEOXO„ˆMO ˆCO‹œ‡OBATœ / SOLVE0;
C                2. OPE„E‹ˆTE‹œ DET(A) =
C                      IPVT(N)*A(1,1)*A(2,2)*...*A(N,N);
C                3. BXO„ B MATPˆ–“ A(I,J)==> —EPE‡  A( ID(I,J) ) :
C                   ID(I,J) = NDIM*(J-1) + I
C
CZZZ-------------------------------------------------------------------
         DIMENSION A(1), WORK(1), IPVT(1)
         REAL A, COND, WORK
         INTEGER NDIM, N, IPVT
C--->
         REAL EK, T, ANORM, YNORM, ZNORM
         INTEGER NM1, I, J, K, KP1, KB, KM1, M
C--->
         ID(I,J) = NDIM * ( J - 1 ) + I
C--->
         IPVT(N) = +1
         IF ( N .EQ. 1 ) GO TO 80
         NM1 = N - 1
C
         IF (COND .EQ. -1.0 ) GO TO 100
C--> B›—ˆC‹ˆTœ 1-HOPM“ MATPˆ–› A
         ANORM = 0.0
         DO 10 J=1,N
               T = 0.0
               DO 5 I=1,N
                   T = T + ABS( A( ID(I,J) ) )
5              CONTINUE
              IF ( T .GT. ANORM ) ANORM = T
10      CONTINUE
C
100      CONTINUE
C
C---> ƒA“COBO ˆCK‹ž—EHˆE C —ACTˆ—H›M B›OPOM BE„“™EƒO ‹EMEHTA
C
         DO 35 K=1, NM1
            KP1 = K + 1
C
C---> HA‰Tˆ BE„“™ˆ‰ ‹EMEHT
C
            M = K
            DO 15 I=KP1, N
               IF ( ABS( A( ID(I,K) ) ) .GT. ABS( A( ID(M,K) ) ) )
     =                      M = I
15          CONTINUE
         IPVT(K) = M
         IF ( M .NE. K ) IPVT(N) = - IPVT(N)
         KK = ID(K,K)
         MK = ID(M,K)
         T = A(MK)
         A(MK) = A(KK)
         A(KK) = T
C
C--->    PO“CK C‹E„. ˜AƒA, EC‹ˆ BE„“™ˆ‰ ‹EMEHT BCE †E PABEH 0
C
         IF ( T .EQ. 0.0 ) GO TO 35
C
C--->    B›—ˆC‹EHˆE MHO†ˆTE‹E‰
C
         DO 20 I=KP1, N
            IK = ID(I,K)
            A(IK) = - A(IK) / T
20       CONTINUE
C
C---->   EPPECTAB‹ŸTœ ˆ ˆCK‹ž—ATœ O CTO‹–AM
C
            DO 30 J=KP1, N
               MJ = ID(M,J)
               KJ = ID(K,J)
               T = A(MJ)
               A(MJ) = A(KJ)
               A(KJ) = T
               IF ( T .EQ. 0.0 ) GO TO 30
               DO 25 I = KP1, N
                  IJ = ID(I,J)
                  IK = ID(I,K)
                  A(IJ) = A(IJ) + A(IK) * T
25             CONTINUE
30          CONTINUE
35       CONTINUE
C
C----------------------------------------------------------------------
         IF ( COND .EQ. -1.0 ) RETURN
C----------------------------------------------------------------------
C
C---> COND = <1-HOPMA MATPˆ–› A> * < O–EHKA „‹Ÿ 1-HOPMA MATPˆ–› A**(-1)>
C     O–EHKA HAXO„ˆTCŸ OCPE„CTBOM O„HOƒO ˜AƒA METO„A OPATH›X
C     ˆTEPA–ˆ‰ „‹Ÿ HAˆMEHœ˜EƒO CˆHƒ“‹ŸPHOƒO BEKTPA, „‹Ÿ —EƒO:
C     PE˜EHˆE „B“X CˆCTEM “PABHEHˆ‰:
C        T
C      A   * Y = E
C
C      A   * Z = Y
C
C      ƒ„E E - BEKTOP ˆ‡ +1 ˆ -1, B›PAHH›‰ TAK, —TO› [Y] --> MAX.
C     (GALIMOTXQ)
C             T
C---> PE˜AEM A  * Y  =  E
C
         DO 50 K=1,N
            T = 0.0
            IF ( K .EQ. 1 ) GO TO  45
            KM1 = K - 1
            DO 40 I = 1, KM1
               T = T + A( ID(I,K) ) * WORK(I)
40          CONTINUE
45          EK = 1.0
            IF ( T .LT. 0.0 ) EK = -1.0
            KK = ID(K,K)
            IF ( A(KK) .EQ. 0.0 ) GO TO 90
            WORK(K) = - ( EK + T ) / A(KK)
50       CONTINUE
C----->
         DO 60 KB = 1, NM1
            K = N - KB
            T = 0.0
            KP1 = K + 1
            DO 55 I = KP1, N
               T = T + A( ID(I,K) ) * WORK(K)
55          CONTINUE
            WORK(K) = T
            M = IPVT(K)
            IF ( M .EQ. K ) GO TO 60
            T = WORK(M)
            WORK(M) = WORK(K)
            WORK(K) = T
60       CONTINUE
C--->
         YNORM = 0.0
         DO 65 I =1, N
            YNORM = YNORM + ABS( WORK(I) )
65       CONTINUE
C
C---->  PE˜ˆTœ  A * Z = Y
C
         CALL SOLVE0( NDIM, N, A, WORK, IPVT, IRC )
C
         ZNORM = 0.0
         DO 70 I=1, N
            ZNORM = ZNORM + ABS( WORK(I) )
70       CONTINUE
C
C--->  O–EHˆTœ "O“C‹OB‹EHHOCTœ"
C
         COND = ANORM * ZNORM / YNORM
         IF ( COND .LT. 1.0 ) COND = 1.0
C
         RETURN
C
C----> C‹“—A‰ MATPˆ–›  1*1
C
80       CONTINUE
         IF ( COND .EQ. -1.0 ) RETURN
         COND = 1.0
         IF ( A(1) .NE. 0.0 ) RETURN
C
C--->  "TO—HOE" B›PO†„EHˆE
C
90       COND = 1.0E+32
         RETURN
         END
