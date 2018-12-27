         SUBROUTINE SEQ1M( X1,  X2, K, IRC )
CXXX----KOM‹EKC "MS1" --
C   O„POƒPAMMA:    SEQ1M( X1,  X2, K, IRC )
C   ------------
C   HA‡HA—EHˆE:     CPABHEHˆE MACCˆBOB X1 ˆ X2 ( K-C‹OB);
C   -----------     IRC = 1 - COBA„AžT, = 2 - HET;
C                 ¥ç âì ¢à¥¬¥­¨ áâ àâ  ¨¬¥­¨ ¤¨à. ¨ ®ç¨áâª  0 áâà.
C
C   METO„:           (*** A‹TEPHATˆBA K SEQ0 )
C   -----
C   APAMETP›:     CM. HA‡HA—EHˆE;
C   ----------
C   O„POƒPAMM›:  HET
C   -------------
C   PˆME—AHˆE:      99 ------> *
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION X1(1),  X2(1)
         INTEGER X1, X2
         INTEGER HXX, MXX, SXX, MSXX, YYY, MYY, DYY
!!!!         WRITE(*,*) ' '
!!!         WRITE(99,*) '[0;0H[K'
         IRC = 1
         DO 1 I=1,K
         IF ( X1(I) .EQ. X2(I) ) GO TO 1
         IRC = 2
         GO TO 2
1        CONTINUE
C2        RETURN
2        IF ( IRC .EQ. 1 ) THEN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C====>  ”ˆKCA–ˆŸ „AT› ˆ BPEMEHˆ BBO„A ˆH”OPMA–ˆˆ
         CALL GETTIM_(HXX,MXX,SXX,MSXX)
         CALL GETDAT_(YYY,MYY,DYY)
         YYY = MOD ( YYY,100 )
C######################################################################
         WRITE(*, 9700) DYY,MYY,YYY,HXX,MXX,SXX, (X1(I),I=1,K)
!!!9700     FORMAT('[11;0H', '[K',
9700     FORMAT(' ',
     ='  START :    DATE ',2(I2.2,'/'),I2.2,
     ='   TIME ',2(I2.2,':'),I2.2,'.' :
     ='      ‚ë¯®«­¥­¨¥ ¤¨à¥ªâ¨¢ë ',8A1 )
C######################################################################
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ENDIF
         RETURN
         END
