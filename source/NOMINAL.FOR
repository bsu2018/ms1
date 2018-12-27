CXXX****äOMèãEäC "MS1" --> BEPCàü 1.0  (OT ÑEäAÅPü 1986 É.)************ 
C  èPOÉPAMMA: NOMINAL							
C -----------								
C HAáHAÁEHàE: "PACÁET" ìCTAHOBàBòEÉOCü èPOñECCA ùã. MAòàHõ		
C -----------								
C	       BBOÑ ÁEPEá CèàCKà HOM, PAR, ZAD:				
C		&HOM  UH- ãàHEâHOE ÑEâCTBìûôEE HAèPüÜEHàE (KB);	
C		      IH- îAáHOE ÑEâCTBìûôEE áHAÁEHàE TOKA(A);		
C		      W0- KPìÉOBAü (CàHXPOHHAü) ÁACTOTA (PAÑ).		
C		      PH- AKTàBHAü MOôHOCTú  (KBT);			
C		      QH- PEAKTàBHAü MOôHOCTú(KBT);			
C		      SH- CKOãúÜEHàE;					
C		&END							
C		&PAR  X - àHÑìKTàBHOE COèPOTàBãEHàE CTATOPA (OM);	
C		      XF- àHÑìKTàBHOE COèPOTàBãEHàE POTOPA  (OM);	
C		     XAF- àHÑìKTàBHOE COèPOTàBãEHàE CTATOP-POTOP (OM);	
C		      R - AKTàBHOE COèPOTàBãEHàE CTATOPA (OM);		
C		      RF- AKTàBHOE COèPOTàBãEHàE POTOPA  (OM);		
C		      KI- KOùîîàñàEHT èPàBEÑEHàü èO TOKì;		
C		      KU- KOùîîàñàEHT èPàBEÑEHàü èO HAèPüÜEHàû;		
C		      GDKB - MAXOBõâ MOMEHT ( KÉ * M * M );		
C		      PTAU - ÁàCãO èAP èOãûCOB;				
C		&END							
C									
C		&ZAD  M -HAÉPìáOÁHõâ MOMEHT BAãA POTOPA (H*M);		
C		      S -TPEÅìEMAü BEãàÁàHA CKOãúÜEHàü;			
C		      KODQ=+1-BõÅOP Q-PEÜàMA àCXOÑü àá èOCTOüHCTBA	
C			      TOKA POTOPA;				
C			  =-1-BõÅOP Q-PEÜàMA àCXOÑü àá èOCTOüHCTBA	
C			    TOKA CTATOPA;				
C			  = 0- TPEÅìETCü áAÑATú Q-PEÜàM, ìKAáAB:	
C		     SZAD - QZAD - áAÑAHHAü PEAKTàB. MOôHOCTú B K.BAP;	
C			    B ùTOM ÜE CèàCKE;				
C		&END							
C									
C	  !!!!	áAèPOCOB &ZAD....&END  MOÜET ÅõTú ãûÅOE KOãàÁECTBO	
C	      ------------------------------------------------------	
C									
C  METOÑ: COÉãACHO:òAKAPüH û.É.ACàHXPOHàáàPOBAHHõE CàHXPOHHõE		
C -------	   MAòàHõ.-M.:ùHEPÉOATOMàáÑAT,1984.-192 C.		
C									
C									
CZZZ******************************************************************* 
      NAMELIST/HOM/UH,IH,W0,PH,QH,SH					
      NAMELIST/PAR/X,XF,XAF,R,RF,KI,KU, GDKB, PTAU			
      NAMELIST/ZAD/M,S,KODQ, QZAD					
      REAL IB,MB,MU,IFH, MMAX, M, KI, KU, NW0				
      REAL IH,IQH,IDH,IQ,ID,I,IFQ,IFD,IF,IQ1,ID1,I1,IFQ1,IFD1,IF1	
      REAL IQH1,IDH1,IH1,IFQH1,IFDH1,IFH1,IFDH,IFQH			
C	 UH-ãàHEâHOE ÑEâCTBìûôEE HAèPüÜEHàE,				
C	 IH-ÑEâCTBìûôEE áHAÁEHàE TOKA,					
C	 W0-ÁACTOTA CETà.						
       QZAD = 0.0							
      NRDR=5								
      NWTR=6								
      READ (NRDR,HOM,END=666)						
      READ (NRDR,PAR,END=666)						
      READ (NRDR,ZAD,END=666)						
      WRITE (NWTR,1)							
    1 FORMAT(/26X,'***	 PACÁET ìCTAHOBàBòEÉOCü PEÜàMA ùãEKTPàÁECKOâ ', 
     *'MAòàHõ	***'/26X,62('-')/)					
C-->   ÅAáOBõE BEãàÁàHõ							
      UB=1000.*UH*SQRT(2.)/SQRT(3.)					
      IB=IH*SQRT(2.)							
      ZB=UB/IB								
      SB=1.5*UB*IB							
      WB=W0								
C-->  OÅ-Tõ B MàH.							
       NW0 = ( W0*30.0)/(3.1415927*PTAU)				
      MB=(SB/WB) * PTAU							
      MU=XAF**2/(X*XF)							
C-->   B O.E. à K CTATOPì						
      XOLD=X								
      XFOLD=XF								
      XAFOLD=XAF							
      ROLD=R								
      RFOLD=RF								
      X=(X/ZB)								
      XF=(XF/ZB)*KU*KI							
      XAF=(XAF/ZB)*KI							
      R=R/ZB								
      RF=(RF/ZB)*KU*KI							
      UHOLD=UH								
      UH=1.								
      X1=(1.-MU)*X							
      PHOLD=PH								
      QHOLD=QH								
      PH=PH*1000.0							
      QH=QH*1000.0							
      PH=PH/SB								
      QH=QH/SB								
       SBKBA = SB / 1000.0						
      WRITE (NWTR,2) UHOLD,UB,IH,IB,W0,ZB,PHOLD,MB,QHOLD,SBKBA ,SH	
C									
    2 FORMAT(/15X,'   ÑAHHõE HOMàHAãúHOÉO PEÜàMA:   ',			
     *	      28X,' ÅAáàCHõE BEãàÁàHõ:'/				
     *	      15X,'  ----------------------------   ',			
     *	      28X,'-------------------'/				
     *1X,' ãàHEâHOE ÑEâCTBìûôEE HAèPüÜEHàE  UH=',G12.5,' KB	',	
     *5X,' HAèPüÜEHàE (îAáõ CTATOPA) UB=',G12.5,' B'/			
     *1X,' îAáHOE ÑEâCTBìûôEE áHAÁEHàE TOKA IH=',G12.5,' A	',	
     *5X,' TOK (îAáõ CTATOPA)	     IB=',G12.5,' A'/			
     *1X,' KPìÉOBAü (CàHXPOHHAü) ÁACTOTA    W0=',G12.5,' PAÑ/CEK',	
     *5X,' COèPOTàBãEHàE	     ZB=',G12.5,' OM'/			
     *1X,' AKTàBHAü MOôHOCTú		    PH=',G12.5,' KBATT	',	
     *5X,' MOMEHT HA BAãì MAòàHõ     MB=',G12.5,' H*M'/			
     *1X,' PEAKTàBHAü MOôHOCTú		    QH=',G12.5,' KBAP	',	
     *5X,' èOãHAü MOôHOCTú	     SB=',G12.5,' KBA'/			
     *1X,' CKOãúÜEHàE			    SH=',G12.5,' O.E.'/ )	
C--->  B CEK--> GDKB <=> SB_KBA						
       HJ = 2.74 * (GDKB*NW0)**2 * 1.0E-6 / ( SBKBA )			
C									
C--->									
      WRITE (NWTR,3)XOLD,X,XFOLD,XF,XAFOLD,XAF,ROLD,R,RFOLD,RF,KI,MU,KU,
     *	  HJ								
    3 FORMAT(/15X,'  èAPAMETPõ ùãEKTPàÁECKOâ MAòàHõ:',			
     *	      25X,'  èPàBEÑEHHõE èAPAMETPõ ùãEKTPàÁECKOâ MAòàHõ:'/	
     *	      15X,' --------------------------------',			
     *	      25X,' --------------------------------------------'/	
     *1X,' àHÑìKTàBHOE COèPOTàBãEHàE CTATOPA	    X=',G12.5,' OM  ',	
     *1X,' àHÑìKTàBHOE COèPOTàBãEHàE CTATOPA	    X=',G12.5,'O.E.'/	
     *1X,' àHÑìKTàBHOE COèPOTàBãEHàE POTOPA	   XF=',G12.5,' OM  ',	
     *1X,' àHÑìKTàBHOE COèPOTàBãEHàE POTOPA	   XF=',G12.5,'O.E.'/	
     *1X,' àHÑìKTàBHOE COèPOTàBãEHàE CTATOP-POTOP XAF=',G12.5,' OM  ',	
     *1X,' àHÑìKTàBHOE COèPOTàBãEHàE CTATOP-POTOP XAF=',G12.5,'O.E.'/	
     *1X,' AKTàBHOE COèPOTàBãEHàE CTATOPA	    R=',G12.5,' OM  ',	
     *1X,' AKTàBHOE COèPOTàBãEHàE CTATOPA	    R=',G12.5,'O.E.'/	
     *1X,' AKTàBHOE COèPOTàBãEHàE POTOPA	   RF=',G12.5,' OM  ',	
     *1X,' AKTàBHOE COèPOTàBãEHàE POTOPA	   RF=',G12.5,'O.E.'/	
     *1X,' KOùîîàñàEHT èPàBEÑEHàü èO TOKì	   KI=',G12.5,' O.E.',	
     *1X,' KOùîîàñàEHT MAÉHàTHOâ CBüáà		   MU=',G12.5/		
     *1X,' KOùîîàñàEHT èPàBEÑEHàü èO HAèPüÜEHàû    KU=',G12.5,' O.E.',	
     *1X,' àHEPñàOHHAü èOCTOüHHAü		   HJ=',G12.5,' CEK.')	
C									
       WRITE(NWTR,3000)        PTAU, GDKB				
3000   FORMAT(								
     *1X,' ÁàCãO èAP èOãûCOB			 PTAU=', G12.5,' òT.' / 
     *1X,' MAXOBõâ MOMEHT			 GDKB=',G12.5,' KÉ*M*M' 
     * / 1X,125('*')//)							
C-->   PACÁET								
      IQH=PH/UH								
      IDH=-QH/UH							
      IH=SQRT(IQH**2+IDH**2)						
      EQH=-PH*X/UH							
      EDH=(QH*X+UH**2)/UH						
      EH=SQRT(EQH**2+EDH**2)						
      IFQH=EQH/XAF							
      IFDH=EDH/XAF							
      IFH=SQRT(IFQH**2+IFDH**2)						
      UFQH=(UH**2*SH*XF/XAF-RF*X*PH/XAF+SH*X1*XF*QH/XAF)/UH		
      UFDH=(UH**2*RF/XAF+RF*X*QH/XAF+SH*X1*XF*PH/XAF)/UH		
      UFH=SQRT(UFQH**2+UFDH**2)						
C-->	àMEHOBAHHõE EÑàHàñõ						
      IQH1=IQH*IB							
      IDH1=IDH*IB							
      IH1=IH*IB								
      EQH1=EQH*UB							
      EDH1=EDH*UB							
      EH1=EH*UB								
      IFQH1=IFQH*KI*IB							
      IFDH1=IFDH*KI*IB							
      IFH1=IFH*KI*IB							
      UFQH1=UFQH*(UB/KU)						
      UFDH1=UFDH*(UB/KU)						
      UFH1=UFH*(UB/KU)							
      FIH=ATAN2(IDH,IQH)*57.29578					
      FEH=ATAN2(EDH,EQH)*57.29578					
      FIFH=ATAN2(IFDH,IFQH)*57.29578					
      FUFH=ATAN2(UFDH,UFQH)*57.29578					
      WRITE (NWTR,6) IQH1,EQH1,IFQH1,UFQH1,				
     *		     IDH1,EDH1,IFDH1,UFDH1,				
     *		     IH1,EH1,IFH1,UFH1,					
     *		     FIH,FEH,FIFH,FUFH					
    6 FORMAT(/45X,' HOMàHAãúHõâ PEÜàM:'/				
     *	      45X,'-------------------'//1X,'I----------I',		
     *4('-------------------------I')/	 1X,'I		I',		
     *'   TOK CTATOPHOâ ñEèà	I   ùÑC CTATOPHOâ ñEèà	  I',		
     *'    TOK POTOPHOâ ñEèà	I    HAèPüÜEHàE POTOPA	  I'/		
     *1X,'I----------I',4('-------------------------I')/		
     *1X,'I èO OCà Q I',4(6X,G12.5,7X,'I')/				
     *1X,'I èO OCà D I',4(6X,G12.5,7X,'I')/				
     *1X,'I  MOÑìãú  I',4(6X,G12.5,7X,'I')/				
     *1X,'I   ìÉOã   I',4(6X,G12.5,7X,'I')/				
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	
  100 CONTINUE								
      OM1=(1.+S)*W0							
      WRITE(NWTR,10) M,S,OM1						
10    FORMAT(/40X,' èAPAMETPõ áAÑAHHOÉO PEÜàMA:'/			
     *	      40X,'----------------------------'/			
     *1X,' HAÉPìáOÁHõâ MOMEHT BAãA POTOPA  M=',G12.5,' H*M'/		
     *1X,' CKOãúÜEHàE			   S=',G12.5/			
     *1X,' ìÉãOBAü CKOPOCTú POTOPA     OMEGA=',G12.5,' PAÑ./CEK' )	
      P=M*W0/SB								
      IF(KODQ) 11,12,13							
   11 CONTINUE								
C-->   Q<0  àá ìCãOBàü	I=IH						
      DTQ=((UH*IH)**2)-P**2						
      MMAX=((UH*IH)**2)*SB/W0						
      WRITE (NWTR,14)KODQ						
   14 FORMAT(/1X,'Q OèPEÑEãüETCü èO ìCãOBàû èOCTOüHCTBA ',		
     *'TOKA CTATOPA ( Q<0, KODQ=',I3,' )'/)				
      IF(DTQ.LT.0.0) WRITE(NWTR,15) M,MMAX				
   15 FORMAT(//1X,'*** BHàMAHàE! áAèPOòEHHõâ MOMEHT M=',G12.5,		
     *'èPEBõòAET MAKCàMAãúHõâ MMAX=',G12.5,' ***'//)			
      IF(DTQ.LT.0.0) GO TO 101						
      Q=-SQRT(DTQ)							
      GO TO 16								
   12 CONTINUE								
      WRITE(NWTR,17) KODQ						
   17 FORMAT(/1X,'Q OèPEÑEãüETCü èO áAÑAHàû èOãúáOBATEãü ',		
     *			   ' ( KODQ=',I3,')'/ )				
C									
C-->  èPàBEÑEM..							
       Q = ( QZAD *  1000.0 ) / SB					
C--->									
      GO TO 16								
   13 CONTINUE								
      WRITE(NWTR,1300) KODQ						
 1300 FORMAT(/1X,'Q OèPEÑEãüETCü èO ìCãOBàû èOCTOüHCTBA',		
     *' TOKA POTOPA (Q>0, KODQ=',I3,')'/)				
      D1=(EH**2)*(UH**2)-(P*X)**2					
      MMAX=(EH**2)*(UH**2)*SB/W0					
      IF(D1.LT.0.0) WRITE(NWTR,15) M,MMAX				
      IF(D1.LT.0.0) GO TO 101						
      D2=SQRT(D1)-UH**2							
      Q=D2/X								
   16 CONTINUE								
C--->	Q HAòãà								
      Q1=(Q*SB)/1000.							
      P1=(P*SB)/1000.							
      WRITE(NWTR,18) P1,Q1						
   18 FORMAT(1X,'  AKTàBHAü MOôHOCTú   P=',G12.5,'KBAT'/		
     *	     1X,'  PEAKTàBHAü MOôHOCTú Q=',G12.5,'KBAP')		
C									
C-->  ìCTOâÁàBO ãà ùTO....						
       TF1 = ( 1.0 - MU ) * XF / RF					
C  èPà S > 0								
       QXX1 = S * TF1 * P - UH**2/X1					
C  èPà S < 0								
       QXX2 = -2.0*HJ* (1. + (S*TF1)**2)/(TF1**2) -			
     -	       S*TF1*P - UH**2/X1					
C									
       QXX1I = (QXX1*SB)/1000.0						
       QXX2I = (QXX2*SB)/1000.0						
       IF ( S .GE. 0.0 )						
     *			 WRITE(NWTR,700) QXX1I				
700    FORMAT(' ( S > 0, PEÜàM ìCTOâÁàB èPà Q > ', G12.5, '  KBAP ) ' ) 
C									
       IF ( S .LE. 0.0 )						
     *			 WRITE(NWTR,7000) QXX2I				
7000   FORMAT(' ( S < 0, PEÜàM ìCTOâÁàB èPà Q > ', G12.5, '  KBAP ) ' ) 
C									
       IF ( S .GT. 0.0 .AND. Q .GT. QXX1 ) GO TO 705			
       IF ( S .LT. 0.0 .AND. Q .GT. QXX2 ) GO TO 705			
       IF ( S .EQ. 0.0 .AND. Q .GT. QXX1 .AND. Q .GT. QXX2 ) GO TO 705	
C****									
       WRITE(NWTR, 706 )						
706    FORMAT(/' ***** BHàMAHàE ---> BOáMOÜHA HEìCTOâÁàBOCTú PEÜàMA',	
     *	       ' *****' / )						
C--->	PACÁET								
705   IQ=P/UH								
      ID=-Q/UH								
      I=SQRT(IQ**2+ID**2)						
      EQ=-P*X/UH							
      ED=(Q*X+UH**2)/UH							
      E=SQRT(EQ**2+ED**2)						
      IFQ=EQ/XAF							
      IFD=ED/XAF							
      IF=SQRT(IFQ**2+IFD**2)						
      UFQ=(UH**2*S*XF/XAF-RF*X*P/XAF+S*X1*XF*Q/XAF)/UH			
      UFD=(UH**2*RF/XAF+RF*X*Q/XAF+S*X1*XF*P/XAF)/UH			
      UF=SQRT(UFQ**2+UFD**2)						
C-->	àMEHOBAHHõE EÑàHàñõ						
      IQ1=IQ*IB								
      ID1=ID*IB								
      I1=I*IB								
      EQ1=EQ*UB								
      ED1=ED*UB								
      E1=E*UB								
      IFQ1=IFQ*KI*IB							
      IFD1=IFD*KI*IB							
      IF1=IF*KI*IB							
      UFQ1=UFQ*(UB/KU)							
      UFD1=UFD*(UB/KU)							
      UF1=UF*(UB/KU)							
      FI=ATAN2(ID,IQ)*57.29578						
      FE=ATAN2(ED,EQ)*57.29578						
      FIF=ATAN2(IFD,IFQ)*57.29578					
      FUF=ATAN2(UFD,UFQ)*57.29578					
      WRITE (NWTR,60)IQ1,EQ1,IFQ1,UFQ1,					
     *		     ID1,ED1,IFD1,UFD1,					
     *		     I1,E1,IF1,UF1,					
     *		     FI,FE,FIF,FUF					
   60 FORMAT(/45X,'   áAÑAHHõâ PEÜàM:'/					
     *	      45X,'  ----------------'//1X,'I----------I',		
     *4('-------------------------I')/	 1X,'I		I',		
     *'   TOK CTATOPHOâ ñEèà	I   ùÑC CTATOPHOâ ñEèà	  I',		
     *'    TOK POTOPHOâ ñEèà	I    HAèPüÜEHàE POTOPA	  I'/		
     *1X,'I----------I',4('-------------------------I')/		
     *1X,'I èO OCà Q I',4(6X,G12.5,7X,'I')/				
     *1X,'I èO OCà D I',4(6X,G12.5,7X,'I')/				
     *1X,'I  MOÑìãú  I',4(6X,G12.5,7X,'I')/				
     *1X,'I   ìÉOã   I',4(6X,G12.5,7X,'I')/				
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	
C********************************************************************** 
C********************************************************************** 
C--->	HOBõâ áAÑAHHõâ PEÜàM						
101   READ (NRDR,ZAD,END=666)						
      GO TO 100								
  666 CONTINUE								
      WRITE (NWTR,6660)							
 6660 FORMAT(/' *** BHàMAHàE *** Ñãü KOMèãEKCA MS1 èPàHüTO ÑPìÉOE',	
     *' ìCãOBHO èOãOÜàTEãúHOE HAèPABãEHàE TOKOB à HAèPüÜEHàâ:'/		
     *'     U_CTATOPA_<MS1> := + U_CTATOPA  '/				
     *'     I_CTATOPA_<MS1> := - I_CTATOPA  '/				
     *'     U_POTOPA__<MS1> := - U_POTOPA   '/				
     *'     I_POTOPA__<MS1> := - I_POTOPA   '/				
     *1X,125('*')//)							
C********************************************************************** 
C********************************************************************** 
C********************************************************************** 
C********************************************************************** 
C********************************************************************** 
C********************************************************************** 
C********************************************************************** 
      STOP								
      END								
