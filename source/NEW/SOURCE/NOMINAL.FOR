CXXX****äOMèãEäC "MS1" --> BEPCàü 1.0  (OT ÑEäAÅPü 1986 É.)************ NOM00010
C  èPOÉPAMMA: NOMINAL							NOM00020
C -----------								NOM00030
C çAáçAÁEçàE: "PACÁET" ìCTAçOBàBòEÉOCü èPOñECCA ùã. MAòàçõ		NOM00040
C -----------								NOM00050
C	       BBOÑ ÁEPEá CèàCäà çOM, PAR, ZAD:				NOM00060
C		&çOM  Uç- ãàçEâçOE ÑEâCTBìûôEE çAèPüÜEçàE (äB);		NOM00070
C		      Iç- îAáçOE ÑEâCTBìûôEE áçAÁEçàE TOäA(A);		NOM00080
C		      W0- äPìÉOBAü (CàçXPOççAü) ÁACTOTA (PAÑ).		NOM00090
C		      Pç- AäTàBçAü MOôçOCTú  (äBT);			NOM00100
C		      Qç- PEAäTàBçAü MOôçOCTú(äBT);			NOM00110
C		      Sç- CäOãúÜEçàE;					NOM00120
C		&END							NOM00130
C		&PAR  X - àçÑìäTàBçOE COèPOTàBãEçàE CTATOPA (OM);	NOM00140
C		      XF- àçÑìäTàBçOE COèPOTàBãEçàE POTOPA  (OM);	NOM00150
C		     XAF- àçÑìäTàBçOE COèPOTàBãEçàE CTATOP-POTOP (OM);	NOM00160
C		      R - AäTàBçOE COèPOTàBãEçàE CTATOPA (OM);		NOM00170
C		      RF- AäTàBçOE COèPOTàBãEçàE POTOPA  (OM);		NOM00180
C		      äI- äOùîîàñàEçT èPàBEÑEçàü èO TOäì;		NOM00190
C		      äU- äOùîîàñàEçT èPàBEÑEçàü èO çAèPüÜEçàû;		NOM00200
C		      GDäB - MAXOBõâ MOMEçT ( äÉ * M * M );		NOM00210
C		      PTAU - ÁàCãO èAP èOãûCOB;				NOM00220
C		&END							NOM00230
C									NOM00240
C		&ZAD  M -çAÉPìáOÁçõâ MOMEçT BAãA POTOPA (ç*M);		NOM00250
C		      S -TPEÅìEMAü BEãàÁàçA CäOãúÜEçàü;			NOM00260
C		      äODQ=+1-BõÅOP Q-PEÜàMA àCXOÑü àá èOCTOüçCTBA	NOM00270
C			      TOäA POTOPA;				NOM00280
C			  =-1-BõÅOP Q-PEÜàMA àCXOÑü àá èOCTOüçCTBA	NOM00290
C			    TOäA CTATOPA;				NOM00300
C			  = 0- TPEÅìETCü áAÑATú Q-PEÜàM, ìäAáAB:	NOM00310
C		     SZAD - QZAD - áAÑAççAü PEAäTàB. MOôçOCTú B ä.BAP;	NOM00320
C			    B ùTOM ÜE CèàCäE;				NOM00330
C		&END							NOM00340
C									NOM00350
C	  !!!!	áAèPOCOB &ZAD....&END  MOÜET ÅõTú ãûÅOE äOãàÁECTBO	NOM00360
C	      ------------------------------------------------------	NOM00370
C									NOM00380
C  METOÑ: COÉãACçO:òAäAPüç û.É.ACàçXPOçàáàPOBAççõE CàçXPOççõE		NOM00390
C -------	   MAòàçõ.-M.:ùçEPÉOATOMàáÑAT,1984.-192 C.		NOM00400
C									NOM00410
C									NOM00420
CZZZ******************************************************************* NOM00430
      NAMELIST/çOM/Uç,Iç,W0,Pç,Qç,Sç					NOM00440
      NAMELIST/PAR/X,XF,XAF,R,RF,äI,äU, GDäB, PTAU			NOM00450
      NAMELIST/ZAD/M,S,äODQ, QZAD					NOM00460
      REAL IB,MB,MU,IFç,I,MMAX, M, äI, äU, NW0				NOM00470
      REAL Iç,IQç,IDç,IQ,ID,I,IFQ,IFD,IF,IQ1,ID1,I1,IFQ1,IFD1,IF1	NOM00480
      REAL IQç1,IDç1,Iç1,IFQç1,IFDç1,IFç1,IFDç,IFQç			NOM00490
C	 Uç-ãàçEâçOE ÑEâCTBìûôEE çAèPüÜEçàE,				NOM00500
C	 Iç-ÑEâCTBìûôEE áçAÁEçàE TOäA,					NOM00510
C	 W0-ÁACTOTA CETà.						NOM00520
       QZAD = 0.0							NOM00530
      NRDR=5								NOM00540
      NWTR=6								NOM00550
      READ (NRDR,çOM,END=666)						NOM00560
      READ (NRDR,PAR,END=666)						NOM00570
      READ (NRDR,ZAD,END=666)						NOM00580
      WRITE (NWTR,1)							NOM00590
    1 FORMAT(/26X,'***	 PACÁET ìCTAçOBàBòEÉOCü PEÜàMA ùãEäTPàÁECäOâ ', NOM00600
     *'MAòàçõ	***'/26X,62('-')/)					NOM00610
C-->   ÅAáOBõE BEãàÁàçõ							NOM00620
      UB=1000.*Uç*SQRT(2.)/SQRT(3.)					NOM00630
      IB=Iç*SQRT(2.)							NOM00640
      ZB=UB/IB								NOM00650
      SB=1.5*UB*IB							NOM00660
      WB=W0								NOM00670
C-->  OÅ-Tõ B Màç.							NOM00680
       NW0 = ( W0*30.0)/(3.1415927*PTAU)				NOM00690
      MB=(SB/WB) * PTAU							NOM00700
      MU=XAF**2/(X*XF)							NOM00710
C-->   B O.E. à ä CTATOPì						NOM00720
      XOLD=X								NOM00730
      XFOLD=XF								NOM00740
      XAFOLD=XAF							NOM00750
      ROLD=R								NOM00760
      RFOLD=RF								NOM00770
      X=(X/ZB)								NOM00780
      XF=(XF/ZB)*äU*äI							NOM00790
      XAF=(XAF/ZB)*äI							NOM00800
      R=R/ZB								NOM00810
      RF=(RF/ZB)*äU*äI							NOM00820
      UçOLD=Uç								NOM00830
      Uç=1.								NOM00840
      X1=(1.-MU)*X							NOM00850
      PçOLD=Pç								NOM00860
      QçOLD=Qç								NOM00870
      Pç=Pç*1000.0							NOM00880
      Qç=Qç*1000.0							NOM00890
      Pç=Pç/SB								NOM00900
      Qç=Qç/SB								NOM00910
       SBäBA = SB / 1000.0						NOM00920
      WRITE (NWTR,2) UçOLD,UB,Iç,IB,W0,ZB,PçOLD,MB,QçOLD,SBäBA ,Sç	NOM00930
C									NOM00940
    2 FORMAT(/15X,'   ÑAççõE çOMàçAãúçOÉO PEÜàMA:   ',			NOM00950
     *	      28X,' ÅAáàCçõE BEãàÁàçõ:'/				NOM00960
     *	      15X,'  ----------------------------   ',			NOM00970
     *	      28X,'-------------------'/				NOM00980
     *1X,' ãàçEâçOE ÑEâCTBìûôEE çAèPüÜEçàE  Uç=',G12.5,' äB	',	NOM00990
     *5X,' çAèPüÜEçàE (îAáõ CTATOPA) UB=',G12.5,' B'/			NOM01000
     *1X,' îAáçOE ÑEâCTBìûôEE áçAÁEçàE TOäA Iç=',G12.5,' A	',	NOM01010
     *5X,' TOä (îAáõ CTATOPA)	     IB=',G12.5,' A'/			NOM01020
     *1X,' äPìÉOBAü (CàçXPOççAü) ÁACTOTA    W0=',G12.5,' PAÑ/CEä',	NOM01030
     *5X,' COèPOTàBãEçàE	     ZB=',G12.5,' OM'/			NOM01040
     *1X,' AäTàBçAü MOôçOCTú		    Pç=',G12.5,' äBATT	',	NOM01050
     *5X,' MOMEçT çA BAãì MAòàçõ     MB=',G12.5,' ç*M'/			NOM01060
     *1X,' PEAäTàBçAü MOôçOCTú		    Qç=',G12.5,' äBAP	',	NOM01070
     *5X,' èOãçAü MOôçOCTú	     SB=',G12.5,' äBA'/			NOM01080
     *1X,' CäOãúÜEçàE			    Sç=',G12.5,' O.E.'/ )	NOM01090
C--->  B CEä--> GDäB <=> SB_äBA						NOM01100
       çJ = 2.74 * (GDäB*NW0)**2 * 1.0E-6 / ( SBäBA )			NOM01110
C									NOM01120
C--->									NOM01130
      WRITE (NWTR,3)XOLD,X,XFOLD,XF,XAFOLD,XAF,ROLD,R,RFOLD,RF,äI,MU,äU,NOM01140
     *	  çJ								NOM01150
    3 FORMAT(/15X,'  èAPAMETPõ ùãEäTPàÁECäOâ MAòàçõ:',			NOM01160
     *	      25X,'  èPàBEÑEççõE èAPAMETPõ ùãEäTPàÁECäOâ MAòàçõ:'/	NOM01170
     *	      15X,' --------------------------------',			NOM01180
     *	      25X,' --------------------------------------------'/	NOM01190
     *1X,' àçÑìäTàBçOE COèPOTàBãEçàE CTATOPA	    X=',G12.5,' OM  ',	NOM01200
     *1X,' àçÑìäTàBçOE COèPOTàBãEçàE CTATOPA	    X=',G12.5,'O.E.'/	NOM01210
     *1X,' àçÑìäTàBçOE COèPOTàBãEçàE POTOPA	   XF=',G12.5,' OM  ',	NOM01220
     *1X,' àçÑìäTàBçOE COèPOTàBãEçàE POTOPA	   XF=',G12.5,'O.E.'/	NOM01230
     *1X,' àçÑìäTàBçOE COèPOTàBãEçàE CTATOP-POTOP XAF=',G12.5,' OM  ',	NOM01240
     *1X,' àçÑìäTàBçOE COèPOTàBãEçàE CTATOP-POTOP XAF=',G12.5,'O.E.'/	NOM01250
     *1X,' AäTàBçOE COèPOTàBãEçàE CTATOPA	    R=',G12.5,' OM  ',	NOM01260
     *1X,' AäTàBçOE COèPOTàBãEçàE CTATOPA	    R=',G12.5,'O.E.'/	NOM01270
     *1X,' AäTàBçOE COèPOTàBãEçàE POTOPA	   RF=',G12.5,' OM  ',	NOM01280
     *1X,' AäTàBçOE COèPOTàBãEçàE POTOPA	   RF=',G12.5,'O.E.'/	NOM01290
     *1X,' äOùîîàñàEçT èPàBEÑEçàü èO TOäì	   äI=',G12.5,' O.E.',	NOM01300
     *1X,' äOùîîàñàEçT MAÉçàTçOâ CBüáà		   MU=',G12.5/		NOM01310
     *1X,' äOùîîàñàEçT èPàBEÑEçàü èO çAèPüÜEçàû    äU=',G12.5,' O.E.',	NOM01320
     *1X,' àçEPñàOççAü èOCTOüççAü		   çJ=',G12.5,' CEä.')	NOM01330
C									NOM01340
       WRITE(NWTR,3000)        PTAU, GDäB				NOM01350
3000   FORMAT(								NOM01360
     *1X,' ÁàCãO èAP èOãûCOB			 PTAU=', G12.5,' òT.' / NOM01370
     *1X,' MAXOBõâ MOMEçT			 GDäB=',G12.5,' äÉ*M*M' NOM01380
     * / 1X,125('*')//)							NOM01390
C-->   PACÁET								NOM01400
      IQç=Pç/Uç								NOM01410
      IDç=-Qç/Uç							NOM01420
      Iç=SQRT(IQç**2+IDç**2)						NOM01430
      EQç=-Pç*X/Uç							NOM01440
      EDç=(Qç*X+Uç**2)/Uç						NOM01450
      Eç=SQRT(EQç**2+EDç**2)						NOM01460
      IFQç=EQç/XAF							NOM01470
      IFDç=EDç/XAF							NOM01480
      IFç=SQRT(IFQç**2+IFDç**2)						NOM01490
      UFQç=(Uç**2*Sç*XF/XAF-RF*X*Pç/XAF+Sç*X1*XF*Qç/XAF)/Uç		NOM01500
      UFDç=(Uç**2*RF/XAF+RF*X*Qç/XAF+Sç*X1*XF*Pç/XAF)/Uç		NOM01510
      UFç=SQRT(UFQç**2+UFDç**2)						NOM01520
C-->	àMEçOBAççõE EÑàçàñõ						NOM01530
      IQç1=IQç*IB							NOM01540
      IDç1=IDç*IB							NOM01550
      Iç1=Iç*IB								NOM01560
      EQç1=EQç*UB							NOM01570
      EDç1=EDç*UB							NOM01580
      Eç1=Eç*UB								NOM01590
      IFQç1=IFQç*äI*IB							NOM01600
      IFDç1=IFDç*äI*IB							NOM01610
      IFç1=IFç*äI*IB							NOM01620
      UFQç1=UFQç*(UB/äU)						NOM01630
      UFDç1=UFDç*(UB/äU)						NOM01640
      UFç1=UFç*(UB/äU)							NOM01650
      FIç=ATAN2(IDç,IQç)*57.29578					NOM01660
      FEç=ATAN2(EDç,EQç)*57.29578					NOM01670
      FIFç=ATAN2(IFDç,IFQç)*57.29578					NOM01680
      FUFç=ATAN2(UFDç,UFQç)*57.29578					NOM01690
      WRITE (NWTR,6) IQç1,EQç1,IFQç1,UFQç1,				NOM01700
     *		     IDç1,EDç1,IFDç1,UFDç1,				NOM01710
     *		     Iç1,Eç1,IFç1,UFç1,					NOM01720
     *		     FIç,FEç,FIFç,FUFç					NOM01730
    6 FORMAT(/45X,' çOMàçAãúçõâ PEÜàM:'/				NOM01740
     *	      45X,'-------------------'//1X,'I----------I',		NOM01750
     *4('-------------------------I')/	 1X,'I		I',		NOM01760
     *'   TOä CTATOPçOâ ñEèà	I   ùÑC CTATOPçOâ ñEèà	  I',		NOM01770
     *'    TOä POTOPçOâ ñEèà	I    çAèPüÜEçàE POTOPA	  I'/		NOM01780
     *1X,'I----------I',4('-------------------------I')/		NOM01790
     *1X,'I èO OCà Q I',4(6X,G12.5,7X,'I')/				NOM01800
     *1X,'I èO OCà D I',4(6X,G12.5,7X,'I')/				NOM01810
     *1X,'I  MOÑìãú  I',4(6X,G12.5,7X,'I')/				NOM01820
     *1X,'I   ìÉOã   I',4(6X,G12.5,7X,'I')/				NOM01830
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	NOM01840
  100 CONTINUE								NOM01850
      OM1=(1.+S)*W0							NOM01860
      WRITE(NWTR,10) M,S,OM1						NOM01870
10    FORMAT(/40X,' èAPAMETPõ áAÑAççOÉO PEÜàMA:'/			NOM01880
     *	      40X,'----------------------------'/			NOM01890
     *1X,' çAÉPìáOÁçõâ MOMEçT BAãA POTOPA  M=',G12.5,' ç*M'/		NOM01900
     *1X,' CäOãúÜEçàE			   S=',G12.5/			NOM01910
     *1X,' ìÉãOBAü CäOPOCTú POTOPA     OMEGA=',G12.5,' PAÑ./CEä' )	NOM01920
      P=M*W0/SB								NOM01930
      IF(äODQ) 11,12,13							NOM01940
   11 CONTINUE								NOM01950
C-->   Q<0  àá ìCãOBàü	I=Iç						NOM01960
      DTQ=((Uç*Iç)**2)-P**2						NOM01970
      MMAX=((Uç*Iç)**2)*SB/W0						NOM01980
      WRITE (NWTR,14)äODQ						NOM01990
   14 FORMAT(/1X,'Q OèPEÑEãüETCü èO ìCãOBàû èOCTOüçCTBA ',		NOM02000
     *'TOäA CTATOPA ( Q<0, äODQ=',I3,' )'/)				NOM02010
      IF(DTQ.LT.0.0) WRITE(NWTR,15) M,MMAX				NOM02020
   15 FORMAT(//1X,'*** BçàMAçàE! áAèPOòEççõâ MOMEçT M=',G12.5,		NOM02030
     *'èPEBõòAET MAäCàMAãúçõâ MMAX=',G12.5,' ***'//)			NOM02040
      IF(DTQ.LT.0.0) GO TO 101						NOM02050
      Q=-SQRT(DTQ)							NOM02060
      GO TO 16								NOM02070
   12 CONTINUE								NOM02080
      WRITE(NWTR,17) äODQ						NOM02090
   17 FORMAT(/1X,'Q OèPEÑEãüETCü èO áAÑAçàû èOãúáOBATEãü ',		NOM02100
     *			   ' ( äODQ=',I3,')'/ )				NOM02110
C									NOM02120
C-->  èPàBEÑEM..							NOM02130
       Q = ( QZAD *  1000.0 ) / SB					NOM02140
C--->									NOM02150
      GO TO 16								NOM02160
   13 CONTINUE								NOM02170
      WRITE(NWTR,1300) äODQ						NOM02180
 1300 FORMAT(/1X,'Q OèPEÑEãüETCü èO ìCãOBàû èOCTOüçCTBA',		NOM02190
     *' TOäA POTOPA (Q>0, äODQ=',I3,')'/)				NOM02200
      D1=(Eç**2)*(Uç**2)-(P*X)**2					NOM02210
      MMAX=(Eç**2)*(Uç**2)*SB/W0					NOM02220
      IF(D1.LT.0.0) WRITE(NWTR,15) M,MMAX				NOM02230
      IF(D1.LT.0.0) GO TO 101						NOM02240
      D2=SQRT(D1)-Uç**2							NOM02250
      Q=D2/X								NOM02260
   16 CONTINUE								NOM02270
C--->	Q çAòãà								NOM02280
      Q1=(Q*SB)/1000.							NOM02290
      P1=(P*SB)/1000.							NOM02300
      WRITE(NWTR,18) P1,Q1						NOM02310
   18 FORMAT(1X,'  AäTàBçAü MOôçOCTú   P=',G12.5,'äBAT'/		NOM02320
     *	     1X,'  PEAäTàBçAü MOôçOCTú Q=',G12.5,'äBAP')		NOM02330
C									NOM02340
C-->  ìCTOâÁàBO ãà ùTO....						NOM02350
       TF1 = ( 1.0 - MU ) * XF / RF					NOM02360
C  èPà S > 0								NOM02370
       QXX1 = S * TF1 * P - Uç**2/X1					NOM02380
C  èPà S < 0								NOM02390
       QXX2 = -2.0*çJ* (1. + (S*TF1)**2)/(TF1**2) -			NOM02400
     -	       S*TF1*P - Uç**2/X1					NOM02410
C									NOM02420
       QXX1I = (QXX1*SB)/1000.0						NOM02430
       QXX2I = (QXX2*SB)/1000.0						NOM02440
       IF ( S .GE. 0.0 )						NOM02450
     *			 WRITE(NWTR,700) QXX1I				NOM02460
700    FORMAT(' ( S > 0, PEÜàM ìCTOâÁàB èPà Q > ', G12.5, '  äBAP ) ' ) NOM02470
C									NOM02480
       IF ( S .LE. 0.0 )						NOM02490
     *			 WRITE(NWTR,7000) QXX2I				NOM02500
7000   FORMAT(' ( S < 0, PEÜàM ìCTOâÁàB èPà Q > ', G12.5, '  äBAP ) ' ) NOM02510
C									NOM02520
       IF ( S .GT. 0.0 .AND. Q .GT. QXX1 ) GO TO 705			NOM02530
       IF ( S .LT. 0.0 .AND. Q .GT. QXX2 ) GO TO 705			NOM02540
       IF ( S .EQ. 0.0 .AND. Q .GT. QXX1 .AND. Q .GT. QXX2 ) GO TO 705	NOM02550
C****									NOM02560
       WRITE(NWTR, 706 )						NOM02570
706    FORMAT(/' ***** BçàMAçàE ---> BOáMOÜçA çEìCTOâÁàBOCTú PEÜàMA',	NOM02580
     *	       ' *****' / )						NOM02590
C--->	PACÁET								NOM02600
705   IQ=P/Uç								NOM02610
      ID=-Q/Uç								NOM02620
      I=SQRT(IQ**2+ID**2)						NOM02630
      EQ=-P*X/Uç							NOM02640
      ED=(Q*X+Uç**2)/Uç							NOM02650
      E=SQRT(EQ**2+ED**2)						NOM02660
      IFQ=EQ/XAF							NOM02670
      IFD=ED/XAF							NOM02680
      IF=SQRT(IFQ**2+IFD**2)						NOM02690
      UFQ=(Uç**2*S*XF/XAF-RF*X*P/XAF+S*X1*XF*Q/XAF)/Uç			NOM02700
      UFD=(Uç**2*RF/XAF+RF*X*Q/XAF+S*X1*XF*P/XAF)/Uç			NOM02710
      UF=SQRT(UFQ**2+UFD**2)						NOM02720
C-->	àMEçOBAççõE EÑàçàñõ						NOM02730
      IQ1=IQ*IB								NOM02740
      ID1=ID*IB								NOM02750
      I1=I*IB								NOM02760
      EQ1=EQ*UB								NOM02770
      ED1=ED*UB								NOM02780
      E1=E*UB								NOM02790
      IFQ1=IFQ*äI*IB							NOM02800
      IFD1=IFD*äI*IB							NOM02810
      IF1=IF*äI*IB							NOM02820
      UFQ1=UFQ*(UB/äU)							NOM02830
      UFD1=UFD*(UB/äU)							NOM02840
      UF1=UF*(UB/äU)							NOM02850
      FI=ATAN2(ID,IQ)*57.29578						NOM02860
      FE=ATAN2(ED,EQ)*57.29578						NOM02870
      FIF=ATAN2(IFD,IFQ)*57.29578					NOM02880
      FUF=ATAN2(UFD,UFQ)*57.29578					NOM02890
      WRITE (NWTR,60)IQ1,EQ1,IFQ1,UFQ1,					NOM02900
     *		     ID1,ED1,IFD1,UFD1,					NOM02910
     *		     I1,E1,IF1,UF1,					NOM02920
     *		     FI,FE,FIF,FUF					NOM02930
   60 FORMAT(/45X,'   áAÑAççõâ PEÜàM:'/					NOM02940
     *	      45X,'  ----------------'//1X,'I----------I',		NOM02950
     *4('-------------------------I')/	 1X,'I		I',		NOM02960
     *'   TOä CTATOPçOâ ñEèà	I   ùÑC CTATOPçOâ ñEèà	  I',		NOM02970
     *'    TOä POTOPçOâ ñEèà	I    çAèPüÜEçàE POTOPA	  I'/		NOM02980
     *1X,'I----------I',4('-------------------------I')/		NOM02990
     *1X,'I èO OCà Q I',4(6X,G12.5,7X,'I')/				NOM03000
     *1X,'I èO OCà D I',4(6X,G12.5,7X,'I')/				NOM03010
     *1X,'I  MOÑìãú  I',4(6X,G12.5,7X,'I')/				NOM03020
     *1X,'I   ìÉOã   I',4(6X,G12.5,7X,'I')/				NOM03030
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	NOM03040
C********************************************************************** NOM03050
C********************************************************************** NOM03060
C--->	çOBõâ áAÑAççõâ PEÜàM						NOM03070
101   READ (NRDR,ZAD,END=666)						NOM03080
      GO TO 100								NOM03090
  666 CONTINUE								NOM03100
      WRITE (NWTR,6660)							NOM03110
 6660 FORMAT(/' *** BçàMAçàE *** Ñãü äOMèãEäCA MS1 èPàçüTO ÑPìÉOE',	NOM03120
     *' ìCãOBçO èOãOÜàTEãúçOE çAèPABãEçàE TOäOB à çAèPüÜEçàâ:'/		NOM03130
     *'     U_CTATOPA_<MS1> := + U_CTATOPA  '/				NOM03140
     *'     I_CTATOPA_<MS1> := - I_CTATOPA  '/				NOM03150
     *'     U_POTOPA__<MS1> := - U_POTOPA   '/				NOM03160
     *'     I_POTOPA__<MS1> := - I_POTOPA   '/				NOM03170
     *1X,125('*')//)							NOM03180
C********************************************************************** NOM03190
C********************************************************************** NOM03200
C********************************************************************** NOM03210
C********************************************************************** NOM03220
C********************************************************************** NOM03230
C********************************************************************** NOM03240
C********************************************************************** NOM03250
      STOP								NOM03260
      END								NOM03270
