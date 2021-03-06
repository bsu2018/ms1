CXXX****�OM��E�C "MS1" --> BEPC�� 1.0  (OT �E�A�P� 1986 �.)************ 
C  �PO�PAMMA: NOMINAL							
C -----------								
C HA�HA�EH�E: "PAC�ET" �CTAHOB�B�E�OC� �PO�ECCA ��. MA��H�		
C -----------								
C	       BBO� �EPE� C��CK� HOM, PAR, ZAD:				
C		&HOM  UH- ��HE�HOE �E�CTB���EE HA�P��EH�E (KB);	
C		      IH- �A�HOE �E�CTB���EE �HA�EH�E TOKA(A);		
C		      W0- KP��OBA� (C�HXPOHHA�) �ACTOTA (PA�).		
C		      PH- AKT�BHA� MO�HOCT�  (KBT);			
C		      QH- PEAKT�BHA� MO�HOCT�(KBT);			
C		      SH- CKO���EH�E;					
C		&END							
C		&PAR  X - �H��KT�BHOE CO�POT�B�EH�E CTATOPA (OM);	
C		      XF- �H��KT�BHOE CO�POT�B�EH�E POTOPA  (OM);	
C		     XAF- �H��KT�BHOE CO�POT�B�EH�E CTATOP-POTOP (OM);	
C		      R - AKT�BHOE CO�POT�B�EH�E CTATOPA (OM);		
C		      RF- AKT�BHOE CO�POT�B�EH�E POTOPA  (OM);		
C		      KI- KO������EHT �P�BE�EH�� �O TOK�;		
C		      KU- KO������EHT �P�BE�EH�� �O HA�P��EH��;		
C		      GDKB - MAXOB�� MOMEHT ( K� * M * M );		
C		      PTAU - �C�O �AP �O��COB;				
C		&END							
C									
C		&ZAD  M -HA�P��O�H�� MOMEHT BA�A POTOPA (H*M);		
C		      S -TPE��EMA� BE���HA CKO���EH��;			
C		      KODQ=+1-B��OP Q-PE��MA �CXO�� �� �OCTO�HCTBA	
C			      TOKA POTOPA;				
C			  =-1-B��OP Q-PE��MA �CXO�� �� �OCTO�HCTBA	
C			    TOKA CTATOPA;				
C			  = 0- TPE��ETC� �A�AT� Q-PE��M, �KA�AB:	
C		     SZAD - QZAD - �A�AHHA� PEAKT�B. MO�HOCT� B K.BAP;	
C			    B �TOM �E C��CKE;				
C		&END							
C									
C	  !!!!	�A�POCOB &ZAD....&END  MO�ET ��T� ���OE KO���ECTBO	
C	      ------------------------------------------------------	
C									
C  METO�: CO��ACHO:�AKAP�H �.�.AC�HXPOH���POBAHH�E C�HXPOHH�E		
C -------	   MA��H�.-M.:�HEP�OATOM���AT,1984.-192 C.		
C									
C									
CZZZ******************************************************************* 
      NAMELIST/HOM/UH,IH,W0,PH,QH,SH					
      NAMELIST/PAR/X,XF,XAF,R,RF,KI,KU, GDKB, PTAU			
      NAMELIST/ZAD/M,S,KODQ, QZAD					
      REAL IB,MB,MU,IFH, MMAX, M, KI, KU, NW0				
      REAL IH,IQH,IDH,IQ,ID,I,IFQ,IFD,IF,IQ1,ID1,I1,IFQ1,IFD1,IF1	
      REAL IQH1,IDH1,IH1,IFQH1,IFDH1,IFH1,IFDH,IFQH			
C	 UH-��HE�HOE �E�CTB���EE HA�P��EH�E,				
C	 IH-�E�CTB���EE �HA�EH�E TOKA,					
C	 W0-�ACTOTA CET�.						
       QZAD = 0.0							
      NRDR=5								
      NWTR=6								
      READ (NRDR,HOM,END=666)						
      READ (NRDR,PAR,END=666)						
      READ (NRDR,ZAD,END=666)						
      WRITE (NWTR,1)							
    1 FORMAT(/26X,'***	 PAC�ET �CTAHOB�B�E�OC� PE��MA ��EKTP��ECKO� ', 
     *'MA��H�	***'/26X,62('-')/)					
C-->   �A�OB�E BE���H�							
      UB=1000.*UH*SQRT(2.)/SQRT(3.)					
      IB=IH*SQRT(2.)							
      ZB=UB/IB								
      SB=1.5*UB*IB							
      WB=W0								
C-->  O�-T� B M�H.							
       NW0 = ( W0*30.0)/(3.1415927*PTAU)				
      MB=(SB/WB) * PTAU							
      MU=XAF**2/(X*XF)							
C-->   B O.E. � K CTATOP�						
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
    2 FORMAT(/15X,'   �AHH�E HOM�HA��HO�O PE��MA:   ',			
     *	      28X,' �A��CH�E BE���H�:'/				
     *	      15X,'  ----------------------------   ',			
     *	      28X,'-------------------'/				
     *1X,' ��HE�HOE �E�CTB���EE HA�P��EH�E  UH=',G12.5,' KB	',	
     *5X,' HA�P��EH�E (�A�� CTATOPA) UB=',G12.5,' B'/			
     *1X,' �A�HOE �E�CTB���EE �HA�EH�E TOKA IH=',G12.5,' A	',	
     *5X,' TOK (�A�� CTATOPA)	     IB=',G12.5,' A'/			
     *1X,' KP��OBA� (C�HXPOHHA�) �ACTOTA    W0=',G12.5,' PA�/CEK',	
     *5X,' CO�POT�B�EH�E	     ZB=',G12.5,' OM'/			
     *1X,' AKT�BHA� MO�HOCT�		    PH=',G12.5,' KBATT	',	
     *5X,' MOMEHT HA BA�� MA��H�     MB=',G12.5,' H*M'/			
     *1X,' PEAKT�BHA� MO�HOCT�		    QH=',G12.5,' KBAP	',	
     *5X,' �O�HA� MO�HOCT�	     SB=',G12.5,' KBA'/			
     *1X,' CKO���EH�E			    SH=',G12.5,' O.E.'/ )	
C--->  B CEK--> GDKB <=> SB_KBA						
       HJ = 2.74 * (GDKB*NW0)**2 * 1.0E-6 / ( SBKBA )			
C									
C--->									
      WRITE (NWTR,3)XOLD,X,XFOLD,XF,XAFOLD,XAF,ROLD,R,RFOLD,RF,KI,MU,KU,
     *	  HJ								
    3 FORMAT(/15X,'  �APAMETP� ��EKTP��ECKO� MA��H�:',			
     *	      25X,'  �P�BE�EHH�E �APAMETP� ��EKTP��ECKO� MA��H�:'/	
     *	      15X,' --------------------------------',			
     *	      25X,' --------------------------------------------'/	
     *1X,' �H��KT�BHOE CO�POT�B�EH�E CTATOPA	    X=',G12.5,' OM  ',	
     *1X,' �H��KT�BHOE CO�POT�B�EH�E CTATOPA	    X=',G12.5,'O.E.'/	
     *1X,' �H��KT�BHOE CO�POT�B�EH�E POTOPA	   XF=',G12.5,' OM  ',	
     *1X,' �H��KT�BHOE CO�POT�B�EH�E POTOPA	   XF=',G12.5,'O.E.'/	
     *1X,' �H��KT�BHOE CO�POT�B�EH�E CTATOP-POTOP XAF=',G12.5,' OM  ',	
     *1X,' �H��KT�BHOE CO�POT�B�EH�E CTATOP-POTOP XAF=',G12.5,'O.E.'/	
     *1X,' AKT�BHOE CO�POT�B�EH�E CTATOPA	    R=',G12.5,' OM  ',	
     *1X,' AKT�BHOE CO�POT�B�EH�E CTATOPA	    R=',G12.5,'O.E.'/	
     *1X,' AKT�BHOE CO�POT�B�EH�E POTOPA	   RF=',G12.5,' OM  ',	
     *1X,' AKT�BHOE CO�POT�B�EH�E POTOPA	   RF=',G12.5,'O.E.'/	
     *1X,' KO������EHT �P�BE�EH�� �O TOK�	   KI=',G12.5,' O.E.',	
     *1X,' KO������EHT MA�H�THO� CB���		   MU=',G12.5/		
     *1X,' KO������EHT �P�BE�EH�� �O HA�P��EH��    KU=',G12.5,' O.E.',	
     *1X,' �HEP��OHHA� �OCTO�HHA�		   HJ=',G12.5,' CEK.')	
C									
       WRITE(NWTR,3000)        PTAU, GDKB				
3000   FORMAT(								
     *1X,' �C�O �AP �O��COB			 PTAU=', G12.5,' �T.' / 
     *1X,' MAXOB�� MOMEHT			 GDKB=',G12.5,' K�*M*M' 
     * / 1X,125('*')//)							
C-->   PAC�ET								
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
C-->	�MEHOBAHH�E E��H���						
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
    6 FORMAT(/45X,' HOM�HA��H�� PE��M:'/				
     *	      45X,'-------------------'//1X,'I----------I',		
     *4('-------------------------I')/	 1X,'I		I',		
     *'   TOK CTATOPHO� �E��	I   ��C CTATOPHO� �E��	  I',		
     *'    TOK POTOPHO� �E��	I    HA�P��EH�E POTOPA	  I'/		
     *1X,'I----------I',4('-------------------------I')/		
     *1X,'I �O OC� Q I',4(6X,G12.5,7X,'I')/				
     *1X,'I �O OC� D I',4(6X,G12.5,7X,'I')/				
     *1X,'I  MO����  I',4(6X,G12.5,7X,'I')/				
     *1X,'I   ��O�   I',4(6X,G12.5,7X,'I')/				
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	
  100 CONTINUE								
      OM1=(1.+S)*W0							
      WRITE(NWTR,10) M,S,OM1						
10    FORMAT(/40X,' �APAMETP� �A�AHHO�O PE��MA:'/			
     *	      40X,'----------------------------'/			
     *1X,' HA�P��O�H�� MOMEHT BA�A POTOPA  M=',G12.5,' H*M'/		
     *1X,' CKO���EH�E			   S=',G12.5/			
     *1X,' ���OBA� CKOPOCT� POTOPA     OMEGA=',G12.5,' PA�./CEK' )	
      P=M*W0/SB								
      IF(KODQ) 11,12,13							
   11 CONTINUE								
C-->   Q<0  �� �C�OB��	I=IH						
      DTQ=((UH*IH)**2)-P**2						
      MMAX=((UH*IH)**2)*SB/W0						
      WRITE (NWTR,14)KODQ						
   14 FORMAT(/1X,'Q O�PE�E��ETC� �O �C�OB�� �OCTO�HCTBA ',		
     *'TOKA CTATOPA ( Q<0, KODQ=',I3,' )'/)				
      IF(DTQ.LT.0.0) WRITE(NWTR,15) M,MMAX				
   15 FORMAT(//1X,'*** BH�MAH�E! �A�PO�EHH�� MOMEHT M=',G12.5,		
     *'�PEB��AET MAKC�MA��H�� MMAX=',G12.5,' ***'//)			
      IF(DTQ.LT.0.0) GO TO 101						
      Q=-SQRT(DTQ)							
      GO TO 16								
   12 CONTINUE								
      WRITE(NWTR,17) KODQ						
   17 FORMAT(/1X,'Q O�PE�E��ETC� �O �A�AH�� �O���OBATE�� ',		
     *			   ' ( KODQ=',I3,')'/ )				
C									
C-->  �P�BE�EM..							
       Q = ( QZAD *  1000.0 ) / SB					
C--->									
      GO TO 16								
   13 CONTINUE								
      WRITE(NWTR,1300) KODQ						
 1300 FORMAT(/1X,'Q O�PE�E��ETC� �O �C�OB�� �OCTO�HCTBA',		
     *' TOKA POTOPA (Q>0, KODQ=',I3,')'/)				
      D1=(EH**2)*(UH**2)-(P*X)**2					
      MMAX=(EH**2)*(UH**2)*SB/W0					
      IF(D1.LT.0.0) WRITE(NWTR,15) M,MMAX				
      IF(D1.LT.0.0) GO TO 101						
      D2=SQRT(D1)-UH**2							
      Q=D2/X								
   16 CONTINUE								
C--->	Q HA���								
      Q1=(Q*SB)/1000.							
      P1=(P*SB)/1000.							
      WRITE(NWTR,18) P1,Q1						
   18 FORMAT(1X,'  AKT�BHA� MO�HOCT�   P=',G12.5,'KBAT'/		
     *	     1X,'  PEAKT�BHA� MO�HOCT� Q=',G12.5,'KBAP')		
C									
C-->  �CTO��BO �� �TO....						
       TF1 = ( 1.0 - MU ) * XF / RF					
C  �P� S > 0								
       QXX1 = S * TF1 * P - UH**2/X1					
C  �P� S < 0								
       QXX2 = -2.0*HJ* (1. + (S*TF1)**2)/(TF1**2) -			
     -	       S*TF1*P - UH**2/X1					
C									
       QXX1I = (QXX1*SB)/1000.0						
       QXX2I = (QXX2*SB)/1000.0						
       IF ( S .GE. 0.0 )						
     *			 WRITE(NWTR,700) QXX1I				
700    FORMAT(' ( S > 0, PE��M �CTO��B �P� Q > ', G12.5, '  KBAP ) ' ) 
C									
       IF ( S .LE. 0.0 )						
     *			 WRITE(NWTR,7000) QXX2I				
7000   FORMAT(' ( S < 0, PE��M �CTO��B �P� Q > ', G12.5, '  KBAP ) ' ) 
C									
       IF ( S .GT. 0.0 .AND. Q .GT. QXX1 ) GO TO 705			
       IF ( S .LT. 0.0 .AND. Q .GT. QXX2 ) GO TO 705			
       IF ( S .EQ. 0.0 .AND. Q .GT. QXX1 .AND. Q .GT. QXX2 ) GO TO 705	
C****									
       WRITE(NWTR, 706 )						
706    FORMAT(/' ***** BH�MAH�E ---> BO�MO�HA HE�CTO��BOCT� PE��MA',	
     *	       ' *****' / )						
C--->	PAC�ET								
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
C-->	�MEHOBAHH�E E��H���						
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
   60 FORMAT(/45X,'   �A�AHH�� PE��M:'/					
     *	      45X,'  ----------------'//1X,'I----------I',		
     *4('-------------------------I')/	 1X,'I		I',		
     *'   TOK CTATOPHO� �E��	I   ��C CTATOPHO� �E��	  I',		
     *'    TOK POTOPHO� �E��	I    HA�P��EH�E POTOPA	  I'/		
     *1X,'I----------I',4('-------------------------I')/		
     *1X,'I �O OC� Q I',4(6X,G12.5,7X,'I')/				
     *1X,'I �O OC� D I',4(6X,G12.5,7X,'I')/				
     *1X,'I  MO����  I',4(6X,G12.5,7X,'I')/				
     *1X,'I   ��O�   I',4(6X,G12.5,7X,'I')/				
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	
C********************************************************************** 
C********************************************************************** 
C--->	HOB�� �A�AHH�� PE��M						
101   READ (NRDR,ZAD,END=666)						
      GO TO 100								
  666 CONTINUE								
      WRITE (NWTR,6660)							
 6660 FORMAT(/' *** BH�MAH�E *** ��� KOM��EKCA MS1 �P�H�TO �P��OE',	
     *' �C�OBHO �O�O��TE��HOE HA�PAB�EH�E TOKOB � HA�P��EH��:'/		
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
