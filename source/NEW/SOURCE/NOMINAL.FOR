CXXX****�OM��E�C "MS1" --> BEPC�� 1.0  (OT �E�A�P� 1986 �.)************ NOM00010
C  �PO�PAMMA: NOMINAL							NOM00020
C -----------								NOM00030
C �A��A�E��E: "PAC�ET" �CTA�OB�B�E�OC� �PO�ECCA ��. MA����		NOM00040
C -----------								NOM00050
C	       BBO� �EPE� C��C�� �OM, PAR, ZAD:				NOM00060
C		&�OM  U�- ���E��OE �E�CTB���EE �A�P��E��E (�B);		NOM00070
C		      I�- �A��OE �E�CTB���EE ��A�E��E TO�A(A);		NOM00080
C		      W0- �P��OBA� (C��XPO��A�) �ACTOTA (PA�).		NOM00090
C		      P�- A�T�B�A� MO��OCT�  (�BT);			NOM00100
C		      Q�- PEA�T�B�A� MO��OCT�(�BT);			NOM00110
C		      S�- C�O���E��E;					NOM00120
C		&END							NOM00130
C		&PAR  X - �����T�B�OE CO�POT�B�E��E CTATOPA (OM);	NOM00140
C		      XF- �����T�B�OE CO�POT�B�E��E POTOPA  (OM);	NOM00150
C		     XAF- �����T�B�OE CO�POT�B�E��E CTATOP-POTOP (OM);	NOM00160
C		      R - A�T�B�OE CO�POT�B�E��E CTATOPA (OM);		NOM00170
C		      RF- A�T�B�OE CO�POT�B�E��E POTOPA  (OM);		NOM00180
C		      �I- �O������E�T �P�BE�E��� �O TO��;		NOM00190
C		      �U- �O������E�T �P�BE�E��� �O �A�P��E���;		NOM00200
C		      GD�B - MAXOB�� MOME�T ( �� * M * M );		NOM00210
C		      PTAU - �C�O �AP �O��COB;				NOM00220
C		&END							NOM00230
C									NOM00240
C		&ZAD  M -�A�P��O獛� MOME�T BA�A POTOPA (�*M);		NOM00250
C		      S -TPE��EMA� BE��爍A C�O���E���;			NOM00260
C		      �ODQ=+1-B��OP Q-PE��MA �CXO�� �� �OCTO��CTBA	NOM00270
C			      TO�A POTOPA;				NOM00280
C			  =-1-B��OP Q-PE��MA �CXO�� �� �OCTO��CTBA	NOM00290
C			    TO�A CTATOPA;				NOM00300
C			  = 0- TPE��ETC� �A�AT� Q-PE��M, ��A�AB:	NOM00310
C		     SZAD - QZAD - �A�A��A� PEA�T�B. MO��OCT� B �.BAP;	NOM00320
C			    B �TOM �E C��C�E;				NOM00330
C		&END							NOM00340
C									NOM00350
C	  !!!!	�A�POCOB &ZAD....&END  MO�ET ��T� ���OE �O���ECTBO	NOM00360
C	      ------------------------------------------------------	NOM00370
C									NOM00380
C  METO�: CO��AC�O:�A�AP�� �.�.AC��XPO����POBA���E C��XPO���E		NOM00390
C -------	   MA����.-M.:��EP�OATOM���AT,1984.-192 C.		NOM00400
C									NOM00410
C									NOM00420
CZZZ******************************************************************* NOM00430
      NAMELIST/�OM/U�,I�,W0,P�,Q�,S�					NOM00440
      NAMELIST/PAR/X,XF,XAF,R,RF,�I,�U, GD�B, PTAU			NOM00450
      NAMELIST/ZAD/M,S,�ODQ, QZAD					NOM00460
      REAL IB,MB,MU,IF�,I,MMAX, M, �I, �U, NW0				NOM00470
      REAL I�,IQ�,ID�,IQ,ID,I,IFQ,IFD,IF,IQ1,ID1,I1,IFQ1,IFD1,IF1	NOM00480
      REAL IQ�1,ID�1,I�1,IFQ�1,IFD�1,IF�1,IFD�,IFQ�			NOM00490
C	 U�-���E��OE �E�CTB���EE �A�P��E��E,				NOM00500
C	 I�-�E�CTB���EE ��A�E��E TO�A,					NOM00510
C	 W0-�ACTOTA CET�.						NOM00520
       QZAD = 0.0							NOM00530
      NRDR=5								NOM00540
      NWTR=6								NOM00550
      READ (NRDR,�OM,END=666)						NOM00560
      READ (NRDR,PAR,END=666)						NOM00570
      READ (NRDR,ZAD,END=666)						NOM00580
      WRITE (NWTR,1)							NOM00590
    1 FORMAT(/26X,'***	 PAC�ET �CTA�OB�B�E�OC� PE��MA ��E�TP��EC�O� ', NOM00600
     *'MA����	***'/26X,62('-')/)					NOM00610
C-->   �A�OB�E BE��爍�							NOM00620
      UB=1000.*U�*SQRT(2.)/SQRT(3.)					NOM00630
      IB=I�*SQRT(2.)							NOM00640
      ZB=UB/IB								NOM00650
      SB=1.5*UB*IB							NOM00660
      WB=W0								NOM00670
C-->  O�-T� B M��.							NOM00680
       NW0 = ( W0*30.0)/(3.1415927*PTAU)				NOM00690
      MB=(SB/WB) * PTAU							NOM00700
      MU=XAF**2/(X*XF)							NOM00710
C-->   B O.E. � � CTATOP�						NOM00720
      XOLD=X								NOM00730
      XFOLD=XF								NOM00740
      XAFOLD=XAF							NOM00750
      ROLD=R								NOM00760
      RFOLD=RF								NOM00770
      X=(X/ZB)								NOM00780
      XF=(XF/ZB)*�U*�I							NOM00790
      XAF=(XAF/ZB)*�I							NOM00800
      R=R/ZB								NOM00810
      RF=(RF/ZB)*�U*�I							NOM00820
      U�OLD=U�								NOM00830
      U�=1.								NOM00840
      X1=(1.-MU)*X							NOM00850
      P�OLD=P�								NOM00860
      Q�OLD=Q�								NOM00870
      P�=P�*1000.0							NOM00880
      Q�=Q�*1000.0							NOM00890
      P�=P�/SB								NOM00900
      Q�=Q�/SB								NOM00910
       SB�BA = SB / 1000.0						NOM00920
      WRITE (NWTR,2) U�OLD,UB,I�,IB,W0,ZB,P�OLD,MB,Q�OLD,SB�BA ,S�	NOM00930
C									NOM00940
    2 FORMAT(/15X,'   �A���E �OM��A���O�O PE��MA:   ',			NOM00950
     *	      28X,' �A��C��E BE��爍�:'/				NOM00960
     *	      15X,'  ----------------------------   ',			NOM00970
     *	      28X,'-------------------'/				NOM00980
     *1X,' ���E��OE �E�CTB���EE �A�P��E��E  U�=',G12.5,' �B	',	NOM00990
     *5X,' �A�P��E��E (�A�� CTATOPA) UB=',G12.5,' B'/			NOM01000
     *1X,' �A��OE �E�CTB���EE ��A�E��E TO�A I�=',G12.5,' A	',	NOM01010
     *5X,' TO� (�A�� CTATOPA)	     IB=',G12.5,' A'/			NOM01020
     *1X,' �P��OBA� (C��XPO��A�) �ACTOTA    W0=',G12.5,' PA�/CE�',	NOM01030
     *5X,' CO�POT�B�E��E	     ZB=',G12.5,' OM'/			NOM01040
     *1X,' A�T�B�A� MO��OCT�		    P�=',G12.5,' �BATT	',	NOM01050
     *5X,' MOME�T �A BA�� MA����     MB=',G12.5,' �*M'/			NOM01060
     *1X,' PEA�T�B�A� MO��OCT�		    Q�=',G12.5,' �BAP	',	NOM01070
     *5X,' �O��A� MO��OCT�	     SB=',G12.5,' �BA'/			NOM01080
     *1X,' C�O���E��E			    S�=',G12.5,' O.E.'/ )	NOM01090
C--->  B CE�--> GD�B <=> SB_�BA						NOM01100
       �J = 2.74 * (GD�B*NW0)**2 * 1.0E-6 / ( SB�BA )			NOM01110
C									NOM01120
C--->									NOM01130
      WRITE (NWTR,3)XOLD,X,XFOLD,XF,XAFOLD,XAF,ROLD,R,RFOLD,RF,�I,MU,�U,NOM01140
     *	  �J								NOM01150
    3 FORMAT(/15X,'  �APAMETP� ��E�TP��EC�O� MA����:',			NOM01160
     *	      25X,'  �P�BE�E���E �APAMETP� ��E�TP��EC�O� MA����:'/	NOM01170
     *	      15X,' --------------------------------',			NOM01180
     *	      25X,' --------------------------------------------'/	NOM01190
     *1X,' �����T�B�OE CO�POT�B�E��E CTATOPA	    X=',G12.5,' OM  ',	NOM01200
     *1X,' �����T�B�OE CO�POT�B�E��E CTATOPA	    X=',G12.5,'O.E.'/	NOM01210
     *1X,' �����T�B�OE CO�POT�B�E��E POTOPA	   XF=',G12.5,' OM  ',	NOM01220
     *1X,' �����T�B�OE CO�POT�B�E��E POTOPA	   XF=',G12.5,'O.E.'/	NOM01230
     *1X,' �����T�B�OE CO�POT�B�E��E CTATOP-POTOP XAF=',G12.5,' OM  ',	NOM01240
     *1X,' �����T�B�OE CO�POT�B�E��E CTATOP-POTOP XAF=',G12.5,'O.E.'/	NOM01250
     *1X,' A�T�B�OE CO�POT�B�E��E CTATOPA	    R=',G12.5,' OM  ',	NOM01260
     *1X,' A�T�B�OE CO�POT�B�E��E CTATOPA	    R=',G12.5,'O.E.'/	NOM01270
     *1X,' A�T�B�OE CO�POT�B�E��E POTOPA	   RF=',G12.5,' OM  ',	NOM01280
     *1X,' A�T�B�OE CO�POT�B�E��E POTOPA	   RF=',G12.5,'O.E.'/	NOM01290
     *1X,' �O������E�T �P�BE�E��� �O TO��	   �I=',G12.5,' O.E.',	NOM01300
     *1X,' �O������E�T MA���T�O� CB���		   MU=',G12.5/		NOM01310
     *1X,' �O������E�T �P�BE�E��� �O �A�P��E���    �U=',G12.5,' O.E.',	NOM01320
     *1X,' ��EP��O��A� �OCTO���A�		   �J=',G12.5,' CE�.')	NOM01330
C									NOM01340
       WRITE(NWTR,3000)        PTAU, GD�B				NOM01350
3000   FORMAT(								NOM01360
     *1X,' �C�O �AP �O��COB			 PTAU=', G12.5,' �T.' / NOM01370
     *1X,' MAXOB�� MOME�T			 GD�B=',G12.5,' ��*M*M' NOM01380
     * / 1X,125('*')//)							NOM01390
C-->   PAC�ET								NOM01400
      IQ�=P�/U�								NOM01410
      ID�=-Q�/U�							NOM01420
      I�=SQRT(IQ�**2+ID�**2)						NOM01430
      EQ�=-P�*X/U�							NOM01440
      ED�=(Q�*X+U�**2)/U�						NOM01450
      E�=SQRT(EQ�**2+ED�**2)						NOM01460
      IFQ�=EQ�/XAF							NOM01470
      IFD�=ED�/XAF							NOM01480
      IF�=SQRT(IFQ�**2+IFD�**2)						NOM01490
      UFQ�=(U�**2*S�*XF/XAF-RF*X*P�/XAF+S�*X1*XF*Q�/XAF)/U�		NOM01500
      UFD�=(U�**2*RF/XAF+RF*X*Q�/XAF+S�*X1*XF*P�/XAF)/U�		NOM01510
      UF�=SQRT(UFQ�**2+UFD�**2)						NOM01520
C-->	�ME�OBA���E E������						NOM01530
      IQ�1=IQ�*IB							NOM01540
      ID�1=ID�*IB							NOM01550
      I�1=I�*IB								NOM01560
      EQ�1=EQ�*UB							NOM01570
      ED�1=ED�*UB							NOM01580
      E�1=E�*UB								NOM01590
      IFQ�1=IFQ�*�I*IB							NOM01600
      IFD�1=IFD�*�I*IB							NOM01610
      IF�1=IF�*�I*IB							NOM01620
      UFQ�1=UFQ�*(UB/�U)						NOM01630
      UFD�1=UFD�*(UB/�U)						NOM01640
      UF�1=UF�*(UB/�U)							NOM01650
      FI�=ATAN2(ID�,IQ�)*57.29578					NOM01660
      FE�=ATAN2(ED�,EQ�)*57.29578					NOM01670
      FIF�=ATAN2(IFD�,IFQ�)*57.29578					NOM01680
      FUF�=ATAN2(UFD�,UFQ�)*57.29578					NOM01690
      WRITE (NWTR,6) IQ�1,EQ�1,IFQ�1,UFQ�1,				NOM01700
     *		     ID�1,ED�1,IFD�1,UFD�1,				NOM01710
     *		     I�1,E�1,IF�1,UF�1,					NOM01720
     *		     FI�,FE�,FIF�,FUF�					NOM01730
    6 FORMAT(/45X,' �OM��A����� PE��M:'/				NOM01740
     *	      45X,'-------------------'//1X,'I----------I',		NOM01750
     *4('-------------------------I')/	 1X,'I		I',		NOM01760
     *'   TO� CTATOP�O� �E��	I   ��C CTATOP�O� �E��	  I',		NOM01770
     *'    TO� POTOP�O� �E��	I    �A�P��E��E POTOPA	  I'/		NOM01780
     *1X,'I----------I',4('-------------------------I')/		NOM01790
     *1X,'I �O OC� Q I',4(6X,G12.5,7X,'I')/				NOM01800
     *1X,'I �O OC� D I',4(6X,G12.5,7X,'I')/				NOM01810
     *1X,'I  MO����  I',4(6X,G12.5,7X,'I')/				NOM01820
     *1X,'I   ��O�   I',4(6X,G12.5,7X,'I')/				NOM01830
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	NOM01840
  100 CONTINUE								NOM01850
      OM1=(1.+S)*W0							NOM01860
      WRITE(NWTR,10) M,S,OM1						NOM01870
10    FORMAT(/40X,' �APAMETP� �A�A��O�O PE��MA:'/			NOM01880
     *	      40X,'----------------------------'/			NOM01890
     *1X,' �A�P��O獛� MOME�T BA�A POTOPA  M=',G12.5,' �*M'/		NOM01900
     *1X,' C�O���E��E			   S=',G12.5/			NOM01910
     *1X,' ���OBA� C�OPOCT� POTOPA     OMEGA=',G12.5,' PA�./CE�' )	NOM01920
      P=M*W0/SB								NOM01930
      IF(�ODQ) 11,12,13							NOM01940
   11 CONTINUE								NOM01950
C-->   Q<0  �� �C�OB��	I=I�						NOM01960
      DTQ=((U�*I�)**2)-P**2						NOM01970
      MMAX=((U�*I�)**2)*SB/W0						NOM01980
      WRITE (NWTR,14)�ODQ						NOM01990
   14 FORMAT(/1X,'Q O�PE�E��ETC� �O �C�OB�� �OCTO��CTBA ',		NOM02000
     *'TO�A CTATOPA ( Q<0, �ODQ=',I3,' )'/)				NOM02010
      IF(DTQ.LT.0.0) WRITE(NWTR,15) M,MMAX				NOM02020
   15 FORMAT(//1X,'*** B��MA��E! �A�PO�E���� MOME�T M=',G12.5,		NOM02030
     *'�PEB��AET MA�C�MA����� MMAX=',G12.5,' ***'//)			NOM02040
      IF(DTQ.LT.0.0) GO TO 101						NOM02050
      Q=-SQRT(DTQ)							NOM02060
      GO TO 16								NOM02070
   12 CONTINUE								NOM02080
      WRITE(NWTR,17) �ODQ						NOM02090
   17 FORMAT(/1X,'Q O�PE�E��ETC� �O �A�A��� �O���OBATE�� ',		NOM02100
     *			   ' ( �ODQ=',I3,')'/ )				NOM02110
C									NOM02120
C-->  �P�BE�EM..							NOM02130
       Q = ( QZAD *  1000.0 ) / SB					NOM02140
C--->									NOM02150
      GO TO 16								NOM02160
   13 CONTINUE								NOM02170
      WRITE(NWTR,1300) �ODQ						NOM02180
 1300 FORMAT(/1X,'Q O�PE�E��ETC� �O �C�OB�� �OCTO��CTBA',		NOM02190
     *' TO�A POTOPA (Q>0, �ODQ=',I3,')'/)				NOM02200
      D1=(E�**2)*(U�**2)-(P*X)**2					NOM02210
      MMAX=(E�**2)*(U�**2)*SB/W0					NOM02220
      IF(D1.LT.0.0) WRITE(NWTR,15) M,MMAX				NOM02230
      IF(D1.LT.0.0) GO TO 101						NOM02240
      D2=SQRT(D1)-U�**2							NOM02250
      Q=D2/X								NOM02260
   16 CONTINUE								NOM02270
C--->	Q �A���								NOM02280
      Q1=(Q*SB)/1000.							NOM02290
      P1=(P*SB)/1000.							NOM02300
      WRITE(NWTR,18) P1,Q1						NOM02310
   18 FORMAT(1X,'  A�T�B�A� MO��OCT�   P=',G12.5,'�BAT'/		NOM02320
     *	     1X,'  PEA�T�B�A� MO��OCT� Q=',G12.5,'�BAP')		NOM02330
C									NOM02340
C-->  �CTO��BO �� �TO....						NOM02350
       TF1 = ( 1.0 - MU ) * XF / RF					NOM02360
C  �P� S > 0								NOM02370
       QXX1 = S * TF1 * P - U�**2/X1					NOM02380
C  �P� S < 0								NOM02390
       QXX2 = -2.0*�J* (1. + (S*TF1)**2)/(TF1**2) -			NOM02400
     -	       S*TF1*P - U�**2/X1					NOM02410
C									NOM02420
       QXX1I = (QXX1*SB)/1000.0						NOM02430
       QXX2I = (QXX2*SB)/1000.0						NOM02440
       IF ( S .GE. 0.0 )						NOM02450
     *			 WRITE(NWTR,700) QXX1I				NOM02460
700    FORMAT(' ( S > 0, PE��M �CTO��B �P� Q > ', G12.5, '  �BAP ) ' ) NOM02470
C									NOM02480
       IF ( S .LE. 0.0 )						NOM02490
     *			 WRITE(NWTR,7000) QXX2I				NOM02500
7000   FORMAT(' ( S < 0, PE��M �CTO��B �P� Q > ', G12.5, '  �BAP ) ' ) NOM02510
C									NOM02520
       IF ( S .GT. 0.0 .AND. Q .GT. QXX1 ) GO TO 705			NOM02530
       IF ( S .LT. 0.0 .AND. Q .GT. QXX2 ) GO TO 705			NOM02540
       IF ( S .EQ. 0.0 .AND. Q .GT. QXX1 .AND. Q .GT. QXX2 ) GO TO 705	NOM02550
C****									NOM02560
       WRITE(NWTR, 706 )						NOM02570
706    FORMAT(/' ***** B��MA��E ---> BO�MO��A �E�CTO��BOCT� PE��MA',	NOM02580
     *	       ' *****' / )						NOM02590
C--->	PAC�ET								NOM02600
705   IQ=P/U�								NOM02610
      ID=-Q/U�								NOM02620
      I=SQRT(IQ**2+ID**2)						NOM02630
      EQ=-P*X/U�							NOM02640
      ED=(Q*X+U�**2)/U�							NOM02650
      E=SQRT(EQ**2+ED**2)						NOM02660
      IFQ=EQ/XAF							NOM02670
      IFD=ED/XAF							NOM02680
      IF=SQRT(IFQ**2+IFD**2)						NOM02690
      UFQ=(U�**2*S*XF/XAF-RF*X*P/XAF+S*X1*XF*Q/XAF)/U�			NOM02700
      UFD=(U�**2*RF/XAF+RF*X*Q/XAF+S*X1*XF*P/XAF)/U�			NOM02710
      UF=SQRT(UFQ**2+UFD**2)						NOM02720
C-->	�ME�OBA���E E������						NOM02730
      IQ1=IQ*IB								NOM02740
      ID1=ID*IB								NOM02750
      I1=I*IB								NOM02760
      EQ1=EQ*UB								NOM02770
      ED1=ED*UB								NOM02780
      E1=E*UB								NOM02790
      IFQ1=IFQ*�I*IB							NOM02800
      IFD1=IFD*�I*IB							NOM02810
      IF1=IF*�I*IB							NOM02820
      UFQ1=UFQ*(UB/�U)							NOM02830
      UFD1=UFD*(UB/�U)							NOM02840
      UF1=UF*(UB/�U)							NOM02850
      FI=ATAN2(ID,IQ)*57.29578						NOM02860
      FE=ATAN2(ED,EQ)*57.29578						NOM02870
      FIF=ATAN2(IFD,IFQ)*57.29578					NOM02880
      FUF=ATAN2(UFD,UFQ)*57.29578					NOM02890
      WRITE (NWTR,60)IQ1,EQ1,IFQ1,UFQ1,					NOM02900
     *		     ID1,ED1,IFD1,UFD1,					NOM02910
     *		     I1,E1,IF1,UF1,					NOM02920
     *		     FI,FE,FIF,FUF					NOM02930
   60 FORMAT(/45X,'   �A�A���� PE��M:'/					NOM02940
     *	      45X,'  ----------------'//1X,'I----------I',		NOM02950
     *4('-------------------------I')/	 1X,'I		I',		NOM02960
     *'   TO� CTATOP�O� �E��	I   ��C CTATOP�O� �E��	  I',		NOM02970
     *'    TO� POTOP�O� �E��	I    �A�P��E��E POTOPA	  I'/		NOM02980
     *1X,'I----------I',4('-------------------------I')/		NOM02990
     *1X,'I �O OC� Q I',4(6X,G12.5,7X,'I')/				NOM03000
     *1X,'I �O OC� D I',4(6X,G12.5,7X,'I')/				NOM03010
     *1X,'I  MO����  I',4(6X,G12.5,7X,'I')/				NOM03020
     *1X,'I   ��O�   I',4(6X,G12.5,7X,'I')/				NOM03030
     *1X,'I----------I',4('-------------------------I')//1X,125('*'))	NOM03040
C********************************************************************** NOM03050
C********************************************************************** NOM03060
C--->	�OB�� �A�A���� PE��M						NOM03070
101   READ (NRDR,ZAD,END=666)						NOM03080
      GO TO 100								NOM03090
  666 CONTINUE								NOM03100
      WRITE (NWTR,6660)							NOM03110
 6660 FORMAT(/' *** B��MA��E *** ��� �OM��E�CA MS1 �P���TO �P��OE',	NOM03120
     *' �C�OB�O �O�O��TE���OE �A�PAB�E��E TO�OB � �A�P��E���:'/		NOM03130
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
