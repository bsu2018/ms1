           
         SUBROUTINE  UALFA(TOK1,WT,EDM,ZC,X,FIC,R,E,ALFA1,ALFA2)
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:    UALFA(TOK1,WT,EDM,ZC,X,FIC,R,E,ALFA1,ALFA2)
C   ------------
C   HA�HA�EH�E:    ��宦����� ��७� �ࠢ����� U(ALFA) - TOK1 = 0
C
C   METO�:         ���⮭�
C
C   �APAMETP�:     EDM  - ������㤠 ��� ����饩 ��
C   ---------      E    - ��⨢����
C                  XG   - �㬬�୮� ����⨢��� ᮯ�⨢�����
C                  X    - ����⨢��� ᮯ�⨢����� 
C                  ALFA1 - ���ᨬ���� ����� �⯨࠭�� ��।���� ���⨫�
C                  ALFA2 - ��������� 㣮�� �⯨࠭�� ��।���� ���⨫�
C                  TOK  - ���பᨬ�஢����� �।��� ���祭�� ⮪�
C
C
c*****************************************************
         DATA PI /3.141593/ , EPS/0.001/
         DO 10 I = 1,100
         ALFA = ( ALFA1 + ALFA2)*0.5
         FF   = (EXP(-PI/(3*WT))*(-EDM/ZC*COS(ALFA - PI/6-FIC)+E/R) +
     +   EDM/ZC*COS(ALFA+PI/6-FIC) - E/R)/(1- EXP(-PI/(3*WT))*X) 
     -  -  TOK1       
         IF (ABS(FF) .LT. EPS)  RETURN
C            write(*,*) alfa,ff
C            pause
         IF ( FF.GT.0 )    ALFA1 = ALFA
         IF ( FF.LT.0 )    ALFA2 = ALFA
 10      CONTINUE
         WRITE(*,*) '��襭�� �ࠢ����� �ࠢ����� ualfa �� �室���� '
         WRITE(*,*) 'ALFA2 =',ALFA2,'ALFA1 = ',ALFA1,'FF',FF,TOK1
         PAUSE
         RETURN
         END
