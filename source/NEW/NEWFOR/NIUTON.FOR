         SUBROUTINE NIUTON(EDM,E, XG,X,ALFA1,ALFA2,TOK )
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   NIUTON(EDM,E,XG,X,ALFA,TOK )
C   ------------
C   HA�HA�EH�E:    ��宦����� ��७� �ࠢ����� F(ALFA) - TOK = 0
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
         FF   = ( SIN(ALFA + PI/6) -  SIN(ALFA - PI/6) /X) * EDM/XG 
     =  + (1/X - 1)*ALFA*E/XG - E*PI/(3.*XG) - TOK        
         IF (ABS(FF) .LT. EPS)  RETURN
C            write(*,*) alfa,ff
C            pause
         IF ( FF.GT.0 )    ALFA1 = ALFA
         IF ( FF.LT.0 )    ALFA2 = ALFA
 10      CONTINUE
         WRITE(*,*) '��襭�� �ࠢ����� �ࠢ����� �� �� �室���� '
         WRITE(*,*) 'ALFA2 =',ALFA2,'ALFA1 = ',ALFA1
         RETURN
         END
