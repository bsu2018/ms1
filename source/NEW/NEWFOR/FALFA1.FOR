         SUBROUTINE FALFA1(EDM,E, XC,BETA1,BETA2,TOK,BETA )
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   FBETA(EDM,E,XG,X,ALFA,TOK )
C   ------------
C   HA�HA�EH�E:    ��宦����� ��७� �ࠢ����� F(BETA) - TOK = 0
C
C   METO�:         ���⮭�
C
C   �APAMETP�:     EDM  - ������㤠 ��� ����饩 ��
C   ---------      E    - ��⨢����
C                  XC1   - �㬬�୮� ����⨢��� ᮯ�⨢�����
C                  XG    - ����⨢��� ᮯ�⨢����� 
C                  BETA1 - ���ᨬ���� ����� �⯨࠭�� ��।���� ���⨫�
C                  BETA2 - ��������� 㣮�� �⯨࠭�� ��।���� ���⨫�
C                  TOK  - ���பᨬ�஢����� �।��� ���祭�� ⮪�
C
C
c*****************************************************
         DATA PI /3.141593/ , EPS/0.001/
         DO 10 I = 1,100
         BETA = ( BETA1 + BETA2)*0.5
         FF = SIN(BETA + PI/6)*EDM/XC  -   E*BETA/XC - TOK
         IF (ABS(FF) .LT. EPS)  RETURN
C            write(*,*) alfa,ff
C            pause
         IF ( FF.GT.0 )    BETA1 = BETA
         IF ( FF.LT.0 )    BETA2 = BETA
 10      CONTINUE
         WRITE(*,*) '��襭�� �ࠢ����� �ࠢ����� �� �� �室���� '
         WRITE(*,*) 'BETA2 =',BETA2,'BETA1=',BETA1,'FF',FF,TOK
         PAUSE
         RETURN
         END
