         SUBROUTINE KSOPEN(kf, forma, irc)
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   KSOPEN(kf, forma, irc )
C   ------------
C   HA�HA�EH�E:    *** ����⨥ ������� �����/�뢮��  ***
C   -----------         ���������������� ������
C
C   METO�:         ��ନ஢���� ����� 䠩��:
C  -------
C                      Jname{masprt(125)}//'.K'//xx
C
C            xx - ����� ������ --> kf
C
C   �APAMETP�:     kf -   ����� ������
C   ---------      form=0 - 'FORMATTED', =1 - 'UNFORMATTED'
C                  irc = 0 - �� ���
C                      <> 0 - ���祭�� iostat
C   �O��PO�PAMM�:  1. COMMON PROF1
C
C   �P�ME�AH�E:    KOHTPO�� �� �஢������
C   ----------
CZZZ-------------------------------------------------------------------
         CHARACTER*2 kanal, k_kk
         COMMON/ PROF1 / MASPRT(200)
         EQUIVALENCE(JNAME,   MASPRT(125))
         EQUIVALENCE( NWTR,   MASPRT(13))
         EQUIVALENCE( NWTRE,  MASPRT(14))
         CHARACTER*8 JNAME
         INTEGER       forma
         DATA  k_kk/'.K'/
         irc = 0
C********
C        OPEN (UNIT=03,FILE='MS1-1989.REZ')
         WRITE(UNIT=kanal,FMT=5555  ) kf
5555     FORMAT(I2.2)
         IF ( forma .EQ. 0 ) THEN
         OPEN (UNIT=kf,FILE=JNAME//k_kk//kanal,ERR=555,IOSTAT=irc,
     *         FORM='FORMATTED')
C********
         IF(irc .NE. 0 )  GO TO 555
         RETURN
         ENDIF
         IF ( forma .EQ. 1 ) THEN
         OPEN (UNIT=kf,FILE=JNAME//k_kk//kanal,ERR=555,IOSTAT=irc,
     *         FORM='UNFORMATTED')
C********
         IF(irc .NE. 0 )  GO TO 555
         RETURN
         ENDIF
C----------------------------------------------------------------------
555    CONTINUE
       WRITE(99,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(99,*) '      ���� ',JNAME,k_kk,kanal,' �� ���뢠����'
       WRITE(99,*) '  ****** iostat = ',irc,'******************* '
C
       WRITE(NWTR,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(NWTR,*) '      ���� ',JNAME,k_kk,kanal,' �� ���뢠����'
       WRITE(NWTR,*) '  ****** iostat = ',irc,'******************* '
C
       WRITE(NWTRE,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(NWTRE,*) '      ���� ',JNAME,k_kk,kanal,' �� ���뢠����'
       WRITE(NWTRE,*) '  ****** iostat = ',irc,'******************* '
       RETURN
       END
