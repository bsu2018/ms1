       SUBROUTINE FIDEF(MSWALL, TOKN, VALLN, KAR, DAT,irest)
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   FIDEF(MSWALL, TOKN, VALLN,KAR, DAT, irest)
C   ------------
C   HA�HA�EH�E:    *** ����⨥ ��������� ������� �����/�뢮��  ***
C   -----------         ���������������� ������
C                       NRDR+NWTR+NWTRE+NSPOOL
C              kf ==>    5     3    6  ???9
C   METO�:         ��ନ஢���� ����� 䠩��:
C  -------
C                      Jname{masprt(125)}//'.K'//xx
C
C            xx - ����� ������ --> kf
C
C   �APAMETP�:
C   ---------
C   �O��PO�PAMM�:  1. COMMON PROF1
C   ------------
C   �P�ME�AH�E:    KOHTPO�� �� �஢������
C   ----------
CZZZ-------------------------------------------------------------------
         COMMON/ PROF1 / MASPRT(200)
         EQUIVALENCE( NWTR,   MASPRT(13))
         EQUIVALENCE( NRDR,   MASPRT(12))
         EQUIVALENCE( NWTRE,  MASPRT(14))
         EQUIVALENCE(NSPOOL,  MASPRT(18))
         EQUIVALENCE(NFILER,  MASPRT(83))
         EQUIVALENCE(NMAK,    MASPRT(89))
         EQUIVALENCE(NFCHEK,  MASPRT(95))
         EQUIVALENCE(NFACHK,  MASPRT(97))
         EQUIVALENCE(NREGX,   MASPRT(122))
         EQUIVALENCE(JNAME,   MASPRT(125))
         EQUIVALENCE(NZRDR,   MASPRT(127))
         DIMENSION MSWALL(*), TOKN(*), VALLN(*), KAR(1), DAT(1)
         CHARACTER*8 JNAME
         CHARACTER*8 NAME
         CHARACTER*1 BLANK
         CHARACTER*12 START_N
         CHARACTER*2 kanal, k_kk
         DATA START_N/'MS1_DATA.$$$'/, k_kk/'.K'/
         DATA BLANK/' '/
         character*12 fullname, tempname
C----------------------------------------------------------------------
!!!         write(*,*)'---@@@@@@---- FIDEF  ---##############---'
          DO 115 i=1,8
           NAME(i:i) = BLANK
115        JNAME(i:i) = BLANK
C----------------------------------------------------------------------
C      �������� ������������ ������:
C----------------------------------------------------------------------
C----------------------------------------------------------------------
!!!!!         OPEN (UNIT=99,FILE='CON',recl=256)
C----------------------------------------------------------------------
         OPEN (UNIT=98,FILE=START_N,ERR=668,
     +         form='FORMATTED',iostat=iioo)
         READ(UNIT=98,FMT=9889,END=666,ERR=667)  NAME
C   irest  = 0  - ����� �� ������: = 1  - ������
         READ(UNIT=98,FMT=9888,END=666,ERR=667)  irest
9889     FORMAT(A8)
9888     FORMAT(i1)
         CLOSE (UNIT=98)

!        write(*,*) '== fidef ==== NAME  irest NWTR ',
!    *                    NAME, ' ', irest, ' ', NWTR  



C********
         IF ( irest .EQ. 1 ) THEN
            CALL LOADMS(MSWALL, TOKN, VALLN, KAR, DAT, iii_rrr)
            IF ( iii_rrr .NE. 0 ) CALL STOPP(20)
         ENDIF
C********
C********
         IF ( irest .EQ. 0 ) THEN
            ii=1
            DO 111 i=1,8
               IF ( NAME(i:i) .EQ. BLANK ) GO TO 111
               JNAME(ii:ii) =  NAME(i:i)
               ii = ii + 1
111         CONTINUE
         ENDIF
C        OPEN (UNIT=03,FILE='MS1-1989.REZ')
         WRITE(UNIT=kanal,FMT=3333  ) NWTR
3333     FORMAT(I2.2)
         iiiiii = NWTR
!!!!!!!!!!
         call conact(8, JNAME, 2, k_kk, 12, tempname)
         call conact(12, tempname, 2, kanal, 12, fullname) 
!!!         write(*,*)'--- NWTR fullname=', fullname 
!!!!!!!!!! 
!!!         OPEN (UNIT=NWTR,FILE=JNAME//k_kk//kanal,ERR=555,
         OPEN (UNIT=NWTR,FILE=fullname,ERR=555,     
     +         form='FORMATTED',iostat=iioo)
C********
         IF ( irest .EQ. 1 ) THEN
333         read(NWTR,*,end=334)
            go to 333
334         backspace NWTR
         ENDIF
C********
C        OPEN (UNIT=06,FILE='MS1-1989.CON')

         WRITE(UNIT=kanal,FMT=3333  ) NWTRE
         iiiiii = NWTRE
!!!!!!!!!!
         call conact(8, JNAME, 2, k_kk, 12, tempname)
         call conact(12, tempname, 2, kanal, 12, fullname) 
!!!        write(*,*)'--- NWTRE fullname=', fullname 
!!!!!!!!!! 
         OPEN (UNIT=NWTRE,FILE=fullname,ERR=555,
!!!         OPEN (UNIT=NWTRE,FILE=JNAME//k_kk//kanal,ERR=555,
     +         form='FORMATTED',iostat=iioo)
C********
         IF ( irest .EQ. 1 ) THEN
663         read(NWTRE,*,end=664)
            go to 663
664         backspace NWTRE
         ENDIF
C********
C        OPEN (UNIT=05,FILE='MS1-1989.JOB')
         WRITE(UNIT=kanal,FMT=3333  ) NRDR
         iiiiii = NRDR
!
!         write(*,*) '== fidef  NRDR=', NRDR
!         write(*,*) '== fidef  JNAME//k_kk//kanal=<',
!     *               JNAME//k_kk//kanal, '>'      
!
!         !!!pause 666
!          write(*,*) 'PaUsE .......'
!!!!!!!!!!
         call conact(8, JNAME, 2, k_kk, 12, tempname)
         call conact(12, tempname, 2, kanal, 12, fullname) 
!!!         write(*,*)'--- NRDR fullname=', fullname 
!!!!!!!!!! 
!         OPEN (UNIT=NRDR,FILE=JNAME//k_kk//kanal,ERR=555,status='old',
          OPEN (UNIT=NRDR,FILE=fullname,ERR=555,status='old',
     +         form='FORMATTED',iostat=iioo)
C********
         IF ( irest .EQ. 1 ) THEN
            DO 5555 ii=1,NZRDR
5555        read(NRDR,*,end=6644)
         ENDIF
C********
C----------------------------------------------------------------------
6644     continue
!!         write(*,*) '== fidef ==== JNAME NRDR ', JNAME, ' ', NRDR       
!!         write(*,*) '===   FILE=JNAME//k_kk//kanal ',JNAME//k_kk//kanal  
        RETURN                                


!!!!!!  99 ---- �������� �� *
666    WRITE(*,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(*,*) '���� MS1_DATA.$$$ �� ᮤ�ন� ����� 䠩��� 楯�'
       WRITE(*,*) '  *****  �믮������ ࠡ��� ���뢠���� ***** '
       call STOPP(16)
C----------------------------------------------------------------------
667    WRITE(*,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(*,*) '       ���� MS1_DATA.$$$ �� �⠥��� '
       WRITE(*,*) '  *****  �믮������ ࠡ��� ���뢠���� ***** '
       call STOPP(24)
C----------------------------------------------------------------------
668    WRITE(*,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(*,*) '      ���� MS1_DATA.$$$ �� ���뢠����'
       WRITE(*,*) '  *****  �믮������ ࠡ��� ���뢠���� ***** '
       WRITE(*,*) '  *****     Iostat = ', iioo,     ' ***** '
       call STOPP(20)
C----------------------------------------------------------------------
555    WRITE(*,*) '        ***** !!!!! MS1 !!!!! ***** '
!!!       WRITE(*,*) '      ���� ',JNAME//k_kk//kanal,' �� ���뢠����'
       WRITE(*,*) '      ���� ',fullname,' �� ���뢠����'
       WRITE(*,*) '  *****  �믮������ ࠡ��� ���뢠���� ***** '
       WRITE(*,*) '  *****     Iostat = ', iioo,     ' ***** '
       WRITE(*,*) '  *****     iiiiii = ', iiiiii,   ' ***** '
       call STOPP(20)
       END
