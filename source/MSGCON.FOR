           SUBROUTINE MSGCON(I)
c-------------------------------------------------------------
C  ����� ᮮ�饭�� �� ���᮫�
c  �ᯮ������ ANSI.SYS
c-------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( TIME, MASPRT(29) )
C
           IF ( I .EQ. 1 ) THEN
           WRITE(99,*)  ''
c          WRITE(99,*)   '[0;0H��������: ᮧ������ ����஫쭠� �窠 ��X
c    X� �६��� ������ ',TIME, ' cek.'
           WRITE(99,1)   TIME
1     format(            '[0;0H��������: ᮧ������ ����஫쭠� �窠 ��X
     X� �६��� ������ ',g12.5,' cek.')
           RETURN
           ENDIF
           IF ( I .EQ. 2 ) THEN
C          WRITE(99,*)  ''
c          WRITE(99,*)   '[0;0H��������: ����஫쭠� �窠  ᮧ����  ��X
c    X� �६��� ������ ',TIME, ' cek.'
           WRITE(99,2)   TIME
2     format(            '[0;0H��������: ����஫쭠� �窠  ᮧ����  ��X
     X� �६��� ������ ',g12.5, ' cek.')
           RETURN
           ENDIF
           END
