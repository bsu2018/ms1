           SUBROUTINE MSGCON(I)
C  ����� ᮮ�饭�� �� ���᮫�
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( TIME, MASPRT(29) )
C
           IF ( I .EQ. 1 ) THEN
           WRITE(99,*)  ''
           WRITE(99,*)   '[0;0H��������: ᮧ������ ����஫쭠� �窠 ��X
     X� �६��� ������ ',TIME, ' cek.'
           RETURN
           ENDIF
           IF ( I .EQ. 2 ) THEN
C          WRITE(99,*)  ''
           WRITE(99,*)   '[0;0H��������: ����஫쭠� �窠  ᮧ����  ��X
     X� �६��� ������ ',TIME, ' cek.'
           RETURN
           ENDIF
           END
