


!$DETALL
      SUBROUTINE DETALL(IUNT11,MAXDM1)
!********1*********2*********3*********4*********5*********6*********7**
! DETALL           94/01/13            9310    PGMR -  SCR
!
!
! FUNCTION:  DETERMINES FROM THE IFF THE AMOUNT OF CORE ALLOCATION
!            NEEDED FOR 2E. IT IS NECESSARY TO DETERMINE  WHETHER THE
!            FILE IS FORMATTED OR UNFORMATTED FIRST.  THEN CLOSE THE
!            FILE AND OPEN IT ACCESS = APPEND (AT EOF) BACKSPACE AND
!            READ THE VALUE. PASS THAT VALUE BACK TO THE ALLOCATION
!            SYSTEM ROUTINES IN GDYN2E
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT11   I    S    INTERFACE FILE UNIT NUMBER
!   MAXDM1   O    S    size of dynamic arrays needed
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      PARAMETER( NCOMTP = 5)
      CHARACTER*8 TYPCOM,COM(NCOMTP)
      DOUBLE PRECISION HEADER
      DIMENSION NCOM(NCOMTP)
      SAVE
      DATA COM/'IBM     ','CRAY    ','HP      ','PC      ','VAX     '/
! NOTE: 32-BIT ARE ODD AND 64-BIT ARE EVEN
      DATA NCOM/1,2,3,5,7/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
! FIRST OPEN THE INTERFACE FILE AS UNFORMATTED
!
      OPEN(IUNT11,FILE='ftn11',STATUS='OLD',IOSTAT=IO_ERR,    &
     &     FORM='UNFORMATTED')
!
! ****
      IF(IO_ERR.EQ.0) THEN
! ****
! INTERFACE FILE IS UNFORMATTED
! ****
!       READ(IUNT11,ERR=1100,END=9999) HEADER,TYPCOM  ! orig
       READ(IUNT11,ERR=1100,END=1100) HEADER,TYPCOM
!@       READ(IUNT11) HEADER,TYPCOM
!       write(6,*) 'detall:unform header ', header
!       write(6,'(1x,a,1x,a)') 'detall:  typcom ', typcom
! ****
      CLOSE(IUNT11)
      OPEN(IUNT11,FILE='ftn11',FORM='UNFORMATTED',STATUS='OLD',  &
     &      IOSTAT=IO_ERR, ACCESS='SEQUENTIAL', POSITION='APPEND')
!     &      IOSTAT=IO_ERR, ACCESS='APPEND')  ! original
!                                                     ! jjm 9/98
      BACKSPACE IUNT11
      READ(IUNT11) MAXDM1
      REWIND(IUNT11)
      CLOSE(IUNT11)
! ****
! ****
! ****
      GOTO 1114
      ENDIF
 1100 CONTINUE
! ****
! INTERFACE FILE IS FORMATTED
! ****
      CLOSE(IUNT11)
! ****
! OPEN FORMATTED INTERFACE FILE AT THE END OF FILE
! BACK ONE RECORD FOR THE CORE ALLOCATION NEEDED FOR 2E
! ****
! ****
      OPEN(IUNT11,FILE='ftn11',STATUS='OLD',FORM='FORMATTED',     &
     &BLANK='ZERO', ACCESS='SEQUENTIAL', POSITION='APPEND')
!     &BLANK='ZERO',ACCESS='APPEND')     ! original
!                                                     ! jjm 9/98
      BACKSPACE(IUNT11)
      READ(IUNT11,600,END=9999) MAXDM1
      CLOSE(IUNT11)
600   FORMAT(I10)
! ****
! ****
 1114     CONTINUE
!
      RETURN
 9999 WRITE(6,10000)
      STOP 69
10000 FORMAT(1X,'EXECUTION TERMINATED DUE TO EOF ON READ OF INTERFACE', &
     &          'FILE, UNIT 11, IN SUBROUTINE DETALL')
10001 FORMAT(1X,'ERROR OCCURRED DURING FORMATTED READ OF INTERFACE',    &
     &          'FILE, UNIT 11, IN SUBROUTINE DETALL')
      END
