!$DETINT
      SUBROUTINE DETINT(IUNT11,IUNT12,KCOM2S,LASCII,LARECL)
!********1*********2*********3*********4*********5*********6*********7**
! DETINT           93/03/23            9212.3    PGMR - SBL
!
!
! FUNCTION:  DETERMINES TYPE OF INTERFACE FILES AND THE COMPUTER
!            2S WAS RUN ON
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IUNT11   I    S    INTERFACE FILE UNIT NUMBER
!   IUNT12   I    S    DATA FILE UNIT NUMBER
!   KCOM2S   O    S    2S COMPUTER TYPE 32 or 64 BIT COMPUTER
!   JCOM2S   O    S    2S COMPUTER SPECIFIC TYPE
!   LASCII   O    S    LOGICAL FOR FORMATTED INTERFACE FILE
!   LARECL   O    S    LOGICAL FOR FORMATTED INTERFACE RECORD LENGTH
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      PARAMETER( NCOMTP = 5 )
      CHARACTER*8 TYPCOM,COM(NCOMTP)
      DIMENSION NCOM(NCOMTP)
      SAVE
      DATA COM/'IBM     ','CRAY    ','HP      ','PC      ','VAX     '/
! NOTE: 32-BIT ARE ODD AND 64-BIT ARE EVEN
      DATA NCOM/1,2,3,5,7/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! FIRST OPEN THE INTERFACE FILE AS UNFORMATTED
      LASCII=.FALSE.
      LARECL=.FALSE.
!
!
      OPEN(IUNT11,FILE='ftn11',STATUS='OLD',IOSTAT=IO_ERR,    &
     &FORM='UNFORMATTED',ERR=1000)
!
!
 1000 CONTINUE
! ****
      IF(IO_ERR.EQ.0) THEN
! ****
! INTERFACE FILE IS UNFORMATTED
!       READ(IUNT11,ERR=1500,END=9999) HEADER,TYPCOM        ! orig
       READ(IUNT11,ERR=1500,END=1500) HEADER,TYPCOM
!
!
      INQUIRE(FILE='ftn12',EXIST=LEXIST)
      IF(LEXIST)       &
     & OPEN(IUNT12,FILE='ftn12',STATUS='OLD',FORM='UNFORMATTED')
!
! ****
      GOTO 1700
      ENDIF
 1500  CONTINUE
! ****
! INTERFACE FILE IS FORMATTED
       LASCII=.TRUE.
       CLOSE(11)
!
      OPEN(IUNT11,FILE='ftn11',STATUS='OLD',FORM='FORMATTED', &
     &BLANK='ZERO',ACCESS='SEQUENTIAL')
!
!
! open unit 12 if it exists
!
!
      INQUIRE(FILE='ftn12',EXIST=LEXIST)
      IF(LEXIST)   &
     &OPEN(IUNT12,FILE='ftn12',STATUS='OLD',FORM='FORMATTED', &
     &BLANK='ZERO',ACCESS='SEQUENTIAL')
!
!
      READ(IUNT11,FMT='(BZ,24X,A8)',IOSTAT=ISTAT2,ERR=2000,             &
     &     END=9999) TYPCOM
!
! ****
 1700 CONTINUE
! ****
! SET 2S COMPUTER TYPE
      KCOM2S=0
      DO 3000 I=1,NCOMTP
!        write(6,'(1x,a,1x,i3,1x,a,1x,a)')
!     &     'detint: i, typcom, com(i) ', i,typcom, com(i)
      IF(TYPCOM.EQ.COM(I)) KCOM2S=NCOM(I)
!        write(6,*) 'detint: i, ncom(i), kcom2s = ', i, ncom(i), kcom2s
 3000 END DO
!      write(6,*) 'detint: kcom2s = ', kcom2s
      RETURN
 2000 WRITE(6,10001)
      stop 68
 9999 WRITE(6,10000)
      STOP 69
10000 FORMAT(1X,'EXECUTION TERMINATED DUE TO EOF ON READ OF INTERFACE', &
     & '         FILE, UNIT 11, IN SUBROUTINE DETINT')
10001 FORMAT(1X,'ERROR OCCURRED DURING FORMATTED READ OF INTERFACE',    &
     & '         FILE, UNIT 11, IN SUBROUTINE DETINT')
      END
