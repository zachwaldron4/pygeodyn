!$SOUCOO
      SUBROUTINE SOUCOO (RASC,DECL,QID,NBUF)
!********1*********2*********3*********4*********5*********6*********7**
! SOUCOO
!
! FUNCTION:        READING QUASAR COORDINATES FRM AN EXTERNAL FILE.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS IN ALPHABETICAL ORDER
!   ------  ---  ---   ------------------------------------------------
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      PARAMETER  ( MBUF = 16*1024 )
      CHARACTER  QID*8
      CHARACTER  STR*80, ALPHA_STR*16, DELTA_STR*16
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      DIMENSION RASC(*),DECL(*),QID(*)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      INQUIRE ( FILE='quasar_coordinates', EXIST=LEX )
      IF ( .NOT. LEX ) THEN
      WRITE(6,*)' VLBI EXTERNAL SOURCE COORDINATE FILE DOES NOT EXIST '
      WRITE(6,*)' TERMINATION IN SOUCOO '
      STOP
      ENDIF
      ALLOCATE ( BUF(MBUF), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
      WRITE(6,*)' ERROR IN ATTEMPTING TO ALLOCATE DYNAMIC MEMORY '
      WRITE(6,*)' EXECUTION TERMINATING IN SOUCOO '
      ENDIF
      CALL RD_TEXT('quasar_coordinates', MBUF, BUF, NBUF, IUER )
      DO 100 I=1,NBUF
      IF(BUF(I)(1:1).EQ. '#' ) GO TO 100
      IF(BUF(I)(1:1).EQ. '$' ) GO TO 100
!     WRITE(6,2222)BUF(I)
2222  FORMAT(A)
3333  FORMAT(A8)
      CALL CLEARL(ALPHA_STR,16)
      CALL CLEARL(DELTA_STR,16)
      ALPHA_STR=BUF(I)(15:16)//'_'//BUF(I)(18:19)//'_'//BUF(I)(21:29)
      DELTA_STR=BUF(I)(35:37)//'_'//BUF(I)(39:40)//'_'//BUF(I)(42:49)
!     write(6,4444) ALPHA_STR, DELTA_STR
4444  FORMAT(2A16)
      CALL HR_TAT(ALPHA_STR,ALPHA,IUER)
      IF(IUER.NE.0) THEN
      WRITE(6,*) 'ERROR IN PARSING LINE ', I ,'OF THE SOURCE COORD FILE '
      ENDIF
!     WRITE(6,*)' dbg ALPHA ',ALPHA,I
      RASC(I)=ALPHA
      CALL GR_TAT(DELTA_STR,DELTA,IUER)
      IF(IUER.NE.0) THEN
      WRITE(6,*) 'ERROR IN PARSING LINE ', I ,'OF THE SOURCE COORD FILE '
      ENDIF
!     WRITE(6,*)' dbg DELTA ',DELTA,I
      DECL(I)=DELTA
      QID(I)=BUF(I)(5:12)
!     write(6,3333) QID(I)
 100  CONTINUE
      DEALLOCATE (BUF)
      RETURN
      END
