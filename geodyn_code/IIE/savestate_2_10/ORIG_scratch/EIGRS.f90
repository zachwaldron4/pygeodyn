!$EIGRS
      SUBROUTINE EIGRS
!*******1*********2*********3*********4*********5*********6********
! EIGRS           00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6********
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
! DUMMY ROUTINE FOR CALL TO EIGRS IN PSEUDO
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      WRITE(6,100)
      STOP 16
  100 FORMAT('** ABNORMAL TERMINATION IN SUBROUTINE EIGRS. ',/,         &
     &' DO NOT SPECIFY A GENERALIZED INVERSION SCHEME',/,               &
     &' FOR NORMAL POINT MATRICES ON THE INVERT CARD')
!CC      RETURN
      END
