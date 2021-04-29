!$ERROR
      SUBROUTINE ERROR
!********1*********2*********3*********4*********5*********6*********7**
! ERROR            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  INDICATE PROGRAM EXECUTION TERMINATED DUE TO ERROR
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      PRINT 10000
      STOP
10000 FORMAT('0ERROR CALLED. EXECUTION TERMINATED.')
      END
