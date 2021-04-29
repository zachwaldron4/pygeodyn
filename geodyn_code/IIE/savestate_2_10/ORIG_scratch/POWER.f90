!$POWER
      SUBROUTINE POWER(IORDER,X,RESULT,L0EVAL)
!********1*********2*********3*********4*********5*********6*********7**
! POWER            00/00/00            0000.0    PGMR - ?
!
!
! FUNCTION:  COMPUTES POWERS OF A NUMBER
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IORDER   I    S    POWER (1,2 OR 3)
!   X        I    S    NUMBER FOR WHICH A POWER CALCULATION IS REQUESTED
!   RESULT   O    S    RESULT OF THE CALCULATION
!   L0EVAL   I    S    .TRUE. IF ZERO POWER IS REQUESTED
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DATA ZERO/0.D0/,ONE/1.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      GO TO (100,200,300),IORDER
  100 CONTINUE
      RESULT=ZERO
      IF(L0EVAL) RESULT=ONE
      RETURN
  200 CONTINUE
      RESULT=X
      RETURN
  300 CONTINUE
      RESULT=X*X
      RETURN
      END
