!$DERIV
      FUNCTION DERIV(IORDER,T,IDEG)
!********1*********2*********3*********4*********5*********6*********7**
! DERIV            83/08/17            8308.0    PGMR - ?
!
! FUNCTION:
!
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IORDER
!   T
!   IDEG
!
! COMMENTS:
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
      DERIV=ZERO
      IF(IORDER.GT.IDEG) RETURN
      IEXP=IDEG
      DERIV=ONE
      IF(IORDER.LT.1) GO TO 20
      DO 10 I=1,IORDER
      DERIV=DERIV*IEXP
      IEXP=IEXP-1
   10 END DO
   20 CONTINUE
      DERIV=DERIV*T**IEXP
      RETURN
      END
