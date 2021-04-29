!$START0
      SUBROUTINE START0(LORBIT,N3,N6,NEQN,                              &
     &   NEQN6,NSAT,XP,X,PX,PX3,DIAG)
!********1*********2*********3*********4*********5*********6*********7**
! START0           82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  INITIALIZE THE ORBIT AND VARIATIONAL EQUATIONS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   LORBIT   I         LOGICAL SWITCH INDICATING ORBIT ONLY
!                      INTEGRATION WHEN .TRUE.
!   H        I         INTEGRATION STEPSIZE
!   N3       I         NSAT*3
!   N6       I         NSAT*6
!   NEQN     I         NUMBER OF FORCE MODEL PARAMETERS INTEGRATED
!   NEQN6    I         NEQN*6
!   NSAT     I         NUMBER OF SATELLITES IN THE SET
!   XP       O         INITIAL EPOCH ELEMENTS
!   X        I         INITIAL EPOCH ELEMENTS
!   PX       O         INITIAL PARTIAL DERIVATIVE STATE
!   PX3      O         PX
!
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XP(N6),X(N6),PX(NEQN6),PX3(NEQN,N3,2)
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! LOAD EPOCH STATE INTO PREDICTION ARRAY
      DO 100 N=1,N6
      XP(N)=X(N)
  100 END DO
      IF(LORBIT)RETURN
! CLEAR PARTIAL DERIVATIVE ARRAY
!
      DO 200 N=1,NEQN6
      PX(N)=ZERO
  200 END DO
!
!
! INITIALIZE STATE TRANSITION MATRIX
      DO 400 ISAT=1,NSAT
      N0=(ISAT-1)*3
      DO 300 J=1,3
      NJ=N0+J
      PX3(J,NJ,1)=DIAG
      PX3(J+3,NJ,2)=DIAG
  300 END DO
  400 END DO
      RETURN
      END
