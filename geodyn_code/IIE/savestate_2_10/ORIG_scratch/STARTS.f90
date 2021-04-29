!$STARTS
      SUBROUTINE STARTS(IORDER,IOL1,IOL2,H,H2,N3,MF1,                   &
     &   MID,XP,SUMX,CIPV,CIVV,XDDOT)
!********1*********2*********3*********4*********5*********6*********7**
! STARTS           82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  RECOMPUTE THE COWELL INTEGRATION SUMS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IORDER   I         ORDER OF NUMERICAL INTEGRATION
!   IOL1     I         IORDER-1
!   IOL2     I         IORDER-2
!   H        I         INTEGRATION STEPSIZE
!   H2       I         H*H
!   N3       I         3 TIMES THE NUMBER OF SATELLITES IN THE SET
!   MF1      O         INDICATOR OF TIME STEP FOR WHICH SUMS COMPUTED
!   MID      O         INDICATOR OF WHICH TIME STEP IS INITIAL EPOCH
!   XP
!   SUMX     O         COWELL INTEGRATION SUMS
!   CIPV     O         COWELL COEFFICIENTS FOR INTERPOLATING
!                      (CORRECTING) POSITION
!   CIVV     O         COWELL COEFFICIENTS FOR INTERPOLATING
!                      (CORRECTING) VELOCITY
!   XDDOT    O         ACCELERATION BACK VALUE ARRAY
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XP(N3,2),SUMX(N3,2),XDDOT(N3,IOL1),                     &
     &   CIPV(IOL1,IORDER),CIVV(IOL1,IORDER)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
! COMPUTE VELOCITY SUMS
!
      DO 100 N=1,N3
      SUMX(N,2)=XP(N,2)/H
  100 END DO
!
!
      DO 300 I=1,IOL1
      IORDLI=IORDER-I
!
      DO 200 N=1,N3
      SUMX(N,2)=SUMX(N,2)-CIVV(MID,I)*XDDOT(N,IORDLI)
  200 END DO
!
!
  300 END DO
! COMPUTE POSITION SUMS
!
      DO 400 N=1,N3
      SUMX(N,1)=SUMX(N,2)*MF1+XP(N,1)/H2
  400 END DO
!
!
      DO 600 I=1,IOL2
      IORDLI=IORDER-I
!
      DO 500 N=1,N3
      SUMX(N,1)=SUMX(N,1)-CIPV(MID,I)*XDDOT(N,IORDLI)
  500 END DO
!
!
  600 END DO
      RETURN
      END
