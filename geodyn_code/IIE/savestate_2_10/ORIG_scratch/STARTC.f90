!$STARTC
      SUBROUTINE STARTC(IORDER,IOL1,IOL2,H,H2,N3,NK,IC,XP,              &
     &   SUMX,CIPV,CIVV,XDDOT)
!********1*********2*********3*********4*********5*********6*********7**
! STARTC           82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  USE CORRECTOR FORMULAS TO RECOMPUTE POS & VEL
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   IORDER   I    S    ORDER OF NUMERICAL INTEGRATION
!   IOL1     I    S    IORDER-1
!   IOL2     I    S    IORDER-2
!   H        I    S
!   H2       I    S    H*H
!   N3       I    S    3 TIMES THE NUMBER OF SATELLITES IN THE SET
!   NK       I    S    INDICATES THE TIME STEP BEING CORRECTED
!   IC       I    S    INDICATES THE COWELL COEFFICIENT SET TO BE USED
!   XP       O    A    CORRECTED SATELLITE STATE
!   SUMX     I    A    COWELL INTEGRATION SUMS
!   CIPV     I    A    COWELL COEFFICIENTS FOR INTERPOLATING
!                      (CORRECTING) POSITION
!   CIVV     I    A    COWELL COEFFICIENTS FOR INTERPOLATING
!                      (CORRECTING) VELOCITY
!   XDDOT    I    A    ACCELERATION BACK VALUE ARRAY
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
! COMPUTE VELOCITY
!
      DO 100 N=1,N3
      XP(N,1)=NK*SUMX(N,2)+SUMX(N,1)
      XP(N,2)=SUMX(N,2)
  100 END DO
!
!
      DO 300 I=1,IOL1
      IORDLI=IORDER-I
!
      DO 200 N=1,N3
      XP(N,2)=XP(N,2)+CIVV(IC,I)*XDDOT(N,IORDLI)
  200 END DO
!
!
  300 END DO
!
      DO 400 N=1,N3
      XP(N,2)=XP(N,2)*H
  400 END DO
!
!
! COMPUTE POSITION
      DO 600 I=1,IOL2
      IORDLI=IORDER-I
!
      DO 500 N=1,N3
      XP(N,1)=XP(N,1)+CIPV(IC,I)*XDDOT(N,IORDLI)
  500 END DO
!
!
  600 END DO
!
      DO 700 N=1,N3
      XP(N,1)=XP(N,1)*H2
  700 END DO
!
!
      RETURN
      END
