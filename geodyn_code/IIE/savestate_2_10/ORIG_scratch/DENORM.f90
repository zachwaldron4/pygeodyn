!$DENORM
      FUNCTION DENORM(N,M)
!********1*********2*********3*********4*********5*********6*********7**
! DENORM           00/00/00            0000.0    PGMR - GEODYN1/EDDY
!
! FUNCTION:  TO COMPUTE THE DENORMALIZATION FACTOR FOR
!            GEOPOTENTIAL COEFFICIENTS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   N        I    S    DEGREE OF GEOPOTENTIAL COEFFICIENT
!   M        I    S    ORDER OF GEOPOTENTIAL COEFFICIENT
!
! COMMENTS:
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      DATA ONE/1.0D0/,TWO/2.0D0/,FOUR/4.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      IF(M.EQ.0) GO TO 120
      XN=SQRT(FOUR*DBLE(N)+TWO)
      NMM1=N-M+1
      NPM=N+M
      FACT=ONE
      DO 110 I=NMM1,NPM
  110 FACT=FACT*SQRT(DBLE(I))
      DENORM=XN/FACT
      RETURN
  120 DENORM=SQRT(TWO*DBLE(N)+ONE)
      RETURN
      END
