!$INTRP
      SUBROUTINE INTRP(S,CIP,CIV,IORDER,H,SUMX,XDDOT,N3,IOL1,XI)
!********1*********2*********3*********4*********5*********6*********7**
! INTRP            82/08/27            8208.0    PGMR - TOM MARTIN
!
! FUNCTION:  INTERPOLATE FOR THE S/C ORBIT AT A SINGLE TIME
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   S        I    S    INTERPOLATION FRACTION
!   CIP      I    A    COWELL INTERPOLATION COEFFICIENTS FOR
!                      POSITION
!   CIV      I    A    COWELL INTERPOLATION COEFFICIENTS FOR
!                      VELOCITY
!   IORDER   I    S    ORDER OF COWELL INTEGRATION
!   H        I    S    INTEGRATION STEPSIZE
!   SUMX     I    A     INTEGRATION SUMS
!   XDDOT    I    A     ACCELERATION BACK VALUES
!   N3       I    S     NUMBER OF SATELLITES TIMES 3
!   IOL1     I    S     IORDER-1
!   XI       O    A     INTERPOLATED ORBIT POSITIONS AND VELOCITIES
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION CIP(IORDER),CIV(IORDER),SUMX(N3,2),XDDOT(N3,IOL1),      &
     &          XI(N3,2)
      DATA ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      CALL COEF1 (S,IORDER,CIP,CIV)
      H2=H**2
      IF(N3.GT.9) GO TO 2000
      IOL2=IORDER-2
      DO 900 N=1,N3
      B=SUMX(N,2)
      DO 300 K=1,IOL1
      KK=IORDER-K
      B=B+CIV(K)*XDDOT(N,KK)
  300 END DO
      XI(N,2)=B*H
      B=SUMX(N,1)+SUMX(N,2)*(S-ONE)
      DO 700 K=1,IOL2
      KK=IORDER-K
      B=B+CIP(K)*XDDOT(N,KK)
  700 END DO
      XI(N,1)=B*H2
  900 END DO
      RETURN
! WHEN N3.GT.9 INVERT ORDER OF LOOPS AND VECTORIZE LOOPS BASED ON N3
 2000 CONTINUE
!
      DO 2100 N=1,N3
      XI(N,2)=SUMX(N,2)
 2100 END DO
!
!
      DO 2300 K=1,IOL1
      KK=IORDER-K
!
      DO 2200 N=1,N3
      XI(N,2)=XI(N,2)+CIV(K)*XDDOT(N,KK)
 2200 END DO
!
!
 2300 END DO
!
      DO 2400 N=1,N3
      XI(N,2)=XI(N,2)*H
 2400 END DO
!
!
      DO 2500 N=1,N3
      XI(N,1)=SUMX(N,1)+SUMX(N,2)*(S-ONE)
 2500 END DO
!
!
      DO 2700 K=1,IOL2
      KK=IORDER-K
!
      DO 2600 N=1,N3
      XI(N,1)=XI(N,1)+CIP(K)*XDDOT(N,KK)
 2600 END DO
!
!
 2700 END DO
!
      DO 2800 N=1,N3
      XI(N,1)=XI(N,1)*H**2
 2800 END DO
!
!
      RETURN
      END
