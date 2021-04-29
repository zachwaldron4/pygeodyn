!$PREDLS
      SUBROUTINE PREDLS(NM,CPPNC,IORDER,IOL1,IOL2,H,H2,                 &
     &                  SUMX,XDDOT,N3,XI)
!********1*********2*********3*********4*********5*********6*********7**
! PREDLS
!
! FUNCTION: PREDICT THE LARGE STEP STRAJECTORY AT SMALL STEPS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   NM       I    S    NUMBER OF TIMES FOR WHICH INTERPOLATION NEEDED
!   CPPNC    I    A    COWELL COEFFICIENTS FOR INTERPOLATING A
!                      VECTOR OF POSITIONS AND VELOCITIES.
!   IORDER   I    S    ORDER OF COWELL INTEGRATION
!   IOL1     I    S    IORDER-1
!   IOL2     I    S    IORDER-2
!   H        I    S    INTEGRATION STEPSIZE
!   H2       I    S    H*H
!   SUMX     I    A    INTEGRATION SUMS
!   XDDOT    I    A    ACCELERATION BACK VALUES
!   N3       I    S    NUMBER OF SATELLITES TIMES 3
!   XI       O    A    INTERPOLATED ORBIT POSITIONS AND VELOCITIES
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION CPPNC(NM,IORDER,2),                                     &
     &          SUMX(N3,2),                                             &
     &          XDDOT(N3,IOL1),XI(N3,2,NM)
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      DO 900 N=1,N3
!
      DO 100 NMI=1,NM
      CPPNC(NMI,IORDER,2)=SUMX(N,2)
  100 END DO
!
!
      DO 300 K=1,IOL1
      KK=IORDER-K
!
      DO 200 NMI=1,NM
      CPPNC(NMI,IORDER,2)=CPPNC(NMI,IORDER,2)+CPPNC(NMI,K,2)*XDDOT(N,KK)
  200 END DO
!
!
  300 END DO
!
      DO 400 NMI=1,NM
      XI(N,2,NMI)=CPPNC(NMI,IORDER,2)*H
  400 END DO
!
!
      DO 500 NMI=1,NM
      CPPNC(NMI,IORDER,2)=SUMX(N,1)-SUMX(N,2)                           &
     &      +CPPNC(NMI,IORDER,1)*SUMX(N,2)
  500 END DO
!
!
      DO 700 K=1,IOL2
      KK=IORDER-K
!
      DO 600 NMI=1,NM
      CPPNC(NMI,IORDER,2)=CPPNC(NMI,IORDER,2)+CPPNC(NMI,K,1)*XDDOT(N,KK)
  600 END DO
!
!
  700 END DO
!
      DO 800 NMI=1,NM
      XI(N,1,NMI)=CPPNC(NMI,IORDER,2)*H2
  800 END DO
!
!
  900 END DO
      RETURN
      END
