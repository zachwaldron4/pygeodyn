!$UCSSS
      SUBROUTINE UCSSS(XSAT,N3,IORDER,IOL1,IOL2,CP,CV,H,H2,SUMX,        &
     &                 XDDOT)
!********1*********2*********3*********4*********5*********6*********7**
! UCSSS
!
! FUNCTION: (RE)COMPUTE INTEGRATION SUM GIVEN STATE VECTOR AT AN
!           EPOCH AND ACCELERATIONS AT NEARBY EPOCHS
!
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XSAT     I    A    PREDICTED SMALL STEP STATE
!   N3       I    S    NUMBER OF SATELLITES TIMES 3
!   IORDER   I    S    ORDER OF COWELL INTEGRATION
!   IOL1     I    S    IORDER-1
!   IOL2     I    S    IORDER-2
!   CP       I    A    COWELL COEFFICIENTS FOR PREDICTING POSITIONS OR
!                      CORRECTING POSITIONS
!   CV       I    A    COWELL COEFFICIENTS FOR PREDICTING VELOCITIES OR
!                      CORRECTING VELOCITIES
!   H        I    S    SMALL STEP SIZE
!   H2       I    S    H*H
!   SUMX     O    A    INTEGRATION SUMS
!   XDDOT    I    A    ACCELERATION BACK VALUES
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XSAT(N3,2)
      DIMENSION CP(IOL2),CV(IOL1)
      DIMENSION SUMX(N3,2),XDDOT(3,IOL1)
      DATA ZERO/0.0D0/,ONE/1.0D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
! POSITION
      DO 2100 J=1,N3
      SUMX(J,1)=XSAT(J,1)/H2
      DO 2000 I=1,IOL2
      K=IORDER-I
      SUMX(J,1)=SUMX(J,1)-CP(I)*XDDOT(J,K)
 2000 END DO
 2100 END DO
! VELOCITY
      DO 2300 J=1,N3
      SUMX(J,2)=XSAT(J,2)/H
      DO 2200 I=1,IOL1
      K=IORDER-I
      SUMX(J,2)=SUMX(J,2)-CV(I)*XDDOT(J,K)
 2200 END DO
 2300 END DO
 2400 CONTINUE
      RETURN
      END
