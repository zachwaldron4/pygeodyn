!$LEGFGL
      SUBROUTINE LEGFGL(N,M,X,P,MXD,MXO)
!********1*********2*********3*********4*********5*********6*********7**
!  LEGFGL          93/03/23            0000.0    PGMR: FGL
!
!  FUNCTION:  COMPUTES LEGENDRE AND ASSOCIATED LEGENDRE FUNCTIONS UP TO
!             DEGREE N AND ORDER M, N.GE.M
!
!  I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!    N       I    S    DEGREE OF SPHERICAL HARMONICS
!    M       I    S    ORDER OF SPHERICAL HARMONICS
!    X       I    S    SIN (LATITUDE)
!    P       O    A    2-D LEGENDRE FUNCTIONS
!
!  REFERENCES
!    JPL EM 312/87-153, 20 APRIL 1987
!  ANALYSIS
!    J. H. KWOK - JPL
!  PROGRAMMER
!    J. H. KWOK - JPL
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DIMENSION P(MXD+1,MXO+1)

      DO III = 1, MXD+1
          P(III,1) = 0.0D0
      END DO

      P(1,1) = 1.0D0
      P(2,1) = X

      ! COMPUTE LEGENDRE FUNCTIONS UP TO DEGREE N
      DO I = 2, N
          P(I+1,1) = ((2*I-1)*X*P(I,1) - (I-1)*P(I-1,1)) / DBLE(I)
      END DO
      IF (M == 0) RETURN

      ! COMPUTE ASSOCIATED LEGENDRE FUNCTIONS UP TO ORDER M

      Y = SQRT(1.0D0-X*X)
      P(2,2) = Y
      ! THIS IS THE SECTORIAL PART OF THE ASSOCIATED FUNCTIONS
      DO I = 2, M
          P(I+1,I+1) = (2*I-1) * Y * P(I,I)
      END DO

      ! THIS THE TESSERAL PART OF THE ASSOCIATED FUNCTIONS
      DO I = 2, N
          I2 = MIN(I-2, M)
          DO J = 1, I2
              P(I+1,J+1) = (2*I-1) * Y * P(I,J) + P(I-1,J+1)
          END DO
          IF (I-1 <= M) THEN
              P(I+1,I) = (2*I-1) * Y * P(I,I-1)
          END IF
      END DO

      END SUBROUTINE
