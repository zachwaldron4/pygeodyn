!$IFRAC
      FUNCTION IFRAC(R)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!                    F U N C T I O N   I F R A C                       C
!                  ===============================                     C
!                                                                      C
!  SUBROUTINE GIVING TRUE INTEGER PART OF A REAL E.G.                  C
!                                                                      C
!    FOR   1. = R < 2.   IS    IFRAC = 1                               C
!    FOR   0. = R < 1.   IS    IFRAC = 0                               C
!    FOR  -1. = R < 0.   IS    IFRAC =-1                               C
!    FOR  -2. = R <-1.   IS    IFRAC =-2                               C
!                                                                      C
!  RF, JUNE 1983                                                       C
!  HD, JANUARY 1987                                                    C
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
      IFRAC=R
      IF (R.GE.0) RETURN
      IF (R.EQ.IFRAC) RETURN
      IFRAC = IFRAC - 1
      RETURN
      END
