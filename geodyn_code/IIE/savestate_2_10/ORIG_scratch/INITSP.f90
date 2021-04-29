!$INITSP
      SUBROUTINE INITSP(Y, N, R, Q)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!                      I N I T S P                                     C
!                                                                      C
!  INITIALIZATION PROCEDURE FOR FAST 1-DIMENSIONAL EQUIDISTANT         C
!  SPLINE INTERPOLATION, WITH FREE BOUNDARY END CONDITIONS             C
!  REFERENCE: JOSEF STOER: EINFUHRUNG IN DIE NUMERISCHE MATHEMATIK     C
!  I, SPRINGER 1972, PAGE 82 AND 86.                                   C
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
!  PARAMETERS (REAL):                                                  C
!                                                                      C
!  Y...   GIVEN VALUES, Y(1), ..., Y(N)                                C
!                                                                      C
!  R...   SPLINE MOMENTS (1 ... N), TO BE USED BY FUNCTION 'SPLINE'    C
!                                                                      C
!  Q...   WORK-ARRAY, DECLARED AT LEAST 1:N                            C
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
!  RENE FORSBERG, JULY 1983                                            C
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION Y(N), R(N), Q(N)
      Q(1) = 0.0
      R(1) = 0.0
      DO 11 K = 2, N-1
        P = Q(K-1)/2+2
        Q(K) = -0.5/P
        R(K) = (3*(Y(K+1)-2*Y(K)+Y(K-1)) - R(K-1)/2)/P
   11 END DO
      R(N) = 0.0
      DO 12 K = N-1, 2, -1
        R(K) = Q(K)*R(K+1)+R(K)
   12 END DO
      RETURN
      END
