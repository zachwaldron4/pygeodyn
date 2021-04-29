!$SPLINE
      FUNCTION SPLINE(X, Y, N, R)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!                          S P L I N E                                 C
!                                                                      C
!  FAST ONE-DIMENSIONAL EQUIDISTANT SPLINE INTERPOLATION FUNCTION.     C
!  REFERENCE: JOSEF STOER: EINFUHRUNG IN DIE NUMERISCHE MATHEMATIK     C
!  I, SPRINGER 1972, PAGE 81.                                          C
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
!  PARAMETERS:                                                         C
!                                                                      C
!  X...  INTERPOLATION ARGUMENT (REAL), X = 1 FIRST DATA-POINT,        C
!        X = N LAST DATA-POINT. OUTSIDE THE RANGE LINEAR EXTRA-        C
!        POLATION IS USED.                                             C
!                                                                      C
!  Y...  REAL*8 ARRAY, 1 .. N : DATA VALUES                            C
!                                                                      C
!  R...  DO: SPLINE MOMENTS CALCULATED BY SUBROUTINE 'INITSP'          C
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
!  PROGRAMMER:                                                         C
!  RENE FORSBERG, JUNE 1983                                            C
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION Y(N), R(N)
      IF (X.LT.1) THEN
        SPLINE = Y(1) + (X-1)*(Y(2)-Y(1)-R(2)/6)
      ELSEIF (X.GT.N) THEN
        SPLINE = Y(N) + (X-N)*(Y(N)-Y(N-1)+R(N-1)/6)
      ELSE
        J = IFRAC(X)
        XX = X - J
        SPLINE = Y(J) +                                                 &
     &           XX * ((Y(J+1)-Y(J)-R(J)/3-R(J+1)/6) +                  &
     &           XX * (R(J)/2 +                                         &
     &           XX * (R(J+1)-R(J))/6))
      ENDIF
      RETURN
      END
