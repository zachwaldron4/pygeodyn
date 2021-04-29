!$BILIN
      FUNCTION BILIN(RI,RJ,A,IMAX,JMAX,IADIM,JADIM)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!                           B I L I N                                  C
!                                                                      C
!  INTERPOLATES VALUES IN AN ARRAY A USING BILINEAR                    C
!  (PARABOLIC HYPERBOLOID) INTERPOLATION.                              C
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
!  PARAMETERS:                                                         C
!                                                                      C
!  BILIN...       INTERPOLATED VALUE                                   C
!                                                                      C
!  RI, RJ...      INTERPOLAPION ARGUMENT, (1,1) IN LOWER LEFT CORNER,  C
!                 (IMAX, JMAX) IN UPPER RIGHT.                         C
!                                                                      C
!  A...           INTEGER*2 ARRAY WITH ARGUMENTS                       C
!                                                                      C
!  IMAX, JMAX...  NUMBER OF POINTS IN GRID                             C
!                                                                      C
!  IADIM, JADIM...DECLARED DIMENSIONS OF 'A'                           C
!                                                                      C
!  OUTSIDE AREA COVERED BY 'A' THE FUNCTION RETURNS THE VALUE OF       C
!  THE NEAREST BOUNDARY POINT.                                         C
!                                                                      C
!----------------------------------------------------------------------C
!                                                                      C
!  PROGRAMMER:                                                         C
!  RENE FORSBERG, JULY 1983                                            C
!                                                                      C
!  MODIFICATIONS BY:                                                   C
!  HEINER DENKER, 07/01/1987                                           C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE
!
      DIMENSION A(IADIM, JADIM)
      REAL A
      IN = IFRAC(RI)
      IE = IFRAC(RJ)
      RN = RI - IN
      RE = RJ - IE
      IF (IN.LT.1) THEN
        IN = 1
        RN = 0.0
      ELSEIF (IN.GE.IMAX) THEN
        IN = IMAX-1
        RN = 1.0
      ENDIF
      IF (IE.LT.1) THEN
        IE = 1
        RE = 0.0
      ELSEIF (IE.GE.JMAX) THEN
        IE = JMAX-1
        RE = 1.0
      ENDIF
      RNM1=1.-RN
      REM1=1.-RE
      BILIN = RNM1*REM1*A(IN,IE) +                                      &
     &RN*REM1*A(IN+1,IE) + RNM1*RE*A(IN,IE+1) +                         &
     &RN*RE*A(IN+1,IE+1)
      RETURN
      END
