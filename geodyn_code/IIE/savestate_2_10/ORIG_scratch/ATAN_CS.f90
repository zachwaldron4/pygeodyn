!$ATAN_CS
      FUNCTION ATAN_CS ( C, S )
! ************************************************************************
! *                                                                      *
! *   Function ATAN_CS  finds angle in the range [0, pi2) given sinus    *
! *   and cosuins of that angle.                                         *
! *                                                                      *
! *  ###  13-AUG-98     ATAN_CS    v1.0  (c)  L. Petrov  13-AUG-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      DOUBLE PRECISION ATAN_CS, C, S
      DOUBLE PRECISION PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) !
      DOUBLE PRECISION       EPS
      PARAMETER ( EPS=1.D-30 )
!
      IF ( C .GT. EPS ) THEN
           ATAN_CS = ATAN2(S,C)
           IF ( S .LT. 0.0D0 ) ATAN_CS = ATAN_CS + PI2
         ELSE IF ( C .LT. -EPS ) THEN
           ATAN_CS = ATAN2(S,C)
           IF ( S .LT. 0.0D0 ) ATAN_CS = ATAN_CS + PI2
         ELSE ! -EPS < C < EPS
           ATAN_CS = P2I
           IF ( S.LT.0 ) ATAN_CS = ATAN_CS - PI
      END IF
!
      RETURN
      END
