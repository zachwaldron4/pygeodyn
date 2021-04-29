!$LGN33
      SUBROUTINE LGN33(CSTHET,SNTHET,COSL,SINL,PNM,COS2L,COS3L,SIN2L,   &
     &                 SIN3L)
!********1*********2*********3*********4*********5*********6*********7**
! LGN33
!
! FUNCTION:  COMPUTES NORMALIZED LGENDRE POLYNOMIALS FOR DEGREES
!            TWO AND THREE
!            DEGREE TWO   TERMS HAVE AN EXTRA (2+1) IN THE DENOMINATOR
!            DEGREE THREE TERMS HAVE AN EXTRA (3+1) IN THE DENOMINATOR
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   CSTHET   I    S    COSINE OF THE COLATITUDE
!   SNTHET   I    S    SINE OF THE COLATITUDE
!   COSL     I    S    COSINE OF LONGITUDE
!   SINL     I    S    SINE OF LONGITUDE
!   PNM      O    A    LGENDRE POLYNOMIALS
!   COS2L    O    S    COSINE OF TWICE THE COLATITUDE
!   COS3L    O    S    COSINE OF THREE TIMES THE COLATITUDE
!   SIN2L    O    S    SINE OF TWICE THE COLATITUDE
!   SIN3L    O    S    SINE OF THREE TIMES THE COLATITUDE
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION PNM(7)
! 2,0      COEF=1.5D0/DSQRT(5.D0)
      DATA C20/0.6708203932499369D+00/
! 2,1      COEF=3.D0/DSQRT(15.D0)
      DATA C21/0.7745966692414834D+00/
! 2,2      COEF=3.D0/DSQRT(60.D0)
      DATA C22/0.3872983346207417D+00/
! 3,0      COEF=5.D0/(8.D0*DSQRT(7.D0))
      DATA C30/0.2362277956307670D+00/
! 3,1      COEF=15.D0/(2.D0*DSQRT(42.D0))
      DATA C31/0.1157275124715689D+01/
! 3,2      COEF=15.D0/DSQRT(420.D0)
      DATA C32/0.7319250547113999D+00/
! 3,3      COEF=15.D0/DSQRT(2520.D0)
      DATA C33/0.2988071523335984D+00/
      DATA THRD/0.3333333333333333D+00/
!
!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
!
      COS2L=COSL*COSL-SINL*SINL
      SIN2L=2.D0*COSL*SINL
      COS3L=COSL*COS2L-SINL*SIN2L
      SIN3L=COSL*SIN2L+COS2L*SINL
!
      C=CSTHET
      C2=C*C
      C3=C*C2
      S=SNTHET
      S2=S*S
      S3=S*S2
!
      PNM(1)=C20*(C2-THRD)
      PNM(2)=C21*C*S
      PNM(3)=C22*S2
      PNM(4)=C30*(C3-3.D0*C*S2+.6D0*C)
      PNM(5)=C31*S*(C2-.2D0)
      PNM(6)=C32*S2*C
      PNM(7)=C33*S3
!
      RETURN
      END
