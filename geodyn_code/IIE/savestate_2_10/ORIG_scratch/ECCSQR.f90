!$ECCSQR
      SUBROUTINE ECCSQR(ELEMS,ESQ)
!***********************************************************************
!    DATE      11/14/90                           PGMR S. B. LUTHCKE
!
!    PURPOSE:  CALCULATES THE SQUARE OF THE ECCENTRICITY FROM CARTESIAN
!              ELEMENTS
!
!    INPUT     ELEMS - CARTESIAN ELEMENTS
!
!    OUTPUT    ESQ - SQUARE OF THE ECCENTRICITY
!
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
!
      DIMENSION ELEMS(6)
!**********************************************************************
!* START OF EXECUTABLE CODE
!**********************************************************************
      X=ELEMS(1)
      Y=ELEMS(2)
      Z=ELEMS(3)
      XDOT=ELEMS(4)
      YDOT=ELEMS(5)
      ZDOT=ELEMS(6)
!
      RRDOT=X*XDOT+Y*YDOT+Z*ZDOT
      R=SQRT(X**2+Y**2+Z**2)
      VSQ=XDOT**2+YDOT**2+ZDOT**2
      AINV=2.0D0/R-VSQ/GM
! COMPUTE ECCENTRICITY
      HX=Y*ZDOT-Z*YDOT
      HY=Z*XDOT-X*ZDOT
      HZ=X*YDOT-Y*XDOT
      HSINI2=HX**2+HY**2
      HSQ=HSINI2+HZ**2
      H=SQRT(HSQ)
      OME2=HSQ*AINV/GM
      ESQ=1.0D0-OME2
!
      RETURN
      END
