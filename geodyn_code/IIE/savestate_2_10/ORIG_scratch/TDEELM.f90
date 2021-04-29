!$TDEELM
      SUBROUTINE TDEELM(XYZ,RATIO,A,DINCL,DNODE,OMEGA,DMEAN,E)
!********1*********2*********3*********4*********5*********6*********7**
! TDEELM           83/08/16            8308.0    PGMR - D. ROWLANDS
!
!
! FUNCTION:  COMPUTE THE KEPLARIAN ELEMENTS OF A BODY
!            IN APPARENT MOTION ABOUT THE EARTH
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XYZ      I         STATE VECTOR OF THE BODY
!   RATIO    I         RATIO (MASS BODY)/(MASS EARTH)
!   A        O         SEMI MAJOR AXIS
!   DINCL    O         INCLINATION
!   DNODE    O         LONGITUDE OF ASCENDING NODE
!   OMEGA    O         ARGUMENT OF PERIGEE
!   DMEAN    O         MEAN ANOMALY
!   E        O         ECCENTRICITY
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
      DIMENSION XYZ(6),XYZXYZ(6)
      COMMON/CGRAV/GM,AE,AESQ,FE,FFSQ32,FSQ32,XK2,XK3,XLAM,SIGXK2,      &
     &      SIGXK3,SIGLAM,RATIOM(2),AU,RPRESS
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
       EQUIVALENCE (X,XYZXYZ(1)),(Y,XYZXYZ(2)),(Z,XYZXYZ(3)),           &
     &   (XDOT,XYZXYZ(4)),(YDOT,XYZXYZ(5)),(ZDOT,XYZXYZ(6))
      DATA ZERO/0.D0/,ONE/1.D0/,TWO/2.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      GMM=GM*(ONE+RATIO)
      DO 10 I=1,6
   10 XYZXYZ(I)=XYZ(I)
      R=SQRT(X**2+Y**2+Z**2)
      VSQ=XDOT**2+YDOT**2+ZDOT**2
      C1=Y*ZDOT-Z*YDOT
      C2=Z*XDOT-X*ZDOT
      C3=X*YDOT-Y*XDOT
      HSQ=C1**2+C2**2+C3**2
      H=SQRT(HSQ)
      COSI=C3/H
      SINI=SQRT(ONE-COSI**2)
! COMPUTE INCLINATION
      DINCL=ATAN2(SINI,COSI)
      IF (DINCL.LT.ZERO) DINCL=DINCL+TWOPI
      AINV=TWO/R-VSQ/GMM
! COMPUTE SEMI-MAJOR AXIS
      A=ONE/AINV
! COMPUTE ECCENTRICITY
      ESQ=ONE-HSQ*AINV/GMM
      E=SQRT(ESQ)
      P=A*(ONE-ESQ)
      RRDOT=X*XDOT+Y*YDOT+Z*ZDOT
      HSI=H*SINI
      SINN=C1/HSI
      COSN=-C2/HSI
! COMPUTE LONGITUDE OF ASCENDING NODE
      DNODE=ATAN2(SINN,COSN)
      IF(DNODE.LT.ZERO)DNODE=DNODE+TWOPI
      PER=P/(E*R)
      SNU=RRDOT*PER/H
      CNU=PER-ONE/E
      XNU=ATAN2(SNU,CNU)
      IF(XNU.LT.ZERO) XNU=XNU+TWOPI
      CEC=(CNU+E)/(ONE+E*CNU)
      SEC=(SQRT(ABS(ONE-ESQ))*SNU)/(ONE+E*CNU)
      EC=  ATAN2(SEC,CEC)
      IF(EC.LT.ZERO)EC = EC + TWOPI
! COMPUTE MEAN ANOMALY
      DMEAN = EC - E*SEC
      CU=( X*COSN+Y*SINN)/R
      SU=((Y*COSN-X*SINN)*COSI+Z*SINI)/R
      U=ATAN2(SU,CU)
      IF(U.LT.ZERO)U=U+TWOPI
! COMPUTE ARGUMENT OF PERIGEE
      OMEGA=U-XNU
      IF(OMEGA.LT.ZERO) OMEGA=OMEGA+TWOPI
      RETURN
      END
