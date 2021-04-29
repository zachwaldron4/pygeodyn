!$POSVEL
      SUBROUTINE POSVEL(XYZ,AEI,IDRAD,GM)
!********1*********2*********3*********4*********5*********6*********7**
! POSVEL           84/11/19            8306.0    PGMR - ?
!
! FUNCTION:  TO CONVERT OSCULATING ORBITAL ELEMENTS TO INERTIAL
!            POSITION AND VELOCITY VECTORS
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   XYZ      O    A    CARTESIAN ELEMENTS
!   (6)
!   AEI      I    A    KEPLER ELEMENTS
!   (6)
!   IDRAD    I    S    =2 MEANS INPUT IN RADIANS
!                      =1 MEANS INPUT IN DEGREES
!   GM       I    S    GRAVITIONAL CONSTANT
!
! COMMENTS:
!
!
!********1*********2*********3*********4*********5********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CVIEW /IOUT6 ,ILINE6,IPAGE6,MLINE6,                        &
     &              IOUT8 ,ILINE8,IPAGE8,MLINE8,                        &
     &              IOUT9 ,ILINE9,IPAGE9,MLINE9,                        &
     &              IOUT10,ILIN10,IPAG10,MLIN10,                        &
     &              IOUT15,ILIN15,IPAG15,MLIN15,                        &
     &              IOUT16,ILIN16,IPAG16,MLIN16,                        &
     &              IOUT7 ,NXCVUE
!
      DIMENSION AEI(6),XYZ(6),AEINPM(6),XYZXYZ(6)
!
      EQUIVALENCE (A,AEINPM(1)),(E,AEINPM(2)),(RINCL,AEINPM(3)),        &
     &   (RNODE,AEINPM(4)),(P,AEINPM(5)),(RMEAN,AEINPM(6)),             &
     &   (X,XYZXYZ(1)),(Y,XYZXYZ(2)),(Z,XYZXYZ(3)),                     &
     &   (XDOT,XYZXYZ(4)),(YDOT,XYZXYZ(5)),(ZDOT,XYZXYZ(6))
! DEFINE CONVERSION CRITERIA
      DATA DELTA/0.1D-10/
!
!**********************************************************************
! START OF EXECUTABE CODE *********************************************
!**********************************************************************
!
      SCALE=1.0D0
      IF(IDRAD.EQ.1) SCALE=DEGRAD
      DO 10 I=1,6
      AEINPM(I)=AEI(I)
      IF(I.LE.2) GO TO 10
      AEINPM(I)=AEI(I)*SCALE
   10 END DO
      SQMUA=SQRT(GM/ABS(A)**3)
      E2=E**2
      ONEME2=SQRT(ABS(1.0D0-E2))
! SINES AND COSINES OF THE ELEMENTS
      COSI=COS(RINCL)
      SINI=SIN(RINCL)
      SINN=SIN(RNODE)
      COSN=COS(RNODE)
      SINP=SIN(P)
      COSP=COS(P)
! SET ECC. ANOM. EQUAL TO MEAN ANOM. FOR FIRST APROX.
      ECC=RMEAN
! ITERATE
      IF (E2.GE.1.0D0) GO TO 150
! ...FOR ELLIPTIC ORBITS
      DO 100 J=1,50
      EOO=ECC
      SINECC=SIN(EOO)
      COSECC=COS(EOO)
      ECOS=1.0D0-E*COSECC
      ECC=EOO-(EOO-E*SINECC-RMEAN)/ECOS
      IF(ABS(EOO-ECC).LT.DELTA) GO TO 200
  100 END DO
      WRITE(IOUT6,1000)
      GO TO 200
! ...FOR HYPERBOLIC ORBITS
  150 DO 160 J=1,100
      EOO=ECC
      SINECC=SINH(EOO)
      COSECC=COSH(EOO)
      ECOS=E*COSECC-1.0D0
      ECC=EOO-(E*SINECC-EOO-RMEAN)/ECOS
      IF (ABS(EOO-ECC).LT.DELTA) GO TO 200
  160 END DO
      WRITE(IOUT6,1000)
  200 SPCN=SINP*COSN
      CPSN=COSP*SINN
      CPCN=COSP*COSN
      SPSN=SINP*SINN
      A2=ABS(A)*ONEME2
      AX=A*(CPCN-SPSN*COSI)
      AY=A*(SPCN*COSI+CPSN)
      AZ=A*SINP*SINI
      BX=-A2*(SPCN+CPSN*COSI)
      BY=A2*(CPCN*COSI-SPSN)
      BZ=A2*COSP*SINI
      C=COSECC-E
      EDOT=SQMUA/ECOS
! ...FOR X,Y,Z
      X=AX*C+BX*SINECC
      Y=AY*C+BY*SINECC
      Z=AZ*C+BZ*SINECC
! ...FOR XDOT,YDOT,ZDOT
      IF (E2.GE.1.0D0) SINECC=-SINECC
      XDOT=EDOT*(BX*COSECC-AX*SINECC)
      YDOT=EDOT*(BY*COSECC-AY*SINECC)
      ZDOT=EDOT*(BZ*COSECC-AZ*SINECC)
      DO 300 I=1,6
  300 XYZ(I)=XYZXYZ(I)
!***  IF(JBODY.EQ.0) RETURN
!***  THETG=THETGR(DAY,DAYN)
!***  CALL MATROT(XYZ,2,.TRUE.)
      RETURN
!
! FORMATS
!
 1000 FORMAT('10',19X,'ECCENTRIC ANOMALY NOT CONVERGED')
      END
