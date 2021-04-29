!$YAWGPS.f
      SUBROUTINE YAWGPS(MJDSEC,FSEC,ISATID,ELEM,YAWN,YAW,IYAWER,IRET,AA)
!********1*********2*********3*********4*********5*********6*********7**
! YAWGPS
!
! FUNCTION:
! subroutine yawgps  calling  "sateclipse"
! that calculates the actual yaw of a satellite during eclipse
! maneuvers, according to Jan Kouba ("A simplified yaw-attitude
! model for eclipsing GPS satellites", in "GPS Solutions" (2009)
! 13:1~V12, DOI 10.1007/s10291-008-0092-1).
! "Sateclipse" is based on Kouba's 2009 subroutine "eclips", with
! some changes explained in the comments at the beginning
! of the subroutine.
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!   MJDSEC   I         INTEGER EPHEMERIS SECONDS SINCE GEODYN REF.TIME
!   FSEC     I         FRACTIONAL REMAINING SECONDS
!   ISATID   I         SATELLITE ID
!   ELEM     I         TRUE OF DATE ELEMENTS
!   YAW      O         YAW ANGLE (RAD)
!   BETA     O         BETA ANGLE(RAD)
!   ORBMU    O         ORBIT ANGLE (RAD)
!
! YAW = ATAN2(-DTAN(beta),DSIN(ORBMU)) ! (Bar-Sever, 1995)
! with the additional bias term: asin(0.0175D0*0.5D0/SINEPSILON)
! in the case of block IIA satellites.
!
!ddi

!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION(A-H,O-Z),LOGICAL(L)
      SAVE

!
      PARAMETER (NPRN=32)
      LOGICAL NIGHT,NOON

      COMMON/CBDTRU/BDTRUE(7,999)
      COMMON/CINTI/IBACK ,IBACKV,IORDER,IORDRV,NOSTEP,NOCORR,           &
     &      NSAT  ,N3    ,NEQN  ,NEQN3 ,NH    ,NHV   ,NSTEPS,           &
     &      NSTEPV,ICPP  ,ICPV  ,ICCP  ,ICCV  ,ICCPV ,ICCVV ,           &
     &      ISUMX ,IXDDOT,ISUMPX,IPXDDT
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      DIMENSION ELEM(6),AA(1)
      DIMENSION ISATID(1),SATPOS(3),SATVEL(3),SUNXYZ(3),SATN(3)
      DIMENSION ZN(3),ACROSS(3),ACROSSN(3),XAX(3),XN(3)
      DIMENSION SUNY(3),HELION(3),SUN(3),SUNX(3),SUNBETAV(3),OUTXYZ(3)

!        IBLK           SV BLOCK  1=I, 2=II, 3=IIA, IIR=(4, 5) IIF=6
!
!  EXTRACT IPRN IBLK
!
       CALL GPSBLOCK(ISATID,ISVN,IPRN,IBLK)
!          write(6,*)' dbg YAWGPS IBLK IPRN ',IBLK,IPRN


        SUNXYZ(1)=BDTRUE(1,8)
        SUNXYZ(2)=BDTRUE(2,8)
        SUNXYZ(3)=BDTRUE(3,8)

!
      PION2 = 0.5D0*PI
      RADEG = 1.D0/DEGRAD
!
! To calculate the nominal yaw angle for comparison with the
! actual yaw according to "sateclipse", and to provide
! the "XN" input vector (with the nominal orientation) to that subroutine,
! Find:
! the sun-pointing unit vector in the satellite's body-frame,
! the sine and cosine of the Earth-satellite-Sun angle "epsilon",
!
! "SUNXYZ" is the vector pointing from the geocenter to the sun (center);
! "SATPOS" is the vector from the geocenter to the satellite;
! "SATVEL" is the satellite velocity vector in inertial coordinates.
!
!DEP THE CODE BELOW IS SIMILAR TO GPSATT
      DO I = 1,3
      SATPOS(I) = ELEM(I)
      SATVEL(I) = ELEM(I+3)
      SUNY(I) = SUNXYZ(I)-SATPOS(I)
      ENDDO
      CALL VNORM(SUNY,SUN,XHELIOSMOD)
      CALL VNORM(SATPOS,SATN,SATPOSMOD)
      CALL VNORM(SUNXYZ,HELION,HELIOSMOD)
!
! Body-frame "z" axis is positive down
      DO I = 1,3
      ZN(I) = -SATN(I)
      ENDDO
!
!
! Find, in inertial space, the vectors in the along
! and across directions, completing, with SATN, the orbit-fixed triad.
!
      CALL vprod(SATN,SATVEL,ACROSS)
      CALL VNORM(ACROSS,ACROSSN,SACROSS)
      CALL SPROD(HELION,ACROSSN,COSALPHA0)
      BETA = PION2-ACOS(COSALPHA0)
!
!
! Find the mu orbit angle from sat. position to orbital midnight.
!
      DO i = 1,3
      SUNX(i) = -(HELION(i)-COSALPHA0*ACROSSN(i))
      ENDDO
!
! Sunbetav = unit "midnight" orbit vector.
      CALL VNORM(SUNX,SUNBETAV,SUNXMOD)
      CALL SPROD(SATN,SUNBETAV,COSMU)
      CALL SPROD(SATVEL,SUNBETAV,SGNMU)

!     OMU - orbit angle measured from the midnight point to the
!            position of the satellite
      ORBMU = ACOS(COSMU)*(-SIGN(1.D0,SGNMU))
      OMU = ORBMU/DEGRAD
      SINMU = SIN(ORBMU)
!
! Finding the sine and cosine of epsilon.
      CALL SPROD(ZN,sun,COSEPSILON)
      if(ABS(COSEPSILON).gt.1.D0.and.ABS(beta0).LT.1.D-6) &
     &COSEPSILON = 1.D0*SIGN(1.D0,COSEPSILON)
      SINEPSILON = SQRT(1.D0-COSEPSILON**2)
!
! Finding the body-fixed axis XN in the inertial frame:
!
      DO I = 1,3
      XAX(I) = SUN(I)-ZN(I)*COSEPSILON
      ENDDO
      CALL VNORM(XAX,XN,XAXNORM)
!DEP THE CODE ABOVE IS SIMILAR TO GPSATT


!
!     COMPUTE TIME: SECONDS OF START OF GPS WEEK.
      CALL MJDGPS(MJDSEC,FSEC,NWEEK,TTAG,AA,.TRUE.)

!
! Calculate the nominal (yaw+yaw bias):
!
! (Bar-Sever, 1995)
      YAWN = ATAN2(-TAN(BETA),SIN(ORBMU))
      YAWNOM = RADEG*YAWN
!     IF(IBLK.EQ.2.OR.IBLK.EQ.3) YAWNOM = YAWNOM +      &
!    & ASIN(0.0175D0*0.5D0/SINEPSILON)*RADEG
!
      CALL SATECLIPSE(IPRN,TTAG,SUNXYZ,ELEM,IBLK,XN,    &
     &OUTXYZ,YANGLE,PHI,NIGHT,NOON,NOPHI,YBIAS)
!
!  The correct yaw is: yawangle = PHI-YANGLE+YAWNOM
!  where  yawnnom is the nominal yaw. It may or may not
!  have an extra 180 degree turn for the IIRs (e.g., GEODYN).
!
      YAWNU = PHI-YANGLE+YAWNOM
      YAW =    YAWNU*(1-NOPHI)+YAWNOM*NOPHI
      YAWIGS = YAWNU*(1-NOPHI)+YAWNOM*NOPHI

!     yawigs=dmod(yawigs,360.d0)
!     if(yawigs.gt.180.d0) yawigs=yawigs-360.D0
!     if(yawigs.lt.-180.d0) yawigs=yawigs+360.D0
!     yaw=yawigs


      IYAWER=NOPHI

!  Geodyn adds 360 degrees to its yaw "yawng".
!     yawigs(kkk,iplace) = yawnu*(1-NOPHI)+yawng*NOPHI-360.D0
!
      RETURN
      END
