!$ UCL_JASON_panel_accn
      SUBROUTINE UCL_JASON_panel_accn(sX,sY,sZ,aX,aY,aZ,POSSADM,side,   &
     &                                theta,nX,nY,nZ,FnTRR)
!
!     Originally written in C++ by Marek Ziebart
!     FORTRAN77 by Ant Sibthorpe 02/05/06
!
!       sX,sY,sZ are the XYZ components of the normalised Sun-probe vector,
!     in the Body-Fixed System (BFS) basis
!       aX,aY,aZ are the output solar radiation pressure accelerations in
!     th BFS basis in units of metres per second squared
!       POSSADM is the solar panel rotation angle in radians, side is a flag
!     indicating right (R) or left (L) solar panel

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DOUBLE PRECISION m,mu,nu,nX,nY,nZ

!     variables
!     DOUBLE PRECISION sX,sY,sZ,aX,aY,aZ,POSSADM
      CHARACTER side
!     speed of light in vaccuum, metres per second
!     DOUBLE PRECISION c_l
      PARAMETER(c_l = 299792458.0)
!     solar irradiance at one astronomical unit, Watts per metre squared
!     DOUBLE PRECISION E_L
      PARAMETER(E_L = 1368.0)
!     solar panel reflectivity coefficient
!     DOUBLE PRECISION mu
      PARAMETER(mu = 0.25)
!     solar panel specularity coefficient
!     DOUBLE PRECISION nu
      PARAMETER(nu = 0.85)
!     area of one solar panel, metres squared
!     DOUBLE PRECISION A_L
      PARAMETER(A_L = 4.768)
!     nominal spacecraft mass, kg
!     DOUBLE PRECISION m
      PARAMETER(m = 489.1)
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      half_M_PI=PI/2.D0

!     BFS components of the unit shear vector
!     DOUBLE PRECISION spX,spY,spZ
!     BFS components of the solar panel unit normal
!     DOUBLE PRECISION nX,nY,nZ
!     angle of incidence of radiation
!     DOUBLE PRECISION theta
!     normal and shear force values
!     DOUBLE PRECISION Fn,Fs
!     variables to hold values of sin(theta) and cos(theta) to avoid
!     multiple calls to series expansion
!     DOUBLE PRECISION sintheta, costheta

!     write(6,6666) side
6666  FORMAT(A1,' SIDE PANEL')

!     compute the components of the solar panel normal
!     IF(side .EQ. 'L')THEN
!         nX = -cos(POSSADM)
!         nY = 0.0;
!         nZ = -sin(POSSADM)
!     ELSE
!         nX = -cos(POSSADM)
!         nY = 0.0;
!         nZ = sin(POSSADM)
!     ENDIF

!     compute the angle of incidence of the incoming radiation
!     theta = acos(-(nX*sX+nY*sY+nZ*sZ))
!     dtheta=theta/degrad
      sintheta = SIN(theta)
      costheta = COS(theta)

      IF(theta .GE. half_M_PI)THEN
!     panel is facing away from radiation flux, no acceleration results
!     from interaction of radiation, with front of panel, accelerations
!     set to zero
           write(6,*)' dbg panel is facing away from radiation flux ',  &
     & theta,half_M_PI
          aX = 0.0
          aY = 0.0
          aZ = 0.0
      ELSE
!     compute the normal and shear forces
          Fn = ((1.0+mu*nu)*costheta+(2.0/3.0)*mu*(1.0-nu))
          Fn = Fn * ((-E_L*A_L*costheta)/c_l)
          Fs = E_L*A_L*costheta*((1.0-mu*nu)*sintheta)/c_l

!     compute the components of the shear vector
          IF(ABS(sintheta) .LT. 1.0D-15)THEN
!         angle of incidence close to zero
              spX = 0.0
              spY = 0.0
              spZ = 0.0
          ELSE
!         note that modulus of s' is equal to the sine of the angle of
!         incidence
              spX = (sX+costheta*nX)/sintheta
              spY = (sY+costheta*nY)/sintheta
              spZ = (sZ+costheta*nZ)/sintheta
          ENDIF
! Scale the normal unit vector by the TRR acceleration and add
! to the final components
      aa=nX*FnTRR
      bb=nY*FnTRR
      cc=nZ*FnTRR

!     scale the normal and shear unit vectors by the normal and shear
!     forces and divide by the nominal spacecraft mass to get the
!     acceleration vector components
          aX = (Fn*nX+Fs*spX)/m
                    aY = (Fn*nY+Fs*spY)/m
                    aZ = (Fn*nZ+Fs*spZ)/m
      ENDIF

      aX = aX + aa
      aY = aY + bb
      aZ = aZ + cc

      END
