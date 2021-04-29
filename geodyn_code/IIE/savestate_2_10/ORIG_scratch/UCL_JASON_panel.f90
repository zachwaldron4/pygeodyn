!$ UCL_JASON_panel
      SUBROUTINE  UCL_JASON_panel(aX,aY,aZ,LON,LAT,side,POSSADM,      &
     &                           theta,nX,nY,nZ,sXc,sYc,sZc,FnTRR)
!-----------------------------------
!
!     Originally written in C++ by Marek Ziebart
!     FORTRAN77 written by Ant Sibthorpe 02/05/06
!
!     **************************************************
!     This program provides an interface to routines (see below) produced by
!     the Geodesy Research Group at University College London that
!     enable the modelling of solar radiation pressure (SRP) force
!     effects on the JASON-1 satellite solar panels.
!     Complete modelling of these effects for JASON-1 must
!     incorporate the solar panel characteristics and pointing
!     vectors (data for which can be found in the document
!     guidelines.doc on ftp.ge.ucl.ac.uk/pub/JASON-models/), and the
!     resulting accelerations must be scaled to account for the actual
!     probe-sun distance and mass of satellite vehicle at epoch under
!     consideration.
!
!     The following function:
!
!     SUBROUTINE UCL_JASON_panel_accn(sXc,sYc,sZc,aX,aY,aZ,POSSADM,side)
!
!     is supplied to use as a check utility on model implementation.
!       The model outputs the acceleration of the spacecraft due to a
!     single solar panel.
!
!     The main calling function within this program represents an
!     example of how an interface to this subroutine may operate, and
!     also presents an opportunity to test the output values of any
!     implementation.
!     **************************************************
!
!
!-----------------------------------
      IMPLICIT NONE
!
      DOUBLE PRECISION lon, lat, aX, aY, aZ,  sXc, sYc, sZc,nX,nY,nZ
      DOUBLE PRECISION POSSADM, FnTRR, theta
      CHARACTER side
!     lon/lat=longitude and latitude [decimal degrees] of the sun in the
!     body fixed frame
!     aX,aY,aZ are the accelerations along the body fixed x,y and
!     z axes due to solar radiation pressure effects on one JASON-1 panel
!     only at 1 astronomical unit
!     POSSADM is the solar array rotation angle [decimal degrees]
!     side is either L[left] or R[right], case insensitive!
!     sXc,sYc,sZc are the unit Sun-probe vector components
!     These accelerations have been calculated at the nominal mass of
!     489.1 Kg
      DOUBLE PRECISION M_PI
      INCLUDE 'COMMON_DECL.inc'
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY

!     WRITE(*,7)
!   7 FORMAT('Enter BFS longitude of the Sun (decimal degrees): ')
!     READ(*,*)lon
!     WRITE(*,8)
!   8 FORMAT('Enter BFS latitude of the Sun (decimal degrees): ')
!     READ(*,*)lat
!     WRITE(*,9)
!   9 FORMAT('Enter the panel identifier (L = left, R = Right): ')
!     READ(*,*)side
!     WRITE(*,10)
!  10 FORMAT('Enter solar panel rotation angle (decimal degrees): ')
!  READ(*,*)POSSADM

      aX=0.0
      aY=0.0
      aZ=0.0

!     convert input to radians
!     POSSADM = POSSADM*(PI/180.0)
! no need they are already in radians
!     lon = lon*(PI/180.0)
!     lat = lat*(PI/180.0)
!       write(6,*)' dbg panel lat lon ',lat,lon
!     sXc = -cos(lat)*cos(lon)
!     sYc = -cos(lat)*sin(lon)
!     sZc = -sin(lat)
!       write(6,*)' dbg 1 panel calling panel_accn '
      CALL UCL_JASON_panel_accn(sXc,sYc,sZc,aX,aY,aZ,POSSADM,side,      &
     &                          theta,nX,nY,nZ,FnTRR)

!     WRITE(*,11)aX
   11 FORMAT(/'RESULTS PAN:'/'Acceleration along BFS X axis: ',1PE16.5)

!     WRITE(*,12)aY
   12 FORMAT('Acceleration along BFS Y axis: ',1PE16.5)

!     WRITE(*,13)aZ
   13 FORMAT('Acceleration along BFS Z axis: ',1PE16.5)

      RETURN
      END
