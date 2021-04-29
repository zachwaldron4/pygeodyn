!$TERRAIN
      Function Terrain(alat,alon)
!********1*********2*********3*********4*********5*********6*********7**
! TERRAIN
!
!
! FUNCTION: CALCULATE TERRAIN HEIGHT (KM) FROM REFRENCE ELLIPSOID
!           BY THE MARS TOPOGRAPHY DATA
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!...  Computes terrain height (km) relative to reference ellipsoid
!     from given array of terrain height data, th (km), at latitude
!     alat, longitude alon (degrees)
      COMMON/MARSTP/THMSTP(19,19)
! in order to fix the order of THMSTP , creat a array STP(19,19)
      DIMENSION STP(19,19)
      DO I=1,19
        DO J=1,19
        STP(I,J)=THMSTP(J,I)
        ENDDO
      ENDDO
!...  Terrain Height at Viking Lander 1 site
!...  Number of latitudes and longitudes in array
      Data nlat,nlon/19,19/
!...  Viking-1 latitude, longitude, height
      Data vl1lat,vl1lon,vl1hgt/22.,48.,-0.5/
!...  Viking-2 latitude, longitude, height
      Data vl2lat,vl2lon,vl2hgt/48.,226.,-2.0/
!...  latitude, longitude increments for data base
      dlat = 180./(nlat-1.)
      dlon = 360./(nlon-1.)
!...  latitude index of interpolation rectangle
      ilat = 1 + INT((90.+alat)/dlat)
      If (ilat .gt. nlat-1)ilat = nlat-1
!...  longitude index of interpolation rectangle
      ilon = 1 + INT(alon/dlon)
      If (ilon .gt. nlon-1)ilon = nlon-1
!...  Terrain heights at corner points of interpolation rectangle
      F1 =    STP(ilat,ilon)
      F2 =    STP(ilat,ilon+1)
      F3 =    STP(ilat+1,ilon)
      F4 =    STP(ilat+1,ilon+1)
!...  Normalized latitude displacement
      ylat = 1. + (90.+alat)/dlat - ilat
!...  Normalized longitude displacement
      xlon = 1. + (alon/dlon) - ilon
!...  Interpolated terrain height
      Terr = F1 + (F2 - F1)*xlon + (F3 - F1)*ylat                       &
     & + (F4 - F2 - F3 + F1)*xlon*ylat
!...  Normalized distances from Viking 1 and Viking 2 Sites
      radvl1 = ((alat-vl1lat)**2 + (alon-vl1lon)**2)/4.0
      radvl2 = ((alat-vl2lat)**2 + (alon-vl2lon)**2)/4.0
!...  Insure proper terrain height at Viking Lander sites
      If (radvl1 .lt. 1.0)then
        Terr = (1.-radvl1)*vl1hgt + Terr*radvl1
      Else If(radvl2 .lt. 1.0)then
        Terr = (1.-radvl2)*vl2hgt + Terr*radvl2
      Endif
      Terrain = Terr
      Return
      END
