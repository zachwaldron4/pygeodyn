!$INTERPOLATEX
      SUBROUTINE interpolateX(lon,lat,data,result)
!
!     Originally written in C++ by Marek Ziebart
!     Translated to FORTRAN77 by Stuart Robson 05/05/03
!     FORTRAN77 Revised by Ant Sibthorpe 07/05/03
!
      IMPLICIT NONE

      DOUBLE PRECISION lat, lon
      DOUBLE PRECISION data(73,144)
      DOUBLE PRECISION result

      DOUBLE PRECISION x1,y1,x2,y2,x3,y3,x4,y4
!     longitudes and latitudes of the grid square corners

      DOUBLE PRECISION accX1,accX2,accX3,accX4
!     BFS X-axis accelerations at the grid node points

      DOUBLE PRECISION m1,c1,m2,c2,m3,c3
!     gradient and intercepts of the lines from p1 to p2, p3 to p4
!     and finally up and down

      INTEGER xfactor, yfactor
      INTEGER i, j, icount

      result = 0.0D0

!-----------------------------------
!
!     take the longitude  and latitude  of the interpolation
!     point and calculate the required
!     latitudes and longitudes of the grid square corners
!
!     x3/y3 ......... x4/y4
!     :                                        :
!     :                                        :
!     :                .lon/lat        :
!     :                                        :
!     x1/y1        ......... x2/y2
!
!-----------------------------------

      xfactor = (lon/2.5)

!-----------------------------------
!     xfactor is the number of steps (in units of 2.5 deg) needed to
!     get the longitudes of the two grid points (x2 and x4)
!     to the right of the interpolation point if lon < 0
!-----------------------------------


!-----------------------------------
!      e.g. if interpolation longitude = -27.3, grid square longitudes
!      required are -30 and -25
!-----------------------------------
      IF(lon.lt.0.0D0)THEN
          x1 = ((xfactor-1)*2.5)
          x2 = (xfactor*2.5)
      ELSE
!-----------------------------------
!      e.g. if interpolation longitude = +27.3, grid square longitudes
!      required are +25 and +30
!-----------------------------------
          x1 = (xfactor*2.5)
          x2 = ((xfactor+1)*2.5)
      ENDIF

      x3 = x1
      x4 = x2
!-----------------------------------
!       x3 is vertically above x1 , x4 is vertically above x2 on the grid
!-----------------------------------

      yfactor = (lat/2.5)
!-----------------------------------
!       similar reasoning as for the x-axis
!-----------------------------------
      IF(lat.lt.0.0D0)THEN
          y1 = ((yfactor-1)*2.5)
          y3 = (yfactor*2.5)
      ELSE
          y1 = (yfactor*2.5)
          y3 = ((yfactor+1)*2.5)
      ENDIF

      y2 = y1
      y4 = y3
!-----------------------------------
!     y2 is at the same latitude as y1, y4 at the same latitude as y3
!
!     having populated x1,y1 -> x4,y4, get the corresponding X values
!     from the data array
!     Note - in the array longitude varies by column
!     and latitude varies by row: the array is designed to look like a map
!
!     Note that the array bounds are structured thus:
!     [1][1]: lambda = -177.5, phi = 90  ............ [1][144]:
!     lambda = 180, phi = 90
!     :
!     :
!     :
!     [73][1]: lambda = -177.5, phi = -90 ........... [73][144]:
!     lambda = 180, phi = -90
!
!
!-----------------------------------
      j = ((x1/2.5)+72.0)
      i = ((-y1/2.5)+37.0)
!-----------------------------------
!     therefore, lambda = -90 entries are in row 73
!-----------------------------------
!     array elements are defined from 1 to 144, and from 1 to 73
!     if j is set as zero then the interpolation point is between
!     -180 and -177.5
!     in which case the corresponding grid value to -180 is on the
!     +180 side of the grid.
!     Similarly, if j turns out to be 145, then the interpolation
!     point must be between
!     +180 and +182.5, in which case the +182.5 grid node must be taken
!     from the -177.5 line.
!     A similar set of if statements cover the associated row issues
!-----------------------------------
      IF(j.eq.0)THEN
          j = 144
      ENDIF
      IF(j.eq.145)THEN
          j = 1
      ENDIF
      IF(i.eq.0)THEN
          i = 73
      ENDIF
      IF(i.eq.74)THEN
          i = 1
      ENDIF

      accX1 = data(i,j)

      j = ((x2/2.5)+72.0)
      i = ((-y2/2.5)+37.0)

      IF(j.eq.0)THEN
          j = 144
      ENDIF
      IF(j.eq.145)THEN
          j = 1
      ENDIF
      IF(i.eq.0) THEN
          i = 73
      ENDIF
      IF(i.eq.74)THEN
          i = 1
      ENDIF

      accX2 = data(i,j)

      j = ((x3/2.5)+72.0)
      i = ((-y3/2.5)+37.0)

      IF(j.eq.0)THEN
          j = 144
      ENDIF
      IF(j.eq.145)THEN
          j = 1
      ENDIF
      IF(i.eq.0)THEN
          i = 73
      ENDIF
      IF(i.eq.74)THEN
          i = 1
      ENDIF

      accX3 = data(i,j)

      j = ((x4/2.5)+72.0)
      i = ((-y4/2.5)+37.0)

      IF(j.eq.0)THEN
          j = 144
      ENDIF
      IF(j.eq.145)THEN
          j = 1
      ENDIF
      IF(i.eq.0)THEN
          i = 73
      ENDIF
      IF(i.eq.74)THEN
          i = 1
      ENDIF

      accX4 = data(i,j)

!-----------------------------------
!     compute gradients and intercepts of interpolation functions
!-----------------------------------
      m1 = (accX1-accX2)/(x1-x2)
      c1 = (x1*accX2-x2*accX1)/(x1-x2)
      m2 = (accX3-accX4)/(x3-x4)
      c2 = (x3*accX4-x4*accX3)/(x3-x4)
!-----------------------------------
!     re-use accX1 and accX2 variables to do the vertical function calculation
!-----------------------------------
      accX1 = m1*lon+c1
      accX2 = m2*lon+c2
      m3 = (accX1-accX2)/(y1-y3)
      c3 = (y1*accX2-y3*accX1)/(y1-y3)

      result = m3*lat + c3

      END
