      SUBROUTINE UCL_GFO_bus(xdata,ydata,zdata,xacc,yacc,zacc,       &
     &                         LAT,LON)
!-----------------------------------
!     SNLAT and SNLON are output in radians.
!
!     Originally written in C++ by Marek Ziebart
!     Translated to FORTRAN77 by Stuart Robson 05/05/03
!     FORTRAN77 Revised by Ant Sibthorpe 07/05/03
!
!     **************************************************
!     This program provides an interface to routines (see below) produced by
!     the Geodesy Research Group at University College London that
!     enable the modelling of solar radiation pressure (SRP) and
!     thermal re-radiation (TRR) force effects on the JASON-1 satellite
!     bus.  Complete modelling of these effects for JASON-1 must include
!     incorporation of the solar panel characteristics and pointing
!     vectors (data for which can be found in the document
!     guidelines.doc on ftp.ge.ucl.ac.uk/pub/JASON-models/), and the
!     resulting accelerations must be scaled to account for the actual
!     probe-sun distance and mass of satellite vehicle at epoch under
!     consideration.
!
!     The following functions:
!
!     SUBROUTINE ReadSRPdatafile(data,filnam,error)
!     SUBROUTINE interpolateX(lon,lat,data,result)
!
!     are used to populate 3 RAM arrays (1 for each of the X,Y and Z body-
!     fixed [BFS] axes) with the grid file data that represents the computed
!     SRP/TRR accelerations at discrete 2.5 degree increments in the body fixed
!     frame.
!     The procedure is to declare and populate the arrays at the start
!     of the orbit analysis.  Given the latitude and longitude of the
!     Sun in the BFS frame, the function interpolateX() is used to
!     calculate the nominal accelerations using each of the X,Y and Z
!     arrays in turn.  The function uses bi-linear interpolation.
!
!     The main calling function within this program represents an
!     example of how an interface to these subroutines may operate, and
!     also presents an opportunity to test the output values of any
!     implementation.
!     **************************************************
!
!     The data array held in RAM has the following structure:
!
!     [maximum latitude, minimum longitude entry] = [1][1] ........
!           .... [maximum latitude, maximum longitude entry] = [1][144]
!             -
!             -
!             -
!             -
!     [minimum latitude, minimum longitude entry] = [73][1] ........
!           .... [minimum latitude, maximum longitude entry] = [73][144]
!
!     With a 2.5 deg grid spacing this results in an array with 73
!            rows and 144 columns, ranging from:
!     minimum longitude: -177.5
!     minimum latidude: -90
!
!     to
!
!     maximum longitude: +180
!     maximum latitude: +90
!
!     The information about the latitudes, longitudes and grid spacing
!               is not explicitly stored, and so the
!     algorithm assumes the data is in this structure already
!
!-----------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
!
      INTEGER error !file reading error flag
!     arrays to hold the respective grid files modelling the forces along the
!     body fixed X,Y and Z axes due to solar radiation pressure and thermal
!     re-radiation
      DOUBLE PRECISION xdata(73,144), ydata(73,144), zdata(73,144)
      DOUBLE PRECISION lon, lat, xacc, yacc, zacc
!     lon/lat=longitude and latitude of the sun in the body fixed frame
!     xacc/yacc/zacc are the accelerations along the body fixed x,y and
!     z axes due to solar radiation pressure and thermal re-radiation
!     effects on the JASON-1 bus only at 1 astronomical unit
!     These accelerations have been calculated at the nominal mass of
!     489.1 Kg
      CHARACTER*80 filnam
      DIMENSION XSNI(3),XSN(3)
      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY
      COMMON/CRMB/RMB(9), rmb0(9)
      COMMON/MNSNSP/XMNSNS(7,2)
      COMMON/UCLJAS/COLMAX,ROWMAX,RMNLON,RMXLON,RMNLAT,RMXLAT,XUCLJA
      DATA DP001/.001D0/

!
!**********************************************************************
!* START OF EXECUTABLE CODE *******************************************
!**********************************************************************
!
        maxcolumn=INT(colmax)
        maxrow   =INT(rowmax)

!  DEBUG GRID DATA
!       write(6,*)' dbg xdata '
!       do i=1,maxrow
!       write(6,*)(xdata(i,j),j=1,maxcolumn)
!       enddo
!       write(6,*)' dbg ydata '
!       do i=1,maxrow
!       write(6,*)(ydata(i,j),j=1,maxcolumn)
!       enddo
!       write(6,*)' dbg zdata '
!       do i=1,maxrow
!       write(6,*)(zdata(i,j),j=1,maxcolumn)
!       enddo
!  DEBUG GRID DATA

        CALL interpolateX(lon,lat,xdata,xacc)
        CALL interpolateX(lon,lat,ydata,yacc)
        CALL interpolateX(lon,lat,zdata,zacc)
!       xacc=-xacc
!       yacc=-yacc

!       WRITE(*,10)xacc
   10   FORMAT(/'RESULTS SR:'/'Acceleration along BFS X axis: ',1PE16.5)

!       WRITE(*,11)yacc
   11   FORMAT('Acceleration along BFS Y axis: ',1PE16.5)

!       WRITE(*,12)zacc
   12   FORMAT('Acceleration along BFS Z axis: ',1PE16.5)

      RETURN
      END
