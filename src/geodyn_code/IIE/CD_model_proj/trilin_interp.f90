!$trilinear_interp
      SUBROUTINE trilinear_interp(GLON,GLAT,ALTKM,VARIABLE,VAL)

!********1*********2*********3*********4*********5*********6*********7**
! trilinear_interp ---------- Feb 2022 ----- Written by Zach Waldron
!
!
! FUNCTION:
!      Manually interpolate using a Trilinear Interpolation Calculation
!      Once we have our saved values we interpolate the values of the cube 
!          to a single point (the one requested) by GLAT, GLON, ALTKM

!
!
!   I/O PARAMETERS:
!   NAME       I/O   A/S   DESCRIPTION OF PARAMETERS
!   ------     ---   ---   ------------------------------------------------
!   GLON        I     S    Geodetic latitude  in degrees
!   GLAT        I     S    Geodetic longitude in degrees
!   ALTKM       I     S    Height of satellite above elipsoid (M)  
!   VARIABLE    I     A    Array containing values for 9 corners of cube from Kamodo
!   VAL         O    S    INTERPOLATED VALUES
!********1*********2*********3*********4*********5*********6*********7**



      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      DOUBLE PRECISION,dimension(9) :: VARIABLE
      DOUBLE PRECISION :: VAL


!!!!! Once we have our saved values we interpolate the values of the cube 
!!!!!      to a single point (the one requested) by GLAT, GLON, ALTKM
    delta_deg = 2
    delta_m   = 2
    x  = GLON
    x0 = x - delta_deg
    x1 = x + delta_deg

    y  = GLAT
    y0 = y - delta_deg
    y1 = y + delta_deg

    z  = ALTKM
    z0 = z - delta_m
    z1 = z + delta_m
    
   C000 = VARIABLE(6)  ![5]    ! # bottom, front, left
   C100 = VARIABLE(8)  ![7]    ! # bottom, front, right
   C001 = VARIABLE(2)  ![1]    ! # top,    front, left
   C101 = VARIABLE(4)  ![3]    ! # top,    front, right
   C010 = VARIABLE(7)  ![6]    ! # bottom, back,  left
   C110 = VARIABLE(9)  ![8]    ! # bottom, back,  right
   C011 = VARIABLE(3)  ![2]    ! # top,    back,  Left
   C111 = VARIABLE(5)  ![4]    ! # top,    back,  right
    
    !WRITE(6,*) 'C000:    ', C000

    !##===================================================================
    !##        Manually INTERPOLATE-- Trilinear Interpolation Calculation
    !##===================================================================

    !### On a periodic and cubic lattice, let xd, yd, zd be the differences 
    !### between each of x, y, z and the smaller coordinate related.
    !#
    !###  x0 indicates the lattice point below x 
    !###  x1 indicates the lattice point above x
    xd =  (x-x0)/(x1-x0)
    yd =  (y-y0)/(y1-y0)
    zd =  (z-z0)/(z1-z0)
    !# print(xd,yd,zd)

    !##### First we interpolate along x:   (push one of the X faces of cube towards the opposing face)
    !###   C000 uis the function value (x0, y0, z0)
    C00 = C000*(1-xd) + (C100*xd)
    C01 = C001*(1-xd) + (C101*xd)
    C10 = C010*(1-xd) + (C110*xd)
    C11 = C011*(1-xd) + (C111*xd)

    !#### Next we interpolate along y:  (pushing the values that are now in middle of cube towards the center Y)
    C0 = C00*(1-yd) + C10*yd
    C1 = C01*(1-yd) + C11*yd

    !#### Finally we interpolate along Z   (walk through the line that remains)
    C = C0*(1-zd) + C1*zd
    
    VAL = C
   99 RETURN
      END