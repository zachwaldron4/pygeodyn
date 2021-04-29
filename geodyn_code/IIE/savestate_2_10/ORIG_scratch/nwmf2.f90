!$NWMF2
      subroutine nwmf2(latitu, elev, wmf)
!***********************************************************************
! NWMF2                      12/00/96     9611.02      PRGM - D.D ROWLAN
!                                                     UPDATE - S. LUO
!
!    FUNCTION:
!
!         Routine to compute the new wmf2.0 mapping function which
!         depends only on latitude.
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  latitu    I    S    - latitude (degrees)
!  elev      I    S    - elevation (degrees)
!  wmf(1)    O         - wet delay mapping function
!  wmf(2)    O         - d_wet_mapping_function/d_elevation
!***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE
!   a,b,c       - the a,b,and c coefficients in the continued fraction
!                form of Marini
!  beta        - intermediate term in calculation
!  gamma       - intermediate term in calculation
!  sine        - Sine of elevation angle
!  cose        - Cos of elevation angle
!  latitu   - latitude (degrees)
!  wmf(1)      - wet delay mapping function
!  wmf(2)      - d_wet_mapping_function/d_elevation
!  topcon      - Constant of top of mapping fuinction to ensure
!                that value is 1.0000 at zenith

      DOUBLE PRECISION a,b,c, beta, cose, wmf(2), gamma, sine, topcon

!  latitu   - latitude (degrees)
!  l          - absolute latitude
!  dl         - incremental latitude from last latwmf
!  elev       - elevation (degrees)
!  dl,da,db,dc  - used for interpolation

      DOUBLE PRECISION latwmf(5), abcw2p0(5,3)
      DOUBLE PRECISION dl, da, db, dc
      DOUBLE PRECISION latitu, l, elev, deg2rad

!  define parameters used for calculating coefficients.

      data latwmf / 15, 30, 45, 60, 75/

!  coefficients are from fits to raytraces of the standard atmospheres
!  for July for latitudes 15, 45, 60, and 75 degrees latitude and for
!  January for 30 degrees latitude (930517).

      data abcw2p0 /                                                    &
     & 5.8021897D-4,5.6794847D-4,5.8118019D-4,5.9727542D-4,6.1641693D-4,&
     & 1.4275268D-3,1.5138625D-3,1.4572752D-3,1.5007428D-3,1.7599082D-3,&
     & 4.3472961D-2,4.6729510D-2,4.3908931D-2,4.4626982D-2,5.4736038D-2/


      deg2rad = 3.14159265/180.

      l = ABS(latitu)

!  Coefficients for the continued fraction expansion for each latitude.

!  for latitudes less than 15 degrees:

      if (l .le. latwmf(1)) then
         a = abcw2p0(1,1)
         b = abcw2p0(1,2)
         c = abcw2p0(1,3)
      endif

!  for latitudes between 15 and 75  degrees:

      do i = 1,4
          if (l .gt. latwmf(i) .and. l .le. latwmf(i+1)) then
             dl = (l-latwmf(i))/(latwmf(i+1)-latwmf(i))
             da  =   abcw2p0(i+1,1)-abcw2p0(i,1)
             a   =   abcw2p0(i,1) + dl*da
!     write(*,'(" dl,da ,a  ",6e15.6)')
!    .            dl,da ,a

             db  =   abcw2p0(i+1,2)-abcw2p0(i,2)
             b   =   abcw2p0(i,2) + dl*db
!     write(*,'(" dl,db ,b ",6e15.6)')
!    .            dl,db ,b

             dc  =   abcw2p0(i+1,3)-abcw2p0(i,3)
             c   =   abcw2p0(i,3) + dl*dc
!     write(*,'(" dl,dc ,c ",6e15.6)')
!    .            dl,dc ,c

          endif
      end do

!  for latitudes greater than 75 degrees:

      if (l .ge. latwmf(5)) then
         a = abcw2p0(5,1)
         b = abcw2p0(5,2)
         c = abcw2p0(5,3)
      endif

!  Now the coefficients exist; calculate the mapping function, wmf(1),
!      and the change of mapping function with elevation,
!      dwmf/d_el =wmf(2).
!  To calculate the delay-rate correction, d_tau/dt:
!      d_tau/dt = d_tau_zen/dt * wmf(1) + tau_zen * dwmf/d_el * d_el/dt

      sine  = SIN( elev *deg2rad)
      cose  = COS( elev *deg2rad)
      beta  = b/( sine + c )
      gamma = a/( sine + beta)
      topcon = (1.D0 + a/(1.D0 + b/(1.D0 + c)))

!     write(*,'("sine, cose, beta, gamma, topcon = ", 5f10.5)')
!    .           sine, cose, beta, gamma, topcon

      wmf(1) = topcon / ( sine + gamma )

      wmf(2) = -topcon / ( sine + gamma )**2 *                          &
     &         ( cose - a/( sine + beta)**2 *cose *                     &
     &         ( 1.D0 - b/( sine + c )**2 ) )

!     write(*,'("wmf(1), wmf(2) = ", 2f10.4)') wmf(1), wmf(2)
!     write(*,'("wmf(1), wmf(2) = ", 2f10.4)') wmf

!aen   write out diagnostic info.
!     write(*,'("  elev, latitu, wmf2.0, dwmf/del = ",4f15.6)')
!    .             elev, latitu, wmf(1), wmf(2)

      return
      END
