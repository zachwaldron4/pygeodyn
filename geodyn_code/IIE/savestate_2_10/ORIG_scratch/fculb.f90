!TITLE

      DOUBLE PRECISION FUNCTION  fculb(latitude,height_m,doy, sine,tzd)
!********1*********2*********3*********4*********5*********6*********7**
! FCUL_A               00/00/00            0000.0    PGMR - V. MENDES
!
! FUNCTION:
! new vbm 31 Mar 2000 compute global total FCUL mapping function
!       no dependence on meteorological data
! function dependent on latitude, temperature
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  latitude  I          - latitude (degrees)
!  height_m  I          - height (metres)
!  doy       I          - day of year
!  sine      I          - sin of elevation angle
!  tzd       I          - total zenith delay (m)
!  fcul_a    O          - total delay (m)
!     local:
!          epsilon      - elevation angle (radians)
!          sine         - sin(elevation angle)
!          cosphi       - cos (latitude)
!          cosdoy       - cos (doy)
!
! COMMENTS:
!
! new vbm 31 Mar 2000 compute global total FCUL mapping function
!       no dependence on meteorological data
! Ref. Mendes, V.B., G. Prates, E.C. Pavlis, D.E. Pavlis,
!      and R.B. Langley (2001). "Improved mapping functions for
!      atmospheric refraction correction in SLR." Submitted to GRL.
!
!    These coefficients are based on a LS adjustment of 87766 (cleaned)
!    set of traces, based on Ciddor routines to compute refractivity,
!    according to IUGG recommendations (1999).
!    Questions and comments: vmendes@fc.ul.pt
!********1*********2*********3*********4*********5*********6*********7**

      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      DOUBLE PRECISION map_zen
      DOUBLE PRECISION latitude

      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY

!     conversions
!     epsilon = elev_deg * (pi/180)
!     sine    = dsin(epsilon)
!     add 182.5 to account for southern hemisphere
      if (latitude.lt.0.0D0) then
        doy_c   = doy + 365.25/2.0
        else
        doy_c   = doy
      end if
      cosdoy = COS ((doy_c - 28.0) * 2.0 * pi / 365.25)
      cosphi = COS (latitude*(pi/180.0))

!         coeficients for the model

      a10 =  0.116131D-02
      a11 = -0.9338D-5
      a12 = -0.5958D-8
      a13 = -0.24627D-07
      a14 =  0.12864D-03
!
      a20 =  0.298151D-02
      a21 = -0.569D-05
      a22 = -0.1655D-07
      a23 = -0.2725D-07
      a24 =  0.3020D-04
!
      a30 =  0.681839D-01
      a31 =  0.935D-04
      a32 = -0.2394D-06
      a33 =  0.304D-07
      a34 = -0.2308D-02

!     a, b, and c in Marini function
      a1 = a10 + a11*cosdoy + a12*latitude**2*cosdoy                    &
     &   + a13 * height_m + a14*cosphi
      a2 = a20 + a21*cosdoy + a22*latitude**2*cosdoy                    &
     &   + a23 * height_m + a24*cosphi
      a3 = a30 + a31*cosdoy + a32*latitude**2*cosdoy                    &
     &   + a33 * height_m + a34*cosphi

!     numerator in continued fraction
      map_zen   = (1.0D0 + a1/(1.0D0 + a2/(1.0D0+a3)))
!
      fculb = tzd*map_zen/(sine+a1/(sine+a2/(sine+a3)))

      RETURN
      END
