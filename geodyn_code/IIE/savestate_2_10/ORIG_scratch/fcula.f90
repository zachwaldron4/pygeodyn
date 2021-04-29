!TITLE

      DOUBLE PRECISION FUNCTION  fcula (latitude,height_m,T_K, sine,tzd)
!********1*********2*********3*********4*********5*********6*********7**
! FCULA               00/00/00            0000.0    PGMR - V. MENDES
!
! FUNCTION:
! new vbm 31 Mar 2000 compute global total FCUL2000 mapping function.
!      include 'model_params.h'
! function dependent on latitude, height, temperature
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  latitude  I          - latitude (degrees)
!  height_m  I          - height (metres)
!  T_K       I          - temperature (kelvin)
!  sine      I          - sin of elevation angle
!  tzd       I          - total zenith delay (m)
!  fcul_d    O          - total delay (m)
!
!     local:
!          epsilon      - elevation angle (radians)
!          T_C          - temperature (Celcius)
!          sine         - sin(elevation angle)
!          cosphi       - cos (latitude)
!
! COMMENTS:
!
! new vbm 31 Mar 2000 compute global total FCULa mapping function.
! function dependent on latitude, height, and surface temperature
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
!     epsilon = elev_deg * (pi/180.0)
!     sine    = dsin(epsilon)
      T_C     = T_K - 273.15D0
      cosphi  = COS (latitude*(pi/180.0))

!         coeficients for the model

      a10 =  0.121008D-02
      a11 =  0.17295D-05
      a12 =  0.3191D-04
      a13 = -0.18478D-07
!
      a20 =  0.304965D-02
      a21 =  0.2346D-05
      a22 = -0.1035D-03
      a23 = -0.1856D-07

!
      a30 =  0.68777D-01
      a31 =  0.1972D-04
      a32 = -0.3458D-02
      a33 =  0.1060D-06

!     a, b, and c in Marini continued fraction (Eq. 5)
      a1 = a10+a11*T_C+a12*cosphi+a13*height_m
      a2 = a20+a21*T_C+a22*cosphi+a23*height_m
      a3 = a30+a31*T_C+a32*cosphi+a33*height_m

!     numerator in continued fraction
      map_zen   = (1.0D0 + a1/(1.0D0 + a2/(1.0D0+a3)))
!
      fcula = tzd*map_zen/(sine+a1/(sine+a2/(sine+a3)))

      RETURN
      END
