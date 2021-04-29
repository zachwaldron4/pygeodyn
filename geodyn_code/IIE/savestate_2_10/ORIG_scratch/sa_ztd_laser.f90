      DOUBLE PRECISION FUNCTION sa_ztd_laser (latitude,ellip_ht, &
     &              pressure,wvp,     &
     &              lambda_um)

!********1*********2*********3*********4*********5*********6*********7**
! SA_TDZ_LASER            00/00/00            0000.0    PGMR - V. MENDES
!
! FUNCTION:
! This function calculates the Saastamoinen zenith total delay,
! for optical wavelengths
!
! I/O PARAMETERS:
!
!   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
!   ------  ---  ---   ------------------------------------------------
!  ellip_ht  I          - height (metres)
!  latitude  I          - latitude (degrees)
!  pressure  I          - surface pressure (hPa) (mbars)
!  wvp       I          - water vapor pressure (hPa) (mbars)
!  lambda_um I          - wavelength (m)
!  sa_ztd_laser O       - zenith total delay (m)
!
! COMMENTS:
!   !! CAUTION: needs this include file to get the wavelength !!
!     include 'model_params.h'
!
!********1*********2*********3*********4*********5*********6*********7**
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      SAVE

      DOUBLE PRECISION latitude,  k,  lambda_um

      COMMON/CONSTR/PI,TWOPI,DEGRAD,SECRAD,SECDAY

!     write(6,*) 'SA_ZTD_LASER DEBUG '
!     write(6,*) ' ELLIP_HEIGHT IN METERS ',ellip_ht
!     write(6,*) ' LATITUDE IN DEGREES ',latitude
!     write(6,*) ' PRESSURE IN MBARS ',pressure
!     write(6,*) ' WATER VAPOR PRESSURE IN MBARS ',wvp
!     write(6,*) ' WAVELENGTH ',LAMBDA_UM
!  correction factor for local latitude and station height
      f = 1 - 0.00266*COS(2*pi/180*latitude) - 0.00028D-3*ellip_ht

!  computing the wavelength-dependent factor
      k = 0.39406*(173.3D0+1/lambda_um**2)/(173.3D0-1/lambda_um**2)**2

!  get the total delay at the zenith
      sa_ztd_laser = k*(pressure+0.06D0*wvp)/f

      RETURN
      END
